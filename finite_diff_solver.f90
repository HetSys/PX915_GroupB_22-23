!> \mainpage
!> A single particle model (SPM) by Group B for the 2023 PX915 group project.
!> \section Intro Introduction
!> SPAMS models the charging and discharging of a lithium ion battery using a Crank-Nicolson semi-implicit finite
!> difference scheme to obtain the concentration of lithium in a sphere, c(iapp, r), at each time step, and includes the following features:
!>
!>   -  Apply a constant, stepwise or custom current
!>   -  Options for parallelism
!>   -  Extend the model to a full battery
!>   -  Uncertainty quantification
!>
!> \section Contributors 
!> Fraser Birks, Laura Cairns, Sebastian Dooley, Arielle Fitkin, Jake Eller, and Yu Lei
!>
!> HetSys CDT, University of Warwick


!NOTE - COMPILE THIS PROGRAM WITH THE FOLLOWING LINE
!gfortran read_inputs.f90 nc_output.f90 finite_diff_solver.f90 -llapack -o finite_diff_solver

!> @brief Crank-Nicolson finite differences solver
PROGRAM MAIN
    USE ISO_FORTRAN_ENV
    USE read_inputs
    USE nc_output
    USE checkpointing
    IMPLICIT NONE
    INTEGER :: n !node number
    INTEGER :: info, i, tstep, filename_length
    INTEGER, DIMENSION(:), ALLOCATABLE :: ipiv
    INTEGER :: tsteps ! user input
    INTEGER :: tstep_init ! initial timestep
    REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: A !solver matrix
    REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: A_copy !copy of solver matrix
    REAL(REAL64), DIMENSION(:), ALLOCATABLE :: c,b !c vector for solving 
    REAL(REAL64), DIMENSION(:,:), ALLOCATABLE :: cstorage
    REAL(REAL64) :: dt !finite diff t
    REAL(REAL64) :: deltar !dist between nodes (finite diff r)
    REAL(REAL64) :: D !diffusion coef
    REAL(REAL64) :: R !total width of block
    REAL(REAL64) :: k !D*dt/(dr**2)
    REAL(REAL64) :: ireal !real version of i for loop
    REAL(REAL64), DIMENSION(:), ALLOCATABLE :: iapp, Z, time_axis !applied current, in general a function of t, time_axis
    REAL(REAL64) :: a_small, L !constants
    REAL(REAL64), PARAMETER :: F = 96485_REAL64 !Faraday constant
    CHARACTER(len=1) :: electrode_charge
    CHARACTER(len=104) :: filename_txt, filename_nc, filename, output_name
    CHARACTER(len=5) :: file_extension
    INTEGER :: file_ext, file_test

    ! Read in user inputs
    !filename = read_command_line()
    CALL read_command_line_further(filename_txt, filename_nc)
    IF (filename_nc=='default.nc') then
        CALL set_inputs(filename_txt, tstep_init, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge, cstorage)
    ELSE
        CALL set_inputs_further(filename_txt,filename_nc, tstep_init, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge, cstorage)
    END IF
    filename=filename_txt
    !generate name of output file as 'filename_output.nc'
    !trim to remove preceeding directories and file extensions
    filename_length = LEN_TRIM(filename)
    file_test = INDEX(filename, '/') ! find '/' to remove preceeding directories
    DO WHILE (file_test/=0)
        filename = filename(file_test+1:)
        file_test = INDEX(filename, '/')
    END DO
    file_ext = INDEX(filename, '.') ! find '.' to remove file extension
    output_name = TRIM(ADJUSTL(filename(1:file_ext-1)))//'_output.nc'

    ALLOCATE(ipiv(n))
    ALLOCATE(A(n,n))
    ALLOCATE(A_copy(n,n))
    ALLOCATE(b(n))
    ALLOCATE(Z(tsteps))
    
    !allocate time axis and set up
    ALLOCATE(time_axis(tsteps))
    !first state is at t=0
    time_axis(1) = 0.0_REAL64
    DO tstep=1,(tsteps-1)
      time_axis(tstep+1) = dt*tstep
    END DO

    deltar = R/(REAL(n,kind=REAL64)-1.0_REAL64)
    k = -D/(2.0_REAL64*(deltar**2)) !k is just a shortcut holding -D/(2(dr^2))
    
    b = 0.0_REAL64
    
    Z = (iapp)/(a_small*F*L*D)
    !build A matrix for solver (constant over time)
    A = 0.0_REAL64
    !boundary condition
    !centre of sphere (left edge)
    A(1,1) = 1.0_REAL64 - 2.0_REAL64*k*dt
    A(1,2) = 2.0_REAL64*k*dt
    !Interior
    DO i = 2,n-1 !iterate through matrix, building each component 
        ireal = real(i-1,kind=REAL64) !(i-1) instead of i, due to r_1 = 0.0 (indexing from 1)
        A(i,i-1) = k*dt*(1.0_REAL64 - (1.0_REAL64/ireal))
        A(i,i) = 1.0_REAL64 - (2.0_REAL64*k*dt)
        A(i,i+1) = k*dt*(1.0_REAL64 + (1.0_REAL64/ireal))
    END DO
    !boundary condition
    !edge of sphere (right edge)
    A(n,n-1) = 2.0_REAL64*k*dt
    A(n,n) = (1.0_REAL64) + ((D/(deltar**2))*dt)
    A_copy = A    
    A_copy = A

    DO tstep = tstep_init,(tsteps-1) !we have the first state (j=1), and each loop finds the j+1th state, so we go to tsteps-1.
        !build solver
        !boundary condition
        !centre of sphere (left edge)
        b(1) = (D/(deltar**2))*(c(2) - c(1))*dt + (c(1))

        DO i = 2,n-1 !iterate through matrix, building each component 
            ireal = real(i-1,kind=REAL64)
            b(i) = (D/2.0_REAL64)*((c(i+1)-2*c(i)+c(i-1))/(deltar**2))*dt+&
            ((D/(ireal*deltar))*((c(i+1)-c(i-1))/(2*deltar)))*dt + c(i)
        END DO
        
        !boundary condition
        !edge of sphere (right edge)
        b(n) = ((D/R)*(Z(tstep+1) + Z(tstep)))*dt+((D*Z(tstep+1))/deltar)*dt &
        +(D/2.0_REAL64)*dt*(((2.0_REAL64*Z(tstep)*deltar)+(2.0_REAL64*c(N-1))-(2.0_REAL64*c(N)))/(deltar**2)) + c(n)

        !implicit scheme
        CALL dgesv(n,1,A,n,ipiv,b,n,info)
        A = A_copy !dgesv affects matrix A
        c = b
        cstorage(:,tstep+1) = c
        
        !By default, writes checkpoints every 10% of total timesteps elapsed. Can add a specific
        !number to the end of the function call to change how often checkpoints are written.
        
        CALL write_checkpoint(tstep, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge, cstorage, filename)
        
    END DO
    
    PRINT*, 'Finished solver, writing data to output file(s).....'

    CALL output_cstorage(cstorage, n, tsteps, R, time_axis, electrode_charge, tstep, dt, c, D, a_small, L, iapp, output_name)

    DEALLOCATE(A)
    DEALLOCATE(A_copy)
    DEALLOCATE(b)
    DEALLOCATE(c)
    DEALLOCATE(ipiv)
    DEALLOCATE(cstorage)
    DEALLOCATE(iapp)
    DEALLOCATE(Z)
    DEALLOCATE(time_axis)
    
END PROGRAM
