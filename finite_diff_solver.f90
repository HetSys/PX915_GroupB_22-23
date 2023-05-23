!NOTE - COMPILE THIS PROGRAM WITH THE FOLLOWING LINE
!gfortran read_inputs.f90 nc_output.f90 finite_diff_solver.f90 -llapack -o finite_diff_solver
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
    CHARACTER(len=104) :: filename, output_name
    CHARACTER(len=5) :: file_extension
    INTEGER :: file_ext

    ! Read user inputs
    filename = read_command_line()
    CALL set_inputs(filename, tstep_init, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge)
    
    !generate name of output file
    filename_length = LEN_TRIM(filename)
    file_ext = INDEX(filename, '.')
    output_name = filename(1:file_ext-1)//'_output.nc'

    ALLOCATE(A(n,n))
    ALLOCATE(ipiv(n))
    ALLOCATE(A_copy(n,n))
    ALLOCATE(b(n))
    ALLOCATE(cstorage(n, tsteps))
    ALLOCATE(Z(tsteps))
    
    !allocate time axis
    ALLOCATE(time_axis(tsteps))
    !first state is at t=0
    time_axis(1) = 0.0_REAL64

    deltar = R/(REAL(n,kind=REAL64)-1.0_REAL64)
    k = -D/(2.0_REAL64*(deltar**2)) !k is just a shortcut holding -D/(2(dr^2))
    
    b = 0.0_REAL64
    
        
    Z = (-iapp)/(a_small*F*L*D)
    cstorage(:,tstep_init) = c !set first entry in storage vector to initial concentration
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
    !print*, 'a',A
    

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
        !print*, 'c',c

        !implicit scheme
        CALL dgesv(n,1,A,n,ipiv,b,n,info)
        !print*, 'a',A
        A = A_copy !dgesv affects matrix A
        !print*, 'a',A
        c = b
        !IF(info/=0) THEN
        !    PRINT*,'Inversion Failed'
        !ELSE
        !    PRINT*, 'Successful Inversion'
        !END IF
        !add solution to storage vector
        cstorage(:,tstep+1) = c
        time_axis(tstep+1) = dt*tstep
        
        CALL write_checkpoint(tstep, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge, filename, 20)
        
    END DO
    
    

    !write to output file
    OPEN(9,file='output.txt',form='formatted')
    DO i = 1,n
        write (9,*) i, cstorage(i,:)
        !print*, i, cstorage(i,:)
    END DO
    CLOSE(9)

    CALL output_cstorage(cstorage, n, tsteps, R, time_axis, electrode_charge, output_name)

    DEALLOCATE(A)
    DEALLOCATE(A_copy)
    DEALLOCATE(b)
    DEALLOCATE(c)
    DEALLOCATE(ipiv)
    DEALLOCATE(cstorage)
    DEALLOCATE(Z)
    DEALLOCATE(time_axis)
    IF (file_extension=='txt') DEALLOCATE(iapp)
    
END PROGRAM


!Suggested changes:

!Write to output file at each time step - saves memory requirements if small timestep
!Wouldn't need a matrix to store values.
!Probably less efficient.

!Simplify edge of sphere equation using d2c/dr2 = 0, due to substitution of equation dc/dr = Z(t) => d2c/dr2 = 0.
!Likely won't use this change - including term allows for consideration of non-zero d2c/dr2, i.e. numerical errors.


!Change log:

!Line 23 (original):
!deltar = R/(REAL(n,kind=REAL64)-1.0_REAL64)
!instead of
!deltar = R/REAL(n,kind=REAL64)
!We have n nodes, so divide by (n-1)

!Lines 39-48 (original):
!Discretisation/Mesh is constant over simulation
!Intialise matrix A moved outside of time loop and kept constant
!Also re-ordered relating to memory storage
!Notably - dgesv affects the matrix A, so a copy is required to store the values.
!Still stops unnecessary extra computation - but now at the cost of memory.

!ireal = real(i-1,kind=REAL64)
!instead of
!ireal = real(i,kind=REAL64)
!Multiplied underlying equations through by dt for stability
