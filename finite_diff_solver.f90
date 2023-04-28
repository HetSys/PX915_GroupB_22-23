!NOTE - COMPILE THIS PROGRAM WITH THE FOLLOWING LINE
!gfortran finite_diff_solver.f90 -llapack -o finite_diff_solver
PROGRAM MAIN 
    USE ISO_FORTRAN_ENV
    USE nc_output
    IMPLICIT NONE
    INTEGER, PARAMETER :: n=2500 !node number
    INTEGER :: info, i, tstep
    INTEGER, DIMENSION(n) :: ipiv
    INTEGER, PARAMETER :: tsteps = 100
    REAL(REAL64), DIMENSION(n,n) :: A !solver matrix
    REAL(REAL64), DIMENSION(n) :: c,b !c vector for solving 
    REAL(REAL64), DIMENSION(n,tsteps) :: cstorage
    REAL(REAL64) :: c0 !initial c value
    REAL(REAL64) :: dt !finite diff t
    REAL(REAL64) :: deltar !dist between nodes (finite diff r)
    REAL(REAL64) :: D !diffusion coef
    REAL(REAL64) :: R !total width of block
    REAL(REAL64) :: k !D*dt/(dr**2)
    REAL(REAL64) :: ireal !real version of i for loop
    REAL(REAL64), DIMENSION(tsteps) :: iapp, Z !applied current, in general a function of t
    REAL(REAL64) :: a_small, F, L !constants
    R  = 5.22_REAL64*(10.0_REAL64**(-6))
    deltar = R/real(n,kind=REAL64)
    dt = 0.1_REAL64 !set same as delta r for now
    D = (3.3_REAL64*(10.0_REAL64**(-13)))
    k = -(D)/(2.0_REAL64*(deltar**2)) !k is just a shortcut holding -D/(2(dr^2))
    
    c0 = 0.0_REAL64
    c = c0 !set c to initial state
    b = 0.0_REAL64
    a_small = 382183.9_REAL64
    F = 96485_REAL64
    L = (75.6_REAL64)*(10.0_REAL64**(-6))
    iapp(1:tsteps/2) = 0.73_REAL64*(10.0_REAL64**(-3))
    iapp(tsteps/2:tsteps) = -0.73_REAL64*(10.0_REAL64**(-3))
    Z = (-iapp)/(a_small*F*L*D)
    cstorage(:,1) = c0 !set first entry in storage vector to initial concentration
    DO tstep = 1,(tsteps-1) !we have the first state (j=1), and each loop finds the j+1th state, so we go to tsteps-1.
        !build solver
        A = 0.0_REAL64
        DO i = 2,n-1 !iterate through matrix, building each component 
            ireal = real(i,kind=REAL64)
            A(i,i-1) = k*(1.0_REAL64 - (1.0_REAL64/ireal))
            A(i,i) = (1.0_REAL64/dt - (2.0_REAL64)*(k))
            A(i,i+1) = k*(1.0_REAL64 + (1.0_REAL64/ireal))
            b(i) = (D/2.0_REAL64)*((c(i+1)-2*c(i)+c(i-1))/(deltar**2))+&
            ((D/(ireal*deltar))*((c(i+1)-c(i-1))/(2*deltar))) + c(i)/dt
        END DO
        
        !boundary conditions
        !centre of sphere (left edge)
        A(1,1) = (1.0_REAL64/dt) - 2.0_REAL64*k
        A(1,2) = 2.0_REAL64*k
        b(1) = (D/(deltar**2))*(c(2) - c(1)) + (c(1)/dt)

        !edge of sphere (right edge)
        A(n,n-1) = 2.0_REAL64*k
        A(n,n) = (1.0_REAL64/dt) + (D/(deltar**2))
        b(n) = ((D/R)*(Z(tstep+1) + Z(tstep)))+((D*Z(tstep+1))/deltar)&
        +(D/2.0_REAL64)*(((2.0_REAL64*Z(tstep)*deltar)+(2.0_REAL64*c(N-1))-(2.0_REAL64*c(N)))/(deltar**2))
        !print*, 'a',A
        !print*, 'c',c

        !implicit scheme
        CALL dgesv(n,1,A,n,ipiv,b,n,info)    
        c = b
        !IF(info/=0) THEN
        !    PRINT*,'Inversion Failed'
        !ELSE
        !    PRINT*, 'Successful Inversion'
        !END IF
        !add solution to storage vector
        cstorage(:,tstep+1) = c
    END DO
    

    !write to output file
    OPEN(9,file='output.txt',form='formatted')
    DO i = 1,n
        write (9,*) i, cstorage(i,:)
        !print*, i, cstorage(i,:)
    END DO
    CLOSE(9)
    CALL output_cstorage(cstorage, n, tsteps, "cstorage.nc")

END PROGRAM