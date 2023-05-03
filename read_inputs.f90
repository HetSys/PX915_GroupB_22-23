MODULE read_inputs

    USE iso_fortran_env
    IMPLICIT none
    SAVE

    ! User input variables, for return from read_inputs function
    TYPE :: UI
        INTEGER :: tsteps ! Number of timesteps, dimensionless
        REAL(REAL64) :: dt ! Time step s
        REAL(REAL64) :: c0 ! Initial concentration mol m^-3
        REAL(REAL64) :: D ! Diffusion coefficient m^2 s^-1
        REAL(REAL64) :: R ! Total width of block
        REAL(REAL64) :: a_small ! Particle surface area per unit volume m^-1, Constant in r=R BC
        REAL(REAL64) :: L ! Electrode thickness m, Constant in r=R BC 
        REAL(REAL64), DIMENSION(:), ALLOCATABLE :: iapp ! Applied current A m^-2
    END TYPE

    CONTAINS 

    FUNCTION read_user_inputs(filename)

        TYPE(UI) :: read_user_inputs
        CHARACTER(len=54), INTENT(IN) :: filename
        INTEGER, PARAMETER :: file_id = 1

        INTEGER, PARAMETER :: n_params = 7
        INTEGER :: i ! Loop variable
        INTEGER :: ios ! Error variable

        CHARACTER(len=30), DIMENSION(n_params+2) :: user_input_vars ! Array of strings read from file, to be parsed
        CHARACTER(len=1), PARAMETER :: parser='=' ! Character to parse input strings
        INTEGER :: parse_idx
        CHARACTER(len=25) :: name, val ! Parsed strings of variable name and value
        

        OPEN(file_id, file=filename, iostat=ios, action='READ', status='OLD') ! 'old' checks for existing file, error else
        IF (ios/=0) THEN
            PRINT*, "Error opening file: ", filename
            STOP 1
        END IF

        ! Read in parameters
        DO  i = 1, n_params+2
            READ(file_id, '(A)', iostat=ios) user_input_vars(i) ! Read entire line into user_input_vars. (A) selects entire line, else splits at spaces + commas
        END DO

        DO  i = 1, n_params
            parse_idx = INDEX(user_input_vars(i), parser) + 2 ! Find index of parser character
            READ(user_input_vars(i)(1:parse_idx-1),*) name
            READ(user_input_vars(i)(parse_idx:),*) val
            ! print*, name, val
            SELECT CASE(name)

                CASE('tsteps')
                    READ(val,*) read_user_inputs%tsteps
                CASE('dt')
                    READ(val,*) read_user_inputs%dt
                CASE('c0')
                    READ(val,*) read_user_inputs%c0
                CASE('D')
                    READ(val,*) read_user_inputs%D
                CASE('R')
                    READ(val,*) read_user_inputs%R
                CASE('a')
                    READ(val,*) read_user_inputs%a_small
                CASE('L')
                    READ(val,*) read_user_inputs%L

            END SELECT

        END DO

        ! Read in iapp array
        ALLOCATE(read_user_inputs%iapp(read_user_inputs%tsteps))
        i = 1
        DO
            IF (i==read_user_inputs%tsteps) EXIT
            READ(file_id, *, iostat=ios) read_user_inputs%iapp(i)
            i = i+1
        END DO

    END FUNCTION

    FUNCTION read_command_line() RESULT(filename)

        INTEGER :: num_args, i, parse_idx
        CHARACTER(len=60) :: arg, name, val
        CHARACTER(len=54) :: filename 

        num_args = COMMAND_ARGUMENT_COUNT()
        IF (num_args > 0) THEN

            DO i = 1, num_args
                CALL get_command_argument(1, arg)
                parse_idx = INDEX(arg, '=')
                READ(arg(1:parse_idx-1),*) name
                READ(arg(parse_idx+1:len(arg)),*) val
                
                SELECT CASE(name)
                    CASE('filename')
                        READ(val,*) filename
                END SELECT

            END DO

        END IF

    END FUNCTION


END MODULE read_inputs
