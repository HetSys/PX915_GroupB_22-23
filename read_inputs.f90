    !> @brief Module to read user inputs.
    !! @details Module contains functions that read user inputs from file and command line.
    MODULE read_inputs

        USE iso_fortran_env
        IMPLICIT none
        SAVE

        !> @var Type UI
        !! Custom type containing user input variables, for return to solver from read_inputs function.
        TYPE :: UI
            !> @var integer tsteps
            !! Number of timesteps (dimensionless)
            INTEGER :: tsteps
            !> @var real dt
            !! Time step (s)
            REAL(REAL64) :: dt
            !> @var real c0
            !! Initial concentration (mol m^-3)
            REAL(REAL64) :: c0
            !> @var real D
            !! Diffusion coefficient (m^2 s^-1)
            REAL(REAL64) :: D
            !> @var real R
            !! Total width of block (m)
            REAL(REAL64) :: R 
            !> @var real a_small
            !! Particle surface area per unit volume (m^-1), Constant in r=R boundary condit
            REAL(REAL64) :: a_small
            !> @var real L
            !! Electrode thickness (m), Constant in r=R boundary condit
            REAL(REAL64) :: L
            !> @var allocatable real array iapp
            !! Applied current (A m^-2)
            !! Will be length tsteps
            REAL(REAL64), DIMENSION(:), ALLOCATABLE :: iapp
            !> @var chararacter len=1 electrode_charge
            !! Label of electrode charge, 'p'=positive, 'n'=negative
            CHARACTER(len=1) :: electrode_charge
        END TYPE

        CONTAINS

        !>@brief Read command line function
        !! @details Function:
        !! 1. Reads in command line arguments.
        !! 2. Parses each argument into 'name' '=' 'value'.
        !! 3. Identifies argument called filename.
        !! 4. Passes filename as result.
        !! @result filename: Name of file containing user inputs.
        FUNCTION read_command_line() RESULT(filename)

            !> @var integer num_args
            !! Number of command line arguments found
            INTEGER :: num_args
            !> @var integer i
            !! Loop variable
            INTEGER :: i
            !> @var integer parse_idx
            !! Index of parser character '=' in command line argument
            INTEGER :: parse_idx
            !> @var character len=60 arg
            !! String to contain command line read in.
            CHARACTER(len=60) :: arg
            !> @var character len=60 name
            !! String to contain name of argument after parsing.
            CHARACTER(len=60) :: name
            !> @var character len=60 val
            !! String to contain value of argument after parsing.
            CHARACTER(len=60) :: val
            !> @var character len=54 filename
            !! Name of file containing user inputs. Result of function.
            CHARACTER(len=54) :: filename 


            num_args = COMMAND_ARGUMENT_COUNT()
            !> If command line arguments are present, parse each into name and value.
            !! Then identify argument by name, and perform action based on value.
            !! If no arguments, or unrecognised argument, stop execution of code and print error.
            IF (num_args > 0) THEN

                DO i = 1, num_args
                    CALL get_command_argument(1, arg)
                    parse_idx = INDEX(arg, '=')
                    READ(arg(1:parse_idx-1),*) name
                    READ(arg(parse_idx+1:len(arg)),*) val
                    
                    SELECT CASE(name)
                        CASE('filename')
                            READ(val,*) filename
                        CASE DEFAULT
                            PRINT*, "Command line argument not recognised: ", name
                            STOP 10
                    END SELECT

                END DO

            ELSE
                PRINT*, "No command line arguments found. Please ensure that argument filename is present."
                STOP 10

            END IF

        END FUNCTION read_command_line


        !> @brief Function reads user inputs from file.
        !! @details Function reads txt file 'filename', parsing and returning input parameters.
        !! Txt file should contain parameters tsteps, dt, c0, D, R, a, L, and electrode_charge, in format 'parameter = value'.
        !! These parameters can be listed in any order above a line of asterixes ***.
        !! Below the asterix line should be iapp, with a label line followed by an array printed one element per line.
        !! @param[in] filename: Name of user input file.
        !! @result input_params: Type UI containing all user inputs.
        FUNCTION read_user_inputs(filename) RESULT(input_params)

            !> @var type(UI) input_params
            !! Result of function containing all inputs for return to solver.
            TYPE(UI) :: input_params
            !> @var character len=* filename
            !! Name of user input file.
            CHARACTER(len=*), INTENT(IN) :: filename
            !> @var integer parameter file_id
            !! ID of file for read actions.
            INTEGER, PARAMETER :: file_id = 1

            !> @var integer parameter
            !! Expected number of parameters to read in: tsteps, dt, c0, D, R, a, L, and electrode_charge
            INTEGER, PARAMETER :: n_params_expected = 8
            !> @var integer n_params
            !! Counts number of parameters read in, initialised to 1.
            INTEGER :: n_params = 1
            !> @var integer i
            !! Loop variable
            INTEGER :: i
            !> @var integer ios
            !! Contains read errors.
            INTEGER :: ios
            !> @var logical asterix
            !! Error checking for read in: Has asterix line been read?
            LOGICAL :: asterix = .FALSE.
            !> @var logical invalid_param
            !! Error checking for read in: Is the value of any parameter invalid?
            LOGICAL :: invalid_param = .FALSE.
            !> @var character len=n_params_expected param_read
            !! Error checking string to ensure each parameter is read in.
            CHARACTER (len=n_params_expected) :: param_read ! 

            !CHARACTER(len=30), DIMENSION(n_params_expected+2) :: user_input_vars ! Array of strings read from file, to be parsed
            !> @var character len=30 read_temp
            !! String to hold read in line before parsing.
            CHARACTER(len=30) :: read_temp
            !> @var character len=1, parameter parser
            !! Character to parse input strings
            CHARACTER(len=1), PARAMETER :: parser='='
            !> @var integer parse_idx
            !! Index of parser in string
            INTEGER :: parse_idx
            !> @var character len=25 name
            !! Parsed string of variable name
            CHARACTER(len=25) :: name
            !> @var character len=25 val
            !! Parsed string of variable value
            CHARACTER(len=25) :: val


            !> 1. Open input file.
            OPEN(file_id, file=filename, iostat=ios, action='READ', status='OLD') ! 'old' checks for existing file, error else
            IF (ios/=0) THEN
                PRINT*, "Error opening file: ", filename
                STOP 1
            END IF

            !> 2. Initiate and label error process if parameter has incorrect type.
            1 IF (ios/=0) CALL invalid_param_type(name, n_params)

            !> 3. Read in parameters until asterix line (***) found.
            DO WHILE (.NOT. asterix)
                !> 3.1. Read entire line into user_input_vars. (A) selects entire line, else splits at spaces and commas.
                READ(file_id, '(A)', iostat=ios) read_temp

                !> 3.2. Check if line is asterix line. Ends loop when true.
                IF (INDEX(read_temp,'***')/= 0) asterix = .TRUE.

                !> 3.3. Find index of parser character.
                parse_idx = INDEX(read_temp, parser) + 2  

                !> 3.4. Ensure line has correct format and can be parsed: parameter = value.
                !! Stop execution if cannot be parsed, and not asterix line.
                IF (parse_idx==2 .AND. .NOT. asterix) THEN
                    PRINT*, "Invalid input detected in input file: line", n_params, ", ", filename
                    PRINT*, "Please ensure all parameters are in format: parameter = value"
                    STOP 2
                END IF

                !> 3.5. Parse line into name and val.
                READ(read_temp(1:parse_idx-1),*) name
                READ(read_temp(parse_idx:),*) val

                !> 3.6. Identify parameter and check validity.
                !! If type invalid, invalid_param_type called; prints error and stops execution.
                !! If value invalid, prints error and sets invalid_param to true. Execution stopped after parsing all parameters.
                !! Checks for other invalid variables with correct parsing format; if true, prints error and stops execution.
                SELECT CASE(name)

                    CASE('tsteps')
                        READ(val,*, iostat=ios, err=1) input_params%tsteps
                        IF (input_params%tsteps<1) THEN
                            PRINT*, "Invalid parameter: Number of timesteps, &
                            &tsteps, must greater than zero."
                            invalid_param = .TRUE.
                        END IF
                        param_read = '1'//param_read

                    CASE('dt')
                        READ(val,*, iostat=ios, err=1) input_params%dt
                        IF (input_params%dt<=0) THEN
                            PRINT*, "Invalid parameter: Size of timestep, &
                            &dt, must have a positive, non zero value."
                            invalid_param = .TRUE.
                        END IF
                        param_read = '2'//param_read

                    CASE('c0')
                        READ(val,*, iostat=ios, err=1) input_params%c0
                        IF (input_params%c0<0) THEN
                            PRINT*, "Invalid parameter: Initial concentration, &
                            &c0, must have a positive value."
                            invalid_param = .TRUE.
                        END IF
                        param_read = '3'//param_read

                    CASE('D')
                        READ(val,*, iostat=ios, err=1) input_params%D
                        ! No validation required
                        param_read = '4'//param_read

                    CASE('R')
                        READ(val,*, iostat=ios, err=1) input_params%R
                        IF (input_params%R<=0) THEN
                            PRINT*, "Invalid parameter: Width of block, &
                            &R, must have a positive, non zero value."
                            invalid_param = .TRUE.
                        END IF
                        param_read = '5'//param_read

                    CASE('a')
                        READ(val,*, iostat=ios, err=1) input_params%a_small
                        IF (input_params%a_small<=0) THEN
                            PRINT*, "Invalid parameter: Particle surface area &
                            &per unit volume, a, must have a positive, non zero value."
                            invalid_param = .TRUE.
                        END IF
                        param_read = '6'//param_read

                    CASE('L')
                        READ(val,*, iostat=ios, err=1) input_params%L
                        IF (input_params%L<=0) THEN
                            PRINT*, "Invalid parameter: Electrode thickness, &
                            &L, must have a positive, non zero value."
                            invalid_param = .TRUE.
                        END IF
                        param_read = '7'//param_read

                    CASE('electrode_charge')
                        READ(val,*, iostat=ios, err=1) input_params%electrode_charge
                        IF (.NOT.(input_params%electrode_charge=='n' .OR. input_params%electrode_charge=='p')) THEN
                            PRINT*, "Invalid parameter: Electrode charge label, &
                            &electrode_charge, must have value 'p' for positive or 'n' for negative."
                            invalid_param = .TRUE.
                        END IF
                        param_read = '8'//param_read

                    CASE('*')
                        asterix = .TRUE.
                    
                    CASE DEFAULT
                        PRINT*, "Unexpected parameter encountered in input file, line",n_params, ", ", filename
                        PRINT*, "Please ensure only parameters tsteps, dt, c0, D, R, a, L,&
                        & and electrode_charge are specified above ************."
                        STOP 2

                END SELECT

                n_params = n_params + 1

            END DO

            !> 4. Check if any parameters are missing; has expected number been read.
            DO i = 1, n_params_expected
                WRITE(read_temp,*) i
                IF (SCAN(param_read, read_temp) == 0) THEN
                    PRINT*, "Parameter missing. Please ensure input file contains tsteps, dt, c0, D, R, a, L, and electrode_charge."
                    invalid_param = .TRUE.
                END IF
            END DO

            !> 5. Exit if any parameters are invalid or missing.
            IF (invalid_param) STOP 4
        

            !> 6. Read iapp label line.
            READ(file_id, '(A)', iostat=ios) read_temp

            !> 7. Read in iapp array.
            ALLOCATE(input_params%iapp(input_params%tsteps))
            i = 1
            name = "iapp"
            !> 7.1. Initiate and label error process if iapp has incorrect type.
            2 IF (ios/=0) CALL invalid_param_type(name, i+n_params)
            
            !> 7.2. Loop through tsteps lines of file, reading into array iapp.
            DO
                READ(file_id, *, iostat=ios, err=2) input_params%iapp(i)
                i = i+1
                IF (i>input_params%tsteps) EXIT
            END DO
            
            !> 8. Check no lines missed in file; is iapp in file longer than tsteps?
            !! If read is successful then iapp is too long in file. Prints error and stops execution.
            READ(file_id, '(A)', iostat=ios) read_temp
            IF (ios==0) THEN
                PRINT*, "Applied current, iapp, is too long; it must be an array of length tsteps,", input_params%tsteps
                STOP 5
            END IF


        END FUNCTION


        !> @brief Invalid parameter error handling subroutine.
        !! @details Subroutine to be called if an invalid parameter is read from file.
        !! 1. Prints name of invalid parameter and line of user input file where this occurs.
        !! 2. Prints reason parameter is invalid.
        !! 3. Stops execution of solver.
        !! @param[in] name: Name of invalid parameter
        !! @param[in] line: Line in input file containing invalid parameter
        SUBROUTINE invalid_param_type(name, line)

            !> @var character len=25 name
            !! Name of invalid parameter
            CHARACTER(len=25), INTENT(IN) :: name
            !> @var integer line
            !! Line in input file containing invalid parameter
            INTEGER, INTENT(IN) :: line

            PRINT*, "Error reading parameter: ", name
            PRINT*, "Occured on line: ", line
        
            SELECT CASE(name)

                CASE('tsteps')
                    PRINT*, "Number of timesteps, tsteps, must be an integer."
                CASE('dt')
                    PRINT*, "Size of timestep, dt, must be a float/real value."
                CASE('c0')
                    PRINT*, "Initial concentration, c0, must be a float/real value."
                CASE('D')
                    PRINT*, "Diffusion coefficient, D, must be a float/real value."
                CASE('R')
                    PRINT*, "Width of block, R, must be a float/real value."
                CASE('a')
                    PRINT*, "Particle surface area per unit volume, a, must be a float/real value."
                CASE('L')
                    PRINT*, "Electrode thickness, L, must be a float/real value."
                CASE('iapp')
                    PRINT*, "Applied current, iapp, must be an array of float/real values of length tsteps."
                CASE('electrode_charge')
                    PRINT*, "Label of electrode charge, electrode_charge, must be a string of len=1."

            END SELECT

            STOP 3

        END SUBROUTINE


    END MODULE read_inputs
