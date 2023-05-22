!> @brief Module to read and write checkpoint files.
!! @details Module contains functions that write checkpoint files and read them in as user input.
MODULE checkpointing

    USE netcdf
    USE iso_fortran_env
    USE read_inputs
    USE nc_output ! Contains 'check' function
    IMPLICIT none

    CONTAINS

    SUBROUTINE write_checkpoint(tstep_in, tsteps, c, n, A, dt, R, D, Z, electrode_charge, filename, freq_in)

        ! Variables to write
        INTEGER, INTENT(IN) :: tstep_in ! Current timestep
        INTEGER :: tstep
        INTEGER, INTENT(IN) :: tsteps ! Total timesteps
        INTEGER, INTENT(IN) :: n 
        REAL(REAL64), DIMENSION(n), INTENT(IN) :: c ! Conc at current timestep
        REAL(REAL64), DIMENSION(n,n), INTENT(IN) :: A
        REAL(REAL64), INTENT(IN) :: dt
        REAL(REAL64), INTENT(IN) :: R
        REAL(REAL64), INTENT(IN) :: D
        REAL(REAL64), DIMENSION(tsteps), INTENT(IN) :: Z
        CHARACTER(len=*), INTENT(IN) :: electrode_charge
        ! Frequency variables
        INTEGER, INTENT(IN), OPTIONAL :: freq_in
        INTEGER :: freq ! desired frequency of checkpoints
        INTEGER :: freq_remainder
        ! Execution variables
        CHARACTER(len=*), INTENT(INOUT) :: filename
        CHARACTER(len=60) :: dir, checkpoint_name
        CHARACTER(len=120) :: filepath
        INTEGER :: ncid, dimid_n, dimid_nxn(2), dimid_tsteps, dimids_parameter(1), status
        INTEGER :: varid_tstep, varid_tsteps, varid_c, varid_n, varid_A, varid_dt, varid_R, varid_D, varid_Z, varid_EC
        INTEGER :: filename_length, file_ext, file_test
        CHARACTER(len=10) :: tstep_str

        tstep = tstep_in + 1
        ! Determine if checkpoint desired at this timestep
        IF(PRESENT(freq_in)) THEN
            freq = freq_in
        ELSE
            freq = 10
        END IF
    
        ! Using integer arithmetic, find remainder of tstep/freq
        freq_remainder = tstep - (tstep/freq)*freq

        ! Output checkpoint file every freq timesteps, and output last timestep
        IF (freq_remainder == 0 .OR. tstep==tsteps) THEN

            ! Make directory to store checkpoints
            filename_length = LEN_TRIM(filename)
            file_test = INDEX(filename, '/')
            DO WHILE (file_test/=0)
                filename = filename(file_test+1:)
                file_test = INDEX(filename, '/')
            END DO

            file_ext = INDEX(filename, '.')
            dir = "checkpoints_"//filename(1:file_ext-1)//"/"
            CALL execute_command_line("mkdir -p "//dir)

            ! Set filepath
            WRITE(tstep_str, '(I0)') tstep
            ! tstep_str = TRIM(ADJUSTL(tstep_str))
            ! print*, len(tstep_str), len(trim(adjustl(tstep_str)))
            checkpoint_name = filename(1:file_ext-1)//"_tstep_"//TRIM(ADJUSTL(tstep_str))//".nc"
            filepath = TRIM(ADJUSTL(dir))//TRIM(ADJUSTL(checkpoint_name))
            


            ! Create NetCDF file
            CALL check(nf90_create(filepath, NF90_CLOBBER, ncid))
        
            ! Define dimensions
            CALL check(status = nf90_def_dim(ncid, "n", n, dimid_n))
            CALL check(status = nf90_def_dim(ncid, "tsteps", tsteps, dimid_tsteps))
            CALL check(status = nf90_def_dim(ncid, "parameter", 1, dimids_parameter(1)))
            dimid_nxn(1) = dimid_n
            dimid_nxn(2) = dimid_n
            
            ! Define variables
            CALL check(status = nf90_def_var(ncid, "tstep", NF90_INT, dimids_parameter, varid_tstep))
            CALL check(status = nf90_def_var(ncid, "tsteps", NF90_INT, dimids_parameter, varid_tsteps))
            CALL check(status = nf90_def_var(ncid, "c", NF90_DOUBLE, dimid_n, varid_c))
            CALL check(status = nf90_def_var(ncid, "node_num", NF90_INT, dimids_parameter, varid_n))
            CALL check(status = nf90_def_var(ncid, "A", NF90_DOUBLE, dimid_nxn, varid_A))
            CALL check(status = nf90_def_var(ncid, "dt", NF90_DOUBLE, dimids_parameter, varid_dt))
            CALL check(status = nf90_def_var(ncid, "R", NF90_DOUBLE, dimids_parameter, varid_R))
            CALL check(status = nf90_def_var(ncid, "D", NF90_DOUBLE, dimids_parameter, varid_D))
            CALL check(status = nf90_def_var(ncid, "Z", NF90_DOUBLE, dimid_tsteps, varid_Z))
            CALL check(status = nf90_def_var(ncid, "electrode_charge", NF90_CHAR, dimids_parameter, varid_EC))
            
            
            ! End define mode and switch to data mode
            CALL check(status = nf90_enddef(ncid))
        
            ! Write data to variable
            CALL check(status = nf90_put_var(ncid, varid_tstep, tstep))
            CALL check(status = nf90_put_var(ncid, varid_tsteps, tsteps))
            CALL check(status = nf90_put_var(ncid, varid_c, c))
            CALL check(status = nf90_put_var(ncid, varid_n, n))
            CALL check(status = nf90_put_var(ncid, varid_A, A))
            CALL check(status = nf90_put_var(ncid, varid_dt, dt))
            CALL check(status = nf90_put_var(ncid, varid_R, R))
            CALL check(status = nf90_put_var(ncid, varid_D, D))
            CALL check(status = nf90_put_var(ncid, varid_EC, electrode_charge))
        
            
            ! Close the file
            CALL check(status = nf90_close(ncid))
            PRINT *, "Success writing checkpoint file, "//filepath

        END IF

    END SUBROUTINE write_checkpoint


    SUBROUTINE read_checkpoint(filename, tstep, tsteps, n, c, A, dt, R, D, Z, electrode_charge)

        SAVE
        !> @var character len=* filename
        !! Name of input file.
        CHARACTER(len=*), INTENT(IN) :: filename

        ! Variables to read
        INTEGER, INTENT(OUT) :: tstep ! Last timestep
        INTEGER, INTENT(OUT) :: tsteps ! Total timesteps
        INTEGER, INTENT(OUT) :: n 
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: c ! Conc at last timestep
        REAL(REAL64), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: A
        REAL(REAL64), INTENT(OUT) :: dt
        REAL(REAL64), INTENT(OUT) :: R
        REAL(REAL64), INTENT(OUT) :: D
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: Z
        CHARACTER(len=*), INTENT(OUT) :: electrode_charge

        ! Execution variables
        INTEGER :: ncid, varid

        ! Open NetCDF file
        CALL check(nf90_open(filename, nf90_nowrite, ncid))

        ! Read tstep
        CALL check(nf90_inq_varid(ncid,'tstep',varid))
        CALL check(nf90_get_var(ncid, varid, tstep))
        
        ! Read tsteps
        CALL check(nf90_inq_varid(ncid,'tsteps',varid))
        CALL check(nf90_get_var(ncid, varid, tsteps))

        ! Read n
        CALL check(nf90_inq_varid(ncid,'node_num',varid))
        CALL check(nf90_get_var(ncid, varid, n))

        ! Read arrays
        ALLOCATE(c(n))
        ALLOCATE(A(n,n))
        ALLOCATE(Z(tsteps))
        ! Read c
        CALL check(nf90_inq_varid(ncid,'c',varid))
        CALL check(nf90_get_var(ncid, varid, c))

        ! Read parameters
        ! Read dt
        CALL check(nf90_inq_varid(ncid,'dt',varid))
        CALL check(nf90_get_var(ncid, varid, dt))

        ! Read n
        CALL check(nf90_inq_varid(ncid,'R',varid))
        CALL check(nf90_get_var(ncid, varid, R))

        ! Read D
        CALL check(nf90_inq_varid(ncid,'D',varid))
        CALL check(nf90_get_var(ncid, varid, D))

        ! Read electrode_charge
        CALL check(nf90_inq_varid(ncid,'electrode_charge',varid))
        CALL check(nf90_get_var(ncid, varid, electrode_charge))


        ! Close the file
        CALL check(status = nf90_close(ncid))
        ! PRINT *, "Success reading checkpoint file, "//filename

    END SUBROUTINE read_checkpoint



    ! FUNCTION set_inputs(filename)

    !     !> @var character len=* filename
    !     !! Name of input file.
    !     CHARACTER(len=*), INTENT(IN) :: filename
    !     !> @var character len=5 file_extension
    !     !! Parsed file extension to choose checkpoint file or user input parameters
    !     CHARACTER(len=5) :: file_extension
    !     !> @var integer parse_idx
    !     !! Index of parser in string
    !     INTEGER :: parse_idx

    !     !> 1. Find index of '.' in filename to identify file extension
    !     parse_idx = INDEX(filename, ".") +1
    !     file_extension = filename(parse_idx:)
        
    !     !> 2. Choose input reader based on file extension.
    !     SELECT CASE(file_extension)

    !         CASE("txt")
    !             input_params = read_user_inputs(filename)

    !         CASE("nc")
    !             PRINT*, "Checkpoint reader called. This is a test."
    !             STOP 6
    !             !inputs_params = ! NEW FUNCTION

    !         CASE DEFAULT
    !             PRINT*, "Invalid file type passed to SPM solver."
    !             PRINT*, "Please pass a 'txt' file of user input parameters or a 'nc' checkpoint file."
    !             STOP 6

    !     END SELECT

    ! END FUNCTION set_inputs

    
    ! FUNCTION read_checkpoint_input(filename) RESULT(input_params)
    !     !> @var type(UI) input_params
    !     !! Result of function containing all inputs for return to solver.
    !     TYPE(UI) :: input_params
    !     !> @var character len=* filename
    !     !! Name of checkpoint file.
    !     CHARACTER(len=*), INTENT(IN) :: filename

    ! END FUNCTION read_checkpoint_input
        

END MODULE checkpointing