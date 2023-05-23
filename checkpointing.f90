!> @brief Module to read and write checkpoint files.
!! @details Module contains functions that write checkpoint files and read them in as user input.
MODULE checkpointing

    USE netcdf
    USE iso_fortran_env
    USE read_inputs
    USE nc_output ! Contains 'check' function
    IMPLICIT none

    CONTAINS

    SUBROUTINE write_checkpoint(tstep_in, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge, filename, freq_in)

        ! Variables to write
        INTEGER, INTENT(IN) :: tstep_in ! Current timestep
        INTEGER :: tstep
        INTEGER, INTENT(IN) :: tsteps ! Total timesteps
        REAL(REAL64), INTENT(IN) :: dt
        INTEGER, INTENT(IN) :: n 
        REAL(REAL64), DIMENSION(n), INTENT(IN) :: c ! Conc at current timestep
        REAL(REAL64), INTENT(IN) :: D
        REAL(REAL64), INTENT(IN) :: R
        REAL(REAL64), INTENT(IN) :: a_small
        REAL(REAL64), INTENT(IN) :: L
        REAL(REAL64), DIMENSION(tsteps), INTENT(IN) :: iapp
        CHARACTER(len=*), INTENT(IN) :: electrode_charge
        ! Frequency variables
        INTEGER, INTENT(IN), OPTIONAL :: freq_in
        INTEGER :: freq ! desired frequency of checkpoints
        INTEGER :: freq_remainder
        ! Execution variables
        CHARACTER(len=*), INTENT(INOUT) :: filename
        CHARACTER(len=60) :: dir, checkpoint_name
        CHARACTER(len=120) :: filepath
        INTEGER :: ncid, dimid_n, dimid_tsteps, dimids_parameter(1), status
        INTEGER :: varid_tstep, varid_tsteps, varid_dt, varid_n, varid_c
        INTEGER :: varid_D, varid_R, varid_a, varid_L, varid_iapp, varid_EC
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
            checkpoint_name = filename(1:file_ext-1)//"_tstep_"//TRIM(ADJUSTL(tstep_str))//".nc"
            filepath = TRIM(ADJUSTL(dir))//TRIM(ADJUSTL(checkpoint_name))
            


            ! Create NetCDF file
            CALL check(nf90_create(filepath, NF90_CLOBBER, ncid))
        
            ! Define dimensions
            CALL check(status = nf90_def_dim(ncid, "n", n, dimid_n))
            CALL check(status = nf90_def_dim(ncid, "tsteps", tsteps, dimid_tsteps))
            CALL check(status = nf90_def_dim(ncid, "parameter", 1, dimids_parameter(1)))
            
            ! Define variables
            CALL check(status = nf90_def_var(ncid, "tstep", NF90_INT, dimids_parameter, varid_tstep))
            CALL check(status = nf90_def_var(ncid, "tsteps", NF90_INT, dimids_parameter, varid_tsteps))
            CALL check(status = nf90_def_var(ncid, "dt", NF90_DOUBLE, dimids_parameter, varid_dt))
            CALL check(status = nf90_def_var(ncid, "node_num", NF90_INT, dimids_parameter, varid_n))
            CALL check(status = nf90_def_var(ncid, "c", NF90_DOUBLE, dimid_n, varid_c))
            CALL check(status = nf90_def_var(ncid, "D", NF90_DOUBLE, dimids_parameter, varid_D))
            CALL check(status = nf90_def_var(ncid, "R", NF90_DOUBLE, dimids_parameter, varid_R))
            CALL check(status = nf90_def_var(ncid, "a_small", NF90_DOUBLE, dimids_parameter, varid_a))
            CALL check(status = nf90_def_var(ncid, "L", NF90_DOUBLE, dimids_parameter, varid_L))
            CALL check(status = nf90_def_var(ncid, "iapp", NF90_DOUBLE, dimid_tsteps, varid_iapp))
            CALL check(status = nf90_def_var(ncid, "electrode_charge", NF90_CHAR, dimids_parameter, varid_EC))
            
            
            ! End define mode and switch to data mode
            CALL check(status = nf90_enddef(ncid))
        
            ! Write data to variable
            CALL check(status = nf90_put_var(ncid, varid_tstep, tstep))
            CALL check(status = nf90_put_var(ncid, varid_tsteps, tsteps))
            CALL check(status = nf90_put_var(ncid, varid_dt, dt))
            CALL check(status = nf90_put_var(ncid, varid_n, n))
            CALL check(status = nf90_put_var(ncid, varid_c, c))
            CALL check(status = nf90_put_var(ncid, varid_D, D))
            CALL check(status = nf90_put_var(ncid, varid_R, R))
            CALL check(status = nf90_put_var(ncid, varid_a, a_small))
            CALL check(status = nf90_put_var(ncid, varid_L, L))
            CALL check(status = nf90_put_var(ncid, varid_iapp, iapp))           
            CALL check(status = nf90_put_var(ncid, varid_EC, electrode_charge))
        
            
            ! Close the file
            CALL check(status = nf90_close(ncid))
            PRINT *, "Success writing checkpoint file, "//filepath

        END IF

    END SUBROUTINE write_checkpoint


    SUBROUTINE read_checkpoint(filename, tstep_init, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge)

        SAVE
        !> @var character len=* filename
        !! Name of input file.
        CHARACTER(len=*), INTENT(IN) :: filename

        ! Variables to read
        INTEGER, INTENT(OUT) :: tstep_init ! Last timestep
        INTEGER, INTENT(OUT) :: tsteps ! Total timesteps
        REAL(REAL64), INTENT(OUT) :: dt
        INTEGER, INTENT(OUT) :: n 
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: c ! Conc at last timestep
        REAL(REAL64), INTENT(OUT) :: D
        REAL(REAL64), INTENT(OUT) :: R
        REAL(REAL64), INTENT(OUT) :: a_small
        REAL(REAL64), INTENT(OUT) :: L
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: iapp
        CHARACTER(len=*), INTENT(OUT) :: electrode_charge

        ! Execution variables
        INTEGER :: ncid, varid

        ! Open NetCDF file
        CALL check(nf90_open(filename, nf90_nowrite, ncid))

        ! Read tstep
        CALL check(nf90_inq_varid(ncid,'tstep',varid))
        CALL check(nf90_get_var(ncid, varid, tstep_init))
        
        ! Read tsteps
        CALL check(nf90_inq_varid(ncid,'tsteps',varid))
        CALL check(nf90_get_var(ncid, varid, tsteps))

        ! Read dt
        CALL check(nf90_inq_varid(ncid,'dt',varid))
        CALL check(nf90_get_var(ncid, varid, dt))

        ! Read n
        CALL check(nf90_inq_varid(ncid,'node_num',varid))
        CALL check(nf90_get_var(ncid, varid, n))


        ! Read arrays
        ALLOCATE(c(n))
        ALLOCATE(iapp(tsteps))
        ! Read c
        CALL check(nf90_inq_varid(ncid,'c',varid))
        CALL check(nf90_get_var(ncid, varid, c))

        ! Read iapp
        CALL check(nf90_inq_varid(ncid,'iapp',varid))
        CALL check(nf90_get_var(ncid, varid, iapp))

        ! Read parameters
        ! Read D
        CALL check(nf90_inq_varid(ncid,'D',varid))
        CALL check(nf90_get_var(ncid, varid, D))

        ! Read R
        CALL check(nf90_inq_varid(ncid,'R',varid))
        CALL check(nf90_get_var(ncid, varid, R))

        ! Read a_small
        CALL check(nf90_inq_varid(ncid,'a_small',varid))
        CALL check(nf90_get_var(ncid, varid, a_small))

        ! Read L
        CALL check(nf90_inq_varid(ncid,'L',varid))
        CALL check(nf90_get_var(ncid, varid, L))

        ! Read electrode_charge
        CALL check(nf90_inq_varid(ncid,'electrode_charge',varid))
        CALL check(nf90_get_var(ncid, varid, electrode_charge))


        ! Close the file
        CALL check(status = nf90_close(ncid))
        ! PRINT *, "Success reading checkpoint file, "//filename

    END SUBROUTINE read_checkpoint


    SUBROUTINE set_inputs(filename, tstep_init, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge)

        !> @var character len=* filename
        !! Name of input file.
        CHARACTER(len=*), INTENT(IN) :: filename
        !> @var character len=5 file_extension
        !! Parsed file extension to choose checkpoint file or user input parameters
        CHARACTER(len=5) :: file_extension
        !> @var integer parse_idx
        !! Index of parser in string
        INTEGER :: parse_idx

        ! Parameters
        !> @var integer tstep_init
        !! Timestep of checkpoint file
        INTEGER, INTENT(OUT) :: tstep_init
        !> @var integer tsteps
        !! Number of timesteps (dimensionless)
        INTEGER, INTENT(OUT) :: tsteps
        !> @var real dt
        !! Time step size (s)
        REAL(REAL64), INTENT(OUT) :: dt
        !> @var integer n
        !! Number of spatial nodes (dimensionless)
        INTEGER, INTENT(OUT) :: n
        !> @var allocatable real array c
        !! Initial concentration (mol m^-3)
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: c ! Conc at timestep of checkpoint tstep_init
        !> @var real c0
        !! Initial concentration (mol m^-3)
        REAL(REAL64) :: c0
        !> @var real D
        !! Diffusion coefficient (m^2 s^-1)
        REAL(REAL64), INTENT(OUT) :: D
        !> @var real R
        !! Total width of block (m)
        REAL(REAL64), INTENT(OUT) :: R 
        !> @var real a_small
        !! Particle surface area per unit volume (m^-1), Constant in r=R boundary condit
        REAL(REAL64), INTENT(OUT) :: a_small
        !> @var real L
        !! Electrode thickness (m), Constant in r=R boundary condit
        REAL(REAL64), INTENT(OUT) :: L
        !> @var allocatable real array iapp
        !! Applied current density (A m^-2)
        !! Will be length tsteps
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: iapp
        !> @var chararacter len=1 electrode_charge
        !! Label of electrode charge, 'p'=positive, 'n'=negative
        CHARACTER(len=1), INTENT(OUT) :: electrode_charge


        !> 1. Find index of '.' in filename to identify file extension
        parse_idx = INDEX(filename, ".") +1
        file_extension = filename(parse_idx:)
        
        !> 2. Choose input reader based on file extension.
        SELECT CASE(file_extension)

            CASE("txt")
                CALL read_user_inputs(filename, tsteps, dt, n, c0, D, R, a_small, L, iapp, electrode_charge)
                tstep_init = 1
                ALLOCATE(c(n))
                c = c0 !set c to initial state

            CASE("nc")
                CALL read_checkpoint(filename, tstep_init, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge)

            CASE DEFAULT
                PRINT*, "Invalid file type passed to SPM solver."
                PRINT*, "Please pass a 'txt' file of user input parameters or a 'nc' checkpoint file."
                STOP 6

        END SELECT


    END SUBROUTINE set_inputs

    
        

END MODULE checkpointing