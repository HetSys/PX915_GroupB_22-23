!> @brief Module to read and write checkpoint files.
!! @details Module contains functions that write checkpoint files and read them in as user input.
MODULE checkpointing

    USE netcdf
    USE iso_fortran_env
    USE read_inputs
    USE nc_output ! Contains 'check' function
    IMPLICIT none

    CONTAINS

    !>@brief Write checkpoint file (netCDF)
    !!@details Writes netCDF checkpoint file every 'freq_in' timesteps.
    !!Checkpoint file contains all user input parameters and concentration up to current timestep.
    !! @param[in] tstep: Current timestep
    !! @param[in] tsteps: Number of timesteps
    !! @param[in] dt: Time step size (s)
    !! @param[in] n: Number of spatial nodes
    !! @param[in] c: Concentration at current timestep (mol m^-3)
    !! @param[in] D: Diffusion constant (m^2 s^-1)
    !! @param[in] R: Radius of simulation sphere (m)
    !! @param[in] a_small: Particle surface area per unit volume (m^-1)
    !! @param[in] L: Electrode thickness (m)
    !! @param[in] iapp: Applied current density (A m^-2)
    !! @param[in] electrode_charge: Electrode charge, 'p'=positive, 'n'=negative
    !! @param[in] cstorage: 2D array containing the concentration profile at each timestep
    !! @param[in] filename: Name of user input file. Used to determin name of checkpoint file
    !! @param[in] freq_in: Optional, interval of timesteps at which to write checkpoints. Default = tsteps/10.
    SUBROUTINE write_checkpoint(tstep, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge, cstorage, filename, freq_in)

        ! Variables to write
        INTEGER, INTENT(IN) :: tstep
        INTEGER, INTENT(IN) :: tsteps
        REAL(REAL64), INTENT(IN) :: dt
        INTEGER, INTENT(IN) :: n 
        REAL(REAL64), DIMENSION(n), INTENT(IN) :: c
        REAL(REAL64), INTENT(IN) :: D
        REAL(REAL64), INTENT(IN) :: R
        REAL(REAL64), INTENT(IN) :: a_small
        REAL(REAL64), INTENT(IN) :: L
        REAL(REAL64), DIMENSION(tsteps), INTENT(IN) :: iapp
        CHARACTER(len=*), INTENT(IN) :: electrode_charge
        REAL(REAL64), DIMENSION(n, tsteps), INTENT(IN) :: cstorage

        ! Frequency variables
        INTEGER, INTENT(IN), OPTIONAL :: freq_in
        INTEGER :: freq ! desired frequency of checkpoints
        INTEGER :: freq_remainder
        ! Execution variables
        CHARACTER(len=*), INTENT(INOUT) :: filename
        CHARACTER(len=60) :: dir, checkpoint_name
        CHARACTER(len=120) :: filepath
        INTEGER :: ncid, dimid_n, dimid_tsteps, dimids_parameter(1), dimids_cstorage(2), status
        INTEGER :: varid_tstep, varid_tsteps, varid_dt, varid_n, varid_c
        INTEGER :: varid_D, varid_R, varid_a, varid_L, varid_iapp, varid_EC, varid_cstorage
        INTEGER :: filename_length, file_ext, file_test
        CHARACTER(len=10) :: tstep_str

        ! Determine if checkpoint desired at this timestep
        IF(PRESENT(freq_in)) THEN
            freq = freq_in
        ELSE
            freq = tsteps/10
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
            
            ! Write checkpoint file
            ! Create NetCDF file
            CALL check(nf90_create(filepath, NF90_CLOBBER, ncid))
        
            ! Define dimensions
            CALL check(status = nf90_def_dim(ncid, "n", n, dimid_n))
            CALL check(status = nf90_def_dim(ncid, "tsteps", tsteps, dimid_tsteps))
            CALL check(status = nf90_def_dim(ncid, "parameter", 1, dimids_parameter(1)))
            dimids_cstorage = (/dimid_n, dimid_tsteps/)

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
            CALL check(status = nf90_def_var(ncid, "cstorage", NF90_DOUBLE, dimids_cstorage, varid_cstorage))
            
            
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
            CALL check(status = nf90_put_var(ncid, varid_cstorage, cstorage))
        
            
            ! Close the file
            CALL check(status = nf90_close(ncid))
            PRINT *, "Success writing checkpoint file, "//filepath

        END IF

    END SUBROUTINE write_checkpoint


    !> @brief Reads in checkpoint file (netCDF).
    !! @param[in] filename: Name of user input file.
    !! @param[out] tstep_init: Timestep of checkpoint file, used as initial timestep for solver.
    !! @param[out] tsteps: Number of timesteps
    !! @param[out] dt: Time step size (s)
    !! @param[out] n: Number of spatial nodes
    !! @param[out] c: Concentration at timestep of checkpoint file (mol m^-3)
    !! @param[out] D: Diffusion constant (m^2 s^-1)
    !! @param[out] R: Radius of simulation sphere (m)
    !! @param[out] a_small: Particle surface area per unit volume (m^-1)
    !! @param[out] L: Electrode thickness (m)
    !! @param[out] iapp: Applied current density (A m^-2)
    !! @param[out] electrode_charge: Electrode charge, 'p'=positive, 'n'=negative
    !! @param[out] cstorage: 2D array containing the concentration profile at each timestep
    SUBROUTINE read_checkpoint(filename, tstep_init, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge, cstorage)

        SAVE

        CHARACTER(len=*), INTENT(IN) :: filename

        ! Variables to read
        INTEGER, INTENT(OUT) :: tstep_init
        INTEGER, INTENT(OUT) :: tsteps
        REAL(REAL64), INTENT(OUT) :: dt
        INTEGER, INTENT(OUT) :: n 
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: c
        REAL(REAL64), INTENT(OUT) :: D
        REAL(REAL64), INTENT(OUT) :: R
        REAL(REAL64), INTENT(OUT) :: a_small
        REAL(REAL64), INTENT(OUT) :: L
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: iapp
        CHARACTER(len=*), INTENT(OUT) :: electrode_charge
        REAL(REAL64), DIMENSION(:, :), ALLOCATABLE, INTENT(OUT) :: cstorage

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
        ALLOCATE(cstorage(n, tsteps))
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

        !Read cstorage
        CALL check(nf90_inq_varid(ncid,'cstorage',varid))
        CALL check(nf90_get_var(ncid, varid, cstorage))       


        ! Close the file
        CALL check(status = nf90_close(ncid))

    END SUBROUTINE read_checkpoint

    !> @brief Reads and selects user input file type: standard or checkpoint file.
    !! @param[in] filename: Name of user input file.
    !! @param[out] tstep_init: Timestep of checkpoint file, used as initial timestep for solver.
    !! @param[out] tsteps: Number of timesteps
    !! @param[out] dt: Time step size (s)
    !! @param[out] n: Number of spatial nodes
    !! @param[out] c: Concentration at timestep of checkpoint file (mol m^-3)
    !! @param[out] D: Diffusion constant (m^2 s^-1)
    !! @param[out] R: Radius of simulation sphere (m)
    !! @param[out] a_small: Particle surface area per unit volume (m^-1)
    !! @param[out] L: Electrode thickness (m)
    !! @param[out] iapp: Applied current density (A m^-2)
    !! @param[out] electrode_charge: Electrode charge, 'p'=positive, 'n'=negative
    !! @param[out] cstorage: 2D array containing the concentration profile at each timestep
    SUBROUTINE set_inputs(filename, tstep_init, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge, cstorage)

        ! Name of input file.
        CHARACTER(len=*), INTENT(IN) :: filename
        ! Parsed file extension to choose checkpoint file or user input parameters
        CHARACTER(len=5) :: file_extension
        !! Index of parser in string
        INTEGER :: parse_idx

        ! Parameters
        INTEGER, INTENT(OUT) :: tstep_init
        INTEGER, INTENT(OUT) :: tsteps
        REAL(REAL64), INTENT(OUT) :: dt
        INTEGER, INTENT(OUT) :: n
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: c ! Conc at timestep of checkpoint tstep_init
        REAL(REAL64) :: c0 ! Initial concentration (mol m^-3)
        REAL(REAL64), INTENT(OUT) :: D
        REAL(REAL64), INTENT(OUT) :: R 
        REAL(REAL64), INTENT(OUT) :: a_small
        REAL(REAL64), INTENT(OUT) :: L
        REAL(REAL64), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: iapp
        CHARACTER(len=1), INTENT(OUT) :: electrode_charge
        REAL(REAL64), DIMENSION(:, :), ALLOCATABLE, INTENT(OUT) :: cstorage


        ! Find index of '.' in filename to identify file extension
        parse_idx = INDEX(filename, ".") +1
        file_extension = filename(parse_idx:)
        
        ! Choose input reader based on file extension.
        SELECT CASE(file_extension)

            CASE("txt")
                CALL read_user_inputs(filename, tsteps, dt, n, c0, D, R, a_small, L, iapp, electrode_charge)
                tstep_init = 1
                ALLOCATE(c(n))
                ALLOCATE(cstorage(n,tsteps))
                c = c0 !set c to initial state
                cstorage(:,tstep_init) = c                

            CASE("nc")
                CALL read_checkpoint(filename, tstep_init, tsteps, dt, n, c, D, R, a_small, L, iapp, electrode_charge, cstorage)

            CASE DEFAULT
                PRINT*, "Invalid file type passed to SPM solver."
                PRINT*, "Please pass a 'txt' file of user input parameters or a 'nc' checkpoint file."
                STOP 6

        END SELECT


    END SUBROUTINE set_inputs

    
        

END MODULE checkpointing
