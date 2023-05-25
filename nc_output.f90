'''! @brief Writes results to a netCDF file'''
MODULE nc_output
  USE netcdf
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE
  CONTAINS
  !Helper subroutine to avoid repeated if statements
  !Stops program in case netcdf fails 
  SUBROUTINE check(status)
    
    INTEGER, INTENT ( in) :: status
    
    IF(status /= nf90_noerr) then 
      PRINT *, trim(nf90_strerror(status))
      RETURN
    END IF
  END SUBROUTINE check
  

  SUBROUTINE output_cstorage(cstorage, n, tsteps, R, time_axis, electrode_charge, filename)
    ! Output the cstorage array to a NetCDF file
    INTEGER, INTENT(IN) :: n, tsteps
    REAL(REAL64), DIMENSION(tsteps), INTENT(IN) :: time_axis
    REAL(REAL64), INTENT(IN) :: R
    REAL(REAL64), DIMENSION(n, tsteps), INTENT(IN) :: cstorage
    CHARACTER(len=*), INTENT(IN) :: electrode_charge
    CHARACTER(len=*), INTENT(IN) :: filename
    INTEGER :: ncid, varid, varid_t, varid_n, varid_R, varid_EC, dimids(2), dimids_parameter(1), status
    INTEGER :: varid_time_axis

    
    ! Create NetCDF file
    CALL check(nf90_create(filename, NF90_CLOBBER, ncid))

    ! Define dimensions
    CALL check(status = nf90_def_dim(ncid, "n", n, dimids(1)))
    CALL check(status = nf90_def_dim(ncid, "tsteps", tsteps, dimids(2)))
    CALL check(status = nf90_def_dim(ncid, "parameter", 1, dimids_parameter(1)))
    
    ! Define variables
    CALL check(status = nf90_def_var(ncid, "cstorage", NF90_DOUBLE, dimids, varid))
    CALL check(status = nf90_def_var(ncid, "time_axis", NF90_DOUBLE, dimids(2),varid_time_axis))
    CALL check(status = nf90_def_var(ncid, "tsteps", NF90_INT,dimids_parameter,varid_t))
    CALL check(status = nf90_def_var(ncid, "node_num", NF90_INT,dimids_parameter,varid_n))
    CALL check(status = nf90_def_var(ncid, "R", NF90_DOUBLE,dimids_parameter,varid_R))
    CALL check(status = nf90_def_var(ncid, "electrode_charge", NF90_CHAR,dimids_parameter,varid_EC))
    
    ! End define mode and switch to data mode
    CALL check(status = nf90_enddef(ncid))

    ! Write data to variable
    CALL check(status = nf90_put_var(ncid, varid, cstorage))
    CALL check(status = nf90_put_var(ncid, varid_t, tsteps))
    CALL check(status = nf90_put_var(ncid, varid_n, n))
    CALL check(status = nf90_put_var(ncid, varid_R, R))
    CALL check(status = nf90_put_var(ncid, varid_time_axis, time_axis))
    CALL check(status = nf90_put_var(ncid, varid_EC, electrode_charge))

    
    ! Close the file
    CALL check(status = nf90_close(ncid))
    PRINT *, "Success writing output file, "//filename
  END SUBROUTINE output_cstorage

END MODULE nc_output

