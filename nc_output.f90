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
  
  SUBROUTINE output_cstorage(cstorage, n, tsteps, filename)
    ! Output the cstorage array to a NetCDF file
    INTEGER, INTENT(IN) :: n, tsteps
    REAL(REAL64), DIMENSION(n, tsteps), INTENT(IN) :: cstorage
    CHARACTER(*), INTENT(IN) :: filename
    INTEGER :: ncid, varid, dimids(2), status
    
    ! Create NetCDF file
    CALL check(nf90_create(filename, NF90_CLOBBER, ncid))

    ! Define dimensions
    CALL check(status = nf90_def_dim(ncid, "n", n, dimids(1)))
    CALL check(status = nf90_def_dim(ncid, "tsteps", tsteps, dimids(2)))
    
    ! Define variables
    CALL check(status = nf90_def_var(ncid, "cstorage", NF90_DOUBLE, dimids, varid))
    
    ! End define mode and switch to data mode
    CALL check(status = nf90_enddef(ncid))
    
    ! Write data to variable
    CALL check(status = nf90_put_var(ncid, varid, cstorage))
    
    ! Close the file
    CALL check(status = nf90_close(ncid))
    PRINT *, "*** SUCCESS writing example file output.nc! "
  END SUBROUTINE output_cstorage

END MODULE nc_output

