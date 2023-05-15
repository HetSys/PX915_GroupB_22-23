fc := gfortran
fstd := -std=f2008
fflags ?= -O3
ldfflags := `nf-config --fflags`
ldfflibs := `nf-config --flibs`
war ?= -Wall -Wextra
finite_diff_solver : finite_diff_solver.o nc_output.mod read_inputs.mod
	$(fc) $(fstd) $(fflags) $(ldfflags) read_inputs.o nc_output.o finite_diff_solver.o $(ldfflibs) -llapack -lnetcdf -o $@ $(war)

finite_diff_solver.o : finite_diff_solver.f90 nc_output.mod read_inputs.mod
	$(fc) $(fstd) $(fflags) -c finite_diff_solver.f90

nc_output.mod : nc_output.f90
	$(fc) $(fstd) $(ldfflags) -c nc_output.f90 -lnetcdf $(ldfflibs)
	
read_inputs.mod : read_inputs.f90
	$(fc) $(fstd) $(fflags) -c read_inputs.f90

clean :
	rm *.o *.mod ./finite_diff_solver
































#finite_diff_solver.out : finite_diff_solver.f90 nc_output.mod read_inputs.mod
#nc_output.mod : nc_output.f90
#read_inputs.mod : read_inputs.f90
#finite_diff_solver.f90 : nc_output.mod read_inputs.mod

#finite_diff_solver.out : finite_diff_solver.f90 nc_output.mod read_inputs.mod
#	$(fc) $(fstd) $(flags) finite_diff_solver.f90 -llapack -lnetcdf nc_output.mod read_inputs.mod -o $@ $(war)
#nc_output.mod : nc_output.f90
#	$(fc) $(fstd) $(ldfflags) nc_output.f90 -lnetcdf $(ldfflibs)
#read_inputs.mod : read_inputs.f90
#	$(fc) $(fstd) $(flags) read_inputs.f90





















#execd : fortran_solver_exe.out
#	./fortran_solver_exe.out
#fortran_solver_exe.out : fortran_solver.f90
#	$(fc) $(fstd) $(fflags) fortran_solver.f90 -llapack -o $@ $(war)

#py := python3

#ifeq ($(origin COMPILER), defined)
#	fc:=$(COMPILER)
#else ifeq ($(origin COMPILER), undefined)
#	fc := gfortran
#endif
#else ifeq ($(COMPILER), gfortran)#if gfortran specified in terminal use default.
#	fc := $(fc)
#else ifeq ($(COMPILER), ifort)#if ifort specified in terminal use ifort.
#	fc := ifort
#else ifeq ($(COMPILER), pgfortran)#if pgfortran specified in terminal use pgfortran.
#	fc :=pgfortran
#else
#	$(error The value you specified for COMPILER was not acceptable. Please choose one of: gfortran, ifort, pgfortran.)#if unsupported compiler is set in terminal, throw an error message and stop.
#endif
#.DEFAULT_GOAL := written_file_name, Exec, plots
#written_file_name : python_write_script
#        $(py) python_write_script#use python write script to write input file for solver.
#
#.PHONY : Exec
#Exec : fortran_solver_exe:
#	.\fortran_solver_exe#execute executable from fortran driver.
#fortran_solver_exe.out : fortran_solver.f90
#	$(fc) $(fstd) $(flags) fortran_solver.f90 -llapack -o $@
#
#plots : python_visualisation_script
#        $(py) python_visualisation_script

