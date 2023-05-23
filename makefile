# on scrtp desktops for ifort, run the following:
# module purge; module load intel/2017.4.196-GCC-6.4.0-2.28  impi/2017.3.196 imkl/2017.3.196 
# module load netCDF-Fortran/4.4.4


# Flags requird if compiling with Intel MKL
iflags= -DMKL
# Libraries required if compiling with Intel MKL
ildflibs=-Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_intel_lp64.a ${MKLROOT}/lib/intel64/libmkl_intel_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a -Wl,--end-group -liomp5 -lpthread -lm -ldl

gfc := gfortran
ifc := ifort
fflags ?= -O3
ldfflags := `nf-config --fflags`
ldfflibs := `nf-config --flibs`
extra ?= 
finite_diff_solver : finite_diff_solver.o nc_output.mod read_inputs.mod checkpointing.mod
	$(ifc) $(fflags) $(ldfflags) $(iflags) read_inputs.o nc_output.o checkpointing.o finite_diff_solver.o $(ldfflibs) $(ildflibs) -lnetcdf -o $@ $(extra)

finite_diff_solver.o : finite_diff_solver.f90 nc_output.mod read_inputs.mod checkpointing.mod
	$(ifc) $(fflags) -c finite_diff_solver.f90

nc_output.mod : nc_output.f90
	$(ifc) $(ldfflags) -c nc_output.f90 -lnetcdf $(ldfflibs)
	
read_inputs.mod : read_inputs.f90
	$(ifc) $(fflags) -c read_inputs.f90

checkpointing.mod : checkpointing.f90 nc_output.mod read_inputs.mod
	$(ifc) $(ldfflags) -c checkpointing.f90 -lnetcdf $(ldfflibs)

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

