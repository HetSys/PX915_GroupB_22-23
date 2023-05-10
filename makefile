fc ?= gfortran
fstd := -std=f2008
fflags ?= -O3
war ?= -Wall -Wextra
execd : fortran_solver_exe.out
	./fortran_solver_exe.out
fortran_solver_exe.out : fortran_solver.f90
	$(fc) $(fstd) $(fflags) fortran_solver.f90 -llapack -o $@ $(war)

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

