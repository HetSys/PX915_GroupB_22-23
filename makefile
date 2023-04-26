fc ?: gfortran #set default value for compiler.
fstd := -std=f2008
flags := -O3
War := -Wall -Wextra
py := python3

ifeq ($(origin COMPILER), undefined)#if no compiler set in terminal, use default.
	fc := $(fc)
else ifeq ($(COMPILER), gfortran)#if gfortran specified in terminal use default.
	fc := $(fc)
else ifeq ($(COMPILER), ifort)#if ifort specified in terminal use ifort.
	fc := ifort
else ifeq ($(COMPILER), pgfortran)#if pgfortran specified in terminal use pgfortran.
	fc :=pgfortran
else
	$(error The value you specified for COMPILER was not acceptable. Please choose one of: gfortran, ifort, pgfortran.)#if unsupported compiler is set in terminal, throw an error message and stop.
endif
.DEFAULT_GOAL := written_file_name, Exec, plots
written_file_name : python_write_script
        $(py) python_write_script#use python write script to write input file for solver.

.PHONY : Exec
Exec : fortran_solver_exe:
	.\fortran_solver_exe#execute executable from fortran driver.
fortran_solver_exe : fortran_driver.f90
	$(fc) $(fstd) $(flags) -llapack fortran_driver.f90#compile fortran_driver.

plots : python_visualisation_script
        $(py) python_visualisation_script

