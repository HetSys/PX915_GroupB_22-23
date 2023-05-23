# SPAMS - Single PArticle Model Solver

A half single particle model by Group B for the 2023 PX915 group project.

SPAMS models the charging and discharging of a lithium ion battery using a Crank-Nicolson semi-implicit finite difference scheme to obtain the
concentration of lithium in a sphere, **c**(i<sub>app</sub>, **r**), at each time step. 

## Dependencies
Prior to installing the program or accessing the tutorial, you will need to ensure you have the following installed:

- intel/2017.4.196-GCC-6.4.0-2.28
- impi/2017.3.196 imkl/2017.3.196
- imkl/2017.3.196
- netCDF-Fortran/4.4.4
- GCC/10.2.0
- Python/3.8.6
- numpy
- netCDF4
- matplotlib

For an scrtp managed system the following procedure will ensure correct dependendcies are installed:

```bash
module purge; module load intel/2017.4.196-GCC-6.4.0-2.28  impi/2017.3.196 imkl/2017.3.196 netCDF-Fortran/4.4.4 GCC/10.2.0 Python/3.8.6
pip3 install numpy netCDF4 Matplotlib
```

## Installation
To install SPAMS, navigate to the directory in your file system where you would like to download it, and use the following command:
```bash
git clone https://github.com/HetSys/PX915_GroupB_22-23
```

## Program Features

Very short summary of what program can do...

## Running the program

Run line: ```python3 user_input.py```

## Accessing the user documentation

A full tutorial for basic usage of the program is provided in Jupyter notebook format.
* The notebook can been found in the Documentation folder (called 'Tutorial.ipynb)
* The notebook can be viewed here in github as a markdown file.
* Alternatively, the notebook can be loaded from the terminal by navigating to the Documentation directory and using the command:
```bash
nohup jupyter notebook Tutorial.ipynb
```

## Developer documentation

Developer documentation is available at https://hetsys.github.io/PX915_GroupB_22-23/.

![logo](https://github.com/HetSys/PX915_GroupB_22-23/assets/120459567/6e1da60c-726c-427d-a9f4-b0177e454a26)
`
## Contributors 
Fraser Birks, Laura Cairns, Sebastian Dooley, Arielle Fitkin, Jake Eller, and Yu Lei

HetSys CDT, University of Warwick
