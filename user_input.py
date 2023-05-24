'''! 
@brief Sets up the user inputs and executes the solver.
@details This file contains all input parameters for the solver and can be changed by the user. 
This file and all tunebale parameters are broken down in full detail in the user tutorial (see Tutorial.ipynb).

The following options can be controlled by editing this file:
- Outputting stdout to a file.
- Changing the name of this user input file.
- Set the values for the following input parameters:
    - tsteps
    - dt
    - c0
    - D
    - R
    - a
    - L
    - iapp
    - iapp_label

- These parameters can either take default values or be entered manually.
- For iapp there are additional options for setting constant or stepwise current and reading in values from a csv file. 

The user does not need to edit anything past #END SET INPUT PARAMETER VALUES
 
The rest of the file calls various functions to:
- Check that the filename and parameters are valid
- Write the parameters to the file
- Call the solver and plotter.
'''

import user_input_mod as UI
import plotter
import sys

# The stdout (command line output) can be output to a file. Uncomment the line below to use this option.
# sys.stdout = open('stdout.txt', 'w')

# Select input file: checkpoint file (True) or user input parameters (False).
# Enter the filename of desired checkpoint file or the desired name of the file containing user input parameters.

checkpoint = True
if checkpoint == False:
    solver_input_filename = 'user_input'
else:
    solver_input_filename = 'checkpoints_user_input/user_input_tstep_300.nc'



######### SET VALUES #########


# Import default values
tsteps, dt, n, c0, D, R, a, L, iapp, iapp_label, electrode_charge = UI.set_defaults_pos()


# Read in applied current density from csv file
'''!@private iapp_filename'''
iapp_filename = 'WLTP_m10.csv'
iapp, iapp_label, tsteps = UI.iapp_read_csv(iapp_filename)


# Additional values important for visualisation

K_pos = 3.42E-6   #  Am^-2(m^3mol^-1)^1.5
K_neg = 6.48E-7   #  Am^-2(m^3mol^-1)^1.5

cmax_pos_sim = 63104.00   #  molm^-3 # m
cmax_neg_sim = 33133.00   #  molm^-3 # m 


######### END SET VALUES #########


# Check if using checkpointing or user defined input parameters
if (not checkpoint):
    #Check parameters and filename are valid
    #If set manually, tsteps must be validated before iapp is set up to ensure iapp has valid length.

    # Check parameters and output filename are valid
    UI.verify_params(solver_input_filename, tsteps, dt, n, c0, D, R, a, L, electrode_charge)


    #Check applied current is valid 
    UI.verify_iapp(iapp, iapp_label, tsteps)

    # Write parameters to file
    UI.write_to_file(solver_input_filename, tsteps, dt, n, c0, D, R, a, L, iapp, iapp_label, electrode_charge)

# Call fortran solver
UI.call_solver(solver_input_filename, checkpoint)


# Build the vector of parameters that the plotter accepts
plot_params_pos = [K_pos,a,cmax_pos_sim,L]

# Call plotter 
plotter.gen_plots(solver_input_filename,pos_params=plot_params_pos)
