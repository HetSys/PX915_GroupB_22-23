'''! 
@brief Sets up the user inputs and executes the solver.
@details This file contains all input parameters for the solver and can be changed by the user. 
This file and all tuneable parameters are broken down in full detail in the user tutorial (see Tutorial.ipynb).

The following options can be controlled by editing this file:
- Outputting stdout to a file.
- Changing the name of this user input file.
- Set the values for the following input parameters:
    - tsteps
    - dt
    - n
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

# Change the name of this of this file containing the user input parameters.
solver_input_filename = 'user_input'


######### SET VALUES #########


# Import default values
tsteps, dt, n, c0, D, R, a, L, iapp, iapp_label, electrode_charge = UI.set_defaults_pos()

# Additional values important for visualisation

K_pos = 3.42E-6   # Reaction rate at positive electrodes (Am^-2(m^3mol^-1)^1.5)
K_neg = 6.48E-7   # Reaction rate at negative electrodes Am^-2(m^3mol^-1)^1.5)

cmax_pos_sim = 63104.00   # Positive electrode maximum concentration (molm^-3)
cmax_neg_sim = 33133.00   # Negative electrode maximum concentration (molm^-3)

######### END SET VALUES #########

#Check parameters and filename are valid.
#If set manually, tsteps must be validated before iapp is set up to ensure iapp has valid length.

# Check parameters and output filename are valid
UI.verify_params(solver_input_filename, tsteps, dt, n, c0, D, R, a, L, electrode_charge)

#Check applied current density is valid 
UI.verify_iapp(iapp, iapp_label, tsteps)


# Write parameters to file
UI.write_to_file(solver_input_filename, tsteps, dt, n, c0, D, R, a, L, iapp, iapp_label, electrode_charge)

# Call fortran solver
UI.call_solver(solver_input_filename)


# Build the vector of parameters that the plotter accepts
plot_params_pos = [K_pos,a,cmax_pos_sim,L]

# Call plotter 
plotter.gen_plots(solver_input_filename,pos_params=plot_params_pos)
