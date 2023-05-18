'''! @brief Set up user inputs to SPM solver and execute solver.'''

import user_input_mod as UI
import plotter
import sys
'''! 1. Option to output stdout (command line output) to file. Uncomment to use option.'''
# sys.stdout = open('test.txt', 'w')

'''! 2. Select input file: checkpoint file or user input parameters.
Filename of desired checkpoint file or name to output file containing user input parameters.
@var boolean checkpoint: Select checkpoint file for input, rather than user input parameters.
@var string solver_input_filename: Name of file produced by code, containing paramaters. Do not enter a file extension. Max characters = 50.
'''
checkpoint = False
solver_input_filename = 'user_input'
#solver_input_filename = 'test.nc'


'''! 3. Set input parameters. 
Input parameters: tsteps, dt, c0, D, R, a, L, iapp, iapp_label
@var integer tsteps: Number of timesteps, integer, > 0. Do not change if read iapp from file.
@var float dt: Timestep size (s), float > 0.
@var integer n: Number of spatial nodes, integer, 100 =< n =< 4000.
@var float c0: Initial concentration (mol m**-3), float >= 0.
@var float D: Diffusion constant (m**2 s**-1), float.
@var float R: Width of block (m), float > 0.
@var float a: Particle surface area per unit volume (m**-1), float >= 0.
@var float L: Electrode thickness(m), float >= 0.
@var 1D array (len=tsteps), float iapp: Applied current density (A m**2), 1D array of floats of length tsteps.
@var string iapp_label: Label of applied current density, string.
@var string electrode_charge: Label of electrode charge as positive or negative.

Options:
- Use default parameters by calling UI.set_defaults_pos() for a positive electrode, or UI.set_defaults_neg() for a negative electrode.
- Manually set values.
Options for iapp:
- Use default values.
- Read values from a csv file 'iapp_filename' using: iapp, iapp_label, tsteps = UI.iapp_read_csv(iapp_filename).
- Set up a constant current density of value 'iapp_const' using: iapp, iapp_label = UI.iapp_constant_setup(tsteps, iapp_const).
- Set up a stepped current density from a 2D array of values and timesteps 'iapp_steps' using: iapp, iapp_label = UI.iapp_step_setup(tsteps, iapp_steps). 
iapp_steps: 2D array of step values and timesteps where step occurs, starting timestep 0. Timesteps must be integers.
- Manually set values. iapp must be an array of floats with length tsteps.
'''
######### SET VALUES #########

# Import default values
tsteps, dt, n, c0, D, R, a, L, iapp, iapp_label, electrode_charge = UI.set_defaults_pos()

# Read in applied current density from csv file
iapp_filename = 'WLTP_m10.csv'
iapp, iapp_label, tsteps = UI.iapp_read_csv(iapp_filename)

######### END SET VALUES #########

'''! 4. Check for checkpointing or user defined input parameters.'''
if (not checkpoint):
    '''! 4.1. Check parameters and filename are valid.
    If setting manually, tsteps must be validated before iapp is set up to ensure than iapp has a valid length.
    '''
    # Check parameters and output filename are valid
    UI.verify_params(solver_input_filename, tsteps, dt, n, c0, D, R, a, L, electrode_charge)

    #Check applied current is valid 
    UI.verify_iapp(iapp, iapp_label, tsteps)

    '''! 4.2. Write parameters to file.'''
    UI.write_to_file(solver_input_filename, tsteps, dt, c0, D, R, a, L, iapp, iapp_label, electrode_charge)

'''! 5. Call fortran solver.'''
UI.call_solver(solver_input_filename, checkpoint)

'''! 6. Call plotter.'''
plotter.gen_plots(solver_input_filename)

