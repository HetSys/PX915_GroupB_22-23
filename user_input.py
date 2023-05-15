'''! @brief Set up user inputs to SPM solver and execute solver.'''

import user_input_mod as UI
import sys
'''! 1. Option to output stdout (command line output) to file. Uncomment to use option.'''
# sys.stdout = open('test.txt', 'w')

'''! 2. Set filename of output file containing user input parameters.
@var string filename: Name of output file. Do not enter a file extension. Max characters = 50.
'''
output_filename = 'user_input'


'''! 3. Set input parameters. 
Input parameters: tsteps, dt, c0, D, R, a, L, iapp, iapp_label
@var integer tsteps: Number of timesteps, integer, > 0. Do not change if read iapp from file.
@var float dt: Timestep size (s), float > 0.
@var float c0: Initial concentration (mol m**-3), float >= 0.
@var float D: Diffusion constant (m**2 s**-1), float.
@var float R: Width of block (m), float > 0.
@var float a: Particle surface area per unit volume (m**-1), float >= 0.
@var float L: Electrode thickness(m), float >= 0.
@var 1D array (len=tsteps), float iapp: Applied current (A m**2), 1D array of floats of length tsteps.
@var string iapp_label: Label of applied current, string.

Options:
- Use default parameters by calling UI.set_defaults_pos() for a positive electrode, or UI.set_defaults_neg() for a negative electrode.
- Manually set values.
Options for iapp:
- Use default values.
- Read values from a csv file 'iapp_filename' using: iapp, iapp_label, tsteps = UI.iapp_read_csv(iapp_filename).
- Set up a constant current of value 'iapp_const' using: iapp, iapp_label = UI.iapp_constant_setup(tsteps, iapp_const).
- Set up a stepped current from a 2D array of values and timesteps 'iapp_steps' using: iapp, iapp_label = UI.iapp_step_setup(tsteps, iapp_steps).
iapp_steps: 2D array of step values and timesteps where step occurs, starting timestep 0. Timesteps must be integers.
- Manually set values. iapp must be an array of floats with length tsteps.
'''
######### SET VALUES #########

# Import default values
tsteps, dt, c0, D, R, a, L, iapp, iapp_label = UI.set_defaults_pos()

# Read in applied current from csv file
iapp_filename = 'WLTP_m10.csv'
iapp, iapp_label, tsteps = UI.iapp_read_csv(iapp_filename)

######### END SET VALUES #########

'''! 4. Check parameters and filename are valid.
If setting manually, tsteps must be validated before iapp is set up to ensure than iapp has a valid length.
'''
# Check parameters and output filename are valid
UI.verify_params(output_filename, tsteps, dt, c0, D, R, a, L)

#Check applied current is valid 
UI.verify_iapp(iapp, iapp_label, tsteps)


'''! 5. Write parameters to file.'''
UI.write_to_file(output_filename, tsteps, dt, c0, D, R, a, L, iapp, iapp_label)

'''! 6. Call fortran solver.'''
UI.call_solver(output_filename)
