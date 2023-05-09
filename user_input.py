import user_input_mod as UI
import sys
###### Uncomment to print stdout to file
# sys.stdout = open('test.txt', 'w')

###### Set filename to output user input parameters ######
# Set string for filename. Do not enter a file extension. Max characters = 50
output_filename = 'user_input'

###### Import default values from https://doi.org/10.1149/1945-7111/ab9050
# Use UI.set_defaults_pos() for positive electrode and ..._neg() for negative electrode 
tsteps, dt, c0, D, R, a, L, iapp, iapp_label = UI.set_defaults_pos()

###### Set values ######

# Read in applied current from csv file
iapp_filename = 'WLTP_m10.csv'
iapp, iapp_label, tsteps = UI.iapp_read_csv(iapp_filename)


# Number of timesteps, integer, greater than 0. Do not change if read iapp from file.
#tsteps = 100
# Timestep size (s), real, greater than 0
dt = 0.1
# Initial concentration (mol m**-3), real, positive
c0 = 0.0
# Diffusion coefficient (m**2 s**-1), real
D = 4.0e-15
# Width of block (m), real, greater than 0
R = 5.86e-6
# Particle surface area per unit volume (m**-1), real, greater than 0
a = 3.821839e5
# Electrode thickness (m), real, greater than 0
L = 85.2e-6

###### Check parameters are valid ######
UI.verify_params(output_filename, tsteps, dt, c0, D, R, a, L)


###### Manually set up applied current ######
### Applied current (A m**2), real array of length tsteps

### Constant current
### Set iapp_const as constant float value
#iapp_const = 0.73*10**(-3)
#iapp, iapp_label = UI.iapp_constant_setup(tsteps, iapp_const)

### Step function
### Create 2D array of step values and timesteps where step occurs, starting timestep 0
### Timesteps must be integers
#iapp_steps = [[0.73*10**(-3), 0], [-0.73*10**(-3), int(tsteps/4)], [0.73*10**(-3), int(tsteps/2)]]
#iapp, iapp_label = UI.iapp_step_setup(tsteps, iapp_steps)
              

# Label to state which form applied current takes
# iapp_label = 



###### Check applied current is valid ######
UI.verify_iapp(iapp, iapp_label, tsteps)

###### Write parameters to file ######
UI.write_to_file(output_filename, tsteps, dt, c0, D, R, a, L, iapp, iapp_label)

####### Call fortran solver ######
UI.call_solver(output_filename)
