import user_input_mod as UI
import plotter 
import sys

# This is a running script which exists to demonstrate how one uses the 
# GITT function to run simulations in parallel

# These simulations consist of applying a step function of current to a battery
# which turns on to a fixed value for a given time interval, then back to 0,
# and then on again, etc. The ascii art below indicates schematically how a current-time
# looks during this experiment.

#I
# |______________               _____________
# |              |              |              
# |              |              |
# |              |              |
# |              |              |
# |              |              |
# |              |              |
# |______________|______________|_____________t
# 
#
# To run one of these simulations, one needs the following things:
#   - A vector containing the fixed values of current applied for each block of current
#   - A vector containing the start times of each current block
#   - A vector containing the duration of each current block


###### Uncomment to print stdout to file
# sys.stdout = open('test.txt', 'w')

###### Set filename to output user input parameters ######
# Set string for filename. Do not enter a file extension. Max characters = 50
output_filename = 'current_step'
electrode = 'positive'

###### Import default values from https://doi.org/10.1149/1945-7111/ab9050
# Use UI.set_defaults_pos() for positive electrode and ..._neg() for negative electrode 
tsteps, dt, c0, D, R, a, L, iapp, iapp_label = UI.set_defaults_pos()

###### Set values ######

c0 = 30.0
dt = 0.01
###### Check parameters are valid ######
UI.verify_params(output_filename, tsteps, dt, c0, D, R, a, L)


###### Manually set up applied current and parallelisation ######

# Number of parallel processors being utilised
nprocs = 5

# Number of current blocks to apply
nsteps = 5

# Set up the currents, start times, run times and wait times for each step
currents = [2.0 for i in range(nsteps)]
start_times = [1.5*i for i in range(nsteps)]
run_times = [1.0 for i in range(nsteps)]
wait_times = [0.5 for i in range(nsteps)]

# Build the vector of parameters that the function accepts
params = [dt, c0, D, R, a, L]

###### Call the function to perform the parallel solve
UI.GITT(output_filename,nprocs,currents,start_times,run_times,wait_times,params)

###### Call the plotting function which plots the results of the GITT test with nstep steps
plotter.plot_GITT_result(output_filename,start_times,electrode)

##### also call the standalone plotting function with a single file
plotter.gen_plots(output_filename+'0',electrode)


