import user_input_mod as UI
import plotter 
import sys

# This is a running script which exists to demonstrate how one uses the 
# GITT function to run simulations in parallel on a full battery.
# This simulation completes 10 current cycles on each electrode over 2 cores each - parallelising perfectly over 40 cores.

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
output_filename_positive = 'current_step_pos'
output_filename_negative = 'current_step_neg'

###### Import default values from https://doi.org/10.1149/1945-7111/ab9050
# Use UI.set_defaults_pos() for positive electrode and ..._neg() for negative electrode 
tsteps, dt, c0, D_neg, R_neg, a_neg, L_neg, iapp_neg, iapp_label_neg, electrode_charge_neg = UI.set_defaults_neg()
tsteps, dt, c0, D_pos, R_pos, a_pos, L_pos, iapp_pos, iapp_label_pos, electrode_charge_pos = UI.set_defaults_pos()

###### Set values ######
c0_pos = 30000.0 #note that the simulation will be charged, then discharged.
c0_neg = 1.0 #not exactly 0 to prevent errors
dt = 1.0 # 1.0 second time steps

###### Check parameters are valid ######
UI.verify_params(output_filename_positive, tsteps, dt, c0_pos, D_pos, R_pos, a_pos, L_pos, electrode_charge_pos)
UI.verify_params(output_filename_negative, tsteps, dt, c0_neg, D_neg, R_neg, a_neg, L_neg, electrode_charge_neg)

###### Manually set up applied current and parallelisation ######
# Number of parallel processors being utilised
nprocs = 20

# Number of current blocks to apply
nsteps = 10

# Set up the currents, start times, run times and wait times for each step
currents = [20.0 for i in range(nsteps)] #positive current corresponds to CHARGING the battery
start_times = [2150.0*i for i in range(nsteps)]
run_times = [150.0 for i in range(nsteps)]
wait_times = [2000.0 for i in range(nsteps)]

# Build the vector of parameters that the function accepts
params_pos = [dt, c0_pos, D_pos, R_pos, a_pos, L_pos, electrode_charge_pos]
params_neg = [dt, c0_neg, D_neg, R_neg, a_neg, L_neg, electrode_charge_neg]

###### Call the function to perform the full battery parallel solve
#UI.GITT_full_cell(output_filename_positive,output_filename_negative,nprocs,currents,start_times,run_times,wait_times,params_pos,params_neg)

###### Call the plotting function which plots the results of the GITT test with nstep steps
plotter.full_battery_GITT_plots(output_filename_positive,output_filename_negative,start_times,a_pos,a_neg,SparsifyAnimation=True,animation_interval_time=10)

##### also call the standalone plotting function with a single file
#plotter.gen_plots(output_filename+'0',electrode,SparsifyAnimation=True)


