''' @brief Example script for a full battery model 

@details This is a running script which exists to demonstrate how one uses the 
GITT function to run simulations in parallel on a full battery.
This simulation completes 10 current cycles on each electrode over 2 cores each - parallelising 
perfectly over 40 cores.
 
These simulations consist of applying a step function of current to a battery which turns on to a 
fixed value for a given time interval, then back to 0, and then on again, etc. 

The ascii art below indicates schematically how a current-time step looks during this experiment.

 I
 |______________               _____________
 |              |              |              
 |              |              |
 |              |              |
 |              |              |
 |              |              |
 |              |              |
 |______________|______________|_____________t
 
To run one of these simulations, one needs the following things:
    - A vector containing the fixed values of current applied for each block of current
    - A vector containing the start times of each current block
    - A vector containing the duration of each current block
 '''

import user_input_mod as UI
import plotter 
import sys

###### Uncomment to print stdout to file
# sys.stdout = open('test.txt', 'w')

###### Set filename to output user input parameters ######
# Set string for filename. Do not enter a file extension. Max characters = 50
output_filename_positive = 'current_step_pos'
output_filename_negative = 'current_step_neg'

###### Import default values from https://doi.org/10.1149/1945-7111/ab9050
# Use UI.set_defaults_pos() for positive electrode and ..._neg() for negative electrode 
tsteps, dt, n, c0, D_neg, R_neg, a_neg, L_neg, iapp_neg, iapp_label_neg, electrode_charge_neg = UI.set_defaults_neg()
tsteps, dt, n, c0, D_pos, R_pos, a_pos, L_pos, iapp_pos, iapp_label_pos, electrode_charge_pos = UI.set_defaults_pos()

###### Set values ######
c0_pos = 30000.0 #note that the simulation will be charged, then discharged.
c0_neg = 1.0 #not exactly 0 to prevent errors
dt = 1.0 # 1.0 second time steps

# Additional values important for visualisation
K_pos = 3.42E-6 #Am^-2(m^3mol^-1)^1.5
K_neg = 6.48E-7 #Am^-2(m^3mol^-1)^1.5

cmax_pos_sim = 63104.00 #molm^-3 # m
cmax_neg_sim = 33133.00 #molm^-3 # m 

n = 1000 #set number of nodes to 100

###### Check parameters are valid ######
UI.verify_params(output_filename_positive, tsteps, dt, n, c0_pos, D_pos, R_pos, a_pos, L_pos, electrode_charge_pos)
UI.verify_params(output_filename_negative, tsteps, dt, n, c0_neg, D_neg, R_neg, a_neg, L_neg, electrode_charge_neg)

###### Manually set up applied current and parallelisation ######
# Number of parallel processors being utilised
nprocs = 40

# Number of current blocks to apply
nsteps = 10

# Set up the currents, start times, run times and wait times for each step
currents = [20.0 for i in range(nsteps)] #positive current corresponds to CHARGING the battery
start_times = [2150.0*i for i in range(nsteps)]
run_times = [150.0 for i in range(nsteps)]
wait_times = [2000.0 for i in range(nsteps)]

# Build the vector of parameters that the solver accepts
params_pos = [dt, c0_pos, D_pos, R_pos, a_pos, L_pos, electrode_charge_pos]
params_neg = [dt, c0_neg, D_neg, R_neg, a_neg, L_neg, electrode_charge_neg]

###### Call the function to perform the full battery parallel solve
UI.GITT_full_cell(output_filename_positive,output_filename_negative,nprocs,currents,start_times,run_times,wait_times,n,params_pos,params_neg)

# Build the vector of parameters that the plotter accepts
plot_params_pos = [K_pos,a_pos,cmax_pos_sim,L_pos]
plot_params_neg = [K_neg,a_neg,cmax_neg_sim,L_neg]

###### Call the plotting function which plots the results of the GITT test with nstep steps
plotter.full_battery_GITT_plots(output_filename_positive,output_filename_negative,start_times,pos_params=plot_params_pos,neg_params=plot_params_neg,SparsifyAnimation=True,animation_interval_time=10)

##### also call the standalone plotting function with a single file
#plotter.gen_plots(output_filename+'0',electrode,SparsifyAnimation=True)


