'''! @brief a running script which exists to demonstrate how one uses the GITT function to run simulations in parallel.'''
import user_input_mod as UI
import plotter 
import sys

# This is a running script which exists to demonstrate how one uses the 
# GITT function to run simulations in parallel

'''! These simulations consist of applying a step function of current to a battery which turns on to a fixed value for a 
given time interval, then back to 0, and then on again, etc. The ascii art below indicates schematically how a current-time
looks during this experiment.'''


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
'''! To run one of these simulations, one needs the following things:
   - A vector containing the fixed values of current applied for each block of current.
   - A vector containing the start times of each current block.
   - A vector containing the duration of each current block.'''

'''!Uncomment to print stdout to file.'''
# sys.stdout = open('test.txt', 'w')

'''!Set filename to output user input parameters'. Do not enter a file extension. Max characters = 50
!@var str output_filename: String for filename.
!@var str electrode: 'positive' or 'negative - denoting if electrode is anode or cathode respectively.'''
# Set string for filename. Do not enter a file extension. Max characters = 50
output_filename = 'current_step'
electrode = 'positive'

'''!Import default values from https://doi.org/10.1149/1945-7111/ab9050'''
# Use UI.set_defaults_pos() for positive electrode and ..._neg() for negative electrode
'''!@var float dt: Timestep size (s), float > 0.
@var float dt: Timestep size (s), float > 0. 
@var float c0: Initial concentration (mol m**-3), float >= 0.
@var float D: Diffusion constant (m**2 s**-1), float.
@var float R: Width of block (m), float > 0.
@var float a: Particle surface area per unit volume (m**-1), float >= 0.
@var float L: Electrode thickness(m), float >= 0.
@var 1D array (len=tsteps), float iapp: Applied current density (A m**2), 1D array of floats of length tsteps.
@var string iapp_label: Label of applied current density, string.
@var string electrode_charge: Label of electrode charge as positive or negative. 
@var float K_pos: Reaction rate at positive electrodes (Am^-2(m^3mol^-1)^1.5).
@var float K_neg: Reaction rate at negative electrodes Am^-2(m^3mol^-1)^1.5.
@var float cmax_pos_sim: Maximum concentration at positive electrode.
@var float cmax_pos_sim: Maximum concentration at negative electrode.
@var int nprocs: Number of parallel processors being utilised.
!@var int nsteps: Number of current blocks to apply.
@var list of floats current: Sequential currents applied at each step in GITT (A/m^2).
@var list of floats start_times: Sequential times (s) that each step of GITT charging begins.
@var list of floats run_times: Sequential times (s) that each charge in GITT is applied for.
@var wait_times list of floats: Sequential times (s) that battery is left to rest after charging time'''
tsteps, dt, n, c0, D, R, a, L, iapp, iapp_label, electrode_charge = UI.set_defaults_neg()

'''!Set parameters.'''

c0 = 30000.0
dt = 1.0

K_pos = 3.42E-6 #Am^-2(m^3mol^-1)^1.5
K_neg = 6.48E-7 #Am^-2(m^3mol^-1)^1.5

cmax_pos_sim = 63104.00 #molm^-3 # m
cmax_neg_sim = 33133.00 #molm^-3 # m 

'''!Check parameters are valid.'''
UI.verify_params(output_filename, tsteps, dt, n, c0, D, R, a, L, electrode_charge)


'''! Manually set up applied current and parallelisation'''

nprocs = 5

nsteps = 5

'''! Set up the currents, start times, run times and wait times for each step.'''
currents = [20.0 for i in range(nsteps)]
start_times = [2150.0*i for i in range(nsteps)]
run_times = [150.0 for i in range(nsteps)]
wait_times = [2000.0 for i in range(nsteps)]

'''! Build the vector of parameters that the function accepts'''
params = [dt, c0, D, R, a, L,electrode_charge]

'''!Call the function to perform the parallel solve.'''
UI.GITT_half_cell(output_filename,nprocs,currents,start_times,run_times,wait_times,n,params)


'''!Build the vector of parameters that the plotter accepts.'''
plot_params_neg = [K_neg,a,cmax_neg_sim,L]

'''!Call the plotting function which plots the results of the GITT test with nstep steps.'''
plotter.plot_halfcell_GITT_result(output_filename,start_times,electrode,neg_params=plot_params_neg,Animation=True,SparsifyAnimation=True)

'''!Uncomment to call the standalone plotting function with a single file.'''
#plotter.gen_plots(output_filename+'0',electrode,SparsifyAnimation=True)


