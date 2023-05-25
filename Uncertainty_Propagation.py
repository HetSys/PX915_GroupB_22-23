
'''! @brief Performs uncertainty propagation

@details Applies normal distributions to parameter inputs and produces a 5 subplot
figure that shows the posterior distribution of the voltage curve.
Each subplot shows the effect of altering one parameter and keeping the others
constant at mean values of input distributions.

'''
#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import user_input_mod as UI
import plotter
import sys

import numpy as np
from scipy.optimize import minimize

import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import netCDF4 as NC

import scipy.stats as st
import seaborn as sns
import pandas as pd

# Define the solver function
# Input - parameter numpy array
# Outputs - V(t)

def in_out_easy_peasy(parameter_np_array):
    output_filename_positive = 'user_input_pos'
    output_filename_negative = 'user_input_neg'

    ######### SET MEAN VALUES #########

    # Import default values
    tsteps, dt, n, _, _, _, _, _, iapp_pos, iapp_label_pos, electrode_charge_pos = UI.set_defaults_pos()
    tsteps, dt, n, c0_neg, D_neg, R_neg, a_neg, L_neg, iapp_neg, iapp_label_neg, electrode_charge_neg = UI.set_defaults_neg()

    # Additional values important for visualisation
    K_pos = 3.42E-6 #Am^-2(m^3mol^-1)^1.5
    K_neg = 6.48E-7 #Am^-2(m^3mol^-1)^1.5

    cmax_pos_sim = 63104.00 #molm^-3 # m
    cmax_neg_sim = 33133.00 #molm^-3 # m 

###############################################################################
    # Read in applied current density from csv file
    #iapp_filename = 'WLTP_m10.csv'
    #iapp, iapp_label, tsteps = UI.iapp_read_csv(iapp_filename)
    iapp = np.concatenate((np.array([20.0 for i in range(50)]),np.array([0.0 for i in range(50)])))
###############################################################################
    
    ######### END MEAN SET VALUES #########
    
    parameters = parameter_np_array
    
    # Check parameters and output filename are valid
    #UI.verify_params(solver_input_filename, tsteps, dt, n, c0, D, R, a, L, electrode_charge)
    #UI.verify_params(solver_input_filename, tsteps, dt, n, parameters[0], parameters[1], parameters[2], parameters[3], parameters[4], electrode_charge)
    
    #Check applied current density is valid 
    #UI.verify_iapp(iapp, iapp_label, tsteps)
    
    '''! 5. Write parameters to file.'''
    UI.write_to_file(output_filename_positive, tsteps, dt, n, parameters[0], parameters[1], parameters[2], parameters[3], parameters[4], iapp, iapp_label_pos, electrode_charge_pos)
    UI.write_to_file(output_filename_negative, tsteps, dt, n, c0_neg, D_neg, R_neg, a_neg, L_neg, iapp, iapp_label_neg, electrode_charge_neg)
    
    
    '''! 6. Call fortran solver.'''
    #UI.call_solver(solver_input_filename)
    
    ###### Call the function to perform the full battery parallel solve
    nprocs = 40 #maximum number of processors you wish solver to use
    
    UI.full_battery_simulation(output_filename_positive,output_filename_negative,nprocs=40)
    
    #######################################################################
    #                       Obtaining QOI (Voltage)                       #
    #######################################################################
    
    # Read in
    cstore_pos,tsteps,nodenum,R_pos,time_axis,dr_pos,electrode_pos = plotter.read_output_file(output_filename_positive)#len(start_times)-1?
    cstore_neg,tsteps,nodenum,R_neg,time_axis,dr_neg,electrode_neg = plotter.read_output_file(output_filename_negative)
    i_app_data_pos = plotter.read_input_current(output_filename_positive)
    i_app_data_neg = plotter.read_input_current(output_filename_negative)
    edge_conc_vals_pos = cstore_pos[:,-1]    #want the values of the edge node for all timesteps
    edge_conc_vals_neg = cstore_neg[:,-1]

    #pos and neg params have form [K,a,cmax,L]
    posparams = [K_pos,parameters[3],cmax_pos_sim,parameters[4]]
    negparams = [K_neg,a_neg,cmax_neg_sim,L_neg]
    #Getting voltage arrays
    pos_voltage = plotter.gen_half_cell_voltage(edge_conc_vals_pos,i_app_data_pos,electrode_pos,tsteps,pos_params=posparams)
    neg_voltage = plotter.gen_half_cell_voltage(edge_conc_vals_neg,i_app_data_neg,electrode_neg,tsteps,neg_params=negparams)
    full_voltage = pos_voltage-neg_voltage
    
    # Return QOI only
    return full_voltage #np.array()

###############################################################################
#               Define the input parameter distributions                      #
###############################################################################
c0_dist = st.norm(1000.0, 25.0)
D_dist = st.norm(4.0e-15, 0.5e-15)
R_dist = st.norm(5.86e-6, 1.0e-6)
a_dist = st.norm(3.0*0.665/5.86e-6, 1.0e5)
L_dist = st.norm(75.6e-6, 5.0e-6)
###############################################################################
###############################################################################

# Specify the number of Monte Carlo samples
num_samples = 40

# Generate random samples from the parameter distributions
c0_samples = c0_dist.rvs(num_samples)
D_samples = D_dist.rvs(num_samples)
R_samples = R_dist.rvs(num_samples)
a_samples = a_dist.rvs(num_samples)
L_samples = L_dist.rvs(num_samples)

# Fix the parameters at the means
fixed_c0 = c0_dist.mean()
fixed_D = D_dist.mean()
fixed_R = R_dist.mean()
fixed_a = a_dist.mean()
fixed_L = L_dist.mean()

# Truncating distributions to remain in range of validity
counter = 0
for i in range(num_samples):
    while (c0_samples[i] < 1.0):
        c0_samples[i] = c0_dist.rvs(1)
        counter += 1
        if (counter > 100):
            print("Error: sample is remaining invalid despite numerous re-assignments")
            sys.exit("Invalid Sample - c0")
    while (D_samples[i] < 0.0):
        D_samples[i] = D_dist.rvs(1)
        counter += 1
        if (counter > 100):
            print("Error: sample is remaining invalid despite numerous re-assignments")
            sys.exit("Invalid Sample - D")
    while (R_samples[i] < 1e-10):
        R_samples[i] = R_dist.rvs(1)
        counter += 1
        if (counter > 100):
            print("Error: sample is remaining invalid despite numerous re-assignments")
            sys.exit("Invalid Sample - R")
    while (a_samples[i] < 1e-10):
        a_samples[i] = a_dist.rvs(1)
        counter += 1
        if (counter > 100):
            print("Error: sample is remaining invalid despite numerous re-assignments")
            sys.exit("Invalid Sample - a")
    while (L_samples[i] < 1e-10):
        L_samples[i] = L_dist.rvs(1)
        counter += 1
        if (counter > 100):
            print("Error: sample is remaining invalid despite numerous re-assignments")
            sys.exit("Invalid Sample - L")

# Number of time steps
tsteps, dt, _, _, _, _, _, _, _, _, _ = UI.set_defaults_pos()
##_, _, tsteps = UI.iapp_read_csv(iapp_filename)
time = np.linspace(0.0,dt*(tsteps-1.),tsteps)

# Initialize an array to store the voltage curves
voltage_curves = np.zeros(tsteps)

#############
#  UQ Plot  #
#############

# Propagate uncertainties and compute the voltage curves for each subplot
fig, axs = plt.subplots(1, 5, figsize=(15, 3), sharex=True, sharey=True)

plt.suptitle("Voltage curves affected by uncertain input parameters")

# Subplot 1: Varying c0
for i in range(num_samples):
    params = np.array([c0_samples[i], fixed_D, fixed_R, fixed_a, fixed_L])
    voltage_curves = in_out_easy_peasy(params)
    axs[0].plot(time, voltage_curves.T, 'b-', alpha=0.1)
axs[0].set_title('Varying c0')
axs[0].set_xlabel('c0')

# Subplot 2: Varying D
for i in range(num_samples):
    params = np.array([fixed_c0, D_samples[i], fixed_R, fixed_a, fixed_L])
    voltage_curves = in_out_easy_peasy(params)
    axs[1].plot(time, voltage_curves.T, 'b-', alpha=0.1)
axs[1].set_title('Varying D')
axs[1].set_xlabel('D')

# Subplot 3: Varying R
for i in range(num_samples):
    params = np.array([fixed_c0, fixed_D, R_samples[i], fixed_a, fixed_L])
    voltage_curves = in_out_easy_peasy(params)
    axs[2].plot(time, voltage_curves.T, 'b-', alpha=0.1)
axs[2].set_title('Varying R')
axs[2].set_xlabel('R')

# Subplot 4: Varying a
for i in range(num_samples):
    params = np.array([fixed_c0, fixed_D, fixed_R, a_samples[i], fixed_L])
    voltage_curves = in_out_easy_peasy(params)
    axs[3].plot(time, voltage_curves.T, 'b-', alpha=0.1)
axs[3].set_title('Varying a')
axs[3].set_xlabel('a')

# Subplot 5: Varying L
for i in range(num_samples):
    params = np.array([fixed_c0, fixed_D, fixed_R, fixed_a, L_samples[i]])
    voltage_curves = in_out_easy_peasy(params)
    axs[4].plot(time, voltage_curves.T, 'b-', alpha=0.1)
axs[4].set_title('Varying L')
axs[4].set_xlabel('L')

fig.supylabel('Voltage, V(t)')
plt.tight_layout()

fig.savefig('UQ.png')

###########################
#   Input Distributions   #
###########################

# Propagate uncertainties and compute the voltage curves for each subplot
figg, axss = plt.subplots(1, 5, figsize=(15, 3), sharex=False, sharey=False)
resol = 1000

# Subplot 1: c0
x = np.linspace(900, 1100, resol)
axss[0].plot(x, c0_dist.pdf(x), 'b-', alpha=1.0)
axss[0].set_title('c0 input distribution')
axss[0].set_xlabel('c0')

# Subplot 2: D
x = np.linspace(0.0, 8.0e-15, resol)
axss[1].plot(x, D_dist.pdf(x), 'b-', alpha=1.0)
axss[1].set_title('D input distribution')
axss[1].set_xlabel('D')

# Subplot 3: R
x = np.linspace(0.0e-6, 11.72e-6, resol)
axss[2].plot(x, R_dist.pdf(x), 'b-', alpha=1.0)
axss[2].set_title('R input distribution')
axss[2].set_xlabel('R')

# Subplot 4: a
x = np.linspace(2.2e5, 4.6e5, resol)
axss[3].plot(x, a_dist.pdf(x), 'b-', alpha=1.0)
axss[3].set_title('a input distribution')
axss[3].set_xlabel('a')

# Subplot 5: L
x = np.linspace(25.0e-6, 126.2e-6, resol)
axss[4].plot(x, L_dist.pdf(x), 'b-', alpha=1.0)
axss[4].set_title('L input distribution')
axss[4].set_xlabel('L')

# Adjust spacing between subplots
plt.tight_layout()

figg.savefig('Input_Distributions.png')

#c0_dist = st.norm(1000.0, 25.0)
#D_dist = st.norm(4.0e-15, 0.5e-15)
#R_dist = st.norm(5.86e-6, 1.0e-6)
#a_dist = st.norm(3.0*0.665/5.86e-6, 1.0e5)#340443.68
#L_dist = st.norm(75.6e-6, 5.0e-6)
