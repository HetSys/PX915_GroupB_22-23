#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''! @brief First order sensitivity analysis.
@details A python file that is used to produce first order sensitivity analysis
Running the file outputs a sensitivity gif.

Credit to Dr. James Kermode for some functions recycled from PX914.

'''

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

#Holder function for code
def sensitivity_over_time():
    # Function that takes parameters for sensitivity analysis only
    # and returns the QOI
    # Notably - this QOI is a scalar for each time step and so the holder
    # function is used to alter the time.
    def in_out_easy_peasy(parameter_np_array):
        solver_input_filename = 'user_input'
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

        # Read in applied current density from csv file
        #iapp_filename = 'WLTP_m10.csv'
        #iapp, iapp_label, tsteps = UI.iapp_read_csv(iapp_filename)
        iapp = np.concatenate((np.array([20.0 for i in range(50)]),np.array([0.0 for i in range(50)])))

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
        return full_voltage[idx]
    
    def first_order_sensitivities(Q, variables, Q0, eps=1e-6):
        x0 = np.array([v.mean() for v in variables])
        dQ_dx = np.zeros(len(x0))
        for i, xi in enumerate(x0):
            xp = x0.copy()
            h = eps*xi
            xp[i] = xi + h
            dQ_dx[i] = (Q(xp) - Q0)/h
        return dQ_dx

    def second_order_sensitivities(Q, variables, Q0, eps=1e-4):
        x0 = np.array([v.mean() for v in variables])
        dQ2_dx2 = np.zeros((len(x0), len(x0)))

        for i, xi in enumerate(x0):
            for j, xj in enumerate(x0):
                if i == j:
                    # diagonal matrix entries
                    hi = eps*xi

                    xp = x0.copy()
                    xp[i] = xi + hi

                    xm = x0.copy()
                    xm[i] = xi - hi

                    dQ2_dx2[i,i] = (Q(xp) - 2*Q0 + Q(xm))/hi**2

                elif i > j:
                    # off-diagonal matrix entries, noting symmetry on swapping of i and j
                    hi = eps*xi
                    hj = eps*xj

                    x1 = x0.copy()
                    x1[i] += hi
                    x1[j] += hj

                    x2 = x0.copy()
                    x2[i] += hi
                    x2[j] -= hj

                    x3 = x0.copy()
                    x3[i] -= hi
                    x3[j] += hj

                    x4 = x0.copy()
                    x4[i] -= hi
                    x4[j] -= hj

                    dQ2_dx2[i,j] = (Q(x1) - Q(x2) - Q(x3) + Q(x4))/(4*hi*hj)

        # fill upper triangle
        i_upper = np.triu_indices(len(x0))
        dQ2_dx2[i_upper] = dQ2_dx2.T[i_upper]

        return dQ2_dx2

    def plot_sensitivities(f, variables, variable_names,
                           Q0=None, dQ_dx=None, d2Q_dx2=None,
                           second_order=False, logscale=False):
        fig = plt.figure()
        x0 = np.array([v.mean() for v in variables])
        sigma0 = np.array([v.std() for v in variables])
        if Q0 is None:
            Q0 = f(x0)
        if dQ_dx is None:
            dQ_dx = first_order_sensitivities(f, x0, Q0)
        df = pd.DataFrame.from_dict(dict(parameter=variable_names,
                                         mean=x0,
                                         sigma=sigma0,
                                         sensitivity=dQ_dx,
                                         scaled_sensitivity=x0*dQ_dx,
                                         sensitivity_index=sigma0*dQ_dx))

        if second_order:
            if d2Q_dx2 is None:
                d2Q_dx2 = second_order_sensitivities(f, x0, Q0)
            for i, p1 in enumerate(variable_names):
                for j, p2 in enumerate(variable_names):
                    if i < j:
                        continue
                    df.loc[len(df)] = {'parameter': f'{p1}{p2}',
                                       'mean': x0[i]*x0[j],
                                       'sigma': sigma0[i]*sigma0[j],
                                       'sensitivity': d2Q_dx2[i,j],
                                       'scaled_sensitivity': x0[i]*x0[j]*d2Q_dx2[i,j],
                                       'sensitivity_index': sigma0[i]*sigma0[j]*d2Q_dx2[i,j]}

        ax = df['scaled_sensitivity'].abs().plot(xticks=df.index, logy=logscale)
        ax.set_xticklabels(df.parameter, rotation=45)
        ax.set_ylabel('Absolute scaled sensitivity')
        ax.set_xlabel('Parameter')
        ax.set_title('Absolute Scaled Sensitivities')
        fig.savefig('sensitivities.png')
        return df, ax

    #import scipy.stats as st
    def Uncert_Prop():
        # Setup distributions
        #c0, D, R, a, L
        c0 = st.norm(1000.0, 25.0)
        D = st.norm(4.0e-15, 0.5e-15)
        R = st.norm(5.86e-6,1.0e-6)
        #e_act = 0.665
        #a = 3*e_act/R
        a = st.norm(3.0*0.665/5.86e-6,1.0e5)
        L = st.norm(75.6e-6,5.0e-6)
        
        #c0 = st.norm(1000.0, 1.0e-16)
        #D = st.norm(4.0e-15, 1.0e-16)
        #R = st.norm(5.86e-6,1.0e-16)
        #e_act = 0.665
        #a = st.norm(3.0*e_act/5.86e-6,1.0e-16)
        #L = st.norm(75.6e-6,1.0e-16)
        
        variables = [c0, D, R, a, L]
        variable_names = ['c0', 'D', 'R', 'a', 'L']
        
        df,ax = plot_sensitivities(in_out_easy_peasy,variables,variable_names)
        #df
        print("Sensitivities Dataframe: ",df)

        #return None#Change this to scaled sensitivities from the dataframe
        return np.abs(df.loc[:, 'scaled_sensitivity'])
    
    # Number of samples of V(t) over time
    num_samples = 50
    # Number of time steps - used to generate sample indices
    t_steps, _, _, _, _, _, _, _, _, _, _ = UI.set_defaults_pos()
    print("t_steps: ", t_steps)
    #t_steps = 100
    volt_idx_vals = np.linspace(1, t_steps-1, num=num_samples, dtype=int)
    print("Volt_idx_vals: ", volt_idx_vals)
    #Initialise matrix
    scaled_sens_mat = np.zeros((num_samples, 5)) # number of parameters = 5
    # Sample and write values to a matrix
    for k, index in enumerate(volt_idx_vals):
        idx = index
        scaled_sens_mat[k,:] = Uncert_Prop()
        print("Scaled sensitivities retrieved: ", scaled_sens_mat[k,:])
    
    # 5 columns - each of these is a scaled sensitivity corresponding to the
    # order [c0,D,R,a,L]
    # Each row corresponds to the sensitivity at a specific time for V(t)
    # i.e. row 0 contains the scaled sensitivities corresponding
    # to inputs [c0,D,R,a,L] at t=dt.
    return scaled_sens_mat

#array_params_with_strings

# Call function to perform sensitivity analysis and store sensitivities over
# time
scaled_sensitivities_matrix = sensitivity_over_time()
print("Scaled sensitivities matrix below: ")
print(scaled_sensitivities_matrix)


# Create the figure and axis
fig, ax = plt.subplots()

# String array for x-axis labels
# c0, D, R, a, L
x_labels = ['c0', 'D', 'R', 'a', 'L']

# Initialize the line plot
line, = ax.plot([], [], 'b-')
#line, = ax.semilogy([],[],'b-')

# Set the x-axis labels
ax.set_xticks(range(len(x_labels)))
ax.set_xticklabels(x_labels)

# Set the y-axis limits
ax.set_ylim(0.0,0.1)
#ax.set_ylim(1e-16, 0.1)

# Set the y-axis label
ax.set_ylabel('Absolute Scaled Sensitivities')

# Set the title
ax.set_title('Absolute Scaled Sensitivities Over Time')

# Update function called for each frame

# Update function called for each frame
def update(frame):
    # Clear the previous plot
    ax.cla()

    # Set the x-axis labels
    ax.set_xticks(range(len(x_labels)))
    ax.set_xticklabels(x_labels)
    ax.set_xlabel('Parameters')

    # Set the y-axis limits
    ax.set_ylim(0.0,0.1)
    #ax.set_ylim(1e-16, 0.1)
    
    # Set the y-axis label
    ax.set_ylabel('Absolute Scaled Sensitivities')

    # Set the title
    ax.set_title('Absolute Scaled Sensitivities Over Time')

    # Plot the current frame of data
    line.set_data(range(len(x_labels)), scaled_sensitivities_matrix[frame])
    
    # Redraw the plot
    ax.plot(range(len(x_labels)), scaled_sensitivities_matrix[frame], 'b-')
    #ax.semilogy(range(len(x_labels)), scaled_sensitivities_matrix[frame], 'b-')

    return line,

# Create the animation
ani = FuncAnimation(fig, update, frames=len(scaled_sensitivities_matrix), blit=True)
ani.save(filename = 'sensitivities.gif', writer = 'pillow', fps = 25)

# Display the animationani.save(filename = 'concentration_animation.gif', writer = 'pillow', fps = 30)
plt.show()


#ani = FuncAnimation(fig, update, interval=animation_interval_time, frames=time_step_axis,blit=True)#plt.xlim([990,1000])
