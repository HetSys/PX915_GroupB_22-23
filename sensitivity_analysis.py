#!/usr/bin/env python3
# -*- coding: utf-8 -*-

#Want to see effect on the output voltage

'''
Credit to Dr. James Kermode for code provided during PX914 module
'''

'''
What's up with the code?

i) Dataframe produced by plot_sensitivities is containing NaN values
ii) sensitivities.png blank due to above
iii) Filenames are fairly fixed, i.e. this reads user_input_output.nc and no
     other filenames are currently specified/accepted
iv) Current read-in from one file - is this a good set of current values?
v) Check that parameters are ok - i.e. do we need to do sensitivity analysis on
   a?
vi) Check QOI. Currently just a voltage value.
vii) Maybe remove the scipy minimize - this was useful for debugging and gives
     some indication of sensitivities.

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


# Sensitivity analysis function
#def sensitivity_analysis(parameters):
#    # Function to calculate the quantity of interest with a given set of parameters
#    def objective(p):
#        return solver(parameters + p)  # Call the solver with the modified parameters
#
#    # Perform sensitivity analysis
#    sensitivity = minimize(objective, np.zeros_like(parameters))
#    return sensitivity.x

def in_out_easy_peasy(parameter_np_array):
    solver_input_filename = 'user_input'
    
    ######### SET MEAN VALUES #########

    # Import default values
    tsteps, dt, n, _, _, _, _, _, iapp, iapp_label, electrode_charge = UI.set_defaults_pos()

    # Additional values important for visualisation
    K_pos = 3.42E-6 #Am^-2(m^3mol^-1)^1.5
    K_neg = 6.48E-7 #Am^-2(m^3mol^-1)^1.5

    cmax_pos_sim = 63104.00 #molm^-3 # m
    cmax_neg_sim = 33133.00 #molm^-3 # m 

    # Read in applied current density from csv file
    iapp_filename = 'WLTP_m10.csv'
    iapp, iapp_label, tsteps = UI.iapp_read_csv(iapp_filename)

    ######### END MEAN SET VALUES #########
    
    parameters = parameter_np_array
    
    # Check parameters and output filename are valid
    #UI.verify_params(solver_input_filename, tsteps, dt, n, c0, D, R, a, L, electrode_charge)
    #UI.verify_params(solver_input_filename, tsteps, dt, n, parameters[0], parameters[1], parameters[2], parameters[3], parameters[4], electrode_charge)
    
    #Check applied current density is valid 
    #UI.verify_iapp(iapp, iapp_label, tsteps)
    
    '''! 5. Write parameters to file.'''
    UI.write_to_file(solver_input_filename, tsteps, dt, n, parameters[0], parameters[1], parameters[2], parameters[3], parameters[4], iapp, iapp_label, electrode_charge)
    
    '''! 6. Call fortran solver.'''
    UI.call_solver(solver_input_filename)
    
    #######################################################################
    
    '''!@package read_output_file: Reads in some data from user input and solver output using NetCDF'''
    '''!@var 2D array of floats cstore: Contains concentrations of Lithium over discretized time and space points.'''
    '''!@var int tsteps: Number of timesteps'''
    '''!@var int nodenum: Number of discrete spatial points'''
    '''!@var float R: radius of sphere in model (m).'''
    '''!@var 1D list of floats time_axis: Times associated with each timestep (s).'''
    '''!@var float dr: Space between the discrete spatial points (m).'''
    '''!@var str electrode: 'positive' or 'negative' - determines the nature of the electrode in this half SPM setup.'''
    def read_output_file(filename,step_num=None):
        #Read in Concs from netcdf
        if step_num is not None:
            filename = filename+str(step_num)
        filename = filename + '_output.nc'
        '''!Read in NetCDF output file'''
        dat=NC.Dataset(filename, "r", format ="NETCDF")
        cstore = dat.variables['cstorage'][:]
        '''!Read in Constants'''
        tsteps=dat.variables['tsteps'][0] 
        nodenum=dat.variables['node_num'][0] 
        R=dat.variables['R'][0] 
        time_axis = dat.variables['time_axis'][:]
        dr = R/(nodenum-1)
        electrode_read = dat.variables['electrode_charge'][0] 
        electrode_temp = np.ma.getdata(electrode_read)
        electrode_temp = electrode_temp.tolist()
        electrode_temp = electrode_temp.decode('UTF-8')
        if (electrode_temp=='p'):
            electrode = 'positive'
        if (electrode_temp=='n'):
            electrode = 'negative'
    
        return cstore,tsteps,nodenum,R,time_axis,dr,electrode
    
    def read_input_current(filename,step_num=None):
        if step_num is not None:
            filename = filename+str(step_num)
        filename = filename + '.txt'
    
        with open(filename, 'r') as iapp_vals:
            lines = iapp_vals.readlines()
            i_app_data = []
            for i, current in enumerate(lines):
                if i >=11:
                    i_app_data.append(current.strip())
    
        i_app_data = np.array(i_app_data, dtype = float)
        return i_app_data
    
    #DEFINE VOLTAGE FUNCTIONS:
    def gen_half_cell_voltage(edge_conc_vals,i_app_data,electrode,tsteps,pos_params=None,neg_params=None):
    
        #pos and neg params have form [K,a,cmax,L]
        ################  Voltage and Concentration Plot ##################
    
        if pos_params is not None:
            [K_pos,a_pos,cmax_pos_sim,L_pos] = pos_params
        else:
            #these won't be used, can assign them as 0
            K_pos = 0.0
            a_pos = 0.0
            cmax_pos_sim = 0.0
            L_pos = 0.0
        
    
        if neg_params is not None:
            [K_neg,a_neg,cmax_neg_sim,L_neg] = neg_params
        else:
            #these won't be used, can assign them as 0
            K_neg = 0.0
            a_neg = 0.0
            cmax_neg_sim = 0.0
            L_neg = 0.0
        
        
        '''!At time = 0s, it is possible for the output of j_function to be zero if Lithium concentration at sphere edge is zero. This results in an error as V_function
        involves division by the output of j_function.'''
    
        #### Constants ####
        F = 96485 #C/mol
        R_g =  8.314 # J K-1 mol-1
        T = 298.15 # K
    
    
        #This is the j(c) function from our model, used to calculate the voltage
        #Inputs
        #Concentration at edge of sphere - c_R
        #c_max constant - c_max
        #Constants - F, K
        #Outputs
        #j(c) for voltage calculation
        #Notes
        #Requires numpy library for sqrt
        def j_function(c_R,c_max,K):
            if c_R<0.0:
                print('Error, concentration should never be less than 0')
                raise ValueError
            ratio = (c_R / c_max)
            return F * K * np.sqrt(np.abs(ratio * (1 - ratio)))
    
        #Inputs
        #Concentration at edge of sphere - c_R
    
        def U_function_pos(c_R):
            x = c_R/cmax_pos_sim
            u = (-0.8090*x) + 4.4875 - (0.0428*np.tanh(18.5138*(x - 0.5542))) - (17.7326*np.tanh(15.7890*(x-0.3117))) + (17.5842*np.tanh(15.9308*(x - 0.3120)))  #positive electrode U
            return u
    
        def U_function_neg(c_R):
            x = c_R/cmax_neg_sim
            u = (1.9793*np.exp(-39.3631*x)) + 0.2482 - (0.0909*np.tanh(29.8538*(x-0.1234))) - (0.04478*np.tanh(14.9159*(x-0.2769))) - (0.0205*np.tanh(30.4444*(x-0.6103)))  #negative electrode U
            return u
    
    
        #This is the V(t) voltage function from our model
        #Inputs
        #Time - t
        #Concentration at edge of sphere - c_R
        #c_max constant - c_max
        #Constants - F, K, R_g, T, a, L
        #Outputs
        #V(t) voltage as a function of time
        #Notes
        #Requires custom j_function.
        #Requires numpy for arcsinh function
        #Requires custom i_app(t) function
        def voltage_function(U,i_app,jay,L,a):
            v = U - ((2*R_g*T)/(F))*np.arcsinh((i_app)/(a*L*jay))
            return v
                
        if electrode=='positive':
            c_max = cmax_pos_sim
            K = K_pos
            a = a_pos
            L = L_pos
        elif electrode=='negative':
            c_max = cmax_neg_sim
            K = K_neg
            a = a_neg
            L = L_neg
        else:
            print('Electrode not recognised!')
            raise ValueError
        
    
        volt_store = []
    
        for i in range(tsteps):
            i_app_temp = i_app_data[i]
            #print('iapp',i_app_temp)
            c_temp = edge_conc_vals[i]      
            #print('c_temp',c_temp)  
            j_temp = j_function(c_temp,c_max,K)
            #print('jtemp',j_temp)
            if electrode == 'positive':
                u_temp = U_function_pos(c_temp)
            elif electrode == 'negative':
                u_temp = U_function_neg(c_temp)
            else:
                print('Electrode charge not recognised!')
                raise ValueError
            #print('u_temp', u_temp)
            if j_temp == 0:
                volt_store.append(0)            #use to prevent division by zero
            else:
                volt_store.append(voltage_function(u_temp,i_app_temp,j_temp,L,a))
    
        return np.array(volt_store)
    #Read in
    fname_read = 'user_input'
    cstore,tsteps,nodenum,R,time_axis,dr,electrode = read_output_file(fname_read,step_num=None)#len(start_times)-1?
    #structure of cstore: each row represents a single timestep, each column a single node
    edge_conc_vals = cstore[:,-1]    #want the values of the edge node for all timesteps
    current_fname = 'user_input'
    i_app_data = read_input_current(current_fname) #What is filename?
    volt_store = gen_half_cell_voltage(edge_conc_vals,i_app_data,electrode,tsteps,pos_params=None,neg_params=None)
    #What is QOI?
    return volt_store[-1]

# Sensitivity analysis - takes np.array([])
def sensitivity_analysis(params):
    #Function to calc QOI with given params
    def objective(p):
        #Perturb parameters
        parameters_perturbed = params + p #Modify parameters
        #Call solver using input to output function
        return in_out_easy_peasy(parameters_perturbed) #QOI

    # Perform sensitivity analysis
    sensitivity = minimize(objective, params)
    return sensitivity.x

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
    fig.savefig('sensitivities.png')
    return df, ax

#import scipy.stats as st
def Uncert_Prop():
    # Setup distributions
    #c0, D, R, a, L
    c0 = st.norm(1000.0, 25.0)
    D = st.norm(4.0e-15, 0.5e-15)
    R = st.norm(5.86e-6,1.0e-6)
    e_act = 0.665
    #a = 3*e_act/R
    a = st.norm(3.0*e_act/5.86e-6,0.1)
    L = st.norm(75.6e-6,5.0e-6)
    variables = [c0, D, R, a, L]
    variable_names = ['c0', 'D', 'R', 'a', 'L']
    
    df,ax = plot_sensitivities(in_out_easy_peasy,variables,variable_names)
    df
    print("Sensitivities Dataframe: ",df)

    return None

# Call uncert prop - should produce a .png of a plot of sensitivities
# Also produces a data frame with information
Uncert_Prop()


# Utilising the sensitivity anaylsis function
solver_input_filename = 'user_input'

######### SET MEAN VALUES #########

# Import default values
tsteps, dt, n, c0, D, R, a, L, iapp, iapp_label, electrode_charge = UI.set_defaults_pos()

# Additional values important for visualisation
K_pos = 3.42E-6 #Am^-2(m^3mol^-1)^1.5
K_neg = 6.48E-7 #Am^-2(m^3mol^-1)^1.5

cmax_pos_sim = 63104.00 #molm^-3 # m
cmax_neg_sim = 33133.00 #molm^-3 # m 

# Read in applied current density from csv file
iapp_filename = 'WLTP_m10.csv'
iapp, iapp_label, tsteps = UI.iapp_read_csv(iapp_filename)

######### END MEAN SET VALUES #########

# Build list of initial parameter means
initial_params = []
#initial_params.append(tsteps)
#initial_params.append(dt)
#initial_params.append(n)
initial_params.append(c0)
initial_params.append(D)
initial_params.append(R)
initial_params.append(a)
initial_params.append(L)
#initial_params.append(electrode_charge)

#List to numpy array
param_arr = np.array(initial_params)
# Perform sensitivity analysis for the initial parameter values
sensitivity_result = sensitivity_analysis(param_arr)

print("Sensitivity analysis result:", sensitivity_result)
