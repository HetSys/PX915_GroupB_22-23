import numpy as np
import user_input_mod as UI
import plotter
import sys


def run_unit_test(nodenum=500,dt=1.0,num_cores=1):
    # unit test which provides a gradually increasing amount of current
    # on a full cell for 500 seconds and then rests for 2000 seconds
    # returns the lithium mass loss as a percentage between the start
    # and end of the simulation for convergence testing. 
    output_filename_positive = 'user_input_pos'
    output_filename_negative = 'user_input_neg'

    tsteps, dt_new, n, c0, D_neg, R_neg, a_neg, L_neg, iapp_neg, iapp_label_neg, electrode_charge_neg = UI.set_defaults_neg()
    tsteps, dt_new, n, c0, D_pos, R_pos, a_pos, L_pos, iapp_pos, iapp_label_pos, electrode_charge_pos = UI.set_defaults_pos()

    n = nodenum
    dt = dt
    tsteps = int(2500/dt)

    c0_pos = 30000.0 #note that the simulation will be charged, then discharged.
    c0_neg = 1.0 #not exactly 0 to prevent errors

    # Additional values important for visualisation
    K_pos = 3.42E-6 #Am^-2(m^3mol^-1)^1.5
    K_neg = 6.48E-7 #Am^-2(m^3mol^-1)^1.5

    cmax_pos_sim = 63104.00 #molm^-3 # m
    cmax_neg_sim = 33133.00 #molm^-3 # m 

    UI.verify_params(output_filename_positive, tsteps, dt, n, c0_pos, D_pos, R_pos, a_pos, L_pos, electrode_charge_pos)
    UI.verify_params(output_filename_negative, tsteps, dt, n, c0_neg, D_neg, R_neg, a_neg, L_neg, electrode_charge_neg)

    iapp = np.concatenate((np.array([20.0*(i/int(500/dt)) for i in range(int(500/dt))]),np.array([0.0 for i in range(int(2500/dt)-int(500/dt))])))

    print('Parameters sucessfully imported, writing to file...')
    
    '''! 5. Write parameters to file.'''
    UI.write_to_file(output_filename_positive, tsteps, dt, n, c0_pos, D_pos, R_pos, a_pos, L_pos, iapp, iapp_label_pos, electrode_charge_pos)
    UI.write_to_file(output_filename_negative, tsteps, dt, n, c0_neg, D_neg, R_neg, a_neg, L_neg, iapp, iapp_label_neg, electrode_charge_neg)

    ###### Call the function to perform the full battery parallel solve
    nprocs = 40 #maximum number of processors you wish solver to use
    
    print('Input file written, calling solver.....')
    UI.full_battery_simulation(output_filename_positive,output_filename_negative,nprocs=num_cores)

    print('Solver finished, extracting final Voltage and Li mass loss')
    
    ##### Get the final voltage and the Li mass loss and return it

    ######## get voltage###############
    cstore_pos,tsteps,nodenum,R_pos,time_axis,dr_pos,electrode_pos = plotter.read_output_file(output_filename_positive)
    cstore_neg,tsteps,nodenum,R_neg,time_axis,dr_neg,electrode_neg = plotter.read_output_file(output_filename_negative)
    i_app_data_pos = plotter.read_input_current(output_filename_positive)
    i_app_data_neg = plotter.read_input_current(output_filename_negative)

    edge_conc_vals_pos = cstore_pos[:,-1]    #want the values of the edge node for all timesteps
    edge_conc_vals_neg = cstore_neg[:,-1]

    # Build the vector of parameters that the plotter accepts
    plot_params_pos = [K_pos,a_pos,cmax_pos_sim,L_pos]
    plot_params_neg = [K_neg,a_neg,cmax_neg_sim,L_neg]
    
    pos_voltage = plotter.gen_half_cell_voltage(edge_conc_vals_pos,i_app_data_pos,electrode_pos,tsteps,pos_params=plot_params_pos)
    neg_voltage = plotter.gen_half_cell_voltage(edge_conc_vals_neg,i_app_data_neg,electrode_neg,tsteps,neg_params=plot_params_neg)

    full_voltage = pos_voltage-neg_voltage


    ##########get mass loss #############
    e_act_pos = (a_pos*R_pos)/3
    e_act_neg = (a_neg*R_neg)/3
    li_frac_list = []
    r_pos_axis = np.array([i*dr_pos for i in range(nodenum)])
    r_neg_axis = np.array([i*dr_neg for i in range(nodenum)])
    
    #get denominator of expression from initial concentrations
    li_avg_conc_pos = plotter.get_avg_li_conc(cstore_pos[0,:],r_pos_axis)
    li_avg_conc_neg = plotter.get_avg_li_conc(cstore_neg[0,:],r_neg_axis)
    c0_eact_L = li_avg_conc_pos*e_act_pos*L_pos + li_avg_conc_neg*e_act_neg*L_neg

    #find fractional lithium mass loss per step and print
    li_avg_conc_pos = plotter.get_avg_li_conc(cstore_pos[tsteps-1,:],r_pos_axis)
    li_avg_conc_neg = plotter.get_avg_li_conc(cstore_neg[tsteps-1,:],r_neg_axis)
    li_frac = (li_avg_conc_pos*e_act_pos*L_pos + li_avg_conc_neg*e_act_neg*L_neg)/c0_eact_L

    return full_voltage[-1], (1.0-li_frac)
