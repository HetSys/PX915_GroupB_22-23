'''! @brief Functions for visualisation of solver outputs. (Plots are saved locally.)
@details Contains functions that produce:
- An animation showing the concentration of lithium as a function of time and space. 
(There is an additional feature for saving the concentration profile at the end of the simulation as a png.)
- Plots of both voltage and applied current over time.
'''
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation
import netCDF4 as NC
#Written so user can specify if is positive or negative electrode


def read_output_file(filename, step_num = None):
    '''!@brief Reads in data from user input and solver output using NetCDF.
    @param[in] filename  The name of the input file, this must be a string and have max 50 characters. No file extension is required.
    @param[in] step_num  Specifies the step number to read for a parallel simulation (defualt = none for serial)
    '''

    #Read in Concs from netcdf
    if step_num is not None:
        filename = filename+str(step_num)
    filename = filename + '_output.nc'

    # Read in NetCDF output file
    dat=NC.Dataset(filename, "r", format ="NETCDF")
    cstore = dat.variables['cstorage'][:]   # concentrations of Li over discretized time and space

    # Read in Constants
    tsteps=dat.variables['tsteps'][0]   # Number of timesteps.
    nodenum=dat.variables['node_num'][0]   # Number of discrete spatial points
    R=dat.variables['R'][0]   # Radius of sphere in model (m)
    time_axis = dat.variables['time_axis'][:]   # Times associated with each timestep (s)
    dr = R/(nodenum-1)   # Space between the discrete spatial points (m)

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
    '''!@brief Reads in the applied current.
    @param[in] filename  The name of the input file, this must be a string and have max 50 characters. No file extension is required.
    @param[in] step_num  Specifies the step number to read for a GITT simulation where there are multiple current steps (defualt = none for serial)
    '''
    if step_num is not None:
        filename = filename+str(step_num)
    filename = filename + '.txt'

    with open(filename, 'r') as iapp_vals:
        lines = iapp_vals.readlines()

        # Array of applied current density (A/m^2) at all time points
        i_app_data = []
        for i, current in enumerate(lines):
            if i >=11:
                i_app_data.append(current.strip())

    i_app_data = np.array(i_app_data, dtype = float)
    return i_app_data



def animated_conc_plot(intervaltime,dr,tsteps,nodenum,cstore,time_axis,SaveFinalState=False,SparsifyAnimation=False):
    '''!@brief Saves animation of lithium concentration over time.
    @details Saves the animation 'concentration_animation.gif', that displays the evolution of lithium concentration over time.
    If passed the argument 'SaveFinalState = True', it saves an image 'final_state.png' of lithium concentration across the sphere at the final timestep.
    If passed the argument 'SparsifyAnimation = True', it saves 1 in every 10 timesteps rather than every single one.
    '''
    
    # Time between frames in animation (ms)
    intervaltime  = 10

    #build time step axis - a list of consecutive numbers from 1-(tsteps-1) 
    # specifies number of iterable frames to render in animation
    if SparsifyAnimation:
        time_step_axis = [i*10 for i in range(1,int(tsteps/10))]
    else:
        time_step_axis = [i for i in range(1,tsteps)]

        # positions of nodes (m) and Li concentrations (molm^-3) at these positions for specified points in time
    vals = np.zeros([nodenum,tsteps+1])

    #place the c storage values in to the valuthiumes matrix.
    for i in range(nodenum):
        vals[i,1:] = cstore[:,i]
        vals[i,0] = i*dr
        #structure of cstore: each row represents a single timestep, each column a single node
        #structure of vals: each column is a single timestep, each row is a single node

    if SaveFinalState:
        plt.figure()
        #make plot of final state and save figure for reference
        plt.plot(vals[:,0],vals[:,-1])
        plt.xlabel('Distance from Sphere Centre (m)')
        plt.ylabel('Concentration (molm$^-3$)')
        plt.title('Concentration Profile at End of Simulation')
        plt.savefig('final_state.png')


    #Generate the subplot axis, label this ax1
    fig, ax1 = plt.subplots(1)
    xdata, ydata = [], []
    #Plot initial graphs for animation
    ax1.set_xlabel('Distance from Sphere Centre (m)')
    ax1.set_ylabel('Concentration of Lithium (molm$^-3$)')
    ax1.set_ylim([np.min(vals[:,1:]),np.max(vals[:,1:])+1])
    graph, = ax1.plot(vals[:,0],vals[:,1])

    #Create a list which holds both the things we wish to animate
    graphlines = [graph]

    r_time_axis = [round(i,2) for i in time_axis]   #
    timestep_list = [str(i) for i in r_time_axis]   #Create list of strings of timestep times.



    def update(t):
        '''!@brief Define an update function for the animation. 
        @details This function is called for each frame to update the graph. As blitting is set to on (it has to be to use 
        reasonable computing power). 
        Returns a single array containing objects to animate with new data set.
        @param[in] t  The timestep at which the function is being called.
        '''
        #Set the data of each element of graphlines to the corresponding value of t passed to the function
        graphlines[0].set_data(vals[:,0],vals[:,t])
        ax1.set_title('Concentration Profile, Time : ' + timestep_list[t-1]+'s')
        #graphlines[1].set_title('Concentration profile at t='+str(t))
        return graphlines

    ani = FuncAnimation(fig, update, interval=intervaltime, frames=time_step_axis,blit=True)#plt.xlim([990,1000])
    	
    ani.save(filename = 'concentration_animation.gif', writer = 'pillow', fps = 30) 





#DEFINE VOLTAGE FUNCTIONS:
def gen_half_cell_voltage(edge_conc_vals,i_app_data,electrode,tsteps,pos_params=None,neg_params=None):
    '''!@brief Generate the voltage against time profile for a half cell and return voltage.
    @details. Return the voltage for a given applied current and edge concentration values for a given electrode.
    If the concentration at the edge of the sphere at anytime goes below 0, computing the voltage
    will not work. This is because negative concentration is unphysical. In this case, a ValueError will be returned.


    @param[in] edge_conc_vals   Lithium concentration at the sphere edge  for all time steps
    @param[in] i_app_data       Vector of applied current density at each timestep
    @param[in] electrode        Electrode type of the half cell
    @param[in] tsteps           The number of timesteps in the simulation
    @param[in] pos_params       [K, a, cmax, L] for the positive electrode, None if the positive is not needed
    @param[in] neg_params       [K, a, cmax, L] for the negative electrode, None if the negative is not needed
    '''
    #pos and neg params have form [K,a,cmax,L]
    

    ##############SET PARAMETERS TO THE ONES WE NEED FOR THE ELECTRODE##############
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
    
    #### Constants ####
    F = 96485   # Faraday constant (C/mol)
    R_g =  8.314   # Ideal gas constant (JK^-1mol^-1)
    T = 298.15   # Standard conditions for temperature (T)


    def j_function(c_R,c_max,K):
        '''!@brief Calculates a quantity that feeds into the voltage calculation for each temporal point.
        @details Requires numpy library for sqrt
        @param[in] c_R   Concentration at the edge of the sphere
        @param[in] c_max Maximum concentration in the simulation
        @param[in] K     User provided K value (Reaction Constant)
        @result    j,    Used for computing voltage
        '''
        if c_R<0.0:
            print('Error, concentration should never be less than 0')
            raise ValueError
        ratio = (c_R / c_max)
        return F * K * np.sqrt(np.abs(ratio * (1 - ratio)))



    def U_function_pos(c_R):
        '''!@brief OCV curve for positive electrode.
        @param[in] c_R   Concentration at the edge of the sphere
        @result u  Used for computing voltage
        '''
        x = c_R/cmax_pos_sim
        u = (-0.8090*x) + 4.4875 - (0.0428*np.tanh(18.5138*(x - 0.5542))) - (17.7326*np.tanh(15.7890*(x-0.3117))) + (17.5842*np.tanh(15.9308*(x - 0.3120)))  #positive electrode U
        return u



    def U_function_neg(c_R):
        '''!@brief OCV curve for negative electrode
        @param[in] c_R   Concentration at the edge of the sphere
        @result u     Used for computing voltage
        '''
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
        '''!@brief Calculates voltage for given time steps.
        @details Calculates the voltage of the system at specific time points.
        @param[in] U    U value, taken from OCV function above
        @param[in] i_app  Applied current value
        @param[in] jay    j value for electrode
        @param[in] L     Electrode width
        @param[in] a    Particle surface area per unit volume
        @return v Voltage value
        '''
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
    
    # voltages calculated for sequential timesteps
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

'''
@var float K_pos: Reaction rate at positive electrodes (Am^-2(m^3mol^-1)^1.5)
@var float K_neg: Reaction rate at negative electrodes Am^-2(m^3mol^-1)^1.5)
@var float cmax_pos_sim: Positive electrode maximum concentration (molm^-3)
@var float cmax_neg_sim: Negative electrode maximum concentration (molm^-3)
@var float L_pos: Positive electrode thickness (m).
@var float L_neg: Negative electrode thickness (m).
@var float a_pos: Positive particle surface area per unit volume (m^-1).
@var float a_neg: Negative particle surface area per unit volume (m^-1).
'''


def voltage_current_plot(electrode,cstore,time_axis,i_app_data,tsteps,pos_params=None,neg_params=None):
    '''!@brief Calculates voltages and plots as a function of time
    @details Calculates the voltage at all time points and saves plots of both Li concentration at the
    sphere edge and voltage as a function of time. 
    This function has different settings determined by the value of electrode.
    @param[in] electrode    Charge on electrode
    @param[in] cstore       The full matrix of concentration values (all nodes at all timesteps)
    @param[in] time_axis    Axis containing all timestep values
    @param[in] i_app_data   Full set of applied current density data
    @param[in] tsteps       The number of timesteps
    @param[in] pos_params   [K, a, cmax, L] for the positive electrode, None if the positive is not needed
    @param[in] neg_params   [K, a, cmax, L] for the negative electrode, None if the negative is not needed
'''
    #structure of cstore: each row represents a single timestep, each column a single node
    edge_conc_vals = cstore[:,-1]    #want the values of the edge node for all timesteps
    
    volt_store = gen_half_cell_voltage(edge_conc_vals,i_app_data,electrode,tsteps,pos_params=pos_params,neg_params=neg_params)
    
    fig, axs = plt.subplots(2,1,sharex=True)
    axs[0].plot(time_axis,volt_store, color = 'b',label='Voltage')
    axs[0].set_ylabel('Voltage (V)', color ='b')
    axs[0].set_title('Voltage and Applied Current Over Time')

    axs[1].set_ylabel(r'Applied Current Density (A/m$^2$)', color = 'r')
    axs[1].plot(time_axis,i_app_data,'r--',label='current')
    axs[1].set_xlabel('Time (s)')
    plt.savefig('Voltage Current Plot')
        

       
def plot_halfcell_GITT_result(filename,start_times,electrode,pos_params=None,neg_params=None,Animation=False,SaveFinalState=False,SparsifyAnimation=True,animation_interval_time=10):
    '''!@brief Generates the voltage current plot and optionally an animation for a half-cell GITT test.
    @details Concatentates the output files generated by a GITT test and then produces a voltage-time and 
    current-time plot and an animation seperately. 
    @param[in] filename     The input filename for the simulation data, excluding any file extension. String.
    @param[in] start_times  The list of start times for each current step in a GITT test. Array of floats
    @param[in] electrode    The electrode type, either positive or negative, string.
    @param[in] pos_params   [K, a, cmax, L] for the positive electrode, None if the positive is not needed
    @param[in] neg_params   [K, a, cmax, L] for the negative electrode, None if the negative is not needed
    @param[in] Animation    True to generate an animation of concentration in the sphere, False otherwise
    @param[in] SaveFinalState True to save the final state of the animation as a seperate PNG
    @param[in] SparsifyAnimation True to construct animation using 1 of every 10 timesteps
    @param[in] interval_time    time between frames in animation(ms)
    '''

    ###########BUILD FULL DATASET FROM DISPARATE GITT TEST FILES###########
    running_tot_start_time = 0.0
    total_tsteps = 0
    #build the full dataset from the deconstructed files
    for i,start_time in enumerate(start_times):
        #read each output file
        cstore,tsteps,nodenum,R,time_axis,dr,electrode = read_output_file(filename,step_num=i)
        #read each input file
        i_app_data = read_input_current(filename,step_num=i)
        #shift time axis by each start time
        time_axis = time_axis + start_time

        #build full cstore, time axis and iapp data from seperate files.
        if i == 0:
            full_cstore = cstore
            full_time_axis = time_axis
            full_iapp_vals = i_app_data
        if i != 0:
            full_cstore = np.concatenate((full_cstore,cstore),axis=0)
            full_time_axis = np.concatenate((full_time_axis,time_axis))
            full_iapp_vals = np.concatenate((full_iapp_vals,i_app_data))
            
        total_tsteps += tsteps
        
    #call the plotter to get voltage time and current time plots
    voltage_current_plot(electrode,full_cstore,full_time_axis,full_iapp_vals,total_tsteps,pos_params=pos_params,neg_params=neg_params)

    if Animation:
        #call the animator to generate animation
        animated_conc_plot(animation_interval_time,dr,total_tsteps,nodenum,full_cstore,full_time_axis,SaveFinalState=SaveFinalState,SparsifyAnimation=SparsifyAnimation)

'''!Function gen_plots: Causes plots corresponding to voltage_current_plot and animated_conc_plot to be generated.'''
def gen_plots(filename,pos_params=None,neg_params=None,animation_interval_time=10,SaveFinalState=True,SparsifyAnimation=False):
    '''!@brief A function which can be called to generate both the voltage-time, current-time plots and the animation.
    @details Calls the relevant functions to generate an animation plot and a voltage-time, current-time set of plots.
    @param[in] filename     The input filename for the simulation data, excluding any file extension. String.
    @param[in] pos_params   [K, a, cmax, L] for the positive electrode, None if the positive is not needed
    @param[in] neg_params   [K, a, cmax, L] for the negative electrode, None if the negative is not needed
    @param[in] animation_interval_time    time between frames in animation(ms)
    @param[in] SaveFinalState True to save the final state of the animation as a seperate PNG
    @param[in] SparsifyAnimation True to construct animation using 1 of every 10 timesteps
    '''
    #generate all the plots that would previously have been generated from calling the plotting script
    cstore,tsteps,nodenum,R,time_axis,dr,electrode = read_output_file(filename)
    i_app_data = read_input_current(filename)
    voltage_current_plot(electrode,cstore,time_axis,i_app_data,tsteps,pos_params=pos_params,neg_params=neg_params)
    animated_conc_plot(animation_interval_time,dr,tsteps,nodenum,cstore,time_axis,SaveFinalState=SaveFinalState,SparsifyAnimation=SparsifyAnimation)


def full_battery_GITT_plots(filename_positive,filename_negative,start_times,pos_params=None,neg_params=None,SparsifyAnimation=True,animation_interval_time=10):
    '''@brief Build an animation of the concentrations, applied current with time and applied voltage with time
    in a full battery GITT test.
    @details Builds the full dataset from the different GITT test files. Generates the voltage for each timestep,
    plots the full voltage-time, current-time and concentration of positive and negative electrode animations.
    Also prints lithium mass loss in first step, to give an indication on how good the model is. Acceptable values for this
    are =<10^-6 kg.
    @param[in] filename_positive     The input positive filename for the simulation data, excluding any file extension. String.
    @param[in] filename_negative     The input negative filename for the simulation data, excluding any file extension. String.
    @param[in] start_times  The list of start times for each current step in a GITT test. Array of floats
    @param[in] pos_params   [K, a, cmax, L] for the positive electrode
    @param[in] neg_params   [K, a, cmax, L] for the negative electrode
    @param[in] SparsifyAnimation True to construct animation using 1 of every 10 timesteps
    @param[in] animation_interval_time    time between frames in animation(ms)
    '''
    
    #build the full dataset from the deconstructed files
    total_tsteps=0
    for i,start_time in enumerate(start_times):
        cstore_pos,tsteps,nodenum,R_pos,time_axis,dr_pos,electrode_pos = read_output_file(filename_positive,step_num=i)
        cstore_neg,tsteps,nodenum,R_neg,time_axis,dr_neg,electrode_neg = read_output_file(filename_negative,step_num=i)
        i_app_data_pos = read_input_current(filename_positive,step_num=i)
        i_app_data_neg = read_input_current(filename_negative,step_num=i) #the current we apply to the cell is seen from the anode perspective
        time_axis = time_axis + start_time
        if i == 0:
            full_cstore_pos = cstore_pos
            full_cstore_neg = cstore_neg
            full_time_axis = time_axis
            full_iapp_vals_pos = i_app_data_pos
            full_iapp_vals_neg = i_app_data_neg
        if i != 0:
            full_cstore_pos = np.concatenate((full_cstore_pos,cstore_pos),axis=0)
            full_cstore_neg = np.concatenate((full_cstore_neg,cstore_neg),axis=0)
            full_time_axis = np.concatenate((full_time_axis,time_axis))
            full_iapp_vals_pos = np.concatenate((full_iapp_vals_pos,i_app_data_pos))
            full_iapp_vals_neg = np.concatenate((full_iapp_vals_neg,i_app_data_neg))
            
        total_tsteps += tsteps
    


    #structure of cstore: each row represents a single timestep, each column a single node
    edge_conc_vals_pos = full_cstore_pos[:,-1]    #want the values of the edge node for all timesteps
    edge_conc_vals_neg = full_cstore_neg[:,-1]

    #generate the positive and negative half cell voltages
    pos_voltage = gen_half_cell_voltage(edge_conc_vals_pos,full_iapp_vals_pos,electrode_pos,total_tsteps,pos_params=pos_params,neg_params=neg_params)
    neg_voltage = gen_half_cell_voltage(edge_conc_vals_neg,full_iapp_vals_neg,electrode_neg,total_tsteps,pos_params=pos_params,neg_params=neg_params)

    #subtract to get full voltage and current
    full_voltage = pos_voltage-neg_voltage

    #note that cell current is the same as the current seen from the perspective of the negative electrode
    full_current = full_iapp_vals_neg

    ###################Print lithium mass loss per step######################

    [K_pos,a_pos,cmax_pos_sim,L_pos] = pos_params
    [K_neg,a_neg,cmax_neg_sim,L_neg] = neg_params
    e_act_pos = (a_pos*R_pos)/3
    e_act_neg = (a_neg*R_neg)/3
    li_frac_list = []
    r_pos_axis = np.array([i*dr_pos for i in range(nodenum)])
    r_neg_axis = np.array([i*dr_neg for i in range(nodenum)])
    
    #get denominator of expression from initial concentrations
    li_avg_conc_pos = get_avg_li_conc(full_cstore_pos[0,:],r_pos_axis)
    li_avg_conc_neg = get_avg_li_conc(full_cstore_neg[0,:],r_neg_axis)
    c0_eact_L = li_avg_conc_pos*e_act_pos*L_pos + li_avg_conc_neg*e_act_neg*L_neg

    #find fractional lithium mass loss per step and print
    li_avg_conc_pos = get_avg_li_conc(full_cstore_pos[tsteps-1,:],r_pos_axis)
    li_avg_conc_neg = get_avg_li_conc(full_cstore_neg[tsteps-1,:],r_neg_axis)
    li_frac = (li_avg_conc_pos*e_act_pos*L_pos + li_avg_conc_neg*e_act_neg*L_neg)/c0_eact_L

    print('Fraction of lithium mass lost per current block applied:',1-li_frac)
    #################generate animation###################
    tsteps = total_tsteps
    #build time step axis
    if SparsifyAnimation:
        time_step_axis = [i*50 for i in range(1,int(tsteps/50))]
    else:
        time_step_axis = [i for i in range(1,tsteps)]
    vals_pos = np.zeros([nodenum,tsteps+1])
    vals_neg = np.zeros([nodenum,tsteps+1])

    #place the c storage values in to the values matrix.
    for i in range(nodenum):
        vals_pos[i,1:] = full_cstore_pos[:,i]
        vals_pos[i,0] = i*dr_pos
        vals_neg[i,1:] = full_cstore_neg[:,i]
        vals_neg[i,0] = i*dr_neg
        #structure of cstore: each row represents a single timestep, each column a single node
        #structure of vals: each column is a single timestep, each row is a single node

        
    #generate figure with animation
    fig, axs = plt.subplots(2,2,figsize=(10, 8))
    font = 12
    plt.rcParams.update({'font.size': font})#
    plt.subplots_adjust(bottom=0.3)
    #plot first frame of animation
    axs[1,0].plot(full_time_axis,full_voltage, color = 'b',label='Voltage')
    axs[1,0].set_ylabel('Full Cell Voltage (V)', color ='b',fontsize=font)
    axs[1,0].set_title('Voltage(V)',fontsize=font)
    axs[1,0].set_xlabel('Time (s)',fontsize=font)
    pointV, = axs[1,0].plot(full_time_axis[0],full_voltage[0],'ro')
    axs[1,1].set_ylabel(r'Applied Current Density (A/m$^2$)', color = 'r',fontsize=font)
    axs[1,1].plot(full_time_axis,full_current,'r--',label='current')
    axs[1,1].set_title(r'Applied Current Density (A/m$^2$)',fontsize=font)
    axs[1,1].set_xlabel('Time (s)',fontsize=font)
    pointI, = axs[1,1].plot(full_time_axis[0],full_current[0],'ro')

    #Plot initial graphs for positive electrode
    axs[0,0].set_xlabel('Distance from Sphere Centre (m)',fontsize=font)
    axs[0,0].set_ylabel('Concentration of Lithium (molm$^-3$)',fontsize=font)
    axs[0,0].set_ylim([np.min(vals_pos[:,1:]),np.max(vals_pos[:,1:])+1])
    graphPos, = axs[0,0].plot(vals_pos[:,0],vals_pos[:,1])

    #Plot initial graphs for 
    axs[0,1].set_xlabel('Distance from Sphere Centre (m)',fontsize=font)
    axs[0,1].set_ylabel('Concentration of Lithium (molm$^-3$)',fontsize=font)
    axs[0,1].set_ylim([np.min(vals_neg[:,1:]),np.max(vals_neg[:,1:])+1])
    graphNeg, = axs[0,1].plot(vals_neg[:,0],vals_neg[:,1])

    #Create a list which holds both the things we wish to animate
    graphlines = [pointV,pointI,graphPos,graphNeg]

    r_time_axis = [round(i,2) for i in full_time_axis]
    timestep_list = [str(i) for i in r_time_axis]   #Create list of strings of timestep times.

    axs[0,0].set_title('Cathode Profile',fontsize=font)
    axs[0,1].set_title('Anode Profile',fontsize=font)

    fig.suptitle('Full cell charging GITT test, Time : ' + timestep_list[0]+'s',fontsize=font)

    plt.tight_layout()
    def update(t):
        #generate the animation for a given timestep
        #Set the data of each element of graphlines to the corresponding value of t passed to the function
        #Updates the points on the voltage time and current time graphs
        #and the full data in the concentration profiles.
        graphlines[0].set_data(t,full_voltage[t])
        graphlines[1].set_data(t,full_current[t])
        graphlines[2].set_data(vals_pos[:,0],vals_pos[:,t])
        graphlines[3].set_data(vals_neg[:,0],vals_neg[:,t])
        fig.suptitle('Full cell charging GITT test, Time : ' + timestep_list[t-1]+'s',fontsize=font)
        #graphlines[1].set_title('Concentration profile at t='+str(t))
        return graphlines



    #generate and save animation
    ani = FuncAnimation(fig, update, interval=animation_interval_time, frames=time_step_axis,blit=True)#plt.xlim([990,1000])
    	
    ani.save(filename = 'concentration_animation.gif', writer = 'pillow', fps = 30) 

def get_avg_li_conc(c_vals,r_vals):
    '''!@brief Gets the average concentration of lithium at a given timestep using trapezium rule inside the sphere.
    !@details Uses the fact that the average concentration of lithium in a sphere at any timestep can be found from
    $\frac{3}{R^{3}}\int_{0}^{R} c(r)r^{2}dr. Note, that this seems weird. It is weird. The reason this is necessary is because the concentration
    values in the sphere are defined per active electrode volume, i.e e_act*L*A, not per sphere volume. Therefore, to calculate the lithium mass loss
    in the whole system it is necessary to first find the concentration as if each sphere was uniformly filled (that is what this function does), and
    then after that multiply this average concentration by the active electrode volume.
    Take it up with whoever came up with the single particle model, not me.
    !@param[in] c_vals: Array of concentration values covering the whole sphere radius, floats.
    !@param[in] r_vals: Array of radius values that correspond the concentration values given in c_vals, floats.
    '''

    integrand = c_vals*(r_vals**2)
    avg_conc = (3/(r_vals[-1]**3))*np.trapz(y=integrand,x=r_vals)
    return avg_conc

