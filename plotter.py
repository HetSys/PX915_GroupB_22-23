import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation
import netCDF4 as NC
#Written so user can specify if is positive or negative electrode


def read_output_file(filename,step_num=None):
    #Read in Concs from netcdf
    if step_num is not None:
        filename = filename+str(step_num)
    filename = filename + '_output.nc'
    dat=NC.Dataset(filename, "r", format ="NETCDF")
    cstore = dat.variables['cstorage'][:]

    #Read in Constants
    tsteps=dat.variables['tsteps'][0] #number of timesteps
    nodenum=dat.variables['node_num'][0] #number of nodes 
    R=dat.variables['R'][0] #radius of sphere
    time_axis = dat.variables['time_axis'][:]
    #compute interval between nodes
    dr = R/(nodenum-1)
    #parse electrode charge
    electrode_read = dat.variables['electrode_charge'][0] #electrode charge
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
            if i >=10:
                i_app_data.append(current.strip())

    i_app_data = np.array(i_app_data, dtype = float)
    return i_app_data


def animated_conc_plot(intervaltime,dr,tsteps,nodenum,cstore,SaveFinalState=False):

    #time between frames in animation
    intervaltime  = 10

    #build time step axis
    time_step_axis = [i for i in range(1,tsteps)]
    vals = np.zeros([nodenum,tsteps+1])


    #place the c storage values in to the values matrix.
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
        plt.ylabel('Concentration (molm^-3)')
        plt.title('Concentration Profile at End of Simulation')
        plt.savefig('final_state.png')


    #Generate the subplot axis, label this ax1
    fig, ax1 = plt.subplots(1)
    xdata, ydata = [], []
    #Plot initial graphs for animation
    ax1.set_xlabel('Distance from Sphere Centre (m)')
    ax1.set_ylabel('Concentration Profile in sphere')
    ax1.set_ylim(np.min(vals),-np.min(vals))
    graph, = ax1.plot(vals[:,0],vals[:,1])

    #Create a list which holds both the things we wish to animate
    graphlines = [graph]

    def update(t):
        #Define an update function for the animation. This is what is called each frame
        #to update the graph. As blitting is set to on (it has to be to use reasonable computing power)
        #This function needs to return a single array containing the objects to animate with new data set.

        #Function takes as an input argument the timestep it is being called at.
        #Set the data of each element of graphlines to the corresponding value of t passed to the function
        graphlines[0].set_data(vals[:,0],vals[:,t])
        #graphlines[1].set_title('Concentration profile at t='+str(t))
        return graphlines

    ani = FuncAnimation(fig, update, interval=intervaltime, frames=time_step_axis,blit=True)#plt.xlim([990,1000])
    	
    ani.save(filename = 'concentration_animation.gif', writer = 'pillow', fps = 30) 



def voltage_current_plot(electrode,cstore,time_axis,i_app_data,no_timesteps):
################  Voltage and Concentration Plot ##################

    #### Constants ####
    F = 96485 #C/mol
    R_g =  8.314 # J K-1 mol-1
    T = 298.15 # K
    a = 5.28E-6 #cm^-2 NEEDS TO BE SPECIFIED/READ IN

    # Set Input Parameterts for j
    K_pos = 3.42E-6 #Am^-2(m^3mol^-1)^1.5
    K_neg = 6.48E-7 #Am^-2(m^3mol^-1)^1.5

    cmax_neg_sim = 33133 #moldm^-3 # m #Surely this should be molm^-3 to be consistent with the concentration units up to this point?
    cmax_pos_sim = 63104 #moldm^-3 # m

    L_pos = 75.6E-6 #m
    L_neg = 85.2E-6 #m

    if electrode.lower() == 'positive' or electrode == 'cathode':   #set constants based on pos or neg electrode setup
        c_max = cmax_pos_sim
        K = K_pos
        L = L_pos
    else:
        c_max = cmax_neg_sim
        K = K_neg
        L = L_neg


    #This is the j(c) function from our model, used to calculate the voltage
    #Inputs
    #Concentration at edge of sphere - c_R
    #c_max constant - c_max
    #Constants - F, K
    #Outputs
    #j(c) for voltage calculation
    #Notes
    #Requires numpy library for sqrt
    def j_function(c_R):
        #print(c_R)
        if c_R<0.0:
            print('Error, concentration should never be less than 0')
            raise ValueError
        ratio = (c_R / c_max)
        return F * K * np.sqrt((ratio * (1 - ratio)))

    #Inputs
    #Concentration at edge of sphere - c_R


    def U_function_pos(c_R):
        x = c_R/c_max
        u = (-0.8090*x) + 4.4875 - (0.0428*np.tanh(18.5138*(x - 0.5542))) - (17.7326*np.tanh(15.7890*(x-0.3117))) + (17.5842*np.tanh(15.9308*(x - 0.3120)))  #positive electrode U
        return u

    def U_function_neg(c_R):
        x = c_R/c_max
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
    def voltage_function(U,i_app,jay):
        v = U - ((2*R_g*T)/(F))*np.arcsinh((i_app)/(a*L*jay))
        return v

    
    #structure of cstore: each row represents a single timestep, each column a single node
    edge_conc_vals = cstore[:,-1]    #want the values of the edge node for all timesteps


    volt_store = []

    for i in range(no_timesteps):
        i_app_temp = i_app_data[i]
        c_temp = edge_conc_vals[i]        
        j_temp = j_function(c_temp)
        if electrode.lower() == 'positive' or electrode == 'cathode':
            u_temp = U_function_pos(c_temp)
        else:
            u_temp = U_function_neg(c_temp)
        if j_temp == 0:
            volt_store.append(0)            #use to prevent division by zero
        else:
            volt_store.append(voltage_function(u_temp,i_app_temp,j_temp))
    volt_store[0] = volt_store[1] - (volt_store[2]-volt_store[1])       ######################CHECK: Assume linear trend around zero!!! Avoids singularity/math error where j_temp == 0.
    fig, axs = plt.subplots(2,1,sharex=True)
    axs[0].plot(time_axis,volt_store, color = 'b',label='Voltage')
    axs[0].set_ylabel('Voltage (V)', color ='b')
    axs[0].set_title('Voltage and Applied Current Over Time')

    axs[1].set_ylabel('Applied Current Density (A/m^2)', color = 'r')
    axs[1].plot(time_axis,i_app_data,'r--',label='current')
    axs[1].set_xlabel('Time(s)')
    plt.savefig('Voltage Current Plot')
        

       
def plot_GITT_result(filename,start_times,electrode):


    running_tot_start_time = 0.0
    total_tsteps = 0
    #build the full dataset from the deconstructed files
    for i,start_time in enumerate(start_times):
        cstore,tsteps,nodenum,R,time_axis,dr = read_output_file(filename,step_num=i)
        i_app_data = read_input_current(filename,step_num=i)
        time_axis = time_axis + start_time
        if i == 0:
            full_cstore = cstore
            full_time_axis = time_axis
            full_iapp_vals = i_app_data
        if i != 0:
            full_cstore = np.concatenate((full_cstore,cstore),axis=0)
            full_time_axis = np.concatenate((full_time_axis,time_axis))
            full_iapp_vals = np.concatenate((full_iapp_vals,i_app_data))
            
        total_tsteps += tsteps

    #call the plotter
    voltage_current_plot(electrode,full_cstore,full_time_axis,full_iapp_vals,total_tsteps)

def gen_plots(filename,animation_interval_time=10):
    #generate all the plots that would previously have been generated from calling the plotting script
    cstore,tsteps,nodenum,R,time_axis,dr,electrode = read_output_file(filename)
    i_app_data = read_input_current(filename)
    voltage_current_plot(electrode,cstore,time_axis,i_app_data,tsteps)
    animated_conc_plot(animation_interval_time,dr,tsteps,nodenum,cstore,SaveFinalState=True)



