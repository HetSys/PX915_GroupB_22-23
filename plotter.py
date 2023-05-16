import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation
import netCDF4 as NC
#Written so user can specify if is positive or negative electrode

#Read in Concs from netcdf
dat=NC.Dataset("cstorage.nc", "r", format ="NETCDF")
cstore = dat.variables['cstorage'][:]


#Read in Constants
tsteps=dat.variables['tsteps'][0] #number of timesteps
nodenum=dat.variables['node_num'][0] #number of nodes 
R=dat.variables['R'][0] #radius of sphere


electrode = 'positive'
timestep = 1 #s

with open('user_input.txt', 'r') as iapp_vals:
    lines = iapp_vals.readlines()
    i_app_data = []
    for i, current in enumerate(lines):
        if i >=9:
            i_app_data.append(current.strip())
i_app_data = np.array(i_app_data, dtype = float)

no_timesteps = len(i_app_data)
time_store = []
for i in range(no_timesteps):
    time_store.append(i*timestep)
time_store = np.array(time_store)


#open output file
#f2 = open('output.txt','r')

#time between frames in animation
intervaltime  = 10


#node step
dr = R/(nodenum-1)

#time step number

tsteps +=1


#build time axis
time_axis = [i for i in range(1,tsteps)]
vals = np.zeros([nodenum,tsteps])

##extract data from file
#for i,rowf in enumerate(f2):
#    row = np.array(rowf.split())
#    vals[i,:] = row
#vals[:,0] = vals[:,0]*dr

dat=NC.Dataset("cstorage.nc", "r", format ="NETCDF")    #Read in Concentration data
cstore = np.array(dat.variables['cstorage'][:])
nodenum = 2500

vals = np.zeros([nodenum,tsteps])
for i in range(nodenum):
    vals[i,1:] = cstore[:,i]
    vals[i,0] = i*dr

#make plot of final state and save figure for reference
plt.plot(vals[:,0],vals[:,-1])
plt.xlabel('Distance from Sphere Centre (m)')
plt.ylabel('Concentration at Sphere Boundary (moldm^-3)')
plt.title('Concentration (c|r=R) Profile at End of Simulation')
plt.savefig(f'{nodenum}.png')


#Generate the subplot axis, label this ax1
fig, ax1 = plt.subplots(1)
xdata, ydata = [], []
#Plot initial graphs for animation
ax1.set_title('Concentration Across Time and Space')
ax1.set_xlabel('Distance from Sphere Centre (m)')
ax1.set_ylabel('Concentration (c|r=R) Profile at End of Simulation')
ax1.set_ylim(np.min(vals),-np.min(vals))
graph, = ax1.plot(vals[:,0],vals[:,1])
print(graph)

#Create a list which holds both the things we wish to animate
graphlines = [graph]

def update(t):
    #Define an update function for the animation. This is what is called each frame
    #to update the graph. As blitting is set to on (it has to be to use reasonable computing power)
    #This function needs to return a single array containing the objects to animate with new data set.

    #Function takes as an input argument the timestep it is being called at.
    #Set the data of each element of graphlines to the corresponding value of t passed to the function
    graphlines[0].set_data(vals[:,0],vals[:,t])

    ####NEW#####################
    #text.set_text(f't = {time_store}')
    ###########################

    return graphlines  ###########NEW##########, text

ani = FuncAnimation(fig, update, interval=intervaltime, frames=time_axis,blit=True)#plt.xlim([990,1000])

plt.show()


################  Voltage and Concentration Plot ##################

#### Constants ####
F = 96485 #C/mol
R_g =  8.314 # J K-1 mol-1
T = 298.15 # K
a = 5.28E-6 #cm^-2 NEEDS TO BE SPECIFIED/READ IN

# Set Input Parameterts for j
K_pos = 3.42E-6 #Am^-2(m^3mol^-1)^1.5
K_neg = 6.48E-7 #Am^-2(m^3mol^-1)^1.5

cmax_neg_sim = 33133 #moldm^-3 # m
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
    ratio = (c_R / c_max)
    return F * K * np.sqrt(np.abs((ratio * (1 - ratio))))  ##################CHECK: ABS used to prevent complex numbers################

#Inputs
#Concentration at edge of sphere - c_R

edge_conc_vals = vals[-1,1:]    #values of edge conc



############################ i_app section ###########
with open('user_input.txt', 'r') as iapp_vals:
    lines = iapp_vals.readlines()
    i_app_data = []
    for i, current in enumerate(lines):
        if i >=9:
            i_app_data.append(current.strip())
i_app_data = np.array(i_app_data, dtype = float)


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


no_timesteps = len(i_app_data)
time_store = []
for i in range(no_timesteps):
    time_store.append(i*timestep)
time_store = np.array(time_store)

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
fig, ax = plt.subplots()
ax.plot(time_store,volt_store, color = 'b')
ax.set_ylabel('Voltage', color ='b')
ax.set_title('Voltage and Applied Current Over Time (Volts)')
ax2 = ax.twinx()
ax2.set_ylabel('Applied Current (Amps)', color = 'r')
ax2.plot(time_store,i_app_data, color='r')
plt.show()
    
    



