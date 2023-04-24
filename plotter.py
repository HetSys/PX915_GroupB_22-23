import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation

#open output file
f2 = open('output.txt','r')

#time between frames in animation
intervaltime  = 10

#radius of sphere
R  = 5.22e-6

#number of nodes (hardcoded in out of laziness)
nodenum = 2500

#node step
dr = R/nodenum

#time step number (hardcoded in out of laziness)
tsteps = 101

#build time axis
time_axis = [i for i in range(1,tsteps)]
vals = np.zeros([nodenum,tsteps])

#extract data from file
for i,rowf in enumerate(f2):
    row = np.array(rowf.split())
    vals[i,:] = row
vals[:,0] = vals[:,0]*dr


#make plot of final state and save figure for reference
plt.plot(vals[:,0],vals[:,-1])
plt.xlabel('r')
plt.ylabel('c')
plt.savefig(f'{nodenum}.png')


#Generate the subplot axis, label this ax1
fig, ax1 = plt.subplots(1)
xdata, ydata = [], []
#Plot initial graphs for animation
ax1.set_xlabel('r')
ax1.set_ylabel('c')
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

    return graphlines

ani = FuncAnimation(fig, update, interval=intervaltime, frames=time_axis,blit=True)#plt.xlim([990,1000])

plt.show()