import netCDF4 as NC
import numpy as np
from matplotlib.animation import FuncAnimation

f=NC.Dataset('cstorage.nc','r',format='NETCDF4')
tsteps=f.variables['tsteps'][0]
nodenum=f.variables['node_num'][0]
R=f.variables['R'][0]
print(R)
print(tsteps)
print(nodenum)