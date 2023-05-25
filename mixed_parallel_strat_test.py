import unit_test
import os
import time
import numpy as np
import matplotlib.pyplot as plt

num_cores = [1,2,4,8]
num_nodes = [2000]
times = np.zeros([len(num_cores),len(num_nodes)])
for i,n in enumerate(num_nodes):
    for j,core_num in enumerate(num_cores):
        start = time.time()
        unit_test.run_unit_test(nodenum=n,dt=5.0,num_cores=core_num)
        end = time.time()
        print(end-start)
        times[j,i] = end-start

for i in range(len(num_nodes)):
    plt.plot(num_cores,times[:,i],label=str(num_nodes[i]))
plt.legend()
plt.xlabel('Number of cores given')
plt.ylabel('runtime(s)')
plt.title('Unit test runtime with different numbers of cores given')
plt.savefig('RuntimeVSNumCores_mixed_strategy.png')