import unit_test
import os
import time
import numpy as np
import matplotlib.pyplot as plt

num_threads = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20]
num_nodes = [100,500,1000,2000]
times = np.zeros([19,4])
for i,n in enumerate(num_nodes):
    for j,thread_num in enumerate(num_threads):
        os.environ['MKL_NUM_THREADS']=str(thread_num)
        os.system('echo $MKL_NUM_THREADS')
        print('thread num = ', thread_num)
        start = time.time()
        unit_test.run_unit_test(nodenum=n,dt=5.0)
        end = time.time()
        print(end-start)
        times[j,i] = end-start

for i in range(4):
    plt.plot(num_threads,times[:,i],label=str(num_nodes[i]))
plt.legend()
plt.xlabel('Number of threads')
plt.ylabel('runtime(s)')
plt.title('Unit test runtime with different numbers of threads and nodes')
plt.savefig('RuntimeVSNumthreads.png')