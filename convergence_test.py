'''!@brief Test convergence of final voltage and lithium mass loss of unit test with dt and number of nodes'''
import numpy as np
import unit_test
import matplotlib.pyplot as plt

#run convergence testing on dt

def run_dt_convergence():
    dt_vals = [0.01,0.05,0.1,0.5,1.0,5.0,10.0]
    final_voltage = []
    final_mass_loss = []
    for dt in dt_vals:
        v, mass_loss = unit_test.run_unit_test(dt=dt,nodenum=200)
        final_voltage.append(v)
        final_mass_loss.append(mass_loss)


    final_voltage = np.array(final_voltage)
    final_mass_loss = np.array(final_mass_loss)

    plt.plot(np.log10(dt_vals),np.log10(final_mass_loss))
    plt.xlabel('log10(dt(s))')
    plt.ylabel('log10(Lithium Mass Loss (g))')
    plt.title('Convergence of lithium mass loss against timestep')
    plt.savefig('dt_vs_mass_loss.png')

    plt.figure()
    plt.plot(np.log10(dt_vals),np.log10(np.abs(final_voltage-(final_voltage[0]))))
    plt.xlabel('log10(dt(s))')
    plt.ylabel('log10(Error in final cell voltage (V))')
    plt.title('Convergence of error in final cell voltage against timestep size')
    plt.savefig('dt_vs_voltage.png')
    #measure the convergence of voltage and lithium mass loss with number of nodes


def run_node_convergence():
    node_vals = [100,500,1000,2500,3000]
    final_voltage = []
    final_mass_loss = []
    for n in node_vals:
        v, mass_loss = unit_test.run_unit_test(nodenum=n)
        final_voltage.append(v)
        final_mass_loss.append(mass_loss)


    final_voltage = np.array(final_voltage)
    final_mass_loss = np.array(final_mass_loss)

    plt.plot(np.log10(node_vals),np.log10(final_mass_loss))
    plt.xlabel('log10(node num)')
    plt.ylabel('log10(Lithium Mass Loss (g))')
    plt.title('Convergence of lithium mass loss against node number')
    plt.savefig('nodenum_vs_mass_loss.png')

    plt.figure()
    plt.plot(np.log10(node_vals),np.log10(np.abs(final_voltage-(final_voltage[-1]))))
    plt.xlabel('log10(node num)')
    plt.ylabel('log10(Error in final cell voltage (V))')
    plt.title('Convergence of error in final cell voltage against node number')
    plt.savefig('nodenum_vs_voltage.png')
    #measure the convergence of voltage and lithium mass loss with number of nodes
#run convergence 



run_node_convergence()