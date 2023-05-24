'''!@package user_input_mod
@brief Package containing functions to set up user input parameters and execute the solver.
@details Options are provided to automatically generate applied current density iapp as constant and step functions, and to read iapp from a csv file.
Options are provided to set default values of parameters from Chen et al. 2020, https://doi.org/10.1149/1945-7111/ab9050.
Additional functions validate the types and values of input parameters, write the user input txt file, and execute the solver.
'''

import subprocess
import shlex
import numpy as np
import os

### CURRENT DENSITY SET UP ###
def iapp_read_csv(filename):
    '''!@brief Reads applied current density iapp from a csv file provided by user, 'filename'.
    @param[in] filename: Name of csv file containing iapp as Time, Current density.
    @result iapp_arr: Array containing applied current density.
    @result iapp_label: String containing description of iapp.
    @result tsteps: Number of timesteps, found from length of iapp array.
    '''

    '''! 1. Read in csv file.'''
    with open(filename, 'r') as iapp_file:
        iapp_all = iapp_file.read()
    iapp_file.close()

    '''! 2. Parse file into array of lines.'''
    iapp_lines = iapp_all.split("\n")
    nlines = len(iapp_lines) -1

    '''! 3. Parse each line at comma.'''
    iapp_arr = np.zeros(nlines)
    for i in range(nlines):
        line = i+1
        iapp_arr[i] = iapp_lines[line].split(",")[1]
    
    '''! 4. Define tsteps and iapp_label.'''
    tsteps = nlines
    iapp_label = 'file = ' + filename

    return iapp_arr, iapp_label, tsteps
    
def iapp_constant_setup(tsteps, iapp):
    '''!@brief Set up applied current density iapp as constant valued array of length tsteps.
    @param[in] tsteps: Number of timesteps, length of array iapp.
    @param[in] iapp: Constant value of iapp.
    @result iapp_arr: Array containing applied current density.
    @result iapp_label: String containing description of iapp.
    '''

    '''! 1. Verify current density iapp is a float.'''
    if (type(iapp)!=float):
        print('Constant applied current density must be a float/real value.')
        exit()

    '''! 2. Set up array. '''
    iapp_label = 'Constant, ' + str(iapp)
    iapp_arr = np.ones(tsteps) *  iapp

    return iapp_arr, iapp_label

def iapp_step_setup(tsteps, iapp_steps):
    '''!@brief Set up applied current density iapp as stepped array of length tsteps.
    @param[in] tsteps: Number of timesteps, length of array iapp.
    @param[in] iapp_steps: 2D Array containing heights of steps and timesteps where step occurs, starting timestep 0. Timesteps must be integers.
    @result iapp_arr: Array containing applied current density.
    @result iapp_label: String containing description of iapp.
    '''

    '''! 1. Set up label and initialise array.'''
    iapp_label = 'Step function, ' + str(iapp_steps)

    iapp_arr = np.ones(tsteps)
    nsteps = len(iapp_steps)

    '''! 2. Verify timesteps are integers, and current density are floats.'''
    problem_timesteps = []
    timestep_err = False
    problem_vals = []
    val_err = False
    for i in range(nsteps):
        if (type(iapp_steps[i][1])!=int):
            problem_timesteps.append(i)
            timestep_err = True

        if (type(iapp_steps[i][0])!=float):
            problem_vals.append(i)
            val_err = True

    if (timestep_err):
        print('Timesteps for current density step function must be integers. Problems occur at steps: ', problem_timesteps)
    if (val_err):
        print('Applied current densities in step function must be float/real values. Problems occur at steps: ', problem_vals)
    if (val_err or timestep_err):
        exit()

    '''! 3. Verify first timestep is 0.'''
    if (iapp_steps[0][1]!=0):
        print('Initial applied current density must be at timestep 0.')
        exit()
        

    '''! 4. Set up array.'''
    for i in range(nsteps):
        start = iapp_steps[i][1]
        if (i == nsteps-1):
            end = tsteps
        else:
            end = iapp_steps[i+1][1]
        
        iapp_arr[start:end] = iapp_steps[i][0]

    return iapp_arr, iapp_label



### DEFAULT PARAMETERS ###
def set_defaults_pos():
    '''!@brief Returns default parameters for a positive electrode
    @details Parameter values are taken from Chen et al. 2020, https://doi.org/10.1149/1945-7111/ab9050. 
    Simulation is set up to run for 100 timesteps of size dt=0.1s, with n=1000 spatial nodes.
    Applied current density is set up as a constant current density of value 0.73mA m^2.
    '''
    # Label of positive electrode
    electrode_charge = "p"

    # Number of timesteps, integer, greater than 0
    tsteps = 100
    # Timestep size (s), real, greater than 0
    dt = 0.1

    # Number of spatial nodes, integer, 100=<n=<4000
    n=1000

    # Initial concentration (mol m**-3), real, positive
    c0 = 1000.0
    # Diffusion coefficient (m**2 s**-1), real
    D = 4.0e-15
    # Width of block (m), real, greater than 0
    R = 5.86e-6
    # Particle surface area per unit volume (m**-1), real, greater than 0
    e_act = 0.665
    a = 3*e_act/R
    # Electrode thickness (m), real, greater than 0
    L = 75.6e-6

    ### Constant applied current density (A m**2), real array of length tsteps
    iapp_const = 0.73*10**(-3)
    iapp, iapp_label = iapp_constant_setup(tsteps, iapp_const)

    return tsteps, dt, n, c0, D, R, a, L, iapp, iapp_label, electrode_charge

def set_defaults_neg():
    '''!@brief Returns default parameters for a negative electrode
    @details Parameter values are taken from Chen et al. 2020, https://doi.org/10.1149/1945-7111/ab9050. 
    Simulation is set up to run for 100 timesteps of size dt=0.1s, with n=1000 spatial nodes.
    Applied current density is set up as a constant current density of value 0.73mA m^2.
    '''
    # Label of negative electrode
    electrode_charge = "n"

    # Number of timesteps, integer, greater than 0
    tsteps = 100
    # Timestep size (s), real, greater than 0
    dt = 0.1

    # Number of spatial nodes, integer, 100=<n=<4000
    n=1000

    # Initial concentration (mol m**-3), real, positive
    c0 = 1000.0
    # Diffusion coefficient (m**2 s**-1), real
    D = 3.3e-14
    # Width of block (m), real, greater than 0
    R = 5.22e-6
    # Particle surface area per unit volume (m**-1), real, greater than 0
    e_act = 0.75
    a = 3*e_act/R
    # Electrode thickness (m), real, greater than 0
    L = 85.2e-6

    ### Applied current density (A m**2), real array of length tsteps
    ### Constant current density
    iapp_const = 0.73*10**(-3)
    iapp, iapp_label = iapp_constant_setup(tsteps, iapp_const)

    return tsteps, dt, n, c0, D, R, a, L, iapp, iapp_label, electrode_charge



### PARAMETER VERIFICATION ###
def verify_params(filename, tsteps, dt, n, c0, D, R, a, L, electrode_charge):
    '''!@brief Verifies types and values of input parameters.
    @details Verifies that parameters output filename, tsteps, dt, c0, D, R, a, and L have the correct type and valid values. 
    If any are found to be invalid, an error is printed and the execution stopped.
    @param[in] filename: Output filename, string, must have less than 50 chars. No file extension required.
    @param[in] tsteps: Number of timesteps, integer > 0.
    @param[in] dt: Timestep size, float > 0.
    @param[in] n: Number of spatial nodes, integer, 100 =< n =< 4000.
    @param[in] c0: Initial concentration, float >= 0.
    @param[in] D: Diffusion constant, float.
    @param[in] R: Width of block, float > 0.
    @param[in] a: Particle surface area per unit volume, float >= 0.
    @param[in] L: Electrode thickness, float >= 0.
    @param[in] electrode_charge: Label of charge of electrode, string, 'p' for positive or 'n' for negative.
    '''

    # Set var_error to True is any errors occur
    var_error = False

    # filename
    if (type(filename)!=str):
        print('Filename must be a string.')
        var_error = True
    if (len(filename)>50):
        print('Filename must have less than 50 characters.')
        var_error = True
    # check file is not a checkpoint file, and does not have a file extension
    file_extension = filename.split(".")
    if (len(file_extension)>1 and file_extension[-1]=="nc"):
        print("Checkpoint file entered as name for user generated parameter file. Please enter a name without the extension '.nc'.")
        var_error = True
    elif (len(file_extension)>1 and file_extension[-1]=="txt"):
        print("File extension not required for user generated parameter file. Please enter a name without the extension '.txt'.")
        var_error = True

    # tsteps
    if (type(tsteps)!=int):
        print('Number of timesteps, tsteps, must be an integer.')
        var_error = True
    if (tsteps<1):
        print('Number of timesteps, tsteps, must greater than zero.')
        var_error = True

    # dt
    if (type(dt)!=float):
        print('Size of timestep, dt, must be a float/real value.')
        var_error = True
    if(dt<=0):
        print('Size of timestep, dt, must have a positive, non zero value.')
        var_error = True
    
    # n
    if (type(n)!=int):
        print('Number of spatial nodes, n, must be an integer.')
        var_error = True
    if (n<100 or n>4000):
        print('Number of spatial nodes, n, must have a value between 100 and 4000.')
        var_error = True

    # c0
    if (type(c0)!=float):
        print('Initial concentration, c0, must be a float/real value.')
        var_error = True
    if(c0<0):
        print('Initial concentration, c0, must have a positive value.')
        var_error = True

    # D
    if (type(D)!=float):
        print('Diffusion coefficient, D, must be a float/real value.')
        var_error = True

    # R
    if (type(R)!=float):
        print('Width of block, R, must be a float/real value.')
        var_error = True
    if(R<=0):
        print('Width of block, R, must have a positive, non zero value.')
        var_error = True

    # a
    if (type(a)!=float):
        print('Particle surface area per unit volume, a, must be a float/real value.')
        var_error = True
    if(a<=0):
        print('Particle surface area per unit volume, a, must have a positive, non zero value.')
        var_error = True

    # L
    if (type(L)!=float):
        print('Electrode thickness, L, must be a float/real value.')
        var_error = True
    if(L<=0):
        print('Electrode thickness, L, must have a positive, non zero value.')
        var_error = True

    # electrode_charge
    if (not(electrode_charge=='p' or electrode_charge=='n')):
        print('Electrode charge must be "p" for positive or "n" for negative.')
        var_error = True


    # If any errors have occured, stop script.
    if (var_error==True):
        exit()

    return

def verify_iapp(iapp, iapp_label, tsteps):
    '''!@brief Verifies types and values of applied current density array and label.
    @details Verifies that parameters output iapp and iapp_label have the correct types, valid values, and correct lengths.
    If any are found to be invalid, an error is printed and the execution stopped.
    @param[in] iapp: Applied current density, 1D array of floats of length tsteps.
    @param[in] iapp_label: Label of applied current density, string.
    @param[in] tsteps: Number of timesteps, to check iapp has correct length.
    '''

    # set var_error to True is any errors occur
    var_error = False

    # iapp
    iapp_type = [(type(iapp[i].item())!=float) for i in range(len(iapp))]
    if (any(iapp_type)):
        print('Applied current density, iapp, must be an array of float/real values.')
        var_error = True
    if (len(iapp)!=tsteps):
        print('Applied current density, iapp, must be an array of length tsteps.')
        var_error = True

    # iapp_label
    if (type(iapp_label)!=str):
        print('Label for applied current density function, iapp_label, must be a string.')
        var_error = True

    # if any errors have occured, stop script
    if (var_error==True):
        exit()

    return



### WRITE TO FILE ###
def write_to_file(filename, tsteps, dt, n, c0, D, R, a, L, iapp, iapp_label, electrode_charge):
    '''!@brief Function writes user inputs to txt file.
    @details Writes user inputs to txt file named 'filename'. 
    Parameters are output in format 'parameter = value'.
    tsteps, dt, c0, D, R, a, L, electrode_charge are output to top of file, followed by an asterix line (***).
    iapp_label follows the asterix line, with iapp array following, written one element per line.
    '''

    # The solver and plotting equations are built to work for the anode (negative electrode), that is 
    # losing concentration is a NEGATIVE current (discharging)
    # gaining concentration is a POSIITIVE current (charging). 
    # Hence, if the electrode is positive, we need to flip the sign on the current that we save to the file,
    # as it sees the reverse of what would be applied to the negative electrode
    if electrode_charge == 'p':
        iapp = -iapp
    
    '''! 1. Set file name.'''
    filename = filename + '.txt'

    '''! 2. Make list of strings to write to file.'''
    parameters = []
    parameters.append('tsteps = ' + str(tsteps) + '\n')
    parameters.append('dt = ' + str(dt) + '\n')
    parameters.append('n = ' + str(n) + '\n')
    parameters.append('c0 = ' + str(c0) + '\n')
    parameters.append('D = ' + str(D) + '\n')
    parameters.append('R = ' + str(R) + '\n')
    parameters.append('a = ' + str(a) + '\n')
    parameters.append('L = ' + str(L) + '\n')
    parameters.append('electrode_charge = ' + electrode_charge + '\n')

    parameters.append('******************\n')

    parameters.append('iapp: ' + iapp_label)
    for i in range(len(iapp)):
        parameters.append('\n')
        parameters.append(str(iapp[i]))

    '''! 3. Write to file.'''
    with open(filename, 'w') as user_input:
        user_input.writelines(parameters)
    user_input.close()

    return



### CALL SOLVER ###
def call_solver(filename, checkpoint):
    '''!@brief Execution of SPM solver.
    @details SPM solver is called using the subprocess package.
    The filename of the user input file is passed to the solver as a command line argument.
    Errors from execution are read in and further execution prevented if necessary.
    @param[in] filename: Name of file containing desired input, either a checkpoint file or user input file with no file extension.
    @param[in] checkpoint: Boolean indicating if a checkpoint file is used.
    '''

    '''! 1. Validate checkpoint file or user input file name.'''
    # If using checkpoint, check input file is a netcdf file with extension '.nc'
    file_extension = filename.split(".")
    if (checkpoint):
        if (len(file_extension)<2):
            # Check filename has an extension.
            print("Invalid checkpoint file. Please enter a file with a '.nc' extension.")
            exit()
        elif (file_extension[-1]!='nc'):
            # Check extension of file name is 'nc'.
            print("Invalid checkpoint file. Please enter a file with a '.nc' extension.")
            exit()
        elif (not os.path.isfile(filename)):
            # Check .nc file exists
            print("Checkpoint file not found:", filename)
            exit()
        else:
            print("Checkpoint file passed as input file, calling solver...")

    else:
        filename = filename + '.txt'
        print("User input file generated, calling solver...")

    '''! 2. Set up solver call line.'''
    solver_call_line = './finite_diff_solver' + ' filename="' + filename + '"'


    '''! 3. Call solver.'''
    command_solver = shlex.split(solver_call_line)
    process_solver = subprocess.run(command_solver, stdout=subprocess.PIPE, universal_newlines=True)
    return_solver = process_solver.returncode
    # Print solver output to command line
    if (process_solver.stdout): 
        print(process_solver.stdout) 
    # Check for errors in execution
    if (return_solver==0):
        print('Solver executed successfully, plotting output...')
    else:
        print('Error executing solver, process terminated.')
        exit()

    return



def get_GITT_initial_concs(currents,run_times, c0, R, a, L, electrode_charge):
    '''@brief Calculation of the initial concentrations for each step in a multi-step parallelised GITT test.
    @details Function computes a list of concentrations which give the initial flat concentrations for the initial step of a 
    parallelised multi-step GITT test. The formula it uses to calculate these is:
    C(T=t) = C_0 + \frac{i_app * t}{F*e_act * L}, where C_0 is C(T=0). Note that due to the definition of current being
    positive for charging, it is important that the sign of the current vector be flipped when considering the positive electrode (cathode),
    which will discharge during electrode charging.
    @param[in] currents: a vector containing the values of current to apply at each current step, floats, must have the same length as
    start_times, run_times, wait_times.
    @param[in] run_times: a vector containing the run time of each current step, floats, must have the same length as 
    start_times, currents, wait_times.
    @param[in] c0: Initial concentration, float >= 0.
    @param[in] R: Width of block, float > 0.
    @param[in] a: Particle surface area per unit volume, float >= 0.
    @param[in] L: Electrode thickness, float >= 0.
    @param[in] electrode_charge: Electrode charge, single character 'p' or 'n'.
    '''
    F = 96485.3321 #faraday constant
    #volume fraction of active material
    e_act = (a*R)/3

    #array of initial concentrations, using the fact that
    # C(T=t) = C0 + ((i_app*t)/(F*e_act*L))

    # if the electrode charge is positive, then a negative current corresponds
    # to gaining concentration, and so concentration should rise. 
    # We correct for this by flipping the sign here.
    currents = np.array(currents)
    if electrode_charge == 'p':
        currents = -currents
    return [c0 + (currents[i]*np.sum(run_times[0:i]))/(F*e_act*L) for i in range(len(currents))]


### INITIALISE A FULL GITT TEST IN PARALLEL ####
def GITT_half_cell(filename,nprocs,currents,start_times,run_times,wait_times,n,params):
    '''!@brief Execution of SPM solver in parallel to run a GITT half cell test over nprocs cores.
    @details Execution of SPM solver in parallel to run a test experiment on a half cell
    in the style of a galvanostatic intermittent titration technique (GITT), as described in 
    W. Weppner and R. A. Huggins 1977 J. Electrochem. Soc. 124 1569. Due to the equilibration time between each current
    step applied in this technique, it is possible to pre-compute the initial constant concentration in each of the single
    particles in the model by considering the amount of lithium removed in each current step.
    @param[in] filename: Name of user input file, no file extension. Note that a version of this file is created for 
    each current step applied in the GITT test.
    @param[in] nprocs: Number of processors to parallelise the current steps over. Note that the most processors
    that can be parallelised over is = the number of current steps applied in the GITT test.
    @param[in] currents: a vector containing the values of current to apply at each current step, floats, must have the same length as
    start_times, run_times, wait_times.
    @param[in] start_times: a vector containing the start times of each current step, floats, must have the same length as
    currents, run_times, wait_times.
    @param[in] run_times: a vector containing the run time of each current step, floats, must have the same length as 
    start_times, currents, wait_times.
    @param[in] wait_times: a vector containing the run time of each current step, floats, must have the same length as 
    currents, start_times, run-times.
    @param[in], n: The number of nodes to use in the simulation. Integer > 100
    @param[in] params: A vector containing the parameters for the simulation: [dt, c0, D, R, a, L, electrode_charge]
        @param[in] dt: Timestep size, float > 0.
        @param[in] c0: Initial concentration, float >= 0.
        @param[in] D: Diffusion constant, float.
        @param[in] R: Width of block, float > 0.
        @param[in] a: Particle surface area per unit volume, float >= 0.
        @param[in] L: Electrode thickness, float >= 0.
        @param[in] electrode_charge: Electrode charge, single character 'p' or 'n'
    '''
    #first, generate the arrays for initial concentration
    #unpack params
    [dt, c0, D, R, a, L, electrode_charge] = params

    initial_concs = get_GITT_initial_concs(currents,run_times, c0, R, a, L, electrode_charge)
    
    #make list of current arrays
    #make an input file for the solver to read from for each batch of the solver
    #also build the commands to be run

    current_list = []
    cmnds = []
    for i in range(len(currents)):
        fname = f'{filename}{i}'
        tsteps = (int(run_times[i]/dt)) + (int(wait_times[i]/dt))
        current_list.append(np.concatenate(([currents[i] for j in range(int(run_times[i]/dt))],[0.0 for j in range(int(wait_times[i]/dt))])))
        iapp_label = str(i)
        write_to_file(fname,tsteps, dt,n, initial_concs[i], D, R, a, L, current_list[i], iapp_label,electrode_charge)
        #### Set file name
        running_name = fname + '.txt'
        solver_call_line = './finite_diff_solver' + ' filename=' + running_name
        cmnds.append(solver_call_line)

    #now, we just need to launch a seperate instance of the solver for each process with each initial concentration and runtime
    #code from: https://stackoverflow.com/questions/30686295/how-do-i-run-multiple-subprocesses-in-parallel-and-wait-for-them-to-finish-in-py
    for j in range(max(int(len(cmnds)/nprocs), 1)):
        #launch processes, distributing evenly between processors
        procs = [subprocess.Popen(i, shell=True) for i in cmnds[j*nprocs: min((j+1)*nprocs, len(cmnds))]]
        for p in procs:
            #wait to ensure all done
            p.wait()

def GITT_full_cell(filename_positive,filename_negative,nprocs,currents,start_times,run_times,wait_times,n,params_pos,params_neg):
    '''!@brief Execution of SPM solver in parallel to run a GITT full cell test over nprocs cores.
    @details Execution of SPM solver in parallel to run a test experiment on a full cell
    in the style of a galvanostatic intermittent titration technique (GITT), as described in 
    W. Weppner and R. A. Huggins 1977 J. Electrochem. Soc. 124 1569. Due to the equilibration time between each current
    step applied in this technique, it is possible to pre-compute the initial constant concentration in each of the single
    particles in the model by considering the amount of lithium removed or added in each current step.
    @param[in] filename_positive: Name of user input file for the cathode, no file extension. Note that a version of this file is created for 
    each current step applied in the GITT test.
    @param[in] filename_negative: Name of user input file for the anode, no file extension. Note that a version of this file is created for 
    each current step applied in the GITT test.
    @param[in] nprocs: Number of processors to parallelise the current steps over. Note that the most processors
    that can be parallelised over is = the number of current steps applied in the GITT test.
    @param[in] currents: a vector containing the values of current to apply at each current step, floats, must have the same length as
    start_times, run_times, wait_times.
    @param[in] start_times: a vector containing the start times of each current step, floats, must have the same length as
    currents, run_times, wait_times.
    @param[in] run_times: a vector containing the run time of each current step, floats, must have the same length as 
    start_times, currents, wait_times.
    @param[in] wait_times: a vector containing the run time of each current step, floats, must have the same length as 
    currents, start_times, run-times.
    @param[in], n: The number of nodes to use in the simulation. Integer > 100
    @param[in] params: A vector containing the parameters for the simulation: [dt, c0, D, R, a, L]
        @param[in] dt: Timestep size, float > 0.
        @param[in] c0: Initial concentration, float >= 0.
        @param[in] D: Diffusion constant, float.
        @param[in] R: Width of block, float > 0.
        @param[in] a: Particle surface area per unit volume, float >= 0.
        @param[in] L: Electrode thickness, float >= 0.
        @param[in] electrode_charge: Electrode charge, single character 'p' or 'n'
    '''
    #first, generate the arrays for initial concentration
    #unpack params
    [dt, c0_pos, D_pos, R_pos, a_pos, L_pos, electrode_charge_pos] = params_pos
    [dt, c0_neg, D_neg, R_neg, a_neg, L_neg, electrode_charge_neg] = params_neg

    initial_concs_pos = get_GITT_initial_concs(currents,run_times, c0_pos, R_pos, a_pos, L_pos, electrode_charge_pos)
    initial_concs_neg = get_GITT_initial_concs(currents,run_times, c0_neg, R_neg, a_neg, L_neg, electrode_charge_neg)

    
    #make list of current arrays
    #make an input file for the solver to read from for each batch of the solver
    #also build the commands to be run

    current_list = []
    cmnds = []
    for i in range(len(currents)):
        fname_pos = f'{filename_positive}{i}'
        fname_neg = f'{filename_negative}{i}'
        tsteps = (int(run_times[i]/dt)) + (int(wait_times[i]/dt))
        current_list.append(np.concatenate(([currents[i] for j in range(int(run_times[i]/dt))],[0.0 for j in range(int(wait_times[i]/dt))])))
        iapp_label = str(i)
        write_to_file(fname_pos,tsteps, dt, n, initial_concs_pos[i], D_pos, R_pos, a_pos, L_pos, current_list[i], iapp_label,electrode_charge_pos)
        write_to_file(fname_neg,tsteps, dt, n, initial_concs_neg[i], D_neg, R_neg, a_neg, L_neg, current_list[i], iapp_label,electrode_charge_neg)
        #### Set file name
        running_name_pos = fname_pos + '.txt'
        running_name_neg = fname_neg + '.txt'
        solver_call_line_pos = './finite_diff_solver' + ' filename=' + running_name_pos
        solver_call_line_neg = './finite_diff_solver' + ' filename=' + running_name_neg
        cmnds.append(solver_call_line_pos)
        cmnds.append(solver_call_line_neg)


    #now, we just need to launch a seperate instance of the solver for each process with each initial concentration and runtime
    #code from: https://stackoverflow.com/questions/30686295/how-do-i-run-multiple-subprocesses-in-parallel-and-wait-for-them-to-finish-in-py
    for j in range(max(int(len(cmnds)/nprocs), 1)):
        #launch processes, distributing evenly between processors
        procs = [subprocess.Popen(i, shell=True) for i in cmnds[j*nprocs: min((j+1)*nprocs, len(cmnds))]]
        for p in procs:
            #wait to ensure all done
            p.wait()

def full_battery_simulation(filename_positive,filename_negative,nprocs):


    '''! 1. Set up solver call line, including file name.'''
    filename_positive = filename_positive + '.txt'
    filename_negative = filename_negative + '.txt'
    solver_call_pos = './finite_diff_solver' + ' filename=' + filename_positive
    solver_call_neg = './finite_diff_solver' + ' filename=' + filename_negative

    cmnds = [solver_call_pos,solver_call_neg]
    '''! 2. Call solver.'''

    #now, we just need to launch a seperate instance of the solver for each process with each initial concentration and runtime
    #code from: https://stackoverflow.com/questions/30686295/how-do-i-run-multiple-subprocesses-in-parallel-and-wait-for-them-to-finish-in-py
    for j in range(max(int(len(cmnds)/nprocs), 1)):
        #launch processes, distributing evenly between processors
        procs = [subprocess.Popen(i, shell=True) for i in cmnds[j*nprocs: min((j+1)*nprocs, len(cmnds))]]
        for p in procs:
            #wait to ensure all done
            p.wait()
