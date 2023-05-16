'''!@package user_input_mod
@brief Package containing functions to set up user input parameters and execute the solver.
@details Options are provided to automatically generate applied current iapp as constant and step functions, and to read iapp from a csv file.
Options are provided to set default values of parameters from Chen et al. 2020, https://doi.org/10.1149/1945-7111/ab9050.
Additional functions validate the types and values of input parameters, write the user input txt file, and execute the solver.
'''

import subprocess
import shlex
import numpy as np

### CURRENT SET UP ###
def iapp_read_csv(filename):
    '''!@brief Reads applied current iapp from a csv file provided by user, 'filename'.
    @param[in] filename: Name of csv file containing iapp as Time, Current.
    @result iapp_arr: Array containing applied current.
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
    '''!@brief Set up applied current iapp as constant valued array of length tsteps.
    @param[in] tsteps: Number of timesteps, length of array iapp.
    @param[in] iapp: Constant value of iapp.
    @result iapp_arr: Array containing applied current.
    @result iapp_label: String containing description of iapp.
    '''

    '''! 1. Verify current iapp is a float.'''
    if (type(iapp)!=float):
        print('Constant applied current must be a float/real value.')
        exit()

    '''! 2. Set up array. '''
    iapp_label = 'Constant, ' + str(iapp)
    iapp_arr = np.ones(tsteps) *  iapp

    return iapp_arr, iapp_label

def iapp_step_setup(tsteps, iapp_steps):
    '''!@brief Set up applied current iapp as stepped array of length tsteps.
    @param[in] tsteps: Number of timesteps, length of array iapp.
    @param[in] iapp_steps: 2D Array containing heights of steps and timesteps where step occurs, starting timestep 0. Timesteps must be integers.
    @result iapp_arr: Array containing applied current.
    @result iapp_label: String containing description of iapp.
    '''

    '''! 1. Set up label and initialise array.'''
    iapp_label = 'Step function, ' + str(iapp_steps)

    iapp_arr = np.ones(tsteps)
    nsteps = len(iapp_steps)

    '''! 2. Verify timesteps are integers, and current are floats.'''
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
        print('Timesteps for current step function must be integers. Problems occur at steps: ', problem_timesteps)
    if (val_err):
        print('Applied currents in step function must be float/real values. Problems occur at steps: ', problem_vals)
    if (val_err or timestep_err):
        exit()

    '''! 3. Verify first timestep is 0.'''
    if (iapp_steps[0][1]!=0):
        print('Initial applied current must be at timestep 0.')
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
    Simulation is set up to run for 100 timesteps of size dt=0.1s.
    Applied current is set up as a constant current of value 0.73mA.
    '''

    # Number of timesteps, integer, greater than 0
    tsteps = 100
    # Timestep size (s), real, greater than 0
    dt = 0.1

    # Initial concentration (mol m**-3), real, positive
    c0 = 0.0
    # Diffusion coefficient (m**2 s**-1), real
    D = 4.0e-15
    # Width of block (m), real, greater than 0
    R = 5.86e-6
    # Particle surface area per unit volume (m**-1), real, greater than 0
    e_act = 0.665
    a = 3*e_act/R
    # Electrode thickness (m), real, greater than 0
    L = 75.6e-6

    ### Constant applied current (A m**2), real array of length tsteps
    iapp_const = 0.73*10**(-3)
    iapp, iapp_label = iapp_constant_setup(tsteps, iapp_const)

    return tsteps, dt, c0, D, R, a, L, iapp, iapp_label

def set_defaults_neg():
    '''!@brief Returns default parameters for a negative electrode
    @details Parameter values are taken from Chen et al. 2020, https://doi.org/10.1149/1945-7111/ab9050. 
    Simulation is set up to run for 100 timesteps of size dt=0.1s.
    Applied current is set up as a constant current of value 0.73mA.
    '''

    # Number of timesteps, integer, greater than 0
    tsteps = 100
    # Timestep size (s), real, greater than 0
    dt = 0.1

    # Initial concentration (mol m**-3), real, positive
    c0 = 0.0
    # Diffusion coefficient (m**2 s**-1), real
    D = 3.3e-14
    # Width of block (m), real, greater than 0
    R = 5.22e-6
    # Particle surface area per unit volume (m**-1), real, greater than 0
    e_act = 0.75
    a = 3*e_act/R
    # Electrode thickness (m), real, greater than 0
    L = 85.2e-6

    ### Applied current (A m**2), real array of length tsteps
    ### Constant current
    iapp_const = 0.73*10**(-3)
    iapp, iapp_label = iapp_constant_setup(tsteps, iapp_const)

    return tsteps, dt, c0, D, R, a, L, iapp, iapp_label



### PARAMETER VERIFICATION ###
def verify_params(filename, tsteps, dt, c0, D, R, a, L):
    '''!@brief Verifies types and values of input parameters.
    @details Verifies that parameters output filename, tsteps, dt, c0, D, R, a, and L have the correct type and valid values. 
    If any are found to be invalid, an error is printed and the execution stopped.
    @param[in] filename: Output filename, string, must have less than 50 chars. No file extension required.
    @param[in] tsteps: Number of timesteps, integer > 0.
    @param[in] dt: Timestep size, float > 0.
    @param[in] c0: Initial concentration, float >= 0.
    @param[in] D: Diffusion constant, float.
    @param[in] R: Width of block, float > 0.
    @param[in] a: Particle surface area per unit volume, float >= 0.
    @param[in] L: Electrode thickness, float >= 0.
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

    # If any errors have occured, stop script.
    if (var_error==True):
        exit()

    return

def verify_iapp(iapp, iapp_label, tsteps):
    '''!@brief Verifies types and values of applied current array and label.
    @details Verifies that parameters output iapp and iapp_label have the correct types, valid values, and correct lengths.
    If any are found to be invalid, an error is printed and the execution stopped.
    @param[in] iapp: Applied current, 1D array of floats of length tsteps.
    @param[in] iapp_label: Label of applied current, string.
    @param[in] tsteps: Number of timesteps, to check iapp has correct length.
    '''

    # set var_error to True is any errors occur
    var_error = False

    # iapp
    iapp_type = [(type(iapp[i].item())!=float) for i in range(len(iapp))]
    if (any(iapp_type)):
        print('Applied current, iapp, must be an array of float/real values.')
        var_error = True
    if (len(iapp)!=tsteps):
        print('Applied current, iapp, must be an array of length tsteps.')
        var_error = True

    # iapp_label
    if (type(iapp_label)!=str):
        print('Label for applied current function, iapp_label, must be a string.')
        var_error = True

    # if any errors have occured, stop script
    if (var_error==True):
        exit()

    return



### WRITE TO FILE ###
def write_to_file(filename, tsteps, dt, c0, D, R, a, L, iapp, iapp_label):
    '''!@brief Function writes user inputs to txt file.
    @details Writes user inputs to txt file named 'filename'. 
    Parameters are output in format 'parameter = value'.
    tsteps, dt, c0, D, R, a, L are output to top of file, followed by an asterix line (***).
    iapp_label follows the asterix line, with iapp array following, written one element per line.
    '''


    '''! 1. Set file name.'''
    filename = filename + '.txt'

    '''! 2. Make list of strings to write to file.'''
    parameters = []
    parameters.append('tsteps = ' + str(tsteps) + '\n')
    parameters.append('dt = ' + str(dt) + '\n')
    parameters.append('c0 = ' + str(c0) + '\n')
    parameters.append('D = ' + str(D) + '\n')
    parameters.append('R = ' + str(R) + '\n')
    parameters.append('a = ' + str(a) + '\n')
    parameters.append('L = ' + str(L) + '\n')

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
def call_solver(filename):
    '''!@brief Execution of SPM solver.
    @details SPM solver is called using the subprocess package.
    The filename of the user input file is passed to the solver as a command line argument.
    Errors from execution are read in and further execution prevented if necessary.
    @param[in] filename: Name of user input file, no file extension.
    '''

    '''! 1. Set up solver call line, including file name.'''
    filename = filename + '.txt'
    solver_call_line = './finite_diff_solver' + ' filename=' + filename

    print('User input successful, calling solver...')


    '''! 2. Call solver.'''
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

### CALL PLOTTER ###
def call_plotter():
    '''!@brief Execution of plotting script.
    @details plotter for the SPM output file(s) is called using the subprocess package.
    Errors from execution are read in and further execution prevented if necessary.
    '''
    
    '''! 3. Call plotter.'''
    command_plotter = shlex.split('python3 plotter.py')    
    process_plotter = subprocess.run(command_plotter, stdout=subprocess.PIPE, universal_newlines=True)
    return_plotter = process_plotter.returncode
    # Print plotter output to command line
    if (process_plotter.stdout):
        print(process_plotter.stdout) 
    # Check for errors in execution
    if (return_plotter==0):
        print('Plotting code executed successfully.')
    else:
        print('Error executing plotting code, process terminated.')
        exit()

    return


### INITIALISE A FULL GITT TEST IN PARALLEL ####
def GITT(filename,nprocs,currents,start_times,run_times,wait_times,params):
    '''!@brief Execution of SPM solver in parallel to run a GITT test over nprocs cores.
    @details Execution of SPM solver in parallel to run a test experiment on a battery
    in the style of a galvanostatic intermittent titration technique (GITT), as described in 
    W. Weppner and R. A. Huggins 1977 J. Electrochem. Soc. 124 1569. Due to the equilibration time between each current
    step applied in this technique, it is possible to pre-compute the initial constant concentration in each of the single
    particles in the model by considering the amount of lithium removed in each current step.
    Errors from execution are read in and further execution prevented if necessary.
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
    @param[in] params: A vector containing the parameters for the simulation: [dt, c0, D, R, a, L]
        @param[in] dt: Timestep size, float > 0.
        @param[in] c0: Initial concentration, float >= 0.
        @param[in] D: Diffusion constant, float.
        @param[in] R: Width of block, float > 0.
        @param[in] a: Particle surface area per unit volume, float >= 0.
        @param[in] L: Electrode thickness, float >= 0.
    '''
    #first, generate the arrays for initial concentration
    F = 96485.3321 #faraday constant
    #unpack params
    [dt, c0, D, R, a, L] = params

    #array of initial concentrations, using the fact that
    # C(T=t) = C0 + ((i_app*t)/(F*L))
    # where i_app is the current into the battery (positive, current flows in, negative, current flows out). 

    initial_concs = [c0 - (currents[i]*np.sum(run_times[0:i]))/(F*L) for i in range(len(start_times))]
    
    #make list of current arrays
    #make an input file for the solver to read from for each batch of the solver
    #also build the commands to be run

    current_list = []
    cmnds = []
    total_tsteps = 0
    for i in range(len(currents)):
        fname = f'{filename}{i}'
        tsteps = (int(run_times[i]/dt)) + (int(wait_times[i]/dt))
        total_tsteps+=tsteps
        current_list.append(np.concatenate(([currents[i] for j in range(int(run_times[i]/dt))],[0.0 for j in range(int(wait_times[i]/dt))])))
        iapp_label = str(i)
        write_to_file(fname,tsteps, dt, initial_concs[i], D, R, a, L, current_list[i], iapp_label)
        #### Set file name
        running_name = fname + '.txt'
        solver_call_line = './finite_diff_solver' + ' filename=' + running_name
        cmnds.append(solver_call_line)


    ##### For visualisation, write a user_input.txt file which holds the full current vector for the simulation
    #print(dt,current_list)
    current_arr = np.array(current_list).flatten()
    write_to_file('user_input',total_tsteps,dt,c0,D,R,a,L,current_arr,'full_current_array')

    #now, we just need to launch a seperate instance of the solver for each process with each initial concentration and runtime
    #code from: https://stackoverflow.com/questions/30686295/how-do-i-run-multiple-subprocesses-in-parallel-and-wait-for-them-to-finish-in-py
    for j in range(max(int(len(cmnds)/nprocs), 1)):
        #launch processes, distributing evenly between processors
        procs = [subprocess.Popen(i, shell=True) for i in cmnds[j*nprocs: min((j+1)*nprocs, len(cmnds))]]
        for p in procs:
            #wait to ensure all done
            p.wait()