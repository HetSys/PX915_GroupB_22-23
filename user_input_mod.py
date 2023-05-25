'''!@package user_input_mod
@brief Package containing the functions needed to set up the user input parameters and execute the solver.
@details Provides options for automatically generating applied current density 'iapp', as a constant or step function, and for reading iapp from a csv file.

Provides the option to set the parameters to default values obtained from \cite Chen2020.

Contains additional functions for validating the types and values of the input parameters, writing the user input txt file, and executing the solver.
'''

import subprocess
import shlex
import numpy as np
import os

### CURRENT DENSITY SET UP ###
def iapp_read_csv(filename):
    '''!@brief Reads in the applied current density 'iapp' from a csv file provided by user.

    @param[in] filename Name of the csv file containing iapp as Time, Current density.
    @result iapp_arr: Array containing applied current density.
    @result iapp_label: String containing a description of iapp.
    @result tsteps: Number of timesteps, found from the length of iapp array.

    This function does the following:
    1. Reads in the csv file.
    2. Parses the file into array of lines.
    3. Parses each line at comma.
    4. Define tsteps and the iapp_label.
    '''

    # Read in csv file
    with open(filename, 'r') as iapp_file:
        iapp_all = iapp_file.read()
    iapp_file.close()

    # Parse the file into array of lines
    iapp_lines = iapp_all.split("\n")
    nlines = len(iapp_lines) -1

    # Parse each line at comma
    iapp_arr = np.zeros(nlines)
    for i in range(nlines):
        line = i+1
        iapp_arr[i] = iapp_lines[line].split(",")[1]
    
    # Define tsteps and iapp_label
    tsteps = nlines
    iapp_label = 'file = ' + filename

    return iapp_arr, iapp_label, tsteps
    
def iapp_constant_setup(tsteps, iapp):
    '''!@brief Sets up the applied current density 'iapp' as a constant valued array of length tsteps.

    @param[in] tsteps Number of timesteps, needs to be the length of the array iapp.
    @param[in] iapp Constant value of iapp.
    @result iapp_arr: An array containing applied current density.
    @result iapp_label: String containing a description of iapp.

    Function does the following:
    1. Verifies that the current density, iapp, is a float.
    2. Sets up an array.
    '''

    # Verify current density iapp is a float
    if (type(iapp)!=float):
        print('Constant applied current density must be a float/real value.')
        exit()

    #Set up array
    iapp_label = 'Constant, ' + str(iapp)
    iapp_arr = np.ones(tsteps) *  iapp

    return iapp_arr, iapp_label

def iapp_step_setup(tsteps, iapp_steps):
    '''!@brief Sets up the applied current density 'iapp' as a stepped array of length tsteps.

    @param[in] tsteps Number of timesteps, needs to be the length of the array iapp.
    @param[in] iapp_steps 2D Array containing heights of the steps and the timesteps at which step occurs, starting at timestep 0. Timesteps must be integers.
    @result iapp_arr: An array containing the applied current density.
    @result iapp_label: A string describing the type for iapp.

    Function does the following:
    1. Sets up the label and initialises the array.
    2. Verifies that timesteps are integers and current density values are floats.
    3. Verifies that the first timestep is 0.
    4. Sets up an array.
    '''

    # Set up label and initialise array
    iapp_label = 'Step function, ' + str(iapp_steps)

    iapp_arr = np.ones(tsteps)
    nsteps = len(iapp_steps)

    # Verify timesteps are integers, and current densitiy values are floats
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

    # Verify first timestep is 0
    if (iapp_steps[0][1]!=0):
        print('Initial applied current density must be at timestep 0.')
        exit()
        

    # Set up array
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
    @details Gives the default parameter values, obtained from \cite Chen2020
    The simulation is set up to run for 100 timesteps of size dt = 0.1s, with n = 1000 spatial nodes.
    Applied current density is set up as a constant current density of value 0.73mA m^2.
    '''
    # Label of positive electrode
    electrode_charge = "p"

    # Number of timesteps, integer, greater than 0
    tsteps = 200
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
    @details Gives the default parameter values, obtained from \cite Chen2020. 
    THe simulation is set up to run for 100 timesteps of size dt = 0.1s, with n = 1000 spatial nodes.
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
    '''!@brief Verifies the types and values of the input parameters.

    @details Verifies that output filename, tsteps, dt, c0, D, R, a, and L have the correct type and valid values. 
    If any are found to be invalid, an error is printed and the execution will stop.

    @param[in] filename  The name of the output file, this must be a string and have max 50 characters. No file extension is required.
    @param[in] tsteps  Number of timesteps, must be an integer greater than 0.
    @param[in] dt  Timestep size, must be a float greater than 0.
    @param[in] n  Number of spatial nodes, must be an integer between 100 and 4000.
    @param[in] c0  Initial concentration, must be a float greater than or equal to 0.
    @param[in] D  Diffusion constant, must be a float.
    @param[in] R  Radius of the sphere, must be a float greater than 0.
    @param[in] a  Particle surface area per unit volume, must be a float greater than or equal to 0.
    @param[in] L  Electrode thickness, must be a float greater than or equal to 0.
    @param[in] electrode_charge  Labels charge of the electrode, must b a string, 'p' for positive or 'n' for negative.
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
    '''!@brief Verifies the types and values of the applied current density array and its label.

    @details Verifies that the parameters output iapp and iapp_label have the correct types, valid values, and correct lengths.
    If any are found to be invalid, an error is printed and the execution will stop.
    @param[in] iapp: Applied current density, 1D array of floats of length tsteps.
    @param[in] iapp_label: Labels the type of applied current density, must be a string.
    @param[in] tsteps: Number of timesteps, in order to check iapp has the correct length.
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
    '''!@brief Writes user inputs to a txt file.

    @details Writes the user inputs to a txt file named 'filename'. 
    Parameters are output in the format 'parameter = value'.
    tsteps, dt, c0, D, R, a, L, electrode_charge are output to the top of the file, followed by an asterix line (***).
    iapp_label is output below the asterix line, with iapp array the following it, written one element per line.

    The function works by:
    1. Setting the file name.
    2. Making a list of strings to write to the file.
    3. Writing to the file.
    '''

    # The solver and plotting equations are built to work for the anode (negative electrode), that is 
    # losing concentration is a NEGATIVE current (discharging)
    # gaining concentration is a POSIITIVE current (charging). 
    # Hence, if the electrode is positive, we need to flip the sign on the current that we save to the file,
    # as it sees the reverse of what would be applied to the negative electrode
    if electrode_charge == 'p':
        iapp = -iapp
    
    # Set file name.'''
    filename = filename + '.txt'

    # Make list of strings to write to file
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

    # Write to file
    with open(filename, 'w') as user_input:
        user_input.writelines(parameters)
    user_input.close()

    return



### CALL SOLVER ###
def call_solver(filename, checkpoint, nprocs=1):
    '''!@brief Executes the SPM solver.

    @details Calls the SPM solver using the subprocess package.
    The filename of the user input file is passed to the solver as a command line argument.
    Errors from the execution are read in and further execution prevented if necessary.
    @param[in] filename: The name of the file containing the desired input, either a checkpoint file or user input file. No file extension is required.
    @param[in] checkpoint: Boolean indicating if a checkpoint file is used.

    The function works by:
    1. Validating whether the input file is a checkpoint file or user input file name.
    2. Setting up the solver call line, including the file name.
    3. Calling the solver.
    '''
    #optimise the number of threads used based on the number of processors provided
    optimise_parallelism(nprocs)

    # 1. Validate checkpoint file or user input file name.
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

    # 2. Set up solver call line.
    solver_call_line = './finite_diff_solver' + ' filename="' + filename + '"'


    # 3. Call solver
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

def call_solver(filename, checkpoint):
    '''!@brief Executes the SPM solver.

    @details Calls the SPM solver using the subprocess package.
    The filename of the user input file is passed to the solver as a command line argument.
    Errors from the execution are read in and further execution prevented if necessary.
    @param[in] filename: The name of the file containing the desired input, either a checkpoint file or user input file. No file extension is required.
    @param[in] checkpoint: Boolean indicating if a checkpoint file is used.

    The function works by:
    1. Validating whether the input file is a checkpoint file or user input file name.
    2. Setting up the solver call line, including the file name.
    3. Calling the solver.
    '''

    # 1. Validate checkpoint file or user input file name.
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

    # 2. Set up solver call line.
    solver_call_line = './finite_diff_solver' + ' filename_txt="' + filename + '"'


    # 3. Call solver
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

def call_solver_further(solver_input_filename_txt,solver_input_filename_nc):
    '''!@brief Executes the SPM solver.

    @details Calls the SPM solver using the subprocess package.
    The filename of the user input file is passed to the solver as a command line argument.
    Errors from the execution are read in and further execution prevented if necessary.
    @param[in] filename: The name of the file containing the desired input, either a checkpoint file or user input file. No file extension is required.
    @param[in] checkpoint: Boolean indicating if a checkpoint file is used.

    The function works by:
    1. Validating whether the input file is a checkpoint file or user input file name.
    2. Setting up the solver call line, including the file name.
    3. Calling the solver.
    '''

    # 1. Validate checkpoint file or user input file name.
    # If using checkpoint, check input file is a netcdf file with extension '.nc'
    file_extension = solver_input_filename_nc.split(".")
    if (len(file_extension)<2):
        # Check filename has an extension.
        print("Invalid checkpoint file. Please enter a file with a '.nc' extension.")
        exit()
    elif (file_extension[-1]!='nc'):
        # Check extension of file name is 'nc'.
        print("Invalid checkpoint file. Please enter a file with a '.nc' extension.")
        exit()
    elif (not os.path.isfile(solver_input_filename_nc)):
        # Check .nc file exists
        print("Checkpoint file not found:", solver_input_filename_nc)
        exit()
    else:
        print("Checkpoint file passed as input file, calling solver...")

    filename_txt = solver_input_filename_txt + '.txt'
    print("User input file generated, calling solver...")

    # 2. Set up solver call line.
    solver_call_line = './finite_diff_solver' + ' filename_txt="' + filename_txt + '" '+'filename_nc="' + solver_input_filename_nc + '"'
    print(solver_call_line)


    # 3. Call solver
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
    '''@brief Calculates the initial concentrations for each step in a multi-step parallelised GITT test.

    @details Computes initial flat concentrations for the initial step of a parallelised multi-step GITT test. 

    The formula used for this calculation is:
    C(T=t) = C_0 + \frac{i_app * t}{F*e_act * L}, where C_0 is C(T=0). 

    As current is defined as positive for charging, the sign of the current vector must be flipped when considering
    the positive electrode (cathode) which will discharge during charging.

    @param[in] currents  A vector containing current values to apply at each current step, must be floats and have the same length as
    start_times, run_times, wait_times.
    @param[in] run_times  A vector containing the run time of each current step, must be floats and have the same length as 
    start_times, currents, wait_times.
    @param[in] c0  Initial concentration, must be a float greater than or equal to 0.
    @param[in] R  Radius of the sphere, must be a float greater than 0.
    @param[in] a  Particle surface area per unit volume, must be a float greater than or equal to 0.
    @param[in] L  Electrode thickness, must be a float greater than or equal to 0.
    @param[in] electrode_charge  Electrode charge, mus be a single character 'p' or 'n'.
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

    '''!@brief Executes the SPM solver in parallel to run a GITT half cell test over nprocs cores.

    @details Executes the SPM solver in parallel to run a test experiment on a half cell using a galvanostatic intermittent titration technique (GITT), 
    see \cite Weppner1977
    
    Due to the equilibration time between each current step applied, it is possible to pre-compute initial constant concentration for each of the single
    particles in the model by considering the amount of lithium removed in each step.
    
    Errors from execution are read in and further execution is prevented if necessary.

    @param[in] filename  Name of user input file, no file extension. Note that a version of this file is created for each current step applied in the GITT test. 
    @param[in] nprocs  Number of processors to parallelise current steps over.
    Cannot parallelise over more processors than the number of steps applied in the GITT test.
    @param[in] currents  A vector containing the values of current to apply at each step, must be floats and have the same length as
    start_times, run_times, wait_times.
    @param[in] start_times  A vector containing the start times of each current step, must be floats and have the same length as
    currents, run_times, wait_times.
    @param[in] run_times  A vector containing the run time of each current step, must be floats and have the same length as 
    start_times, currents, wait_times.
    @param[in] wait_times  A vector containing the wait time of each current step, must be floats and have the same length as 
    currents, start_times, run-times.
    @param[in], n: The number of nodes to use in the simulation. Must be an integer greater than 100.
    @param[in] params: A vector containing the parameters for the simulation: [dt, c0, D, R, a, L, electrode_charge]
    '''

    # Optimise the the number of threads based on both nprocs and the 
    # Number of GITT steps
    optimise_parallelism(nprocs,n_gitt_steps=len(currents))

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
    '''!@brief Executes the SPM solver in parallel to run a GITT full cell test over nprocs cores.

    @details Executes the SPM solver in parallel to run a test experiment on a full cell using a galvanostatic intermittent titration technique (GITT), 
    see \cite Weppner1977
    
    Due to the equilibration time between each current step applied, it is possible to pre-compute initial constant concentration for each of the single
    particles in the model by considering the amount of lithium removed in each step. 

    @param[in] filename_positive  Name of user input file for the cathode, no file extension. Note that a version of this file is created for 
    each current step applied in the GITT test.
    @param[in] filename_negative  Name of user input file for the anode, no file extension. Note that a version of this file is created for 
    each current step applied in the GITT test.
    @param[in] nprocs  Number of processors to parallelise current steps over.
    Cannot parallelise over more processors than the number of steps applied in the GITT test.
    @param[in] currents  A vector containing the values of current to apply at each step, must be floats and have the same length as
    start_times, run_times, wait_times.
    @param[in] start_times  A vector containing the start times of each current step, must be floats and have the same length as
    currents, run_times, wait_times.
    @param[in] run_times  A vector containing the run time of each current step, must be floats and have the same length as 
    start_times, currents, wait_times.
    @param[in] wait_times  A vector containing the wait time of each current step, must be floats and have the same length as 
    currents, start_times, run-times.
    @param[in], n: The number of nodes to use in the simulation. Must be an integer greater than 100.
    @param[in] params: A vector containing the parameters for the simulation: [dt, c0, D, R, a, L]

    The function works by:
    1. Setting up the solver call line, including the file name.
    2. Calling the solver.    '''

    # Optimise the the number of threads based on both nprocs and the 
    # Number of GITT steps
    optimise_parallelism(nprocs,n_gitt_steps=len(currents),full_battery=True)

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

def full_battery_simulation(filename_positive,filename_negative,nprocs=1):
    '''!@brief Executes the SPM solver.

    @details Calls the SPM solver using the subprocess package for both the anode and cathode simultaneously
    The filenames of the user input files for both the anode and cathode are passed to 
    instances of the solver as command line arguments. Note that if 2 processors are supplied,
    both the anode and cathode will run simultaneously.
    @param[in] filename_positive: The name of the positive output file, this must be a string and have max 50 characters. No file extension is required.
    @param[in] filename_negative: The name of the negative output file, this must be a string and have max 50 characters. No file extension is required.
    
    The function works by:
    1. Seting up the solver call line, including the file name.
    2. Calling the solver.
    '''
    # Optimise the the number of threads based on nprocs
    optimise_parallelism(nprocs,full_battery=True)

    # Set up solver call line, including file name 
    filename_positive = filename_positive + '.txt'
    filename_negative = filename_negative + '.txt'
    solver_call_pos = './finite_diff_solver' + ' filename=' + filename_positive
    solver_call_neg = './finite_diff_solver' + ' filename=' + filename_negative

    cmnds = [solver_call_pos,solver_call_neg]
    # Call solver

    #now, we just need to launch a seperate instance of the solver for each process with each initial concentration and runtime
    #code from: https://stackoverflow.com/questions/30686295/how-do-i-run-multiple-subprocesses-in-parallel-and-wait-for-them-to-finish-in-py
    for j in range(max(int(len(cmnds)/nprocs), 1)):
        #launch processes, distributing evenly between processors
        procs = [subprocess.Popen(i, shell=True) for i in cmnds[j*nprocs: min((j+1)*nprocs, len(cmnds))]]
        for p in procs:
            #wait to ensure all done
            p.wait()

def optimise_parallelism(n_procs,n_gitt_steps=1,full_battery=False):
    '''!@brief Decides the correct number of threads to use for a given simulation.

    @details Given a user supplied number of processors, as well as further details about the simulation
    (the number of current steps if it's a GITT test, if the simulation is a full cell or not), this function
    decides on the optimum number of threads to use. It never picks a number of threads greater than 4,
    as any larger than this and the communication overhead between threads during the matrix solve begins to dominate
    and the computation time increases. This function therefore allows a balanced optimum parallelisation strategy.
    @param[in] n_procs: The number of processors that the user is happy for the program to exploit. The program will exploit a number
    of cores less than or equal to this number. Integer.
    @param[in] n_gitt_steps: The number of GITT steps if the experiment being performed is a GITT test. 1 by default (in the case of 
    the experiment not being a GITT test) integer.
    @param[in] full_battery: Bool, True means that both sides of a battery are being simulated, whilst False means only one side.
    
    The function works by:
    1. Deciding what number of threads is best to use given the type of simulation and input parameters, up to a maximum of 4.
       Note that this is based off the principle that parallelisation over independent seperate instances of the solver is always better
       than multithreading.
    2. Setting the environment variable.
    '''
    #function which decides how many MKL_NUM_THREADS to use
    # based on: total number of processors available (n_procs)
    # number of nodes in each matrix inversion (node_num)
    # number of steps in a gitt test if it's being run.

    # essentially, compute n_gitt_steps*2(if full battery)
    # look at this number. If it is <n_procs/4, use 4 thread, if it is
    # n_procs/4< but <n_procs/2, use 2 thread, if it is >n_procs/2 use 1 thread

    if full_battery == True:
        multip = 2
    else:
        multip = 1

    n_gitt_full_batt = multip*n_gitt_steps

    if n_gitt_full_batt<=int(n_procs/4):
        os.environ['MKL_NUM_THREADS']='4'
        os.system('echo set MKL_NUM_THREADS to $MKL_NUM_THREADS')
    elif (n_gitt_full_batt>int(n_procs/4) and n_gitt_full_batt<=int(n_procs/2)):
        os.environ['MKL_NUM_THREADS']='2'
        os.system('echo set MKL_NUM_THREADS to $MKL_NUM_THREADS')
    else:
        os.environ['MKL_NUM_THREADS']='1'
        os.system('echo set MKL_NUM_THREADS to $MKL_NUM_THREADS')
    
