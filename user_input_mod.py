'''!@package user_input_mod
@brief Package containing functions to set up user input parameters and execute the solver.
@details Options are provided to automatically generate applied current density iapp as constant and step functions, and to read iapp from a csv file.
Options are provided to set default values of parameters from Chen et al. 2020, https://doi.org/10.1149/1945-7111/ab9050.
Additional functions validate the types and values of input parameters, write the user input txt file, and execute the solver.
'''

import subprocess
import shlex
import numpy as np

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
    Simulation is set up to run for 100 timesteps of size dt=0.1s.
    Applied current density is set up as a constant current density of value 0.73mA.
    '''
    # Label of positive electrode
    electrode_charge = "p"

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

    ### Constant applied current density (A m**2), real array of length tsteps
    iapp_const = 0.73*10**(-3)
    iapp, iapp_label = iapp_constant_setup(tsteps, iapp_const)

    return tsteps, dt, c0, D, R, a, L, iapp, iapp_label, electrode_charge

def set_defaults_neg():
    '''!@brief Returns default parameters for a negative electrode
    @details Parameter values are taken from Chen et al. 2020, https://doi.org/10.1149/1945-7111/ab9050. 
    Simulation is set up to run for 100 timesteps of size dt=0.1s.
    Applied current density is set up as a constant current density of value 0.73mA.
    '''
    # Label of negative electrode
    electrode_charge = "n"

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

    ### Applied current density (A m**2), real array of length tsteps
    ### Constant current density
    iapp_const = 0.73*10**(-3)
    iapp, iapp_label = iapp_constant_setup(tsteps, iapp_const)

    return tsteps, dt, c0, D, R, a, L, iapp, iapp_label, electrode_charge



### PARAMETER VERIFICATION ###
def verify_params(filename, tsteps, dt, c0, D, R, a, L, electrode_charge):
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
def write_to_file(filename, tsteps, dt, c0, D, R, a, L, iapp, iapp_label, electrode_charge):
    '''!@brief Function writes user inputs to txt file.
    @details Writes user inputs to txt file named 'filename'. 
    Parameters are output in format 'parameter = value'.
    tsteps, dt, c0, D, R, a, L, electrode_charge are output to top of file, followed by an asterix line (***).
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
def call_solver(filename):
    '''!@brief Execution of SPM solver and plotting script.
    @details SPM solver and plotting scripts are called using the subprocess package.
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
