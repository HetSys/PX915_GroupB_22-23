import subprocess
import shlex
import numpy as np

### CURRENT FILE READ ###
def iapp_read_csv(filename):

    # Read in csv file
    with open(filename, 'r') as iapp_file:
        iapp_all = iapp_file.read()
    iapp_file.close()

    # Parse file into array of lines
    iapp_lines = iapp_all.split("\n")
    nlines = len(iapp_lines) -1

    # Parse each line at comma
    iapp_arr = np.zeros(nlines)
    for i in range(nlines):
        line = i+1
        iapp_arr[i] = iapp_lines[line].split(",")[1]
    
    tsteps = nlines
    iapp_label = 'From file: ' + filename

    return iapp_arr, iapp_label, tsteps
    
    

### CURRENT SET UP ###
def iapp_constant_setup(tsteps, iapp):
    # Verify current value is a float
    if (type(iapp)!=float):
        print('Constant applied current must be a float/real value.')
        exit()

    # Set up array
    iapp_label = 'Constant, ' + str(iapp)
    iapp_arr = np.ones(tsteps) *  iapp
    return iapp_arr, iapp_label


def iapp_step_setup(tsteps, iapp_steps):
    iapp_label = 'Step function, ' + str(iapp_steps)

    iapp_arr = np.ones(tsteps)
    nsteps = len(iapp_steps)

    # Verify timesteps are integers, and current are floats
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

    # Verify first timestep is 0
    if (iapp_steps[0][1]!=0):
    	print('Initial applied current must be at timestep 0.')
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



### DEFAULT VALUES
def set_defaults_pos():
    # Default constants taken from Chen et al. 2020, https://doi.org/10.1149/1945-7111/ab9050

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

    ### Applied current (A m**2), real array of length tsteps
    ### Constant current
    iapp_const = 0.73*10**(-3)
    iapp, iapp_label = iapp_constant_setup(tsteps, iapp_const)

    return tsteps, dt, c0, D, R, a, L, iapp, iapp_label

def set_defaults_neg():
    # Default constants taken from Chen et al. 2020, https://doi.org/10.1149/1945-7111/ab9050

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

    # set var_error to True is any errors occur
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


    # if any errors have occured, stop script
    if (var_error==True):
        exit()


### APPLIED CURRENT VERIFICATION ###
def verify_iapp(iapp, iapp_label, tsteps):
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


### WRITE TO FILE ###
def write_to_file(filename, tsteps, dt, c0, D, R, a, L, iapp, iapp_label):

    # Set file name
    filename = filename + '.txt'

    # Make strings to write to file
    parameters = []
    parameters.append('tsteps = ' + str(tsteps) + '\n')
    parameters.append('dt = ' + str(dt) + '\n')
    parameters.append('c0 = ' + str(c0) + '\n')
    parameters.append('D = ' + str(D) + '\n')
    parameters.append('R = ' + str(R) + '\n')
    parameters.append('a = ' + str(a) + '\n')
    parameters.append('L = ' + str(L) + '\n')

    parameters.append('******************\n')

    parameters.append('iapp = ' + iapp_label)
    for i in range(len(iapp)):
        parameters.append('\n')
        parameters.append(str(iapp[i]))

    # Write to file
    with open(filename, 'w') as user_input:
        user_input.writelines(parameters)
    user_input.close()




### CALL SOLVER ###
def call_solver(filename):

    #### Set file name
    filename = filename + '.txt'
    solver_call_line = './finite_diff_solver' + ' filename=' + filename

    print('User input successful, calling solver...')

    #### Call solver 
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

    #### Call plotter
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

