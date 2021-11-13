#!/usr/bin/env python

#simulation parameters
replicates=1
beh_per_TS = 1 #behaviors individuals perform per timestep

### Population parameters ###
N = 100 #population size
initial_knowledgable_prop = 1 #initial proportion of knowledgable individuals in the population
full_initial_weights = False
graph_types = ["random_regular", "random_erdos","random_barabasi","random_small_world"] #sets network structure
custom_adj_list_filename = ""

### NBDA parameters ###
NBDA_type = 0 # 0: Unbounded general form; 1: bounded, linked S and (1-S);
f_T_values = [1]
s_param_values = [0] #s parameter from NBDA indicating strength of social learning per unit of connection
z_jt_type_values = ["proportional"] #"binary" or "proportional", proportional assigns z_j(t) a number between [0,1] depending on how frequently the produced behavior in previous timestep
asocial_learning = True #True or false, depending on if asocial learning occurs
base_rate = .05 #the base hazard is the underlying rate in every timestep that the behavior could be acquired

#EWA Parameters
sigma_values = [0.25,0.5,0.75]#np.around(np.arange(0.1, 1.1, 0.4), 1) #social information bias EWA values
phi_values = [0.01,0.5,0.99]#np.around(np.arange(0.1, 1.1, 0.4), 1) #recent payoff bias EWA values
f_P_values = [1,3] #conformity exponent EWA values
tau_values = [1,2] #sensitivity to differences in payoffs
memory_window_values = [10,20,30] #how far back the agent can remember (in time steps) the behaviors that other agents produce

behavior_names = ["a","b"]
behavior_payoffs = [1,1]

#output file placement bookkeeping
condition_name = "NBDA_asocial_proportional"
file_name = "{}_timestep_data.csv".format(condition_name)
file_name_acq_prod = "{}_acq_prod.csv".format(condition_name)
