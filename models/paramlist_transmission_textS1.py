#!/usr/bin/env python

#simulation parameters
replicates=100
beh_per_TS = 1 #behaviors individuals perform per timestep
production_only=False

### Population parameters ###
N = 24 #population size
initial_knowledgable_prop = 1 #initial proportion of knowledgable individuals in the population
full_initial_weights = True
graph_types = ["random_regular", "random_erdos","random_barabasi","random_small_world"] #sets network structure
custom_adj_list_filename = ""

### NBDA parameters ###
s_param_values = [1,5,10,15] #s parameter from NBDA indicating strength of social learning per unit of connection
transmission_weight_values = ["proportional"] #"binary" or "proportional", proportional assigns z_j(t) a number between [0,1] depending on how frequently the produced behavior in previous timestep
asocial_learning_values = [True, False] #True or false, depending on if asocial learning occurs
base_rate = 0.1 #the base hazard is the underlying rate in every timestep that the behavior could be acquired, tested 0.01, 0.05, 0.1

#EWA Parameters
sigma_values = [0.5]#np.around(np.arange(0.1, 1.1, 0.4), 1) #social information bias EWA values
rho_values = [0.1]#np.around(np.arange(0.1, 1.1, 0.4), 1) #recent payoff bias EWA values
chi_values = [1] #conformity exponent EWA values
alpha_values = [1] #sensitivity to differences in payoffs
memory_window_values = [10] #how far back the agent can remember (in time steps) the behaviors that other agents produce

behavior_names = ["a","b"]
behavior_payoffs = [1,1]

#output file placement bookkeeping
condition_name = "transmission_params_textS1"
file_name = "{}_lambda_{}_timestep_data.csv".format(condition_name,base_rate)
file_name_acq_prod = "{}_lambda_{}_acq_prod.csv".format(condition_name,base_rate)
file_name_agent_mat_val = "{}_lambda_{}_agent_mat_val.csv".format(condition_name,base_rate)
