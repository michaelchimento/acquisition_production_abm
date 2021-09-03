#!/usr/bin/env python

#simulation parameters
replicates=100
beh_per_TS = 1 #behaviors individuals perform per timestep
full_initial_weights=False

### Population parameters ###
N = 16 #population size
initial_knowledgable_prop = 1 #initial proportion of knowledgable individuals in the population
graph_types = ["random_regular", "random_erdos","random_barabasi","random_small_world"] #sets network structure
custom_adj_list_filename = ""

### NBDA parameters ###
NBDA_type = 3 # 0: Unbounded general form; 1: bounded, linked S and (1-S); NEED TO ADD: ILV form allowing for effects for social AND individual learning, requires vectors Gamma_i, Beta_i
f_SL_values = [1,2]
s_param_values = [100] #s parameter from NBDA indicating strength of social learning per unit of connection
z_jt_type_values = ["proportional"] #"binary" or "proportional", proportional assigns z_j(t) a number between [0,1] depending on how frequently the produced behavior in previous timestep
asocial_learning = False #True or false, depending on if asocial learning occurs
base_rate = .01 #the base hazard is the underlying rate in every timestep that the behavior could be acquired

#EWA Parameters
sigma_values = [0.5]#np.around(np.arange(0.1, 1.1, 0.4), 1) #social information bias EWA values
phi_values = [0.5]#np.around(np.arange(0.1, 1.1, 0.4), 1) #recent payoff bias EWA values
f_SI_values = [1,2] #conformity exponent EWA values
tau_values = [1,2] #sensitivity to differences in payoffs
memory_window_values = [1,50,100] #how far back the agent can remember (in time steps) the behaviors that other agents produce

behavior_names = ["a","b"]
behavior_payoffs = [1,1]


#name of csv with containing all sims
file_name = "equiv_payoffs_memoryconformity.csv"
