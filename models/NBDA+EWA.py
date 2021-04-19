#!/usr/bin/env python

from collections import Counter
import math
import sys
import networkx as nx
import random
import numpy as np
from os.path import isfile,join
from multiprocessing import Pool,Lock,Value,Manager
import pandas as pd
from os import listdir,cpu_count,remove


#define the class behavior
class behavior:
    def __init__(self,name,payoff,base_haz):
        self.name = name
        self.payoff = payoff
        self.base_haz = base_haz

class agent:

    '''
    instances of agents are created and attached to nodes in the network
    agent in node x can be referenced with G.nodes[x]["data"]
    '''
    def __init__(self,ID,params_list):
        self.id = ID #keeps agents identifiable across simulations
        self.knowledge = {} #name, a_mat, i_mat, s_mat, p_mat,solve_count
        self.ind_memory = []
        self.temp_social_memory = []
        self.long_social_memory = []
        self.exposure = 0
        self.naive = True        
        #EWA parameters
        self.s_i = params_list[0]
        self.g_i = params_list[1]
        self.conformity = params_list[2]
        self.inverse_temp = params_list[3]  

    def observe(self,behavior):
        if behavior in self.knowledge:
            self.temp_social_memory.append(behavior)
            #print("observation by {}: {}".format(self.id, self.knowledge[behavior]))

    def prune_ind_memory(self,timestep):
        if timestep >= memory_window:
            self.ind_memory.pop(0)

    def prune_social_memory(self,timestep):
        if timestep >= memory_window:
            self.long_social_memory.pop(0)

    def consolidate_social_memory(self):
        self.long_social_memory.append(self.temp_social_memory)

    def A_mat_update(self, behavior):
        #print(behavior)
        #print("Agent{} A_mat_update(). old A_mat: {}".format(self.id, self.A_mat))
        reward = all_behaviors[behavior].payoff
        #print("payoff {}".format(reward))
        new_A_kit = (1 - self.g_i) * self.knowledge[behavior]["a_mat"] + self.g_i * reward
        #print("new attraction score {}".format(new_A_kit))
        self.knowledge[behavior]["a_mat"] = new_A_kit
        #print("Agent{} A_mat_update(). new A_mat: {}".format(self.id, self.A_mat))

    def I_mat_update(self):
        #print("Agent{} I_mat_update(). old I_mat: {}".format(self.id, self.I_mat))
        A_mat = np.array([behavior["a_mat"] for behavior in self.knowledge.values()])
        exp_A_mat = np.exp(np.multiply(A_mat, self.inverse_temp))
        for count, behavior in enumerate(self.knowledge.values()):
            behavior["i_mat"] = np.clip(exp_A_mat[count] / np.sum(exp_A_mat),0,1)
        #print("Agent{} I_mat_update(). new I_mat: {}".format(self.id, self.I_mat))

    def S_mat_update(self):
        social_memory = [item for subl in self.long_social_memory for item in subl]
        #print("Agent{} S_mat_update(). old S_mat: {}".format(self.id, social_memory))
        if len(social_memory)>0:
            #print("Agent{} S_mat_update(). has social memory {}.".format(self.id,social_memory))
            denom=0
            for behavior in self.knowledge.keys():
                #print(behavior)
                denom += social_memory.count(behavior)**self.conformity
                #print(denom)
            for behavior in self.knowledge.keys():
                new_S_kit = ((social_memory.count(behavior)**self.conformity) / denom)
                self.knowledge[behavior]["s_mat"] = np.clip(new_S_kit,0,1)
        else:
            #print("no social memories, potential problem?")
            for count,behavior in enumerate(self.knowledge.values()):
                new_S_kit = 0
                behavior["s_mat"] = new_S_kit

    def P_mat_update(self):
        #print("Agent{} P_mat_update(). Old P_mat{}".format(self.id, self.P_mat))
        social_memory = np.array([x["s_mat"] for x in self.knowledge.values()])
        #print("social memory {}".format(social_memory))
        if social_memory.any():
            for behavior in self.knowledge.values():
                behavior["p_mat"] = (1 - self.s_i)*behavior["i_mat"] + self.s_i*behavior["s_mat"]

        else:
            for behavior in self.knowledge.values():
                behavior["p_mat"] = behavior["i_mat"]

    def produceBehavior(self):
        #print("Agent{} produceBehavior(). P_mat is {}".format(self.id, self.P_mat))
        behavior_choices = [behavior for behavior in self.knowledge.keys()]
        #print(behavior_choices)
        p_mat = np.array([x["p_mat"] for x in self.knowledge.values()])

        #prevent negative values from floating point error
        p_mat = np.clip(p_mat, 0, 1)

        #choose production with probability from p_mat
        try:
            production = np.random.choice(behavior_choices, p=p_mat)
        except:
            print(p_mat)
            sys.exit()

        #adds production string to memory
        self.ind_memory.append(production)

        #update payoff, attraction and probability for next production
        self.A_mat_update(production)
        self.I_mat_update()
        self.P_mat_update()

        return production

    def acquire_behavior(self, G):
        neighbors = [node for node in G.neighbors(self.id)]

        for behavior.name in all_behaviors:
            if behavior.name not in self.knowledge.keys():
                acquisition_prob = lambda_t(behavior.name,neighbors,G)
                assert 0 <= acquisition_prob <=1, "resulting acquision prob from NBDA must be between 0 and 1"
                if random.random() < acquisition_prob:
                    #print("Agent {} learned behavior {}".format(self.id, behavior.name))
                    self.knowledge[behavior.name] = {"a_mat": 0,"i_mat": 0,"s_mat":0,"p_mat":0}
                    #we can conservatively assume that the behavior has been seen at least once
                    self.long_social_memory.append([behavior.name])
                    self.I_mat_update()
                    self.S_mat_update()
                    self.P_mat_update()
                    self.naive=False


def generate_network(graph_type,params_list):
    if graph_type == "complete":
        G = nx.complete_graph(N)
    if graph_type == "random":
        G = nx.fast_gnp_random_graph(N, .25)
    #give instance of agent object as 'data' attribute in each node
    for x in list(G.nodes()):
        G.nodes[x]['data'] = agent(x,params_list)
        if random.random() <= initial_knowledgable_prop:
            G.nodes[x]['data'].knowledge["a"] = {"a_mat": 10,"i_mat": 0,"s_mat":0,"p_mat":0}
            G.nodes[x]['data'].long_social_memory.append(["a"])
            G.nodes[x]['data'].I_mat_update()
            G.nodes[x]['data'].P_mat_update()
            G.nodes[x]['data'].naive=False
            #print("Agent {} begins as knowledgable".format(x))
    return G


##the following functions are used in the NBDA calculations of the sim###

def NBDA(G):
    for agent in list(G.nodes()):
        G.nodes[agent]["data"].acquire_behavior(G)

#loops through neighbors and produces either a binary or proportion of all behaviors produced by each neighbor which are a given behavior. This value is then summed for all neighbors of the focal individual.
def z_jt(behavior, neighbors, G):
    z_jt = 0

    for neighbor in neighbors:
        if behavior in G.nodes[neighbor]["data"].knowledge.keys():
            #Values range integers between [0,length(neighbors)]
            if z_jt_type=="binary":
                z_jt += 1
            #Values range continuously from [0,length(neighbors)] with each neighbor weighted in proportion to solution type
            elif z_jt_type=="proportional":
            	#how many times a neighbor produces the behavior, over the total # of behaviors produced within the memory window
                z_jt += G.nodes[neighbor]["data"].ind_memory.count(behavior)/memory_window

    return z_jt

#this is the function T((a_i,z(t))) (Firth 2020)
def transmission_function(behavior,neighbors,G):

    z_jt_param = z_jt(behavior, neighbors, G)

    #Hoppitt_Laland 2013 General form, s is unbounded
    #lambda(t) = baseline rate function*(rate of acquisition * sum(a_ij*z_j(t)) + A)
    if NBDA_type == 0 or NBDA_type == 1:
        transmission_func = s_param * z_jt_param

    #Proportional rule model Firth 2020
    elif NBDA_type == 2:
        assert z_jt_type == "binary", "z_jt must be binary to appropriately calculate NBDA type 2 (proportional rule)"
        transmission_func = s_param * (z_jt_param / len(neighbors))

    #Freq dependent rule model Firth 2020
    elif NBDA_type == 3:
        assert z_jt_type=="binary", "z_jt must be binary to appropriately calculate NBDA type 3 (conformity rule)"
        transmission_func = s_param * (z_jt_param**NBDA_conformity / (z_jt_param ** NBDA_conformity + (z_jt_param - len(neighbors)) ** NBDA_conformity))

    return transmission_func


#returns the second half of the NBDA equation relating to individual learning. Simplest case is a value of 1 or 0, but could take ind level variables
def A_param():

    if asocial_learning:
        if NBDA_type=="1":
            assert 0 <= s_param <=1, "s parameter must take a value between 0 and 1 for this type of NBDA."
            asocial_learning_func=1-s_param
        else:
            asocial_learning_func=1
    else:
        asocial_learning_func=0

    return asocial_learning_func

#calculates the acquisition rate of a behavior given the productions of the neighbors
#all neighbors are considered as equivalently associated, but this could be changed by adding a term
def lambda_t(behavior, neighbors, G):

    transmission_func = transmission_function(behavior, neighbors, G)

    asocial_learning_func = A_param()

    #lambda(t) = baseline rate function*(transmission_func + ind_learning_parameters)
    acq_rate = all_behaviors[behavior].base_haz * (transmission_func + asocial_learning_func)

    #convert from rate to probability within the last timestep that an agent has acquired the behavior
    acq_prob = 1-math.exp( -acq_rate )

    return acq_prob

def production_event(G, producer):
    #print("\n***new production_event***\nproducer is agent {:d} ".format(producer))
    beh_production = G.nodes[producer]["data"].produceBehavior()
    #print("\n***new production_event***\nproducer is agent {:d}, produces {}".format(producer, beh))

    neighbors = [node for node in G.neighbors(producer)]
    for neighbor in neighbors:
        G.nodes[neighbor]["data"].observe(beh_production)
    return beh_production

def update_exposure(G):
    for agent in list(G.nodes()):
        G.nodes[agent]["data"].exposure += 1

def turnover_event(G,timestep,params_list):
    #gets list of time steps each agent has been in the population
    exposure_list = np.array([[i,G.nodes[i]["data"].exposure] for i in range(N)])
    #print(exposure_list[:,1])
    probs = np.divide(exposure_list[:,1], np.sum(exposure_list[:,1]))
    #print(probs)

    #chooses num_turnover agents for replacement
    turnover_list = np.random.choice(exposure_list[:,0],replace = False, size = num_turnover,p=probs)
    for n in turnover_list:
        G.nodes[n]["data"] = agent(n,params_list)
    #print("turnover_event(). turnover event: {} replaced".format(turnover_list))

def update_s_mat(G):
    for agent in list(G.nodes()):
        G.nodes[agent]["data"].S_mat_update()
        G.nodes[agent]["data"].P_mat_update()

def create_csv(file_path):
    #create variable names for behaviors to be included in simulation
    behavior_list = ["behavior_{}_{}".format(behavior.name,behavior.payoff) for behavior in all_behaviors.values()]
    labels=["A_mat","I_mat","S_mat","P_mat"]
    agent_matrix_list = ["behavior_{}_{}".format(behavior.name,label) for behavior in all_behaviors.values() for label in labels]

    #writes header for main data
    with open(file_path,"w") as fout:
        fout.write("sim, turnover, graph_type, pop_size, memory_window, NBDA_type, NBDA_basehazard, NBDA_s_param, NBDA_z_jt_type, NBDA_conformity, EWA_soc_info_weight, EWA_recent_payoff_weight, EWA_conformity, EWA_inverse_temp, timestep, num_producers, {}, {}\n".format(",".join(behavior_list),",".join(agent_matrix_list)))

def write_csv(file_path,sim_num,params_list,timestep,num_producers,beh_freqs,amv_string):
    behavior_counts = [str(beh) for beh in beh_freqs.values()]
    #print(behavior_counts)
    count_string = ",".join(behavior_counts)

    #writes header for main data
    with open(file_path,"a+") as fout:
        fout.write("{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}\n".format(sim_num,
        turnover,
        graph_type,
        N,
        params_list[6],
        NBDA_type,
        base_hazard,
        params_list[4],
        params_list[5],
        NBDA_conformity,
        params_list[0],
        params_list[1],
        params_list[2],
        params_list[3],
        timestep,
        num_producers,
        count_string,
        amv_string))

def agent_values(agent, behavior,G):
    #if the agent knows a behavior, return associated matrix values, otherwise return zeroes
    if behavior in G.nodes[agent]["data"].knowledge.keys():
        return [G.nodes[agent]["data"].knowledge[behavior]["a_mat"],G.nodes[agent]["data"].knowledge[behavior]["i_mat"],G.nodes[agent]["data"].knowledge[behavior]["s_mat"],G.nodes[agent]["data"].knowledge[behavior]["p_mat"]]
    else:
        return [0,0,0,0]

def peek_inside(G):
    to_write_list = []
    for behavior in all_behaviors.values():
        #values for A_mat, I_mat, S_mat and P_mat
        total_values = [0,0,0,0]
        for agent in range(N):
            agt_values = agent_values(agent,behavior.name,G)
            #print(agt_values)
            total_values = np.add(total_values,agt_values)
        to_write_list += list(total_values)

    agent_matrix_values_string = [str(v) for v in to_write_list]
    amv_string = ",".join(agent_matrix_values_string)
    return amv_string


def simulation(params_list):
    global s_param
    s_param = params_list[4]
    global z_jt_type
    z_jt_type = params_list[5]
    global memory_window
    memory_window = params_list[6]

    if NBDA_type==3:
        global NBDA_conformity
        NBDA_conformity = params_list[2]
    else:
        NBDA_conformity = 1

    #get unique simulation number for this round, increment global master_sim_num
    with lock:
        sim_num = master_sim_num.value
        master_sim_num.value += 1

    #create csv file with headers
    file_path = "../raw_data/output_turnover_{}_gtype_{}_sim_{}.csv".format(turnover,graph_type, sim_num)

    #if not isfile(file_path):
    create_csv(file_path)

    print("simulation{} EWA(s_i{} g_i{} conformity{} inverse_temp{}) NBDA(s_param{}, z_jt_type{}, soc_memory_window{})".format(sim_num,params_list[0],params_list[1],params_list[2],params_list[3],params_list[4],params_list[5],params_list[6]))

    #create network populated with agents
    G = generate_network(graph_type,params_list)

    for timestep in range(t_steps):
        #print("**** the timestep {} starts".format(timestep))

        #create counter object to keep track of frequencies of behaviors for this timestep
        beh_freqs = Counter()
        for behavior in all_behaviors.values():
            beh_freqs[behavior.name] += 0

        #create list of knowledgable agents
        knowledgable = [agent for agent in range(N) if G.nodes[agent]["data"].naive == False ]

        #let knowledgable individuals produce given number of behaviors
        for individual in knowledgable:
            for interactions in range(beh_per_TS):
                beh = production_event(G, individual)
                #print(beh)
                beh_freqs[beh] += 1

        #resolves internal counts to the memory window
        for agent in range(N):
            G.nodes[agent]["data"].consolidate_social_memory()
            G.nodes[agent]["data"].prune_ind_memory(timestep)
            G.nodes[agent]["data"].prune_social_memory(timestep)

        #update social cue matrix once per timestep
        update_s_mat(G)

        #print(beh_freqs)
        producers = [agent for agent in range(N) if "b" in G.nodes[agent]["data"].knowledge.keys()]
        num_producers = len(producers)

        #function that returns sums of Smat Imat Amat Pmat values, just to keep track of how each variant changes in the minds of the agents over time.
        agent_matrix_values = peek_inside(G)

        #write data
        write_csv(file_path,sim_num,params_list,timestep,num_producers,beh_freqs,agent_matrix_values)

        #run NBDA diffusion portion
        NBDA(G)

        #update how long the agents have been in the population, important for Turnover
        update_exposure(G)
        if turnover:
            if (timestep+1)%turnover_interval == 0:
                turnover_event(G,timestep,params_list)



if __name__=="__main__":

    #simulation parameters
    replicates=100
    t_steps = 100 #timesteps to run simulation

    ### Population parameters ###
    N = 20 #population size
    initial_knowledgable_prop = 1 #initial proportion of knowledgable individuals in the population
    graph_type = "random" #sets network structure
    custom_adj_list_filename = ""
    turnover_values = [False] #toggle turnover events
    turnover_interval = 10 #interval between turnover events
    num_turnover = 2 #number of individuals turned over

    ### NBDA parameters ###
    NBDA_type = 0 # 0: Unbounded general form; 1: bounded, linked S and (1-S); NEED TO ADD: ILV form allowing for effects for social AND individual learning, requires vectors Gamma_i, Beta_i
    s_param_values = [1,5,10] #s parameter from NBDA indicating strength of social learning per unit of connection
    z_jt_type_values = ["proportional"] #"binary" or "proportional", proportional assigns z_j(t) a number between [0,1] depending on how frequently the produced behavior in previous timestep
    asocial_learning = True #True or false, depending on if asocial learning occurs
    base_hazard_values = [.01] #the base hazard is the underlying rate in every timestep that the behavior could be acquired
    keenness = 1 #this is the ILV value passed to NBDA function, indicates aptitude for individual learning
    beh_per_TS = 1 #behaviors individuals perform per timestep

    #EWA Parameters
    s_i_values = np.around(np.arange(0.1, 1.1, 0.2), 1) #social information bias EWA values
    g_i_values = np.around(np.arange(0.1, 1.1, 0.2), 1) #recent payoff bias EWA values
    conformity_values = [1,2] #conformity exponent EWA values
    inverse_temp_values = [0.5,1,2] #sensitivity to differences in payoffs
    memory_window_values = [1,25,50] #how far back the agent can remember (in time steps) the behaviors that other agents produce

    #loop through lists of parameter values
    for turnover in turnover_values:
        for base_hazard in base_hazard_values:
            #create dictionary of all behaviors
            all_behaviors = {"a": behavior(name = "a", payoff = .5, base_haz = base_hazard),
                             "b": behavior("b", 1, base_hazard)}

            #create manager to handle common memory for multicore processing
            with Manager() as manager:

                master_sim_num = manager.Value('i', 0)
                # creating a Lock object to handle multiple processes accessing master sim #
                lock = manager.Lock()

                #create massive list of other parameter values
                params_list = [(s_i,g_i,conformity,inverse_temp,s_param,z_jt_type,memory_window) for s_i in s_i_values for g_i in g_i_values for conformity in conformity_values for inverse_temp in inverse_temp_values for s_param in s_param_values for z_jt_type in z_jt_type_values for memory_window in memory_window_values]
                print("{} simulations to go".format(len(turnover_values)*len(base_hazard_values)*replicates*len(params_list)))

                #repeat sims for number of desired replicates
                for x in range(replicates):
                    with Pool(cpu_count()-1) as pool:
                        pool.map(simulation,params_list)

    #where raw data will be put
    directory_path="../raw_data"

    #where concatenated dataframes will be put after all sims finish running
    new_directory_path="../concat_data"

    #concating dataframes
    df_list = [pd.read_csv(join(directory_path,f)) for f in listdir(directory_path) if ".csv" in f]
    df_concat = pd.concat(df_list)

    #set final filename here
    df_concat.to_csv(join(new_directory_path,"eff_against_ineff_payoff_diffusion.csv"), index = False)

    #remove raw data files
    for f in listdir(directory_path):
        if ".csv" in f:
            remove(join(directory_path,f))
