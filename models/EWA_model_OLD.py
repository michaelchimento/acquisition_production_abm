#!/usr/bin/env python
# coding: utf-8
# run program with 2 argvs: python3 EWA_model learning_function[positive,negative,neutral] turnover[True,False]

import numpy as np
import networkx as nx
import random
import warnings
from sys import argv

class agent:
    def __init__(self):
        global masterID
        #this variable keeps agents identifiable across simulations
        self.id = masterID
        masterID += 1
        '''
        social memory records frequencies of observations, which I've equated to the frequency
        of choice k among social cues at time t (or the  n_kt term in the S_kit equation). Does
        this need to have a memory constraint attached to it, like a sliding window?
        '''
        self.social_memory = np.zeros(shape=solutions, dtype=(float, 1))

        '''
        Attraction_matrix is composed of the individual attraction scores for choice k
        before being passed to the soft-max choice rule implemented in I_mat.
        current A_kit = (1-g_i)* previous(A_kit) + g_i* pi_k
        g_i is the importance of newly experienced payoff from choice k, pi_k
        '''
        self.A_mat = np.zeros(solutions, dtype=(float, 1))

        '''
        Individual_attraction_matrix is the result of raising e to the values from A_matrix
        and dividing by the sum of all other exponentiated entries in A_matrix.
        '''
        self.I_mat = np.zeros(shape=solutions, dtype=(float, 1))

        '''
        Social_matrix is the frequency of choice k among all social cues at time t, raised to
        the conformist exponent lambda (1= is neutral, >1 is conformist, <1 is anti-conformist)
        These probabilities will influence choice in the probability_matrix

        S_kit = n_kt^lambda / sum(n_kt^lambda)
        '''
        self.S_mat = np.zeros(shape=solutions, dtype=(float, 1))

        '''
        Probability_matrix contains the probability of observing choice k at time t by individual i
        by taking into account individual experience and social information. It's initially set to
        equal probabilities for all choices. Is this right?

        p_kit= (1-s_i)I_kit + s_i*S_kit
        s_i is social influence parameter, where higher values discount individual information and
        prioritize social information.
        '''
        self.P_mat = np.zeros(shape=solutions, dtype=(float, 1))
        self.s_i = s_i_param
        self.g_i = g_i_param
        self.conformity = conformity
        self.inverse_temp = inverse_temp_param

        self.learn_prob = learning_hazard[0]
        self.days_exposure = 0
        self.solve_count = [0,0] #keep track of personal solves, to be used when calculating socially observed frequencies


        #print("s_i {}; g_i {}, lambda {}".format(self.s_i, self.g_i, self.conformity))
        self.naive = True
        self.I_mat_update()
        self.P_mat_update()

    def reset_solve_count(self):
        self.solve_count = [0,0]

    def A_kit_update(self, solution):
        #print("Agent{} A_kit_update(). old A_mat: {}".format(self.id, self.A_mat))
        payoff = payoffs[solution]
        #print("payoff {}".format(payoff))
        new_A_kit = (1 - self.g_i) * self.A_mat[solution] + self.g_i * payoff
        #print("new attraction score {}".format(new_A_kit))
        self.A_mat[solution] = new_A_kit
        #print("Agent{} A_kit_update(). new A_mat: {}".format(self.id, self.A_mat))


    def I_mat_update(self):
        #print("Agent{} I_mat_update(). old I_mat: {}".format(self.id, self.I_mat))
        exp_A_mat = np.exp(np.multiply(self.A_mat, self.inverse_temp))
        for solution in range(solutions):
            self.I_mat[solution] = exp_A_mat[solution] / np.sum(exp_A_mat)
        #print("Agent{} I_mat_update(). new I_mat: {}".format(self.id, self.I_mat))

    def S_mat_update(self):
        #print("Agent{} S_mat_update(). old S_mat: {}".format(self.id, self.S_mat))
        if self.social_memory.any():
            #print("Agent{} S_mat_update(). has social memory {}.".format(self.id,self.social_memory))
            for solution in range(solutions):
                new_S_kit = (self.social_memory[solution]**self.conformity
                            / np.sum(self.social_memory**self.conformity))
                self.S_mat[solution] = new_S_kit
        else:
            #print("Agent{} S_mat_update(). has no social memories.".format(self.id))
            for solution in range(solutions):
                new_S_kit = 0
                self.S_mat[solution] = new_S_kit
        #print("Agent{} S_mat_update(). new S_mat: {}".format(self.id, self.S_mat))


    def P_mat_update(self):
        #print("Agent{} P_mat_update(). Old P_mat{}".format(self.id, self.P_mat))
        if self.social_memory.any():
            for solution in range(solutions):
                self.P_mat[solution] = (1 - self.s_i)*self.I_mat[solution] + self.s_i*self.S_mat[solution]

        else:
            self.P_mat[0:solutions] = self.I_mat

        #print("Agent{} P_mat_update(). New P_mat{}".format(self.id, self.P_mat))

    def produceBehavior(self):
        #print("Agent{} produceBehavior(). P_mat is {}".format(self.id, self.P_mat))
        production = np.random.choice(np.arange(0, solutions), p=self.P_mat)
        #print("Agent{} produced {}".format(self.id, production))

        self.solve_count[production] += 1
        self.A_kit_update(production)
        self.I_mat_update()
        self.P_mat_update()

        return production

def generateNetwork(graph_type):
    if graph_type == "complete":
        G = nx.complete_graph(pop_size)
    elif graph_type == "cycle":
        G = nx.cycle_graph(pop_size)
    elif graph_type == "barabasi":
        G = nx.barabasi_albert_graph(pop_size, edge_parameter, seed=np.random.randint(1,1000))

    for i in range(pop_size):
        G.add_node(i, data=agent())

    if init_tutor == True:
        G.nodes[0]["data"].naive=False
        #print("***start training***")
        G.nodes[0]["data"].social_memory[0] = 10
        G.nodes[0]["data"].S_mat_update()
        for i in range(100):
            G.nodes[0]["data"].A_kit_update(0)
            G.nodes[0]["data"].I_mat_update()
            G.nodes[0]["data"].P_mat_update()
        #print("***end training***")
    return G

def update_learn_prob(G):
    for agent in range(pop_size):
        G.nodes[agent]["data"].days_exposure += 1
        if G.nodes[agent]["data"].naive == True:
            G.nodes[agent]["data"].learn_prob =  learning_hazard[G.nodes[agent]["data"].days_exposure]

def choose_solver(G, knowledgable):
    choice = np.random.choice(knowledgable)
    return choice

def game(G, solver):
    #print("\n***new game***\nsolver is agent {:d} ".format(solver))
    solution = G.nodes[solver]["data"].produceBehavior()
    return solution

def state_switch(G, knowledgable):
    naive_agents = [agent for agent in range(pop_size) if G.nodes[agent]["data"].naive == True ]
    for agent in naive_agents:
        chance = random.random()
        if chance <= G.nodes[agent]["data"].learn_prob:
            #print("new agent has learned")
            G.nodes[agent]["data"].naive = False
            knowledgable.append(agent)

    return knowledgable

def obs_learning(G, frequencies):
    for agent in range(pop_size):
        #print("obs_learning() agent {}".format(G.nodes[agent]["data"].id) )
        for variant in range(solutions):
            #subtraction here excludes their own solves from their social memory
            G.nodes[agent]["data"].social_memory[variant] = frequencies[variant] - G.nodes[agent]["data"].solve_count[variant]
        G.nodes[agent]["data"].S_mat_update()
        G.nodes[agent]["data"].P_mat_update()

def turnover_event(G,timestep):

    #gets list of days each agent has been in the population
    exposure_list = np.array([[i,G.nodes[i]["data"].days_exposure] for i in range(pop_size)])
    #print(exposure_list[:,1])
    probs = np.divide(exposure_list[:,1], np.sum(exposure_list[:,1]))
    #print(probs)
    #chooses num_turnover agents for replacement
    turnover_list = np.random.choice(exposure_list[:,0],replace = False, size = num_turnover,p=probs)
    for n in turnover_list:
        G.nodes[n]["data"] = agent()
    #print("turnover_event(). turnover event: {} replaced".format(turnover_list))

def calculate_probabilities(G, naives):

    if naives:
        uptake_vector = [G.nodes[agent]["data"].learn_prob for agent in naives]
        prob_learn = np.prod(uptake_vector)

    elif not naives:
        prob_learn=0

    return prob_learn

def extract_data(G,solution_list,knowledgable):
    frequencies = [solution_list.count(n) for n in range(solutions)]
    naives = [agent for agent in range(pop_size) if G.nodes[agent]["data"].naive==True]

    #prob_learn = calculate_probabilities(G,naives)
    prob_learn=0

    count_inefficient = frequencies[0]
    count_efficient = frequencies[1]

    num_solvers = len(knowledgable)

    return count_inefficient, count_efficient, num_solvers, prob_learn

def create_csv():
    #writes header for main data
    fout=open("data_"+str(condition)+".csv","w")
    fout.write("sim,condition,pop_size,turnover,turnover_prop,turnover_interval,g_i,s_i,inverse_temp,conformity,learning_function,solves_per_day,timestep,count_inefficient,count_efficient,num_solvers,prob_learn")
    fout.write("\n")
    fout.close()

def write_csv(sim_num, timestep, condition,
              pop_size, g_i, s_i,
              count_inefficient, count_efficient, num_solvers,
               prob_learn):

    fout = open("data_"+str(condition)+".csv","a")
    fout.write(str(sim_num) + ","+
               str(condition) + "," +
               str(pop_size)+","+
               str(turnover)+","+
               str(turnover_prop)+","+
               str(turnover_interval)+","+
               str(g_i_param)+ "," +
               str(s_i_param)+","+
               str(inverse_temp)+","+
               str(conformity)+","+
               str(learning_function)+","+
               str(solves_per_day)+","+
               str(timestep) + "," +
               str(count_inefficient) +"," +
               str(count_efficient)+ ","+
               str(num_solvers)+","+
               str(prob_learn))
    fout.write("\n")
    fout.close()

def simulation(num_replicates):
    global master_sim_no
    global payoffs
    for sim_num in range(num_replicates):
        print("simulation{}".format(sim_num))
        G = generateNetwork(graph_type)
        for timestep in range(t_steps):
            #print("**** the timestep {} starts".format(timestep))
            if timestep < 10:
                payoffs=[10,0]
            else:
                payoffs=[10,20]

            solution_list = []
            solver_list = []
            knowledgable = [agent for agent in range(pop_size) if G.nodes[agent]["data"].naive == False ]
            #print(knowledgable)
            for interactions in range(solves_per_day*len(knowledgable)):
                solver = choose_solver(G, knowledgable)
                solution = game(G, solver)
                solution_list.append(solution)
                solver_list.append(solver)

            #update social cue matrix
            frequencies = [solution_list.count(n) for n in range(solutions)]
            #tallies socially observed solutions
            obs_learning(G, frequencies)
            #resets internal solve counts to 0 for next timestep
            for agent in range(pop_size):
                G.nodes[agent]["data"].reset_solve_count()
            count_inefficient, count_efficient, num_solvers, prob_learn = extract_data(G,solution_list,knowledgable)
            #if not all the agents know how to solve, check to see if a new agent can learn
            if len(knowledgable) < pop_size:
                knowledgable = state_switch(G, knowledgable)
            update_learn_prob(G)
            write_csv(master_sim_no,timestep,condition,pop_size,g_i_param,s_i_param,count_inefficient,count_efficient,num_solvers,prob_learn)
            if turnover==True:
                if (timestep+1)%turnover_interval == 0:
                    turnover_event(G,timestep)

        master_sim_no += 1

masterID = 0
solutions = 2
payoffs = [10, 20]
t_steps = 100 #timesteps to run simulation
graph_type = "complete" #sets network structure
solves_per_day = 10
conformity = 5 #conformity exponent
init_tutor = True

learning_function = str(argv[1]) if len(argv) > 1 else "negative"

#learning function is linear and goes to 0 by end of simulation
def learn_func(x):
    y = .1-(x/1000)
    return y

learning_hazard = np.around([learn_func(x) for x in range(0,t_steps+1)],decimals=3)
if learning_function=="positive":
    learning_hazard = learning_hazard[::-1]
elif learning_function=="neutral":
    learning_hazard = [.05]*t_steps
print(learning_hazard)

parameter_values = np.around(np.arange(0.1, 1, 0.2), 1)
inverse_temp_values = np.around(np.arange(1,3,.5), 1)
num_replicates = 10
master_sim_no = 0
turnover = eval(argv[2])
init_tutor = True
pop_sizes = [10,50,100]

print(turnover)
if turnover:
    turnover_intervals = [5,10,20,40]
    turnover_props = [.1,.2,.3,.5]
else:
    turnover_intervals = ["NA"]
    turnover_props = [0]

for pop_size in pop_sizes:
    condition="N{}_turnover{}_learning{}".format(pop_size,turnover,learning_function)
    create_csv()
    for turnover_interval in turnover_intervals:
        for turnover_prop in turnover_props:
            num_turnover = int(pop_size*turnover_prop)
            for s_i_param in parameter_values:
                for g_i_param in parameter_values:
                    for inverse_temp_param in inverse_temp_values:
                        inverse_temp = inverse_temp_param
                        print("pop_size{} turnover_int{} turnover_prop{} s_i{} g_i{} lambda{}".format(pop_size,turnover_interval,turnover_prop,s_i_param,g_i_param,inverse_temp))
                        simulation(num_replicates)
