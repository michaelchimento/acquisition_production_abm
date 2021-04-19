# A walkthrough of the Full Stack Social Learning model

## Table of Contents

* [Introduction](#introduction)
* [Behaviors](#behaviors)
* [Agents](#agents)
    * [Instance variables: knowledge state, memory and parameter values](#agents_1)
    * [Class methods: observation and memory](#agents_2)
    * [Class methods: EWA functions](#agents_3)
* [NBDA](#NBDA)
* [Networks](#networks)
* [Simulation](#simulation)
    * [Helper functions](#helper-agent)
    * [Helper functions: turnover](#helper-turnover)
    * [Helper functions: extracting data](#helper-extract)
* [Running the simulation](#running)
    * [Simulation parameters](#sim_params)
    * [Population parameters](#pop_params)
    * [EWA parameters](#EWA_params)
    * [NBDA parameters](#NBDA_params)
    * [Execution](#run_sim)
    * [Data management](#data_mgmt)





## Introduction <a class="anchor" id="introduction"></a>

This companion document is mean to directly show how the FSSL model is implemented in Python3.

Let's load the required libraries. The networks of agents are handled by [networkx](https://networkx.org/), a very powerful library that has many options for graph creation. Counter is used to keep track of behavioral frequencies. 


```python
from collections import Counter
import math
import sys
import networkx as nx
import random
import numpy as np
from os.path import isfile,join
import pandas as pd
from os import listdir, remove
import jdc #cleanly split class definitions
```

## Behaviors <a class="anchor" id="behaviors"></a>

We can take advantage of Python's classes and object oriented programming to increase the flexibility of the model, and make life easier when the model becomes complicated. Let's define a class called ```behavior```. Each instance of ```behavior``` will represent a possible behavior that agents will be able to learn during the simulation. Any python class can be given its own variables and methods using ```self.var_name``` convention. Here, we define a initialization method that assigns each instance of a behavior with a name, an associated payoff, and a base hazard rate. The payoff will be used later during the EWA portion of the model, and the base hazard is used in the NBDA portion of the model. The base hazard represents the likelihood of acquiring the behavior at each timestep which is independent of any social or asocial influence. A higher base hazard value essentially means the behavior will be easier to acquire.


```python
class behavior:
    def __init__(self,name,payoff,base_haz):
        self.name = name
        self.payoff = payoff
        self.base_haz = base_haz
```

## Agents <a class="anchor" id="agents"></a>

Next, let's define the ```agent``` class. During a simulation, each node in our network will be occupied by an instance of this class.

### Instance variables: knowledge state, memory and parameter values <a class="anchor" id="agents_1"></a>
We will initialize agents with a few different instance variables, including:

* ```self.id``` a unique ID, 
* ```self.knowledge``` a dictionary that will hold the behaviors that they know, keys are the behavior names, and values are the associated information with each behavior, including its attraction score, individual and social weights for behavior, the final probability of producing the behavior.
* ```self.ind_memory``` a list that will hold a history of behaviors that they've produced themselves within the memory window
* ```self.temp_social_memory``` a list that is a short-term social memory, which holds all the behaviors that an agent has observed its neighbors produce in the current time step
* ```self.long_social_memory``` a list that is a long-term social memory, which holds all the behaviors that an agent has observed its neighbors produce within the memory window.
* ```self.exposure``` represents how many timesteps the agent has been in the simulation, or exposed to the simulation environment. This will be later used for informing a population turnover function.
* ```self.naive``` a boolean value that represents whether an agent knows any behaviors at all

Each agent is also initialized with some parameter values that will affect it's behavioral productions. These parameter values can vary from agent to agent, and can even be redefined using a function, all depending on the requirements of the simulation. This allows for flexible definitions of population heterogeneity. 
* ```self.s_i``` the agent's social information bias parameter (used in EWA)
* ```self.g_i``` the agent's recent information bias parameter (used in EWA)
* ```self.conformity``` the agent's conformity exponent (used in EWA)
* ```self.inverse_temp``` the agent's sensitivity to differences in attraction scores (used in EWA)


```python
class agent:
    def __init__(self,ID):
        self.id = ID
        self.knowledge = {} #name, a_mat, i_mat, s_mat, p_mat
        self.ind_memory = []
        self.temp_social_memory = []
        self.long_social_memory = []
        self.exposure = 0
        self.naive = True        
        #EWA parameters
        self.s_i = EWA_s_i
        self.g_i = EWA_g_i
        self.conformity = EWA_conformity
        self.inverse_temp = EWA_inverse_temp 
```

### Class methods: observation and memory <a class="anchor" id="agents_2"></a>

The following class methods handle aspects of the agents memory of their own productions, and their social productions.

* ```observe(behavior)``` is called after each behavior is produced. An agent may only observe it's neighbors, and the observed behavior is added to its short term social memory.
* ```consolidate_social_memory()``` is called once per timestep, after all agents have produced their behaviors. It adds the memory from the current timestep to the long term social memory.
* ```prune_ind_memory(timestep)``` is called once per timestep and, using a first in, first out (FIFO) rule, removes any behaviors from an agent's individual memory that were produced outside of the memory window.
* ```prune_social_memory(timestep)``` is called once per timestep and, using a FIFO rule, removes any behaviors from an agent's social memory that were produced outside of the memory window.



```python
%%add_to agent

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

```

### Class methods: EWA functions <a class="anchor" id="agents_3"></a>

Each agent contains a method that calculate an attraction score from received behavioral payoffs. This attraction score is turned into an individual weight using a softmax function. The agents also keep track of what behaviors their neighbors produce within the memory window. These values are turned into a social weight. Finally, the individual and social weights are combined into a final probability that the agents would produce a given behavior.

```A_mat_update(behavior)``` updates an agent's attraction score matrix using the following equation: $A_{kt} = g_i\pi_k + (1-g_i)A_{k,t-1}$


```python
%%add_to agent
def A_mat_update(self, behavior):
    reward = all_behaviors[behavior].payoff
    new_A_kit = (1 - self.g_i) * self.knowledge[behavior]["a_mat"] + self.g_i * reward
    self.knowledge[behavior]["a_mat"] = new_A_kit
```

```I_mat_update(behavior)``` updates an agent's individual weight matrix using the following equation: $I_{kt} = \frac{exp(\tau A_{kt})}{\sum_m{exp(\tau A_{mt})}}$. The use of the softmax equation here guarantees that the sum of all values for known behaviors is 1.


```python
%%add_to agent
def I_mat_update(self):
    A_mat = np.array([behavior["a_mat"] for behavior in self.knowledge.values()])
    exp_A_mat = np.exp(np.multiply(A_mat, self.inverse_temp))
    for count, behavior in enumerate(self.knowledge.values()):
        behavior["i_mat"] = np.clip(exp_A_mat[count] / np.sum(exp_A_mat),0,1)
```

```S_mat_update(behavior)``` updates an agent's social weight matrix using the following equation: $S_{kt} = \frac{n^\lambda_{kt}}{\sum_{m}^K{n^\lambda_{mt}}}$. This equation ensures that the values for each known behavior are bounded between 0 and 1. First the long term memory is flattened. $n$ values are determined using the ```count()``` function on the long term memory list. The denominator is calculated first to save later computation, and then each new social weight matrix value is calculated for known behaviors, and written into the knowledge dictionary under that behavior key's ```["s_mat"]``` value.


```python
%%add_to agent
def S_mat_update(self):
    social_memory = [item for subl in self.long_social_memory for item in subl]
    if len(social_memory)>0:
        denom=0
        for behavior in self.knowledge.keys():
            denom += social_memory.count(behavior)**self.conformity
        for behavior in self.knowledge.keys():
            new_S_kit = ((social_memory.count(behavior)**self.conformity) / denom)
            self.knowledge[behavior]["s_mat"] = np.clip(new_S_kit,0,1)
    else:
        for count,behavior in enumerate(self.knowledge.values()):
            new_S_kit = 0
            behavior["s_mat"] = new_S_kit
```

```P_mat_update(behavior)``` updates an agent's probability matrix using the following equation: $P_{kit} = (1-s_i)I_{kit} + s_iS_{kit}$. If an agent has no experience observing others, the value from their individual weight matrix is the final production probability.


```python
%%add_to agent
def P_mat_update(self):
    social_memory = np.array([x["s_mat"] for x in self.knowledge.values()])
    if social_memory.any():
        for behavior in self.knowledge.values():
            behavior["p_mat"] = (1 - self.s_i)*behavior["i_mat"] + self.s_i*behavior["s_mat"]
            assert 0 <= behavior["p_mat"] <=1, "p_mat value exceeds [0,1] boundaries: {}".format(behavior["p_mat"])
    else:
        for behavior in self.knowledge.values():
            behavior["p_mat"] = behavior["i_mat"]
            assert 0 <= behavior["p_mat"] <=1, "p_mat value exceeds [0,1] boundaries: {}".format(behavior["p_mat"])
```

Finally, we need the agents to do something with the knowledge they have. They can produce behaviors using the following method. This creates a list of possible behaviors from their knowledge dictionary keys. One behavior is chosen from using Numpy's ```random.choice()``` function, with the probability values passed as the probability argument to weight the choice.

That behavior is then added to the agent's individual memory, and is passed to ```A_mat_update()``` to update attraction, then individual weight, and finally probability (in case the simulation allows for more than one behavioral production per timestep). The name of the behavior produced is returned.


```python
%%add_to agent
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
```

The final method definition in the agent class is ```acquire_behavior(G)``` which takes the social network object G as an argument, is run once per timestep, and determines whether agents learn a behavior in a given timestep. First, it creates a list of neighboring nodes of the focal agent. Then, for every behavior that's not known to the focal agent, it calculates the probability of acquisition. This requires the use of the NBDA equation, which takes into account information about the agent's neighbors. This will be covered in the next section. Then, a random number between $[0,1)$ is generated, and if this number is smaller than the acquisition probability, the focal agent acquires the behavior. 


```python
%%add_to agent
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
```

## NBDA <a class="anchor" id="NBDA"></a>

While the production probabilities are handled by methods in the ```agent``` class, the calculation of acquisition probabilities by NBDA equations is handled by the following modular series of functions. The basic form of NBDA is given by $\lambda_i(t) = \lambda_0(t)(T(a_i,z(t))+A)(1-z_i(t))$.

* $\lambda_i(tk)$ - the rate of transmission for individual $i$ at time $t$ for behavior $k$.
* $ \lambda_0(tk)$ - the baseline (hazard) rate of transmission at time $t$ for behavior $k$
* $(T(a_i,z(tk))) = s\sum{a_{ij}z_{jk}(t)}$ - the transmission function, which can be represented by a variety of forms accommodating simple or complex contagion. This is composed of:
 + $s$ - the rate of social acquisition per unit connection, and can take values from $[0,\infty]$
 + $a_i$ - the association matrix for the focal individual representing all connections to neighbors
 + $z(t) = \frac{n_{kjt}}{\sum_m{n_{mjt}}}$ - usually in NBDA, this is a binary variable, representing the knowledge state of the neighbor. However, in the FSSL model, this takes a value between 0 or 1, such that it includes information about the production frequency of the behavior within the memory window.
* $A$ - is the presence of asocial learning, in this version of the model it's binary, but could be further defined by ILVs
* $(1-z_i(t))$ ensures the equation equals zero if the behavior is already known.

We will cover how this is implemented in detail below.

First, we have a convenience function that is called within the simulation, once per timestep, that loops through all agents and calls the ```acquire_behavior()``` method.


In the preceeding code section for the agent method ```acquire_behavior()```, ```acquisition_prob``` requires a call to ```lambda_t()```. This function is at the "top" level of NBDA, and first calculates the value for $T(a_i,z(t)))$, $A$, and with those then $\lambda_i(tk)$. That value is a rate, which is converted into a probability using $P(acquire)=1-exp(-\lambda_i(t))$. This probability is returned for further use in ```acquisition_prob()```.


```python
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
```

The ```transmission_function()``` takes information about the knowledge states of neighbors and multiplies it by the $s$ parameter, which represents the strength of social learning. Here, we have implemented several forms which this function could take, whose definitions are taken from (Firth et al., 2020).

* ```NBDA_type=0``` the "default" NBDA, in which $s$ is unbounded and $z_{jk}(t)$ is passed as its raw form.
* ```NBDA_type=1``` NBDA in which $s$ and $A$ are linked ($A=1-s$) and bounded between $[0,1]$.
* ```NBDA_type=2``` "proportional rule" NBDA, $z_{jk}(t)$ is normalized by the total number of neighbors.
* ```NBDA_type=3``` "frequency dependent rule" NBDA, $z_{jk}(t)$ is exponentiated by a conformity exponent, in the same manner as in the EWA equations. The exponent here takes the value from the EWA parameters, although this does not have to be the case and the two could vary independently. This form of NBDA is untested in the current manuscript.


```python
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
```

The transmission function first makes a call to the ```z_jt``` function to get its value. The form is determined by the simulation variable ```z_jt_type```. The standard NBDA is ```binary```, where a knowledgeable neighbor counts as +1. Setting this in FSSL decouples EWA from NBDA, and is included here for the purpose of comparison of simulations in which the two models are not allowed to inform each other. FSSL is meant to run with type ```proportional```, in which information about how frequently the neighbor produced the behavior is included in their contribution to the value of $z_{jk}(t)$. This is done by dividing the number of behavioral productions of a given behavior by the total number of behaviors produced, such that a neighbors contribution can range from $[0,1]$.


```python
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
```

For future extensibility, the value of $A$ is defined as a function. This function could eventually make use of ILVs to calculate it's value. In this version, the function first checks the value of simulation variable ```asocial_learning```. If this is false, $A=0$. If this is true and the NBDA is of the bounded form ```NBDA_type=1```, $A=1-s$. Otherwise, $A=1$, which is now the accepted "default" in much of the literature.


```python
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
```

## Networks <a class="anchor" id="networks"></a>

Key to these simulations is that agents are situated within a social network. This is generated using the following function, which employs the ```networkx``` library. This is a powerful library with many pre-made network generation functions available. The full list at time of publication is [available here](https://networkx.org/documentation/stable/reference/generators.html). The type of graph is defined using the simulation parameter ```graph_type```. We have included options for complete networks and random Erdős-Rényi graphs, and the import of a custom graph from an adjacency list. However, this function can be easily modified with other options.

Once the graph is created, each node is populated with an instance of the agent class as it's ```data``` key (each node is defined as a dictionary in networkx). Once an agent is created, a dice is rolled to see whether they will be initially knowledgable, governed by the simulation variable ```intial_knowledgable_prop```. The agent is then preprogrammed with knowledge of behavior "a", an intial attraction score, and a memory of socially observing the behavior. How this is done is totally up to the design and purpose of the simulation.


```python
def generate_network(graph_type):
    if graph_type == "complete":
        G = nx.complete_graph(N)
    elif graph_type == "random":
        G = nx.fast_gnp_random_graph(N, .25)
    elif graph_type == "custom_adj_list":
        G = nx.read_adjlist(custom_adj_list_filename)
    for x in list(G.nodes()):
        G.nodes[x]['data'] = agent(x)
        if random.random() <= initial_knowledgable_prop:
            G.nodes[x]['data'].knowledge["a"] = {"a_mat": 10,"i_mat": 0,"s_mat":0,"p_mat":0}
            G.nodes[x]['data'].long_social_memory.append(["a"])
            G.nodes[x]['data'].I_mat_update()
            G.nodes[x]['data'].P_mat_update()
            G.nodes[x]['data'].naive=False
    return G
```

## Simulation <a class="anchor" id="simulation"></a>

We will now cover the structure of the simulation, and review the various helper functions it requires.

1. We assign the simulation a unique number
2. We create a csv to hold the data output
3. We generate a unique network for the simulation and populate it with agents
4. We begin a loop with one pass per timestep. In this loop:
    1. A counter object is created to keep track of the frequencies of each behavior
    2. A list of knowledgable agents is created. 
    3. Each knowledgable agent produces 1 behavior, which is added to the `beh_freqs` counter.
    4. After all knowledgable agents have produced their behavior for the timestep, every agent in the population 
        * consolidates their short term social memory into long term memory
        * prunes any individual memory that lays beyond the memory window
        * prunes any social observation memory that lays beyond the memory window
        * updates their social weight matrix and probability matrix
    6. The number knowledgable of the novel behavior is counted.
    7. All relevant data is written to the csv.
    8. Agents update their knowledge states using the NBDA equations. Here they may acquire a new behavior to produce in the next time step.
    9. Agents update their `exposure` variable, which is a count of how many time steps they have existed in the simulation
    10. If there is turnover and the timestep is one in which turnover occurs, the oldest agents are replaced with new agents.
5. When all timesteps have passed, the simulation ends.



```python
def simulation():
    global master_sim_num
    sim_num = master_sim_num
    master_sim_num += 1
    
    #create csv file with headers
    file_path = join(directory_path,"output_turnover_{}_gtype_{}_sim_{}.csv".format(turnover,graph_type, sim_num))
    create_csv(file_path)

    #create network populated with agents
    G = generate_network(graph_type)
    
    print("simulation{} EWA(s_i{} g_i{} conformity{} inverse_temp{}) NBDA(s_param{}, z_jt_type{}, memory_window{})".format(sim_num,EWA_s_i,EWA_g_i,EWA_conformity,EWA_inverse_temp,s_param,z_jt_type,memory_window))


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
            G.nodes[agent]["data"].S_mat_update()
            G.nodes[agent]["data"].P_mat_update()

        #update social cue matrix once per timestep
        update_s_mat(G)

        know_novel = [agent for agent in range(N) if "b" in G.nodes[agent]["data"].knowledge.keys()]
        num_know_novel = len(know_novel)

        #write data
        write_csv(file_path,sim_num,timestep,num_know_novel,beh_freqs)

        NBDA(G)

        update_exposure(G)
        if turnover:
            if (timestep+1)%turnover_interval == 0:
                turnover_event(G,timestep)
```

### Helper functions <a class="anchor" id="helper-agent"></a>


```python
def production_event(G, producer):
    beh_production = G.nodes[producer]["data"].produceBehavior()

    neighbors = [node for node in G.neighbors(producer)]
    for neighbor in neighbors:
        G.nodes[neighbor]["data"].observe(beh_production)
    return beh_production
```


```python
def NBDA(G):
    for agent in list(G.nodes()):
        G.nodes[agent]["data"].acquire_behavior(G)
```

### Helper functions: turnover <a class="anchor" id="helper-turnover"></a>


```python
def update_exposure(G):
    for agent in list(G.nodes()):
        G.nodes[agent]["data"].exposure += 1
```


```python
def turnover_event(G,timestep):
    exposure_list = np.array([[i,G.nodes[i]["data"].exposure] for i in range(N)])
    #print(exposure_list[:,1])
    probs = np.divide(exposure_list[:,1], np.sum(exposure_list[:,1]))
    #print(probs)

    #chooses num_turnover agents for replacement
    turnover_list = np.random.choice(exposure_list[:,0],replace = False, size = num_turnover,p=probs)
    for n in turnover_list:
        G.nodes[n]["data"] = agent(n)
    #print("turnover_event(). turnover event: {} replaced".format(turnover_list))
```

### Helper functions: extracting data <a class="anchor" id="helper-extract"></a>


```python
def create_csv(file_path):
    #create variable names for behaviors to be included in simulation
    behavior_list = ["behavior_{}_{}".format(behavior.name,behavior.payoff) for behavior in all_behaviors.values()]
    labels=["A_mat","I_mat","S_mat","P_mat"]
    agent_matrix_list = ["behavior_{}_{}".format(behavior.name,label) for behavior in all_behaviors.values() for label in labels]

    #writes header for main data
    with open(file_path,"w") as fout:
        fout.write("sim,turnover,graph_type,pop_size,memory_window,NBDA_type,NBDA_basehazard,NBDA_s_param,NBDA_z_jt_type,NBDA_conformity,EWA_soc_info_weight,EWA_recent_payoff_weight,EWA_conformity,EWA_inverse_temp,timestep,num_know_novel,{}\n".format(",".join(behavior_list)))
```


```python
def write_csv(file_path,sim_num,timestep,num_know_novel,beh_freqs):
    behavior_counts = [str(beh) for beh in beh_freqs.values()]
    #print(behavior_counts)
    count_string = ",".join(behavior_counts)

    #writes header for main data
    with open(file_path,"a+") as fout:
        fout.write("{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}\n".format(sim_num,turnover,graph_type,N,memory_window,NBDA_type,base_hazard,s_param,z_jt_type,NBDA_conformity,EWA_s_i,EWA_g_i,EWA_conformity,EWA_inverse_temp,timestep,num_know_novel,count_string))
```

## Running the simulation <a class="anchor" id="running"></a>

Finally, it's time to run the thing. This tutorial notebook has implemented a simpler version that runs one point in parameter space at a time, and doesn't capture full information from the agent's matrices. The full python version uses multi-threading to run these simulations much faster, across many points in parameter space.

### Simulation parameters <a class="anchor" id="sim_params"></a>


```python
#simulation parameters
replicates=3
t_steps = 100 #timesteps to run simulation
master_sim_num=0

#where raw data will be put
directory_path="../raw_data"

#where concatenated dataframes will be put after all sims finish running
new_directory_path="../concat_data"
```

### Population parameters <a class="anchor" id="pop_params"></a>


```python
### Population parameters ###
N = 20 #population size
initial_knowledgable_prop = 1 #initial proportion of knowledgable individuals in the population
graph_type = "random" #sets network structure
custom_adj_list_filename = ""
turnover = False #toggle turnover events
turnover_interval = 10 #interval between turnover events
num_turnover = 2 #number of individuals turned over
```

### EWA parameters <a class="anchor" id="EWA_params"></a>


```python
#EWA Parameters
EWA_s_i = .5 #social information bias EWA values
EWA_g_i = .5 #recent payoff bias EWA values
EWA_conformity = 1 #conformity exponent EWA values
EWA_inverse_temp = 1 #sensitivity to differences in payoffs

```

### NBDA parameters <a class="anchor" id="NBDA_params"></a>


```python
### NBDA parameters ###
NBDA_type = 0 # 0: Unbounded general form; 1: bounded, linked S and (1-S); NEED TO ADD: ILV form allowing for effects for social AND individual learning, requires vectors Gamma_i, Beta_i
NBDA_conformity = 1
s_param = 5 #s parameter from NBDA indicating strength of social learning per unit of connection
z_jt_type = "proportional" #"binary" or "proportional", proportional assigns z_j(t) a number between [0,1] depending on how frequently the produced behavior in previous timestep
asocial_learning = True #True or false, depending on if asocial learning occurs
base_hazard = .01 #the base hazard is the underlying rate in every timestep that the behavior could be acquired
keenness = 1 #this is the ILV value passed to NBDA function, indicates aptitude for individual learning
beh_per_TS = 1 #behaviors individuals perform per timestep
memory_window = 25 #how far back the agent can remember (in time steps) the behaviors that other agents produce
```


```python
all_behaviors = {"a": behavior(name = "a", payoff = .5, base_haz = base_hazard),
                 "b": behavior("b", 1, base_hazard)}
```

### Run the sim <a class="anchor" id="run_sim"></a>


```python
#repeat sims for number of desired replicates
for x in range(replicates):
    simulation()
```

    simulation0 EWA(s_i0.5 g_i0.5 conformity1 inverse_temp1) NBDA(s_param5, z_jt_typeproportional, memory_window25)
    simulation1 EWA(s_i0.5 g_i0.5 conformity1 inverse_temp1) NBDA(s_param5, z_jt_typeproportional, memory_window25)
    simulation2 EWA(s_i0.5 g_i0.5 conformity1 inverse_temp1) NBDA(s_param5, z_jt_typeproportional, memory_window25)


### Data management <a class="anchor" id="data_mgmt"></a>


```python
#concating dataframes
df_list = [pd.read_csv(join(directory_path,f)) for f in listdir(directory_path) if ".csv" in f]
df_concat = pd.concat(df_list)

#set final filename here
df_concat.to_csv(join(new_directory_path,"concatenated_data.csv"), index = False)

#remove raw data files
for f in listdir(directory_path):
    if ".csv" in f:
        remove(join(directory_path,f))
```


```python

```
