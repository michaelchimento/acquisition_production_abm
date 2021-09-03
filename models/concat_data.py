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


#where raw data will be put
directory_path="../raw_data"

#where concatenated dataframes will be put after all sims finish running
new_directory_path="../concat_data"

#concating dataframes
df_list = [pd.read_csv(join(directory_path,f)) for f in listdir(directory_path) if ".csv" in f]
df_concat = pd.concat(df_list)

#set final filename here
df_concat.to_csv(join(new_directory_path,"diff_payoffs_newgraph.csv"), index = False)

#remove raw data files
for f in listdir(directory_path):
    if ".csv" in f:
        remove(join(directory_path,f))
