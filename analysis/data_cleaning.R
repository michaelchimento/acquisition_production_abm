library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
directory = "../concat_data/"

####diffusion of equivalent payoff behavior ####
df_ABM_equiv_payoff = read.csv(paste(directory,"equiv_payoff_diffusion.csv",sep=""))
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% filter(timestep%%10==1)
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% mutate(NBDA_s_param = factor(NBDA_s_param,labels=c("S = 1","S = 5", "S = 10")))
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% mutate(prop_b=(behavior_b_1/(behavior_a_1+behavior_b_1)))
#df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% pivot_longer(cols=c(behavior_a_10,behavior_b_20), names_to="behavior", values_to="count")
save(df_ABM_equiv_payoff,file="../concat_data/df_ABM_equiv_payoff.Rda")
remove(df_ABM_equiv_payoff)


####diffusion of higher payoff behavior ####
df_ABM_diff_payoff = read.csv(paste(directory,"diff_payoff_diffusion.csv",sep=""))
df_ABM_diff_payoff = df_ABM_diff_payoff %>% filter(timestep%%10==1)
df_ABM_diff_payoff = df_ABM_diff_payoff %>% mutate(NBDA_s_param = factor(NBDA_s_param,labels=c("S = 1","S = 5", "S = 10")))
df_ABM_diff_payoff = df_ABM_diff_payoff %>% mutate(prop_b=(behavior_b_1/(behavior_a_0.5+behavior_b_1)))
#df_ABM_diff_payoff = df_ABM_diff_payoff %>% pivot_longer(cols=c(behavior_a_10,behavior_b_20), names_to="behavior", values_to="count")
save(df_ABM_diff_payoff,file="../concat_data/df_ABM_diff_payoff.Rda")
remove(df_ABM_diff_payoff)





#### read in lots of data ####
#file_names <- dir(directory) #where you have your files
#df_ABM <- do.call(rbind,lapply(paste(directory,file_names, sep=""),read.csv))

