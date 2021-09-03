library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
directory = "../concat_data/csvs/"

####diffusion of equiv payoff behavior ####
df_ABM_equiv_payoff = read.csv(paste(directory,"equiv_payoffs.csv",sep=""))
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% mutate(conformity = paste("NBDA conf.",NBDA_conformity,"EWA conf.",EWA_conformity))

df_ABM_equiv_payoff$conformity = as.factor(df_ABM_equiv_payoff$conformity)
levels(df_ABM_equiv_payoff$conformity) = c("None","EWA","NBDA","EWA & NBDA")
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         EWA_tau = as.factor(EWA_tau),
         conformity = as.factor(conformity),
         graph_type = as.factor(graph_type))

levels(df_ABM_equiv_payoff$EWA_soc_info_weight) = c("weak","medium","strong")
levels(df_ABM_equiv_payoff$EWA_tau) = c("non-conservative","conservative")
levels(df_ABM_equiv_payoff$EWA_recent_payoff_weight) = c("weak","medium","strong")
df_ABM_equiv_payoff$graph_type <- factor(df_ABM_equiv_payoff$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_ABM_equiv_payoff$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")

save(df_ABM_equiv_payoff,file="../concat_data/df_ABM_equiv.Rda")
remove(df_ABM_equiv_payoff)


####diffusion of equiv payoff behavior with memory test ####
df_ABM_equiv_payoff = read.csv(paste(directory,"equiv_payoffs_memoryconformity.csv",sep=""))
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% mutate(conformity = paste("NBDA conf.",NBDA_conformity,"EWA conf.",EWA_conformity))

df_ABM_equiv_payoff$conformity = as.factor(df_ABM_equiv_payoff$conformity)
levels(df_ABM_equiv_payoff$conformity) = c("None","EWA","NBDA","EWA & NBDA")
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         EWA_tau = as.factor(EWA_tau),
         conformity = as.factor(conformity),
         graph_type = as.factor(graph_type))

levels(df_ABM_equiv_payoff$EWA_soc_info_weight) = c("medium")
levels(df_ABM_equiv_payoff$EWA_tau) = c("non-conservative","conservative")
levels(df_ABM_equiv_payoff$EWA_recent_payoff_weight) = c("medium")
df_ABM_equiv_payoff$graph_type <- factor(df_ABM_equiv_payoff$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_ABM_equiv_payoff$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")

df_memory = df_ABM_equiv_payoff

save(df_memory,file="../concat_data/df_ABM_memory.Rda")
remove(df_ABM_equiv_payoff)


####diffusion of equiv payoff behavior with full initial weights for established behavior ####
df_ABM_equiv_payoff = read.csv(paste(directory,"equiv_payoffs_fullweights.csv",sep=""))
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% mutate(conformity = paste("NBDA conf.",NBDA_conformity,"EWA conf.",EWA_conformity))

df_ABM_equiv_payoff$conformity = as.factor(df_ABM_equiv_payoff$conformity)
levels(df_ABM_equiv_payoff$conformity) = c("None","EWA","NBDA","EWA & NBDA")
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         EWA_tau = as.factor(EWA_tau),
         conformity = as.factor(conformity),
         graph_type = as.factor(graph_type))

levels(df_ABM_equiv_payoff$EWA_soc_info_weight) = c("weak","medium","strong")
levels(df_ABM_equiv_payoff$EWA_tau) = c("non-conservative","conservative")
levels(df_ABM_equiv_payoff$EWA_recent_payoff_weight) = c("weak","medium","strong")
df_ABM_equiv_payoff$graph_type <- factor(df_ABM_equiv_payoff$graph_type, levels=c('random_small_world'))
levels(df_ABM_equiv_payoff$graph_type) = c('small world')


save(df_ABM_equiv_payoff,file="../concat_data/df_ABM_equiv_fullweights.Rda")
remove(df_ABM_equiv_payoff)


####diffusion of equivalent payoff behavior with asocial learning, varying s values####
df_ABM_asocial = read.csv(paste(directory,"equiv_payoffs_asocial_sparam.csv",sep=""))
df_ABM_asocial = df_ABM_asocial %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))
df_ABM_asocial = df_ABM_asocial %>% mutate(conformity = paste("NBDA conf.",NBDA_conformity,"EWA conf.",EWA_conformity))

df_ABM_asocial$conformity = as.factor(df_ABM_asocial$conformity)
levels(df_ABM_asocial$conformity) = c("None","EWA","NBDA","EWA & NBDA")
df_ABM_asocial = df_ABM_asocial %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         EWA_tau = as.factor(EWA_tau),
         conformity = as.factor(conformity),
         graph_type = as.factor(graph_type))

levels(df_ABM_asocial$EWA_soc_info_weight) = c("medium")
levels(df_ABM_asocial$EWA_tau) = c("non-conservative","conservative")
levels(df_ABM_asocial$EWA_recent_payoff_weight) = c("medium")

df_ABM_asocial$graph_type <- factor(df_ABM_asocial$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_ABM_asocial$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")

save(df_ABM_asocial,file="../concat_data/df_ABM_equiv_asocial_sparam.Rda")
remove(df_ABM_asocial)


#### EWA inference homogeneous condition ####
df_EWA = read.csv(paste(directory,"EWA_inference_homogeneous_agents.csv",sep="")) %>% arrange(sim)
save(df_EWA, file="../concat_data/EWA_inference_homogeneous_agents.Rda")

#### EWA inference Heterogeneous: social learning condition ####
df_EWA = read.csv(paste(directory,"EWA_inference_social_agents.csv",sep="")) %>% arrange(sim)
save(df_EWA, file="../concat_data/EWA_inference_social_agents.Rda")

#### EWA inference Heterogeneous: asocial learning condition ####
df_EWA = read.csv(paste(directory,"EWA_inference_asocial_agents.csv",sep="")) %>% arrange(sim)
save(df_EWA, file="../concat_data/EWA_inference_asocial_agents.Rda")




