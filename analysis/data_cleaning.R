library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
directory = "../model_outputs/csvs_concat/"

####GENERATIVE timestep data equivalent payoffs ####
df_ABM_equiv_payoff = read.csv(paste(directory,"GENERATIVE_equiv_payoffs_timestep_data.csv",sep=""))
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))
#df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% mutate(conformity = paste("NBDA conf.",NBDA_conformity,"EWA conf.",EWA_conformity))
#df_ABM_equiv_payoff$conformity = as.factor(df_ABM_equiv_payoff$conformity)
#levels(df_ABM_equiv_payoff$conformity) = c("None","Production","Transmission","Production & Transmission")
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         full_first_prod=ifelse(num_produced_b==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         EWA_tau = as.factor(EWA_tau),
         graph_type = as.factor(graph_type))

levels(df_ABM_equiv_payoff$EWA_soc_info_weight) = c("weak","medium","strong")
levels(df_ABM_equiv_payoff$EWA_tau) = c("non-conservative","conservative")
levels(df_ABM_equiv_payoff$EWA_recent_payoff_weight) = c("weak","medium","strong")
df_ABM_equiv_payoff$graph_type <- factor(df_ABM_equiv_payoff$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_ABM_equiv_payoff$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")

df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% select(!c(behavior_a_A_mat,behavior_a_I_mat,behavior_a_S_mat,behavior_a_P_mat, behavior_b_A_mat,behavior_b_I_mat,behavior_b_P_mat,behavior_b_S_mat))

save(df_ABM_equiv_payoff,file="../model_outputs/Rda_files/df_GEN_equiv_payoff.Rda")
remove(df_ABM_equiv_payoff)


####GENERATIVE Equivalent payoff acquisition / production ####
df_equiv_payoffs_acq_prod = read.csv(paste(directory,"GENERATIVE_equiv_payoffs_acq_prod.csv",sep=""))

#df_equiv_payoffs_acq_prod = df_equiv_payoffs_acq_prod %>% mutate(conformity = paste("NBDA conf.",NBDA_conformity,"EWA conf.",EWA_conformity))
#df_equiv_payoffs_acq_prod$conformity = as.factor(df_equiv_payoffs_acq_prod$conformity)
#levels(df_equiv_payoffs_acq_prod$conformity) = c("None","Production","Transmission","Production & Transmission")
df_equiv_payoffs_acq_prod = df_equiv_payoffs_acq_prod %>%
  mutate(EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         EWA_tau = as.factor(EWA_tau),
         graph_type = as.factor(graph_type))

levels(df_equiv_payoffs_acq_prod$EWA_soc_info_weight) = c("weak","medium","strong")
levels(df_equiv_payoffs_acq_prod$EWA_recent_payoff_weight) = c("weak","medium","strong")
levels(df_equiv_payoffs_acq_prod$EWA_tau) = c("non-conservative","conservative")
df_equiv_payoffs_acq_prod$graph_type <- factor(df_equiv_payoffs_acq_prod$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_equiv_payoffs_acq_prod$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")
summary(df_equiv_payoffs_acq_prod)
df_equiv_payoffs_acq_prod = df_equiv_payoffs_acq_prod %>% mutate(NBDA_zjt_type=as.factor(NBDA_zjt_type))
save(df_equiv_payoffs_acq_prod,file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod.Rda")
remove(df_equiv_payoffs_acq_prod)

####GENERATIVE fullweight equivalent payoffs ####
df_ABM_fullweight = read.csv(paste(directory,"GENERATIVE_fullweights_timestep_data.csv",sep=""))
df_ABM_fullweight = df_ABM_fullweight %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))
#df_ABM_fullweight = df_ABM_fullweight %>% mutate(conformity = paste("NBDA conf.",NBDA_conformity,"EWA conf.",EWA_conformity))

#df_ABM_fullweight$conformity = as.factor(df_ABM_fullweight$conformity)
#levels(df_ABM_fullweight$conformity) =c("None","Production","Transmission","Production & Transmission")
df_ABM_fullweight = df_ABM_fullweight %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         full_first_prod=ifelse(num_produced_b==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         EWA_tau = as.factor(EWA_tau),
         graph_type = as.factor(graph_type))

levels(df_ABM_fullweight$EWA_soc_info_weight) = c("weak","medium","strong")
levels(df_ABM_fullweight$EWA_tau) = c("non-conservative","conservative")
levels(df_ABM_fullweight$EWA_recent_payoff_weight) = c("weak","medium","strong")
df_ABM_fullweight$graph_type <- factor(df_ABM_fullweight$graph_type, levels=c('random_regular'))
levels(df_ABM_fullweight$graph_type) = c('random regular')

df_ABM_fullweight = df_ABM_fullweight %>% select(!c(behavior_a_A_mat,behavior_a_I_mat,behavior_a_S_mat,behavior_a_P_mat, behavior_b_A_mat,behavior_b_I_mat,behavior_b_P_mat,behavior_b_S_mat))
save(df_ABM_fullweight,file="../model_outputs/Rda_files/df_fullweight.Rda")
remove(df_ABM_fullweight)

####GENERATIVE fullweight acquisition / production ####
df_fullweight_acq_prod = read.csv(paste(directory,"GENERATIVE_fullweights_acq_prod.csv",sep=""))
#df_fullweight_acq_prod = df_fullweight_acq_prod %>% mutate(conformity = paste("NBDA conf.",NBDA_conformity,"EWA conf.",EWA_conformity))
#df_fullweight_acq_prod$conformity = as.factor(df_fullweight_acq_prod$conformity)
#levels(df_fullweight_acq_prod$conformity) = c("None","Production","Transmission","Production & Transmission")
df_fullweight_acq_prod = df_fullweight_acq_prod %>%
  mutate(EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         EWA_tau = as.factor(EWA_tau),
         graph_type = as.factor(graph_type))

levels(df_fullweight_acq_prod$EWA_soc_info_weight) = c("weak","medium","strong")
levels(df_fullweight_acq_prod$EWA_recent_payoff_weight) = c("weak","medium","strong")
df_fullweight_acq_prod$graph_type <- factor(df_fullweight_acq_prod$graph_type, levels=c('random_regular'))
levels(df_fullweight_acq_prod$graph_type) = c('random regular')
summary(df_fullweight_acq_prod)
df_fullweight_acq_prod = df_fullweight_acq_prod %>% mutate(NBDA_zjt_type=as.factor(NBDA_zjt_type))
save(df_fullweight_acq_prod,file="../model_outputs/Rda_files/df_GEN_fullweight_acq_prod.Rda")
remove(df_fullweight_acq_prod)



#### BINARY NBDA SOCIAL ####
df_NBDA_SB = read.csv(paste(directory,"NBDA_social_binary_acq_prod.csv",sep=""))
df_NBDA_SB = df_NBDA_SB %>%
  mutate(EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         graph_type = as.factor(graph_type),
         EWA_tau = as.factor(EWA_tau))

levels(df_NBDA_SB$EWA_soc_info_weight) = c("weak","medium","strong")
levels(df_NBDA_SB$EWA_recent_payoff_weight) = c("weak","medium","strong")
levels(df_NBDA_SB$EWA_tau) = c("non-conservative","conservative")
df_NBDA_SB$graph_type <- factor(df_NBDA_SB$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_NBDA_SB$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")
df_NBDA_SB = df_NBDA_SB %>% mutate(NBDA_zjt_type=as.factor(NBDA_zjt_type))
save(df_NBDA_SB,file="../model_outputs/Rda_files/df_NBDA_SB.Rda")
summary(df_NBDA_SB)
remove(df_NBDA_SB)

#### BINARY NBDA ASOCIAL ####
df_NBDA_AB = read.csv(paste(directory,"NBDA_asocial_binary_acq_prod.csv",sep=""))
df_NBDA_AB = df_NBDA_AB %>%
  mutate(EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         graph_type = as.factor(graph_type),
         EWA_tau = as.factor(EWA_tau))

levels(df_NBDA_AB$EWA_soc_info_weight) = c("weak","medium","strong")
levels(df_NBDA_AB$EWA_recent_payoff_weight) = c("weak","medium","strong")
levels(df_NBDA_AB$EWA_tau) = c("non-conservative","conservative")
df_NBDA_AB$graph_type <- factor(df_NBDA_AB$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_NBDA_AB$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")
df_NBDA_AB = df_NBDA_AB %>% mutate(NBDA_zjt_type=as.factor(NBDA_zjt_type))
summary(df_NBDA_AB)
save(df_NBDA_AB,file="../model_outputs/Rda_files/df_NBDA_AB.Rda")
remove(df_NBDA_AB)

#### PROPORTIONAL NBDA SOCIAL ####
df_NBDA_SP = read.csv(paste(directory,"NBDA_social_proportional_acq_prod.csv",sep=""))
df_NBDA_SP = df_NBDA_SP %>%
  mutate(EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         graph_type = as.factor(graph_type),
         EWA_tau = as.factor(EWA_tau))

levels(df_NBDA_SP$EWA_soc_info_weight) = c("weak","medium","strong")
levels(df_NBDA_SP$EWA_recent_payoff_weight) = c("weak","medium","strong")
levels(df_NBDA_SP$EWA_tau) = c("non-conservative","conservative")
df_NBDA_SP$graph_type <- factor(df_NBDA_SP$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_NBDA_SP$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")
df_NBDA_SP = df_NBDA_SP %>% mutate(NBDA_zjt_type=as.factor(NBDA_zjt_type))
save(df_NBDA_SP,file="../model_outputs/Rda_files/df_NBDA_SP.Rda")
summary(df_NBDA_SP)
remove(df_NBDA_SP)

#### PROPORTIONAL NBDA ASOCIAL ####
df_NBDA_AP = read.csv(paste(directory,"NBDA_asocial_proportional_acq_prod.csv",sep=""))
df_NBDA_AP = df_NBDA_AP %>%
  mutate(EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         graph_type = as.factor(graph_type),
         EWA_tau = as.factor(EWA_tau))

levels(df_NBDA_AP$EWA_soc_info_weight) = c("weak","medium","strong")
levels(df_NBDA_AP$EWA_recent_payoff_weight) = c("weak","medium","strong")
levels(df_NBDA_AP$EWA_tau) = c("non-conservative","conservative")
df_NBDA_AP$graph_type <- factor(df_NBDA_AP$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_NBDA_AP$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")
df_NBDA_AP = df_NBDA_AP %>% mutate(NBDA_zjt_type=as.factor(NBDA_zjt_type))
summary(df_NBDA_AP)
save(df_NBDA_AP,file="../model_outputs/Rda_files/df_NBDA_AP.Rda")
remove(df_NBDA_AP)


#### EWA inference homogeneous condition ####
df_EWA = read.csv(paste(directory,"EWA_homogeneous_agents.csv",sep="")) %>% arrange(sim)
save(df_EWA, file="../model_outputs/Rda_files/EWA_homogeneous_agents.Rda")

#### EWA inference Heterogeneous: social learning condition ####
df_EWA = read.csv(paste(directory,"EWA_heterogeneous_social_agents.csv",sep="")) %>% arrange(sim)
save(df_EWA, file="../model_outputs/Rda_files/EWA_heterogeneous_social_agents.Rda")

#### EWA inference Heterogeneous: asocial learning condition ####
df_EWA = read.csv(paste(directory,"EWA_heterogeneous_asocial_agents.csv",sep="")) %>% arrange(sim)
save(df_EWA, file="../model_outputs/Rda_files/EWA_heterogeneous_asocial_agents.Rda")



####GENERATIVE timestep data conformity ####
df_ABM_conformity = read.csv(paste(directory,"GENERATIVE_memory_conformity_timestep_data.csv",sep=""))
df_ABM_conformity = df_ABM_conformity %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))
df_ABM_conformity = df_ABM_conformity %>% mutate(conformity = paste("NBDA conf.",NBDA_conformity,"EWA conf.",EWA_conformity))
summary(df_ABM_conformity)
df_ABM_conformity$conformity = as.factor(df_ABM_conformity$conformity)
levels(df_ABM_conformity$conformity) = c("None","Production","Transmission","Production & Transmission")
df_ABM_conformity = df_ABM_conformity %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         full_first_prod=ifelse(num_produced_b==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         EWA_tau = as.factor(EWA_tau),
         graph_type = as.factor(graph_type))

levels(df_ABM_conformity$EWA_soc_info_weight) = c("medium")
levels(df_ABM_conformity$EWA_tau) = c("non-conservative")
levels(df_ABM_conformity$EWA_recent_payoff_weight) = c("medium")
df_ABM_conformity$graph_type <- factor(df_ABM_conformity$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_ABM_conformity$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")

df_ABM_conformity = df_ABM_conformity %>% select(!c(behavior_a_A_mat,behavior_a_I_mat,behavior_a_S_mat,behavior_a_P_mat, behavior_b_A_mat,behavior_b_I_mat,behavior_b_P_mat,behavior_b_S_mat))

save(df_ABM_conformity,file="../model_outputs/Rda_files/df_GEN_memory_conformity.Rda")
remove(df_ABM_conformity)


####GENERATIVE acquisition production CONFORMITY ####
df_conf_acq_prod = read.csv(paste(directory,"GENERATIVE_memory_conformity_acq_prod.csv",sep=""))

df_conf_acq_prod = df_conf_acq_prod %>% mutate(conformity = paste("NBDA conf.",NBDA_conformity,"EWA conf.",EWA_conformity))
df_conf_acq_prod$conformity = as.factor(df_conf_acq_prod$conformity)
levels(df_conf_acq_prod$conformity) = c("None","Production","Transmission","Production & Transmission")
df_conf_acq_prod = df_conf_acq_prod %>%
  mutate(EWA_recent_payoff_weight = as.factor(EWA_recent_payoff_weight),
         EWA_soc_info_weight = as.factor(EWA_soc_info_weight),
         graph_type = as.factor(graph_type))

levels(df_conf_acq_prod$EWA_soc_info_weight) = c("medium")
levels(df_conf_acq_prod$EWA_recent_payoff_weight) = c("medium")
df_conf_acq_prod$graph_type <- factor(df_conf_acq_prod$graph_type, levels=c('random_regular','random_small_world', 'random_erdos', 'random_barabasi'))
levels(df_conf_acq_prod$graph_type) = c('random regular', 'small world', 'Erdos-Renyi',"Barabasi-Albert")
summary(df_conf_acq_prod)
df_conf_acq_prod = df_conf_acq_prod %>% mutate(NBDA_zjt_type=as.factor(NBDA_zjt_type))
save(df_conf_acq_prod,file="../model_outputs/Rda_files/df_GEN_memory_conformity_acq_prod.Rda")
remove(df_conf_acq_prod)



