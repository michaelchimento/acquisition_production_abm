library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
directory = "../model_outputs/csvs_concat/"

#

#### BASELINE production timestep data####
df_baseline_P = read.csv(paste(directory,"BASELINE_production_timestep_data.csv",sep=""))
df_baseline_P = df_baseline_P %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))
df_baseline_P = df_baseline_P %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         full_first_prod=ifelse(num_produced_b==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi),
         graph_type = as.factor(graph_type))
summary(df_baseline_P)
#levels(df_baseline_P$EWA_sigma) = c("weak","medium","strong")
#levels(df_baseline_P$EWA_alpha) = c("risk-tolerant","risk-neutral","risk-averse")
#levels(df_baseline_P$EWA_rho) = c("weak","medium","strong")
#levels(df_baseline_P$EWA_chi) = c("anti-conformist bias","linear bias","conformist bias")

df_baseline_P$graph_type <- factor(df_baseline_P$graph_type, levels=c('random_regular'))
levels(df_baseline_P$graph_type) = c('random regular')
#df_baseline_P = df_baseline_P %>% select(!c(behavior_a_A_mat,behavior_a_I_mat,behavior_a_S_mat,behavior_a_P_mat, behavior_b_A_mat,behavior_b_I_mat,behavior_b_P_mat,behavior_b_S_mat))
save(df_baseline_P,file="../model_outputs/Rda_files/df_baseline_P.Rda")
remove(df_baseline_P)

#### BASELINE Production acquisition / production ####
df_baseline_p_acq_prod = read.csv(paste(directory,"BASELINE_production_acq_prod.csv",sep=""))
summary(df_baseline_p_acq_prod)
df_baseline_p_acq_prod = df_baseline_p_acq_prod %>%
  mutate(EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi),
         graph_type = as.factor(graph_type))

levels(df_baseline_p_acq_prod$EWA_sigma) = c("weak","medium","strong")
#levels(df_baseline_p_acq_prod$EWA_rho) = c("weak","medium","strong")
#levels(df_baseline_p_acq_prod$EWA_alpha) = c("risk-tolerant","risk-neutral","risk-averse")
#levels(df_baseline_p_acq_prod$EWA_chi) = c("anti-conformist bias","linear bias","conformist bias")
df_baseline_p_acq_prod$graph_type <- factor(df_baseline_p_acq_prod$graph_type, levels=c('random_regular'))
levels(df_baseline_p_acq_prod$graph_type) = c('random regular')
summary(df_baseline_p_acq_prod)
df_baseline_p_acq_prod = df_baseline_p_acq_prod %>% mutate(NBDA_transmission_weight=as.factor(NBDA_transmission_weight))
save(df_baseline_p_acq_prod,file="../model_outputs/Rda_files/df_baseline_p_acq_prod.Rda")
remove(df_baseline_p_acq_prod)


#### BASELINE transmission timestep data ####
df_baseline_T = read.csv(paste(directory,"BASELINE_acquisition_timestep_data.csv",sep=""))
df_baseline_T = df_baseline_T %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))
df_baseline_T = df_baseline_T %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         full_first_prod=ifelse(num_produced_b==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi),
         graph_type = as.factor(graph_type))

levels(df_baseline_T$EWA_sigma) = c("medium")
levels(df_baseline_T$EWA_rho) = c("medium")
levels(df_baseline_T$EWA_alpha) = c("risk-neutral")
levels(df_baseline_T$EWA_chi) = c("linear bias")
df_baseline_T$graph_type <- factor(df_baseline_T$graph_type, levels=c('random_regular'))
levels(df_baseline_T$graph_type) = c('random regular')
#df_baseline_T = df_baseline_T %>% select(!c(behavior_a_A_mat,behavior_a_I_mat,behavior_a_S_mat,behavior_a_P_mat, behavior_b_A_mat,behavior_b_I_mat,behavior_b_P_mat,behavior_b_S_mat))
save(df_baseline_T,file="../model_outputs/Rda_files/df_baseline_T.Rda")
remove(df_baseline_T)

#### BASELINE transmission acquisition / production ####
df_baseline_t_acq_prod = read.csv(paste(directory,"BASELINE_acquisition_acq_prod.csv",sep=""))
summary(df_baseline_t_acq_prod)

df_baseline_t_acq_prod = df_baseline_t_acq_prod %>%
  mutate(EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi),
         graph_type = as.factor(graph_type))

levels(df_baseline_t_acq_prod$EWA_sigma) = c("medium")
levels(df_baseline_t_acq_prod$EWA_rho) = c("medium")
levels(df_baseline_t_acq_prod$EWA_alpha) = c("risk-neutral")
levels(df_baseline_t_acq_prod$EWA_chi) = c("linear bias")

df_baseline_t_acq_prod$graph_type <- factor(df_baseline_t_acq_prod$graph_type, levels=c('random_regular'))
levels(df_baseline_t_acq_prod$graph_type) = c('random regular')
summary(df_baseline_t_acq_prod)
df_baseline_t_acq_prod = df_baseline_t_acq_prod %>% mutate(NBDA_transmission_weight=as.factor(NBDA_transmission_weight))
save(df_baseline_t_acq_prod,file="../model_outputs/Rda_files/df_baseline_t_acq_prod.Rda")
remove(df_baseline_t_acq_prod)


#### MAIN TEXT timestep data ####
df_ABM_maintext = read.csv(paste(directory, "production_params_maintext_timestep_data.csv",sep=""))
summary(df_ABM_maintext)
df_ABM_maintext = df_ABM_maintext %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))

df_ABM_maintext = df_ABM_maintext %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         full_first_prod=ifelse(num_produced_b==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi),
         graph_type = as.factor(graph_type))

summary(df_ABM_maintext)
levels(df_ABM_maintext$EWA_rho) = c("weak","medium","strong")
levels(df_ABM_maintext$EWA_sigma) = c("weak","medium","strong")
levels(df_ABM_maintext$EWA_alpha) = c("risk-tolerant","risk-neutral","risk-averse")
levels(df_ABM_maintext$EWA_chi) = c("anti-conformist bias","linear bias","conformist bias")

df_ABM_maintext$graph_type <- factor(df_ABM_maintext$graph_type, levels=c('random_regular'))
levels(df_ABM_maintext$graph_type) = c('random regular')

save(df_ABM_maintext,file="../model_outputs/Rda_files/df_maintext.Rda")
remove(df_ABM_maintext)


#### MAIN TEXT acquisition / production ####
df_maintext_acq_prod = read.csv(paste(directory,"production_params_maintext_acq_prod.csv",sep=""))
df_maintext_acq_prod = df_maintext_acq_prod %>%
  mutate(EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi),
         graph_type = as.factor(graph_type))

levels(df_maintext_acq_prod$EWA_rho) = c("weak","medium","strong")
levels(df_maintext_acq_prod$EWA_sigma) = c("weak","medium","strong")
levels(df_maintext_acq_prod$EWA_alpha) = c("risk-tolerant","risk-neutral","risk-averse")
levels(df_maintext_acq_prod$EWA_chi) = c("anti-conformist bias","linear bias","conformist bias")

df_maintext_acq_prod$graph_type <- factor(df_maintext_acq_prod$graph_type, levels=c('random_regular'))
levels(df_maintext_acq_prod$graph_type) = c('random regular')
df_maintext_acq_prod = df_maintext_acq_prod %>% mutate(NBDA_transmission_weight=as.factor(NBDA_transmission_weight))
summary(df_maintext_acq_prod)
save(df_maintext_acq_prod,file="../model_outputs/Rda_files/df_maintext_acq_prod.Rda")
remove(df_maintext_acq_prod)

#### MAIN TEXT Agent beliefs ####
df_maintext_agents = read.csv(paste(directory,"production_params_maintext_agent_mat_val.csv",sep=""))
df_maintext_agents = df_maintext_agents %>% group_by(sim,agent) %>% mutate(timestep_knowledgable=timestep-min(timestep), B_i=exp(B_i))
save(df_maintext_agents,file="../model_outputs/Rda_files/df_maintext_agents.Rda")


#### SUPP TEXT S1 transmission params - timestep data ####
df1 = read.csv(paste(directory, "transmission_params_textS1_lambda_0.1_timestep_data.csv",sep=""))
df2 = read.csv(paste(directory, "transmission_params_textS1_lambda_0.05_timestep_data.csv",sep=""))
df3 = read.csv(paste(directory, "transmission_params_textS1_lambda_0.01_timestep_data.csv",sep=""))

df_ABM_trans = bind_rows(df1,df2,df3)

df_ABM_trans = df_ABM_trans %>% mutate(prop_b=(behavior_b/(behavior_a+behavior_b)))

df_ABM_trans = df_ABM_trans %>%
  mutate(full_diffusion=ifelse(num_know_novel==max(pop_size), TRUE, FALSE),
         full_first_prod=ifelse(num_produced_b==max(pop_size), TRUE, FALSE),
         num_know_novel=num_know_novel/max(pop_size),
         EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi),
         graph_type = as.factor(graph_type),
         NBDA_basehazard = as.factor(NBDA_basehazard),
         asocial_learning = as.factor(asocial_learning),
         NBDA_s_param = as.factor(NBDA_s_param))

summary(df_ABM_trans)
levels(df_ABM_trans$asocial_learning) = c("0","1")
levels(df_ABM_trans$EWA_rho) = c("medium")
levels(df_ABM_trans$EWA_sigma) = c("medium")
levels(df_ABM_trans$EWA_alpha) = c("risk-neutral")
levels(df_ABM_trans$EWA_chi) = c("linear bias")
summary(df_ABM_trans)
df_ABM_trans$graph_type <- factor(df_ABM_trans$graph_type, levels=c('random_regular',
                                                                    'random_small_world',
                                                                    'random_erdos',
                                                                    'random_barabasi'))
levels(df_ABM_trans$graph_type) = c('random regular', 'small world', 'erdos', 'barabasi')

summary(df_ABM_trans)
save(df_ABM_trans,file="../model_outputs/Rda_files/df_supptext.Rda")
remove(df_ABM_trans)


#### SUPP TEXT S1 transmission params - acquisition / production ####
df1 = read.csv(paste(directory, "transmission_params_textS1_lambda_0.1_acq_prod.csv",sep=""))
df2 = read.csv(paste(directory, "transmission_params_textS1_lambda_0.05_acq_prod.csv",sep=""))
df2$sim= df2$sim+3200
df3 = read.csv(paste(directory, "transmission_params_textS1_lambda_0.01_acq_prod.csv",sep=""))
df2$sim= df2$sim+6400

df_supptext_acq_prod = bind_rows(df1,df2,df3)
summary(df_supptext_acq_prod)
df_supptext_acq_prod = df_supptext_acq_prod %>%
  mutate(EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi),
         asocial_learning = as.factor(asocial_learning),
         graph_type = as.factor(graph_type),
         NBDA_basehazard = as.factor(NBDA_basehazard),
         asocial_learning = as.factor(asocial_learning),
         NBDA_s_param = as.factor(NBDA_s_param))
summary(df_supptext_acq_prod)
levels(df_supptext_acq_prod$asocial_learning) = c("0","1")
levels(df_supptext_acq_prod$EWA_rho) = c("medium")
levels(df_supptext_acq_prod$EWA_sigma) = c("medium")
levels(df_supptext_acq_prod$EWA_alpha) = c("risk-neutral")
levels(df_supptext_acq_prod$EWA_chi) = c("linear bias")

df_supptext_acq_prod$graph_type <- factor(df_supptext_acq_prod$graph_type, levels=c('random_regular',
                                                                                    'random_small_world',
                                                                                    'random_erdos',
                                                                                    'random_barabasi'))
levels(df_supptext_acq_prod$graph_type) = c('random regular', 'small world', 'erdos', 'barabasi')
summary(df_supptext_acq_prod)
df_supptext_acq_prod = df_supptext_acq_prod %>% mutate(NBDA_transmission_weight=as.factor(NBDA_transmission_weight))
save(df_supptext_acq_prod,file="../model_outputs/Rda_files/df_supptext_acq_prod.Rda")
remove(df_supptext_acq_prod)

#### BINARY NBDA SOCIAL ####
df_NBDA_SB = read.csv(paste(directory,"NBDA_social_binary_acq_prod.csv",sep=""))
df_NBDA_SB = df_NBDA_SB %>%
  mutate(EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         graph_type = as.factor(graph_type),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi))%>%
  group_by(sim) %>%
  mutate(TTFP=max(timestep_production_b)+1) %>%
  ungroup() %>%
  mutate(transmission_weight=total_productions_b / (TTFP-timestep_acquisition_b) )

levels(df_NBDA_SB$EWA_sigma) = c("weak","medium","strong")
levels(df_NBDA_SB$EWA_rho) = c("weak","medium","strong")
levels(df_NBDA_SB$EWA_alpha) = c("risk-tolerant","risk-neutral","risk-averse")
levels(df_NBDA_SB$graph_type) = c('random regular')
df_NBDA_SB = df_NBDA_SB %>% mutate(NBDA_transmission_weight=as.factor(NBDA_transmission_weight))
save(df_NBDA_SB,file="../model_outputs/Rda_files/df_NBDA_SB.Rda")
summary(df_NBDA_SB)
remove(df_NBDA_SB)

#### BINARY NBDA ASOCIAL ####
df_NBDA_AB = read.csv(paste(directory,"NBDA_asocial_binary_acq_prod.csv",sep=""))
df_NBDA_AB = df_NBDA_AB %>%
  mutate(EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         graph_type = as.factor(graph_type),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi))%>%
  group_by(sim) %>%
  mutate(TTFP=max(timestep_production_b)+1) %>%
  ungroup() %>%
  mutate(transmission_weight=total_productions_b / (TTFP-timestep_acquisition_b) )

levels(df_NBDA_AB$EWA_sigma) = c("weak","medium","strong")
levels(df_NBDA_AB$EWA_rho) = c("weak","medium","strong")
levels(df_NBDA_AB$EWA_alpha) = c("risk-tolerant","risk-neutral","risk-averse")
levels(df_NBDA_AB$graph_type) = c('random regular')
df_NBDA_AB = df_NBDA_AB %>% mutate(NBDA_transmission_weight=as.factor(NBDA_transmission_weight))
summary(df_NBDA_AB)
save(df_NBDA_AB,file="../model_outputs/Rda_files/df_NBDA_AB.Rda")
remove(df_NBDA_AB)

#### PROPORTIONAL NBDA SOCIAL ####
df_NBDA_SP = read.csv(paste(directory,"NBDA_social_proportional_acq_prod.csv",sep=""))
df_NBDA_SP = df_NBDA_SP %>%
  mutate(EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         graph_type = as.factor(graph_type),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi))%>%
  group_by(sim) %>%
  mutate(TTFP=max(timestep_production_b)+1) %>%
  ungroup() %>%
  mutate(transmission_weight=total_productions_b / (TTFP-timestep_acquisition_b) )

levels(df_NBDA_SP$EWA_sigma) = c("weak","medium","strong")
levels(df_NBDA_SP$EWA_rho) = c("weak","medium","strong")
levels(df_NBDA_SP$EWA_alpha) = c("risk-tolerant","risk-neutral","risk-averse")
levels(df_NBDA_SP$graph_type) = c('random regular')
df_NBDA_SP = df_NBDA_SP %>% mutate(NBDA_transmission_weight=as.factor(NBDA_transmission_weight))
summary(df_NBDA_SP)
save(df_NBDA_SP,file="../model_outputs/Rda_files/df_NBDA_SP.Rda")
remove(df_NBDA_SP)

#### PROPORTIONAL NBDA ASOCIAL ####
df_NBDA_AP = read.csv(paste(directory,"NBDA_asocial_proportional_acq_prod.csv",sep=""))
summary(df_NBDA_AP)
df_NBDA_AP = df_NBDA_AP %>%
  mutate(EWA_rho = as.factor(EWA_rho),
         EWA_sigma = as.factor(EWA_sigma),
         graph_type = as.factor(graph_type),
         EWA_alpha = as.factor(EWA_alpha),
         EWA_chi = as.factor(EWA_chi))%>%
  group_by(sim) %>%
  mutate(TTFP=max(timestep_production_b)+1) %>%
  ungroup() %>%
  mutate(transmission_weight=total_productions_b / (TTFP-timestep_acquisition_b) )

levels(df_NBDA_AP$EWA_sigma) = c("weak","medium","strong")
levels(df_NBDA_AP$EWA_rho) = c("weak","medium","strong")
levels(df_NBDA_AP$EWA_alpha) = c("risk-tolerant","risk-neutral","risk-averse")
levels(df_NBDA_AP$graph_type) = c('random regular')
df_NBDA_AP = df_NBDA_AP %>% mutate(NBDA_transmission_weight=as.factor(NBDA_transmission_weight))
save(df_NBDA_AP,file="../model_outputs/Rda_files/df_NBDA_AP.Rda")
remove(df_NBDA_AP)


#### EWA inference homogeneous condition ####
df_EWA = read.csv(paste(directory,"EWA_homogeneous_agents.csv",sep="")) %>% arrange(sim)
save(df_EWA, file="../model_outputs/Rda_files/df_EWA_homogeneous_agents.Rda")

#### EWA inference Heterogeneous: social learning condition ####
df_EWA = read.csv(paste(directory,"EWA_heterogeneous_social_agents.csv",sep="")) %>% arrange(sim)
save(df_EWA, file="../model_outputs/Rda_files/df_EWA_heterogeneous_social_agents.Rda")

#### EWA inference Heterogeneous: asocial learning condition ####
df_EWA = read.csv(paste(directory,"EWA_heterogeneous_asocial_agents.csv",sep="")) %>% arrange(sim)
save(df_EWA, file="../model_outputs/Rda_files/df_EWA_heterogeneous_asocial_agents.Rda")
