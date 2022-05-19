library(tidyverse)
library(NBDA)
library(igraph)
library(sna)
library(magrittr)
library(reader)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####BINARY SOCIAL DATA####
load(file="../model_outputs/Rda_files/df_NBDA_SB.Rda")
df = df_NBDA_SB %>% ungroup()
temp <- data.frame(matrix(ncol = 7, nrow = 0))
#provide column names
colnames(temp) <- c("data_type", "model_type", "sim", "est_s", "est_rate", "aicc_social", "aicc_asocial")

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  sim=i
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_social_binary/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)

  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  ##ORDER OF ACQUISITION
  ##TRANSMISSION WEIGHTS
  sim_data = df %>%
    filter(sim==i) %>%
    mutate(agent=agent+1) %>%
    arrange(agent)

  t_weights = sim_data$transmission_weight

  ##ORDER OF ACQUISITION
  oata = sim_data %>%
    arrange(timestep_acquisition_b) %>%
    select(timestep_acquisition_b, agent) %>%
    mutate(tied_acq = ifelse(lead(timestep_acquisition_b)==timestep_acquisition_b | lag(timestep_acquisition_b)==timestep_acquisition_b,1,0)) %>%
    mutate(tied_acq = ifelse(is.na(tied_acq),0,tied_acq))

  oata

  oa1 = oata$agent
  seed_agent = oa1[1]
  ties_vector = oata$tied_acq

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[seed_agent] = 1

  #Time of acquisition
  ta1 = oata$timestep_acquisition_b

  #weightless
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1,  demons=demons)
  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  est_s = model_social@outputPar[2]
  est_rate = 1/model_social@outputPar[1]
  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc

  data_type="time_of_acquisition"
  model_type="weightless"
  acquisition_weightless = data.frame(data_type, model_type, sim, est_s, est_rate, aicc_social, aicc_asocial)
  temp = rbind(temp, acquisition_weightless)
}

df_summary = df %>% group_by(EWA_rho, EWA_sigma, EWA_chi, EWA_alpha, memory_window, graph_type,sim) %>% summarize()
df_summary_SB = left_join(df_summary,temp)

####BINARY ASOCIAL DATA####
load(file="../model_outputs/Rda_files/df_NBDA_AB.Rda")
summary(df_NBDA_AB)
df = df_NBDA_AB %>% ungroup()
temp <- data.frame(matrix(ncol = 7, nrow = 0))
#provide column names
colnames(temp) <- c("data_type", "model_type", "sim", "est_s", "est_rate", "aicc_social", "aicc_asocial")

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  sim=i
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_asocial_binary/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)

  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  ##ORDER OF ACQUISITION
  ##TRANSMISSION WEIGHTS
  sim_data = df %>%
    filter(sim==i) %>%
    mutate(agent=agent+1) %>%
    arrange(agent)

  t_weights = sim_data$transmission_weight

  ##ORDER OF ACQUISITION
  oata = sim_data %>%
    arrange(timestep_acquisition_b) %>%
    select(timestep_acquisition_b, agent) %>%
    mutate(tied_acq = ifelse(lead(timestep_acquisition_b)==timestep_acquisition_b | lag(timestep_acquisition_b)==timestep_acquisition_b,1,0)) %>%
    mutate(tied_acq = ifelse(is.na(tied_acq),0,tied_acq))

  oa1 = oata$agent
  seed_agent = oa1[1]
  ties_vector = oata$tied_acq

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[seed_agent] = 1

  #Time of acquisition
  ta1 = oata$timestep_acquisition_b

  #weightless
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1,  endTime = max(ta1) + 1, demons=demons)
  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  est_s = model_social@outputPar[2]
  est_rate = 1/model_social@outputPar[1]
  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc

  data_type="time_of_acquisition"
  model_type="weightless"
  acquisition_weightless = data.frame(data_type, model_type, sim, est_s, est_rate, aicc_social, aicc_asocial)

  temp = rbind(temp, acquisition_weightless)
}

df_summary = df %>% group_by(EWA_rho, EWA_sigma, EWA_chi, EWA_alpha, memory_window, graph_type,sim) %>% summarize()
df_summary_AB = left_join(df_summary,temp)

#bind social and asocial together
df_summary_AB$condition="asocial learning"
df_summary_SB$condition="social transmission"

df_ideal = bind_rows(df_summary_SB, df_summary_AB) %>% mutate(data_type=as.factor(data_type),model_type=as.factor(model_type), condition=as.factor(condition))
save(df_ideal,file="../model_outputs/Rda_files/df_NBDA_figure_data_binary.Rda")

ggplot(df_ideal, aes(x=condition,y=aicc_asocial-aicc_social))+
  facet_grid(model_type~data_type)+
  geom_jitter()+
  geom_boxplot()

####PROPORTIONAL SOCIAL DATA####
load(file="../model_outputs/Rda_files/df_NBDA_SP.Rda")
df = df_NBDA_SP %>% ungroup()
temp <- data.frame(matrix(ncol = 7, nrow = 0))
#provide column names
colnames(temp) <- c("data_type", "model_type", "sim", "est_s", "est_rate", "aicc_social", "aicc_asocial")

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  sim=i
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_social_proportional/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)

  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  ##TRANSMISSION WEIGHTS
  sim_data = df %>%
    filter(sim==i) %>%
    mutate(agent=agent+1) %>%
    arrange(agent)

  t_weights = sim_data$transmission_weight

  ##ORDER OF ACQUISITION
  oata = sim_data %>%
    arrange(timestep_acquisition_b) %>%
    select(timestep_acquisition_b, agent)

  oa1 = oata$agent
  seed_agent = oa1[1]
  ties_vector = oata$tied_acq

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[seed_agent] = 1

  #Time of acquisition
  ta1 = oata$timestep_acquisition_b

  #weightless
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons)
  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  est_s = model_social@outputPar[2]
  est_rate = 1/model_social@outputPar[1]
  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc

  data_type="time_of_acquisition"
  model_type="weightless"
  acquisition_weightless = data.frame(data_type, model_type, sim, est_s, est_rate, aicc_social, aicc_asocial)

  #weighted
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons, weights = t_weights)
  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  est_s = model_social@outputPar[2]
  est_rate = 1/model_social@outputPar[1]
  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc

  data_type="time_of_acquisition"
  model_type="weighted"
  acquisition_weighted = data.frame(data_type, model_type, sim, est_s, est_rate, aicc_social, aicc_asocial)

  #ORDER OF PRODUCTION
  oata = sim_data %>%
    arrange(timestep_production_b) %>%
    select(timestep_production_b, agent)
  oa1 = oata$agent
  seed_agent = oa1[1]
  ties_vector = oata$tied_acq

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[seed_agent] = 1

  #Time of acquisition
  ta1 = oata$timestep_production_b

  #weightless
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons)
  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  est_s = model_social@outputPar[2]
  est_rate = 1/model_social@outputPar[1]
  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc

  data_type="time_of_first_production"
  model_type="weightless"

  production_weightless = data.frame(data_type, model_type, sim, est_s, est_rate, aicc_social, aicc_asocial)

  #weighted
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1,  demons=demons, weights = t_weights)
  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  #collect estimates and AICCs
  est_s = model_social@outputPar[2]
  est_rate = 1/model_social@outputPar[1]
  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc

  data_type="time_of_first_production"
  model_type="weighted"

  production_weighted = data.frame(data_type, model_type, sim, est_s, est_rate, aicc_social, aicc_asocial)


  temp = rbind(temp, acquisition_weighted, acquisition_weightless, production_weighted, production_weightless)
}

df_summary = df %>% group_by(EWA_rho, EWA_sigma, EWA_chi, EWA_alpha, memory_window, graph_type,sim) %>% summarize()
df_summary = left_join(df_summary,temp)
df_summary_SP = df_summary


####proportional ASOCIAL DATA####
load(file="../model_outputs/Rda_files/df_NBDA_AP.Rda")
df = df_NBDA_AP %>% ungroup()
temp <- data.frame(matrix(ncol = 7, nrow = 0))
#provide column names
colnames(temp) <- c("data_type", "model_type", "sim", "est_s", "est_rate", "aicc_social", "aicc_asocial")

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  sim=i
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_asocial_proportional/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)

  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  ##TRANSMISSION WEIGHTS
  sim_data = df %>%
    filter(sim==i) %>%
    mutate(agent=agent+1) %>%
    arrange(agent)

  t_weights = sim_data$transmission_weight

  ##ORDER OF ACQUISITION
  oata = sim_data %>%
    arrange(timestep_acquisition_b) %>%
    select(timestep_acquisition_b, agent)

  oa1 = oata$agent
  seed_agent = oa1[1]
  ties_vector = oata$tied_acq

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[seed_agent] = 1

  #Time of acquisition
  ta1 = oata$timestep_acquisition_b

  #weightless
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1,  demons=demons)
  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  est_s = model_social@outputPar[2]
  est_rate = 1/model_social@outputPar[1]
  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc

  data_type="time_of_acquisition"
  model_type="weightless"
  acquisition_weightless = data.frame(data_type, model_type, sim, est_s, est_rate, aicc_social, aicc_asocial)

  #weighted
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1,  demons=demons, weights = t_weights)
  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  est_s = model_social@outputPar[2]
  est_rate = 1/model_social@outputPar[1]
  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc

  data_type="time_of_acquisition"
  model_type="weighted"
  acquisition_weighted = data.frame(data_type, model_type, sim, est_s, est_rate, aicc_social, aicc_asocial)

  #ORDER OF PRODUCTION
  oata = sim_data %>%
    arrange(timestep_production_b) %>%
    select(timestep_production_b, agent) %>%
    mutate(tied_acq = ifelse(lead(timestep_production_b)==timestep_production_b | lag(timestep_production_b)==timestep_production_b,1,0)) %>%
    mutate(tied_acq = ifelse(is.na(tied_acq),0,tied_acq))

  oa1 = oata$agent
  seed_agent = oa1[1]
  ties_vector = oata$tied_acq

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[seed_agent] = 1

  #Time of acquisition
  ta1 = oata$timestep_production_b

  #weightless
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1,  demons=demons)
  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  est_s = model_social@outputPar[2]
  est_rate = 1/model_social@outputPar[1]
  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc

  data_type="time_of_first_production"
  model_type="weightless"

  production_weightless = data.frame(data_type, model_type, sim, est_s, est_rate, aicc_social, aicc_asocial)

  #weighted
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1,  demons=demons, weights = t_weights)
  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  #collect estimates and AICCs
  est_s = model_social@outputPar[2]
  est_rate = 1/model_social@outputPar[1]
  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc

  data_type="time_of_first_production"
  model_type="weighted"

  production_weighted = data.frame(data_type, model_type, sim, est_s, est_rate, aicc_social, aicc_asocial)


  temp = rbind(temp, acquisition_weighted, acquisition_weightless, production_weighted, production_weightless)
}

df_summary = df %>% group_by(EWA_rho, EWA_sigma, EWA_chi, EWA_alpha, memory_window, graph_type,sim) %>% summarize()
df_summary = left_join(df_summary,temp)
df_summary_AP= df_summary

df_summary_SP$condition="social transmission"
df_summary_AP$condition="asocial learning"

df_realistic = bind_rows(df_summary_SP,df_summary_AP)
df_realistic = df_realistic %>% mutate(data_type=as.factor(data_type),model_type=as.factor(model_type), condition=as.factor(condition))
save(df_realistic,file="../model_outputs/Rda_files/df_NBDA_figure_data_proportional.Rda")

detach(package:NBDA, unload=T)
detach(package:igraph, unload=T)
detach(package:sna, unload=T)
detach(package:reader, unload=T)


ggplot(df_realistic, aes(x=condition,y=aicc_asocial-aicc_social))+
  facet_grid(model_type~data_type)+
  geom_jitter()+
  geom_boxplot()
