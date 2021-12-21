library(tidyverse)
library(NBDA)
library(igraph)
library(sna)
library(magrittr)
library(reader)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####BINARY SOCIAL DATA####
load(file="../model_outputs/Rda_files/df_NBDA_SB.Rda")
df_ABM_acq_prod = df_NBDA_SB
df = df_ABM_acq_prod %>% ungroup()
temp <- data.frame(matrix(ncol = 9, nrow = 0))
#provide column names
colnames(temp) <- c('sim', 'est_s', 's_CI_lower', 's_CI_lower', 'est_rate', 'rate_CI_lower', 'rate_CI_upper', 'aicc_social', 'aicc_asocial')
pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  i=1
  print(i)
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_social_binary/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)
  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  #get order of acquisition
  oata = df %>% filter(sim==i) %>% arrange(timestep_acquisition_b) %>% select(timestep_acquisition_b,agent) %>% mutate(agent=agent+1)
  oa1 = oata$agent

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[oa1[1]] = 1
  #Time of acquisition
  ta1 = oata$timestep_acquisition_b
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons)

  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  sim=i

  est_s = model_social@outputPar[2]

  est_rate = 1/model_social@outputPar[1]

  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc


  new = data.frame(sim,est_s, s_CI_lower , s_CI_upper, est_rate, rate_CI_lower , rate_CI_upper, aicc_social, aicc_asocial)
  new
  temp = rbind(temp,new)
}

df_summary = df %>% group_by(EWA_recent_payoff_weight, EWA_soc_info_weight, EWA_conformity, memory_window, graph_type,sim) %>% summarize()
df_summary = left_join(df_summary,temp)
df_summary_SB_AC = df_summary


load(file="../model_outputs/Rda_files/df_NBDA_SB.Rda")
df_ABM_acq_prod = df_NBDA_SB
df = df_ABM_acq_prod %>% ungroup()
temp <- data.frame(matrix(ncol = 9, nrow = 0))
#provide column names
colnames(temp) <- c('sim', 'est_s', 's_CI_lower', 's_CI_lower', 'est_rate', 'rate_CI_lower', 'rate_CI_upper', 'aicc_social', 'aicc_asocial')

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_social_binary/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)
  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  #get order of acquisition
  oata = df %>% filter(sim==i) %>% arrange(timestep_production_b) %>% select(timestep_production_b,agent) %>% mutate(agent=agent+1)
  oa1 = oata$agent

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[oa1[1]] = 1
  #Time of acquisition
  ta1 = oata$timestep_production_b
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons)

  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  sim=i

  est_s = model_social@outputPar[2]
  s_CI_lower = 0  #profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[1]
  s_CI_upper = 0 # profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[2]

  est_rate = 1/model_social@outputPar[1]
  rate_CI_upper = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[1]
  rate_CI_lower = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[2]

  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc


  new = data.frame(sim,est_s, s_CI_lower , s_CI_upper, est_rate, rate_CI_lower , rate_CI_upper, aicc_social, aicc_asocial)
  new
  temp = rbind(temp,new)
}

df_summary = df %>% group_by(EWA_recent_payoff_weight, EWA_soc_info_weight, EWA_conformity, memory_window, graph_type,sim) %>% summarize()
df_summary = left_join(df_summary,temp)
df_summary_SB_FP= df_summary


####BINARY ASOCIAL DATA####
load(file="../model_outputs/Rda_files/df_NBDA_AB.Rda")
summary(df_NBDA_AB)
df_ABM_acq_prod = df_NBDA_AB
df = df_ABM_acq_prod %>% ungroup()
summary(df)
temp <- data.frame(matrix(ncol = 9, nrow = 0))
#provide column names
colnames(temp) <- c('sim', 'est_s', 's_CI_lower', 's_CI_lower', 'est_rate', 'rate_CI_lower', 'rate_CI_upper', 'aicc_social', 'aicc_asocial')

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_asocial_binary/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)
  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  #get order of acquisition
  oata = df %>% filter(sim==i) %>% arrange(timestep_acquisition_b) %>% select(timestep_acquisition_b,agent) %>% mutate(agent=agent+1)
  oa1 = oata$agent

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[oa1[1]] = 1
  #Time of acquisition
  ta1 = oata$timestep_acquisition_b
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons)

  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  sim=i

  est_s = model_social@outputPar[2]
  s_CI_lower = 0  #profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[1]
  s_CI_upper = 0 # profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[2]

  est_rate = 1/model_social@outputPar[1]
  rate_CI_upper = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[1]
  rate_CI_lower = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[2]

  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc


  new = data.frame(sim,est_s, s_CI_lower , s_CI_upper, est_rate, rate_CI_lower , rate_CI_upper, aicc_social, aicc_asocial)
  new
  temp = rbind(temp,new)
}

df_summary = df %>% group_by(EWA_recent_payoff_weight, EWA_soc_info_weight, EWA_conformity, memory_window, graph_type,sim) %>% summarize()
df_summary = left_join(df_summary,temp)
df_summary_AB_AC = df_summary

load(file="../model_outputs/Rda_files/df_NBDA_AB.Rda")
summary(df_NBDA_AB)
df_ABM_acq_prod = df_NBDA_AB
df = df_ABM_acq_prod %>% ungroup()
summary(df)
temp <- data.frame(matrix(ncol = 9, nrow = 0))
#provide column names
colnames(temp) <- c('sim', 'est_s', 's_CI_lower', 's_CI_lower', 'est_rate', 'rate_CI_lower', 'rate_CI_upper', 'aicc_social', 'aicc_asocial')

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_asocial_binary/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)
  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  #get order of acquisition
  oata = df %>% filter(sim==i) %>% arrange(timestep_production_b) %>% select(timestep_production_b,agent) %>% mutate(agent=agent+1)
  oa1 = oata$agent

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[oa1[1]] = 1
  #Time of acquisition
  ta1 = oata$timestep_production_b
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons)

  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  sim=i

  est_s = model_social@outputPar[2]
  s_CI_lower = 0  #profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[1]
  s_CI_upper = 0 # profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[2]

  est_rate = 1/model_social@outputPar[1]
  rate_CI_upper = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[1]
  rate_CI_lower = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[2]

  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc


  new = data.frame(sim,est_s, s_CI_lower , s_CI_upper, est_rate, rate_CI_lower , rate_CI_upper, aicc_social, aicc_asocial)
  new
  temp = rbind(temp,new)
}

df_summary = df %>% group_by(EWA_recent_payoff_weight, EWA_soc_info_weight, EWA_conformity, memory_window, graph_type,sim) %>% summarize()
df_summary = left_join(df_summary,temp)
df_summary_AB_FP= df_summary


df_summary_SB_AC$feeder_data="acquisition"
df_summary_SB_FP$feeder_data="first production"
df_summary_SB_AC$diffusion="social diffusion"
df_summary_SB_FP$diffusion="social diffusion"
df_summary_AB_AC$feeder_data="acquisition"
df_summary_AB_FP$feeder_data="first production"
df_summary_AB_AC$diffusion="asocial diffusion"
df_summary_AB_FP$diffusion="asocial diffusion"

df = bind_rows(df_summary_AB_AC,df_summary_AB_FP,df_summary_SB_AC,df_summary_SB_FP)
save(df,file="../model_outputs/Rda_files/NBDA_figure_data_binary.Rda")

####PROPORTIONAL SOCIAL DATA####
load(file="../model_outputs/Rda_files/df_NBDA_SP.Rda")
df = df_NBDA_SP %>% ungroup()
summary(df)
temp <- data.frame(matrix(ncol = 9, nrow = 0))
#provide column names
colnames(temp) <- c('sim', 'est_s', 's_CI_lower', 's_CI_lower', 'est_rate', 'rate_CI_lower', 'rate_CI_upper', 'aicc_social', 'aicc_asocial')

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_social_proportional/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)
  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  #get order of acquisition
  oata = df %>% filter(sim==i) %>% arrange(timestep_acquisition_b) %>% select(timestep_acquisition_b,agent) %>% mutate(agent=agent+1)
  oa1 = oata$agent

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[oa1[1]] = 1
  #Time of acquisition
  ta1 = oata$timestep_acquisition_b
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons)

  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  sim=i

  est_s = model_social@outputPar[2]
  s_CI_lower = 0  #profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[1]
  s_CI_upper = 0 # profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[2]

  est_rate = 1/model_social@outputPar[1]
  rate_CI_upper = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[1]
  rate_CI_lower = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[2]

  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc


  new = data.frame(sim,est_s, s_CI_lower , s_CI_upper, est_rate, rate_CI_lower , rate_CI_upper, aicc_social, aicc_asocial)
  temp = rbind(temp,new)
}

df_summary = df %>% group_by(EWA_recent_payoff_weight, EWA_soc_info_weight, EWA_conformity, EWA_tau, memory_window, graph_type,sim) %>% summarize()
df_summary = left_join(df_summary,temp)
df_summary_SP_AC = df_summary

load(file="../model_outputs/Rda_files/df_NBDA_SP.Rda")
df = df_NBDA_SP %>% ungroup()
temp <- data.frame(matrix(ncol = 9, nrow = 0))
#provide column names
colnames(temp) <- c('sim', 'est_s', 's_CI_lower', 's_CI_lower', 'est_rate', 'rate_CI_lower', 'rate_CI_upper', 'aicc_social', 'aicc_asocial')

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_social_proportional/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)
  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  #get order of acquisition
  oata = df %>% filter(sim==i) %>% arrange(timestep_production_b) %>% select(timestep_production_b,agent) %>% mutate(agent=agent+1)
  oa1 = oata$agent

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[oa1[1]] = 1
  #Time of acquisition
  ta1 = oata$timestep_production_b
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons)

  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  sim=i

  est_s = model_social@outputPar[2]
  s_CI_lower = 0  #profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[1]
  s_CI_upper = 0 # profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[2]

  est_rate = 1/model_social@outputPar[1]
  rate_CI_upper = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[1]
  rate_CI_lower = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[2]

  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc


  new = data.frame(sim,est_s, s_CI_lower , s_CI_upper, est_rate, rate_CI_lower , rate_CI_upper, aicc_social, aicc_asocial)
  temp = rbind(temp,new)
}

df_summary = df %>% group_by(EWA_recent_payoff_weight, EWA_soc_info_weight, EWA_conformity, EWA_tau, memory_window, graph_type,sim) %>% summarize()
df_summary = left_join(df_summary,temp)
df_summary_SP_FP= df_summary

####proportional ASOCIAL DATA####
load(file="../model_outputs/Rda_files/df_NBDA_AP.Rda")
df = df_NBDA_AP %>% ungroup()
temp <- data.frame(matrix(ncol = 9, nrow = 0))
#provide column names
colnames(temp) <- c('sim', 'est_s', 's_CI_lower', 's_CI_lower', 'est_rate', 'rate_CI_lower', 'rate_CI_upper', 'aicc_social', 'aicc_asocial')

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_asocial_proportional/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)
  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  #get order of acquisition
  oata = df %>% filter(sim==i) %>% arrange(timestep_acquisition_b) %>% select(timestep_acquisition_b,agent) %>% mutate(agent=agent+1)
  oa1 = oata$agent

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[oa1[1]] = 1
  #Time of acquisition
  ta1 = oata$timestep_acquisition_b
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons)

  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  sim=i

  est_s = model_social@outputPar[2]
  s_CI_lower = 0  #profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[1]
  s_CI_upper = 0 # profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[2]

  est_rate = 1/model_social@outputPar[1]
  rate_CI_upper = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[1]
  rate_CI_lower = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[2]

  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc


  new = data.frame(sim,est_s, s_CI_lower , s_CI_upper, est_rate, rate_CI_lower , rate_CI_upper, aicc_social, aicc_asocial)
  temp = rbind(temp,new)
}

df_summary = df %>% group_by(EWA_recent_payoff_weight, EWA_soc_info_weight, EWA_conformity, EWA_tau, memory_window, graph_type,sim) %>% summarize()
df_summary = left_join(df_summary,temp)
df_summary_AP_AC = df_summary

load(file="../model_outputs/Rda_files/df_NBDA_AP.Rda")
df = df_NBDA_AP %>% ungroup()
temp <- data.frame(matrix(ncol = 9, nrow = 0))
#provide column names
colnames(temp) <- c('sim', 'est_s', 's_CI_lower', 's_CI_lower', 'est_rate', 'rate_CI_lower', 'rate_CI_upper', 'aicc_social', 'aicc_asocial')

pop_size = max(df$agent+1)

#Read in the csv file containing the social network, converting it to a matrix
for (i in unique(df$sim)) {
  print(i)
  edgelist <- read.table(paste0("../model_outputs/csvs_raw/adjlists_data/NBDA_asocial_proportional/adjlist_sim_",i,".txt"))
  edgelist=edgelist[1:2]+1
  edgelist=as.matrix(edgelist)
  G <- graph.edgelist(edgelist,directed=FALSE);
  socNet1 <- as_adjacency_matrix(G,type="both",names=TRUE,sparse=FALSE)
  socNet1<-array(socNet1,dim=c(pop_size,pop_size,1))

  #get order of acquisition
  oata = df %>% filter(sim==i) %>% arrange(timestep_production_b) %>% select(timestep_production_b,agent) %>% mutate(agent=agent+1)
  oa1 = oata$agent

  #exclude demonstrators
  demons<-rep(0,pop_size)
  demons[oa1[1]] = 1
  #Time of acquisition
  ta1 = oata$timestep_production_b
  nbdaData_cTADA<-nbdaData(label="Sim_0",assMatrix=socNet1, orderAcq = oa1, timeAcq = ta1, endTime = max(ta1) + 1, demons=demons)

  model_social<-tadaFit(nbdaData_cTADA, type="social", baseline = "constant", standardErrors = F)
  model_asocial<-tadaFit(nbdaData_cTADA, type="asocial", baseline = "constant", standardErrors = F)

  sim=i

  est_s = model_social@outputPar[2]
  s_CI_lower = 0  #profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[1]
  s_CI_upper = 0 # profLikCI(which=1,model=model_social,lowerRange=c(0,5),upperRange=c(5,10))[2]

  est_rate = 1/model_social@outputPar[1]
  rate_CI_upper = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[1]
  rate_CI_lower = 0 #profLikCI(which=0,model=model_social,lowerRange=c(0,1000),upperRange = c(1000,2000))[2]

  aicc_social = model_social@aicc
  aicc_asocial = model_asocial@aicc


  new = data.frame(sim,est_s, s_CI_lower , s_CI_upper, est_rate, rate_CI_lower , rate_CI_upper, aicc_social, aicc_asocial)
  temp = rbind(temp,new)
}

df_summary = df %>% group_by(EWA_recent_payoff_weight, EWA_soc_info_weight, EWA_conformity, EWA_tau, memory_window, graph_type,sim) %>% summarize()
df_summary = left_join(df_summary,temp)
df_summary_AP_FP= df_summary


df_summary_SP_AC$feeder_data="acquisition"
df_summary_SP_FP$feeder_data="first production"
df_summary_SP_AC$diffusion="social diffusion"
df_summary_SP_FP$diffusion="social diffusion"
df_summary_AP_AC$feeder_data="acquisition"
df_summary_AP_FP$feeder_data="first production"
df_summary_AP_AC$diffusion="asocial diffusion"
df_summary_AP_FP$diffusion="asocial diffusion"

df = bind_rows(df_summary_AP_AC,df_summary_AP_FP,df_summary_SP_AC,df_summary_SP_FP)
save(df,file="../model_outputs/Rda_files/NBDA_figure_data_proportional.Rda")
