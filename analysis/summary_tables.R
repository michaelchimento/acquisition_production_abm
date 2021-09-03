library(tidyverse)
library(knitr)
library(kableExtra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Overview of graph type, inverse temperature ####
load(file="../concat_data/df_ABM_equiv.Rda")


df_ABM_equiv_payoff %>%
  filter(full_diffusion==T, conformity=="None") %>%
  group_by(graph_type) %>%
  summarize(median=median(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep), mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[graph_type=="random regular"])

#### Table S1: Network architecture across conformity conditions ####
t1 = df_ABM_equiv_payoff %>%
  filter(full_diffusion==T) %>%
  group_by(conformity,graph_type) %>%
  summarize(mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% ungroup() %>% group_by(conformity) %>% mutate(ref_comparison = mean/mean[graph_type=="random regular"])

kable(t1[2:ncol(t1)],
      booktabs = TRUE,
      col.names = c('graph type', 'mean', 'CV', 'reference comparison'),
      align = "rccc",
      digits=2,
      format="latex") %>%
  kable_styling(full_width = F) %>%
  pack_rows(index=c("None"=4,"Conformity of social influence"=4,"Conformity of social learning"=4,"Conformity of SI & SL"=4))


#### Sigma effect ####
df_ABM_equiv_payoff %>%
  filter(full_diffusion==T, conformity=="None") %>%
  group_by(EWA_soc_info_weight) %>%
  summarize(median=median(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep), mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_soc_info_weight=="medium"])

#### Table S2 ####
t2 = df_ABM_equiv_payoff %>%
  filter(full_diffusion==T, conformity=="None") %>%
  group_by(EWA_tau,EWA_soc_info_weight) %>%
  summarize(mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% ungroup() %>% group_by(EWA_tau) %>% mutate(ref_comparison = mean/mean[EWA_soc_info_weight=="medium"])

kable(t2[2:ncol(t2)],
      booktabs = TRUE,
      col.names = c('Social info. bias', 'mean', 'CV', 'reference comparison'),
      align = "rccc",
      digits=2,
      format="latex") %>%
  kable_styling(full_width = F) %>%
  pack_rows(index=c("Non-conservative"=3,"conservative"=3))

#### Phi effect ####
df_ABM_equiv_payoff %>%
  filter(full_diffusion==T, conformity=="None") %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(median=median(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep), mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_recent_payoff_weight=="weak"])

load(file="../concat_data/df_ABM_equiv_fullweights.Rda")
df_ABM_equiv_payoff %>%
  filter(full_diffusion==T, conformity=="None") %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(median=median(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep), mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_recent_payoff_weight=="weak"])


####Memory and conformity####
load(file="../concat_data/df_ABM_memory.Rda")
load(file="../concat_data/df_ABM_equiv.Rda")
df_baseline = df_ABM_equiv_payoff %>% filter(EWA_soc_info_weight=="medium",EWA_recent_payoff_weight=="medium")

df_memory = bind_rows(df_memory,df_baseline)

summary(df_memory)

df_memory %>% filter(full_diffusion==T) %>%
  group_by(memory_window) %>%
  summarize(mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[memory_window==1])

df_memory %>% filter(full_diffusion==T) %>%
  group_by(NBDA_conformity,memory_window) %>%
  summarize(mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% ungroup() %>% group_by(NBDA_conformity) %>% mutate(ratio = mean/mean[memory_window==1])

df_memory %>% filter(full_diffusion==T) %>%
  group_by(EWA_conformity,memory_window) %>%
  summarize(mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% ungroup() %>% group_by(EWA_conformity) %>% mutate(ratio = mean/mean[memory_window==1])

df = df_memory %>% filter(full_diffusion==T) %>%
  group_by(EWA_conformity,memory_window) %>%
  summarize(median=median(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep), mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% ungroup()

df %>% group_by(EWA_conformity) %>% mutate(ratio = mean/mean[memory_window==1])

df_memory %>% filter(full_diffusion==T,graph_type=="small world") %>%
  group_by(conformity,memory_window) %>%
  summarize(median=median(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep), mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% ungroup()


####Asocial learning and conformity####
load(file="../concat_data/df_ABM_equiv_asocial_sparam.Rda")
load(file="../concat_data/df_ABM_equiv.Rda")

df_baseline = df_ABM_equiv_payoff %>% filter(EWA_soc_info_weight=="medium",EWA_recent_payoff_weight=="medium") %>% mutate(asocial=FALSE)

df_baseline2 = df_ABM_asocial %>% filter(NBDA_s_param==100) %>% mutate(asocial=TRUE)

df = bind_rows(df_baseline,df_baseline2)

summary(df)
df %>% filter(full_diffusion==T) %>%
  group_by(asocial) %>%
  summarize(median=median(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep), mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% ungroup()

df_ABM_asocial %>%
  filter(full_diffusion==T, EWA_conformity==1) %>%
  group_by(NBDA_s_param, NBDA_conformity) %>%
  summarize(median=median(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep), mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% ungroup() %>% group_by(NBDA_s_param) %>% mutate(ratio = mean/mean[NBDA_conformity==1])

df_ABM_asocial %>%
  filter(full_diffusion==T, NBDA_conformity==1) %>%
  group_by(NBDA_s_param, EWA_conformity) %>%
  summarize(median=median(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep), mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% ungroup() %>% group_by(NBDA_s_param) %>% mutate(ratio = mean/mean[EWA_conformity==1])

df = df_ABM_asocial %>%
  filter(full_diffusion==T) %>%
  group_by(NBDA_s_param, conformity) %>%
  summarize(median=median(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep), mean=mean(timestep),CV= sd(timestep)/mean(timestep)) %>% ungroup()

df %>% group_by(NBDA_s_param) %>% mutate(ratio = mean/mean[conformity=="None"])



