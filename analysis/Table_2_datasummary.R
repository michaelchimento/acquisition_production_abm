library(tidyverse)
library(knitr)
library(kableExtra)
library(rethinking)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod.Rda")
df = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

df %>% ungroup() %>% filter(order_acquisition!=1) %>% summarize(divergence = sum(order_acquisition==order_production)/n())

df = df %>% ungroup() %>% group_by(sim, graph_type, NBDA_s_param, memory_window, EWA_soc_info_weight, EWA_recent_payoff_weight, EWA_tau, EWA_conformity) %>% summarize(TTD=max(timestep_acquisition_b), TTFP=max(timestep_production_b), divergence = 1-sum(order_acquisition==order_production)/n(), manhattan_divergence=sum(abs(order_acquisition-order_production))/23, manhattan_delay=sum(abs(delta))/24)

####3.1 effect of network architecture####
t1 = df %>%
  filter(NBDA_s_param==5, memory_window==10, EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", EWA_conformity==1) %>%
  group_by(graph_type) %>%
  summarize(variable = "network architecture", meanTTD=mean(TTD), CI_TTD= paste0("[",round(HPDI(TTD)[1],2), ",", upperCI_TTD=round(HPDI(TTD)[2],2),"]"), meanTTFP=mean(TTFP), CI_TTFP= paste0("[",round(HPDI(TTFP)[1],2), ",", upperCI_TTD=round(HPDI(TTFP)[2],2),"]"), mean_perc_div = mean(divergence), mean_d_order = mean(manhattan_divergence), CI_d_order= paste0("[",round(HPDI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_divergence)[2],2),"]"), mean_d_time = mean(manhattan_delay), CI_d_time= paste0("[",round(HPDI(manhattan_delay)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_delay)[2],2),"]"),) %>% rename(value= graph_type) %>% mutate(value=as.character(value))


####3.1 effect of social learning rate####
t2 = df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", EWA_conformity==1) %>%
  group_by(NBDA_s_param) %>%
  summarize(variable = "social learning rate", meanTTD=mean(TTD), CI_TTD= paste0("[",round(HPDI(TTD)[1],2), ",", upperCI_TTD=round(HPDI(TTD)[2],2),"]"), meanTTFP=mean(TTFP), CI_TTFP= paste0("[",round(HPDI(TTFP)[1],2), ",", upperCI_TTD=round(HPDI(TTFP)[2],2),"]"), mean_perc_div = mean(divergence), mean_d_order = mean(manhattan_divergence), CI_d_order= paste0("[",round(HPDI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_divergence)[2],2),"]"), mean_d_time = mean(manhattan_delay), CI_d_time= paste0("[",round(HPDI(manhattan_delay)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_delay)[2],2),"]"),) %>% rename(value= NBDA_s_param) %>% mutate(value=as.character(value))


#### 3.2 Sigma ####
t3 = df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", EWA_conformity==1, NBDA_s_param==5) %>%
  group_by(EWA_soc_info_weight) %>%
  summarize(variable = "social info. bias", meanTTD=mean(TTD), CI_TTD= paste0("[",round(HPDI(TTD)[1],2), ",", upperCI_TTD=round(HPDI(TTD)[2],2),"]"), meanTTFP=mean(TTFP), CI_TTFP= paste0("[",round(HPDI(TTFP)[1],2), ",", upperCI_TTD=round(HPDI(TTFP)[2],2),"]"), mean_perc_div = mean(divergence), mean_d_order = mean(manhattan_divergence), CI_d_order= paste0("[",round(HPDI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_divergence)[2],2),"]"), mean_d_time = mean(manhattan_delay), CI_d_time= paste0("[",round(HPDI(manhattan_delay)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_delay)[2],2),"]"),) %>% rename(value= EWA_soc_info_weight) %>% mutate(value=as.character(value))

#### 3.2 Production conformity ####
t4 = df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", NBDA_s_param==5) %>%
  group_by(EWA_conformity) %>%
  summarize(variable = "production conformity", meanTTD=mean(TTD), CI_TTD= paste0("[",round(HPDI(TTD)[1],2), ",", upperCI_TTD=round(HPDI(TTD)[2],2),"]"), meanTTFP=mean(TTFP), CI_TTFP= paste0("[",round(HPDI(TTFP)[1],2), ",", upperCI_TTD=round(HPDI(TTFP)[2],2),"]"), mean_perc_div = mean(divergence), mean_d_order = mean(manhattan_divergence), CI_d_order= paste0("[",round(HPDI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_divergence)[2],2),"]"), mean_d_time = mean(manhattan_delay), CI_d_time= paste0("[",round(HPDI(manhattan_delay)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_delay)[2],2),"]"),) %>% rename(value= EWA_conformity) %>% mutate(value=as.character(value))

df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_soc_info_weight=="strong", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", NBDA_s_param==5) %>%
  group_by(EWA_conformity) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[EWA_conformity==1])

#### 3.2 Memory window ####
t5 = df %>%
  filter(graph_type=="random regular", EWA_conformity==1, EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", NBDA_s_param==5) %>%
  group_by(memory_window) %>%
  summarize(variable = "memory window", meanTTD=mean(TTD), CI_TTD= paste0("[",round(HPDI(TTD)[1],2), ",", upperCI_TTD=round(HPDI(TTD)[2],2),"]"), meanTTFP=mean(TTFP), CI_TTFP= paste0("[",round(HPDI(TTFP)[1],2), ",", upperCI_TTD=round(HPDI(TTFP)[2],2),"]"), mean_perc_div = mean(divergence), mean_d_order = mean(manhattan_divergence), CI_d_order= paste0("[",round(HPDI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_divergence)[2],2),"]"), mean_d_time = mean(manhattan_delay), CI_d_time= paste0("[",round(HPDI(manhattan_delay)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_delay)[2],2),"]"),) %>% rename(value= memory_window) %>% mutate(value=as.character(value))

#### 3.2 Phi ####
t6 = df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_conformity==1, EWA_soc_info_weight=="medium", EWA_tau=="non-conservative", NBDA_s_param==5) %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(variable = "recent experience bias", meanTTD=mean(TTD), CI_TTD= paste0("[",round(HPDI(TTD)[1],2), ",", upperCI_TTD=round(HPDI(TTD)[2],2),"]"), meanTTFP=mean(TTFP), CI_TTFP= paste0("[",round(HPDI(TTFP)[1],2), ",", upperCI_TTD=round(HPDI(TTFP)[2],2),"]"), mean_perc_div = mean(divergence), mean_d_order = mean(manhattan_divergence), CI_d_order= paste0("[",round(HPDI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_divergence)[2],2),"]"), mean_d_time = mean(manhattan_delay), CI_d_time= paste0("[",round(HPDI(manhattan_delay)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_delay)[2],2),"]"),) %>% rename(value= EWA_recent_payoff_weight)%>% mutate(value=as.character(value))

t6

load(file="../model_outputs/Rda_files/df_GEN_fullweight_acq_prod.Rda")
df_fw = df_fullweight_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_fw = df_fw %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_fw = df_fw %>% ungroup() %>% group_by(sim, graph_type, NBDA_s_param, memory_window, EWA_soc_info_weight, EWA_recent_payoff_weight, EWA_tau, EWA_conformity) %>% summarize(TTD=max(timestep_acquisition_b), TTFP=max(timestep_production_b), avgdelta=mean(delta), manhattan_delay=sum(abs(delta)), manhattan_divergence=sum(abs(order_acquisition-order_production)), divergence = 1-sum(order_acquisition==order_production)/n())
df_fw %>%
  filter(graph_type=="random regular", memory_window==10, EWA_conformity==1, EWA_soc_info_weight==0.5, EWA_tau==1, NBDA_s_param==5) %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[EWA_recent_payoff_weight==0.5])

#### 3.2 Tau ####
t7 = df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_conformity==1, EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", NBDA_s_param==5) %>%
  group_by(EWA_tau) %>%
  summarize(variable = "conservatism bias", meanTTD=mean(TTD), CI_TTD= paste0("[",round(HPDI(TTD)[1],2), ",", upperCI_TTD=round(HPDI(TTD)[2],2),"]"), meanTTFP=mean(TTFP), CI_TTFP= paste0("[",round(HPDI(TTFP)[1],2), ",", upperCI_TTD=round(HPDI(TTFP)[2],2),"]"), mean_perc_div = mean(divergence), mean_d_order = mean(manhattan_divergence), CI_d_order= paste0("[",round(HPDI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_divergence)[2],2),"]"), mean_d_time = mean(manhattan_delay), CI_d_time= paste0("[",round(HPDI(manhattan_delay)[1],2), ",", upperCI_TTD=round(HPDI(manhattan_delay)[2],2),"]"),) %>% rename(value= EWA_tau) %>% mutate(value=as.character(value))
t7
t = bind_rows(t1,t2, t6,t7, t4, t3,t5)  %>% relocate(variable)

t = t %>% mutate(mean_perc_div = round(mean_perc_div,2), mean_d_order = round(mean_d_order,2), mean_d_time = round(mean_d_time,2))


kable(t[2:ncol(t)],booktabs = TRUE,
      col.names = c('value', 'mean TTD', 'CI', 'mean TTFP', 'CI', '% divergent', 'mean $d_order$', 'CI', 'mean $d_time$', "CI"),
      align = "rccccccccc",
      format="latex") %>%
  kable_styling(full_width = F) %>%
  pack_rows(index=c("network architecture"=4, "social learning rate"=3, "recent experience bias"=3, "conservatism bias"=2, "production conformity"=2, "social info. bias"=3, "memory window"=3))

kable(t[2:ncol(t)],booktabs = TRUE,
      col.names = c('value', 'mean', 'CI', 'mean', 'CI', '% divergent', 'mean', 'CI', 'mean', "CI"),
      align = "rccccccccc",
      format="latex") %>%
  kable_styling(full_width = F,  latex_options = c("scale_down")) %>%
  pack_rows(index=c("network"=4, "s"=3, "phi"=3, "tau"=2, "f_P"=2, "sigma"=3, "memory"=3))%>%
  add_header_above(c(" "=1,"TTD"=2,"TTFP"=2,"$d_{order}$"=3, "$d_{time}$"=2))

