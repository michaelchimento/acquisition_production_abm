library(tidyverse)
library(knitr)
library(kableExtra)
library(rethinking)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod.Rda")
df = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

df %>% ungroup() %>% filter(order_acquisition!=1) %>% summarize(divergence = sum(order_acquisition==order_production)/n())

df = df %>% ungroup() %>% group_by(sim, graph_type, NBDA_s_param, memory_window, EWA_soc_info_weight, EWA_recent_payoff_weight, EWA_tau, EWA_conformity) %>% summarize(TTD=max(timestep_acquisition_b), TTFP=max(timestep_production_b), avgdelta=mean(delta), manhattan_delay=sum(abs(delta)), manhattan_divergence=sum(abs(order_acquisition-order_production)), divergence = 1-sum(order_acquisition==order_production)/n())

df %>% ungroup() %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), meanTTFP=mean(TTFP), lowerCI_TTFP=HPDI(TTFP)[1], upperCI_TTFP=HPDI(TTFP)[2], mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2])

####3.1 effect of network architecture####
df %>%
  filter(NBDA_s_param==5, memory_window==10, EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", EWA_conformity==1) %>%
  group_by(graph_type) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[graph_type=="random regular"])


####3.1 effect of social learning rate####
df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", EWA_conformity==1) %>%
  group_by(NBDA_s_param) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[NBDA_s_param==5])


#### 3.2 Sigma ####
df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", EWA_conformity==1, NBDA_s_param==5) %>%
  group_by(EWA_soc_info_weight) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[EWA_soc_info_weight=="medium"])

#### 3.2 Production conformity ####
df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", NBDA_s_param==5) %>%
  group_by(EWA_conformity) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[EWA_conformity==1])

154/126

df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_soc_info_weight=="strong", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", NBDA_s_param==5) %>%
  group_by(EWA_conformity) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[EWA_conformity==1])

326/216

#### 3.2 Memory window ####
df %>%
  filter(graph_type=="random regular", EWA_conformity==1, EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", NBDA_s_param==5) %>%
  group_by(memory_window) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[memory_window==10])

#### 3.2 Phi ####
df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_conformity==1, EWA_soc_info_weight=="medium", EWA_tau=="non-conservative", NBDA_s_param==5) %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[EWA_recent_payoff_weight=="medium"])

load(file="../model_outputs/Rda_files/df_GEN_fullweight_acq_prod.Rda")
df_fw = df_fullweight_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_fw = df_fw %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_fw = df_fw %>% ungroup() %>% group_by(sim, graph_type, NBDA_s_param, memory_window, EWA_soc_info_weight, EWA_recent_payoff_weight, EWA_tau, EWA_conformity) %>% summarize(TTD=max(timestep_acquisition_b), TTFP=max(timestep_production_b), avgdelta=mean(delta), manhattan_delay=sum(abs(delta)), manhattan_divergence=sum(abs(order_acquisition-order_production)), divergence = 1-sum(order_acquisition==order_production)/n())
df_fw %>%
  filter(graph_type=="random regular", memory_window==10, EWA_conformity==1, EWA_soc_info_weight==0.5, EWA_tau==1, NBDA_s_param==5) %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[EWA_recent_payoff_weight==0.5])

#### 3.2 Tau ####
df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_conformity==1, EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", NBDA_s_param==5) %>%
  group_by(EWA_tau) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=HPDI(TTD)[1], upperCI_TTD=HPDI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=HPDI(manhattan_divergence)[1], upperCI_div=HPDI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=HPDI(manhattan_delay)[1], upperCI_del=HPDI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[EWA_tau=="non-conservative"])

249/126

