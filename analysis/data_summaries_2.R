library(tidyverse)
library(knitr)
library(kableExtra)
library(rethinking)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####3.2 effect of network architecture####
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod2.Rda")

df = df_equiv_payoffs_acq_prod %>% group_by(sim, graph_type, NBDA_s_param, memory_window, EWA_soc_info_weight, EWA_recent_payoff_weight, EWA_tau, EWA_conformity) %>% summarize(timestep=max(timestep_acquisition_b))

df %>%
  filter(conformity=="None") %>%
  group_by(graph_type) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[graph_type=="random regular"])

#### Table S1: Network architecture across conformity conditions ####
t1 = df %>%
  group_by(conformity,graph_type) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[graph_type=="random regular"])
t1
kable(t1[2:ncol(t1)],
      booktabs = TRUE,
      col.names = c('graph type', 'mean', 'CI low', 'CI high', 'CV', 'reference comparison'),
      align = "rccc",
      digits=2,
      format="latex") %>%
  kable_styling(full_width = F) %>%
  pack_rows(index=c("None"=4,"Production conformity"=4,"Transmission conformity"=4,"Both"=4))


#### 3.2 Sigma ####

#sigma TTD
df %>%
  filter(EWA_conformity==1, EWA_tau=="non-conservative", EWA_recent_payoff_weight=="medium", NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_soc_info_weight) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_soc_info_weight=="medium"])

#divergence
df_div = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_div %>%
  ungroup() %>%
  filter(order_acquisition!=1, EWA_conformity==1, EWA_tau=="non-conservative", EWA_recent_payoff_weight=="medium", NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_soc_info_weight) %>%
  summarize(n= 1-sum(order_acquisition==order_production)/n())

#sigma delta
df_delta = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b)
df_delta %>%
  filter(EWA_conformity==1, EWA_tau=="non-conservative", EWA_recent_payoff_weight=="medium", NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_soc_info_weight) %>%
  summarize(mean=mean(delta), lowerCI=HPDI(delta)[1], upperCI=HPDI(delta)[2], CV= sd(delta)/mean(delta)) %>% mutate(ref_comparison = mean/mean[EWA_soc_info_weight=="medium"])

#### 3.2 Production conformity ####

#TTD
df %>%
  filter(EWA_tau=="non-conservative", EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium",  NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_conformity) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_conformity==1])

#divergence
df_div = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_div %>%
  ungroup() %>%
  filter(order_acquisition!=1, EWA_tau=="non-conservative", EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium",  NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_conformity) %>%
  summarize(n= 1-sum(order_acquisition==order_production)/n())

#delta
df_delta = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b)
df_delta %>%
  filter(EWA_tau=="non-conservative", EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium",  NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_conformity) %>%
  summarize(mean=mean(delta), lowerCI=HPDI(delta)[1], upperCI=HPDI(delta)[2], CV= sd(delta)/mean(delta)) %>% mutate(ref_comparison = mean/mean[EWA_conformity==1])

#strong sigma
#TTD
df %>%
  filter(EWA_tau=="non-conservative", EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="strong",  NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_conformity) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_conformity==1])

#divergence
df_div = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_div %>%
  ungroup() %>%
  filter(order_acquisition!=1, EWA_tau=="non-conservative", EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="strong",  NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_conformity) %>%
  summarize(n= 1-sum(order_acquisition==order_production)/n())

#delta
df_delta = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b)
df_delta %>%
  filter(EWA_tau=="non-conservative", EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="strong",  NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_conformity) %>%
  summarize(mean=mean(delta), lowerCI=HPDI(delta)[1], upperCI=HPDI(delta)[2], CV= sd(delta)/mean(delta)) %>% mutate(ref_comparison = mean/mean[EWA_conformity==1])

#### 3.2 Conservatism ####

#TTD
df %>%
  filter(EWA_conformity==1, EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium",  NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_tau) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_tau=="non-conservative"])

#divergence
df_div = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_div %>%
  ungroup() %>%
  filter(order_acquisition!=1, EWA_conformity==1, EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium",  NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_tau) %>%
  summarize(n= 1-sum(order_acquisition==order_production)/n())

#delta
df_delta = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b)
df_delta %>%
  filter(EWA_conformity==1, EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium",  NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_tau) %>%
  summarize(mean=mean(delta), lowerCI=HPDI(delta)[1], upperCI=HPDI(delta)[2], CV= sd(delta)/mean(delta)) %>% mutate(ref_comparison = mean/mean[EWA_tau=="non-conservative"])

#### 3.2 Memory ####

#TTD
df %>%
  filter(EWA_conformity==1, EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium", EWA_tau=="non-conservative",  NBDA_s_param==5) %>%
  group_by(memory_window) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[memory_window==10])

#divergence
df_div = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_div %>%
  ungroup() %>%
  filter(order_acquisition!=1, EWA_conformity==1, EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium", EWA_tau=="non-conservative",  NBDA_s_param==5) %>%
  group_by(memory_window) %>%
  summarize(n= 1-sum(order_acquisition==order_production)/n())

#delta
df_delta = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b)
df_delta %>%
  filter(EWA_conformity==1, EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium", EWA_tau=="non-conservative",  NBDA_s_param==5) %>%
  group_by(memory_window) %>%
  summarize(mean=mean(delta), lowerCI=HPDI(delta)[1], upperCI=HPDI(delta)[2], CV= sd(delta)/mean(delta)) %>% mutate(ref_comparison = mean/mean[memory_window==10])

#### 3.2 Phi ####

#TTD
df %>%
  filter(EWA_conformity==1, EWA_soc_info_weight=="medium", EWA_tau=="non-conservative",  NBDA_s_param==5,memory_window==10) %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_recent_payoff_weight=="medium"])

#divergence
df_div = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_div %>%
  ungroup() %>%
  filter(order_acquisition!=1, EWA_conformity==1, EWA_soc_info_weight=="medium", EWA_tau=="non-conservative",  NBDA_s_param==5, memory_window==10) %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(n= 1-sum(order_acquisition==order_production)/n())

#delta
df_delta = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b)
df_delta %>%
  filter(EWA_conformity==1, EWA_soc_info_weight=="medium", EWA_tau=="non-conservative",  NBDA_s_param==5, memory_window==10) %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(mean=mean(delta), lowerCI=HPDI(delta)[1], upperCI=HPDI(delta)[2], CV= sd(delta)/mean(delta)) %>% mutate(ref_comparison = mean/mean[EWA_recent_payoff_weight=="medium"])


#Phi TTD
df %>%
  filter(conformity=="None") %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_recent_payoff_weight=="weak"])

load(file="../model_outputs/Rda_files/df_GEN_fullweights_acq_prod.Rda")
df_fullweight = df_fullweight_acq_prod %>% filter(NBDA_s_param==10) %>% group_by(sim, graph_type, NBDA_s_param, EWA_soc_info_weight, EWA_recent_payoff_weight,conformity) %>% summarize(timestep=max(timestep_acquisition_b))

df_fullweight %>%
  filter(conformity=="None") %>%
  group_by(EWA_recent_payoff_weight) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_recent_payoff_weight=="weak"])


####3.4 conformity####
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod.Rda")

df = df_equiv_payoffs_acq_prod %>% filter(NBDA_s_param==10) %>% group_by(sim, graph_type, NBDA_s_param, EWA_soc_info_weight, EWA_recent_payoff_weight,EWA_conformity, NBDA_conformity, conformity) %>% summarize(timestep=max(timestep_acquisition_b))

df %>%
  group_by(conformity) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[conformity=="None"])

df %>%
  group_by(EWA_soc_info_weight,EWA_conformity) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[EWA_conformity==1])

df %>%
  group_by(EWA_soc_info_weight,NBDA_conformity) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[NBDA_conformity==1])


df_delta = df_equiv_payoffs_acq_prod %>% filter(NBDA_s_param==10) %>% mutate(delta=timestep_production_b - timestep_acquisition_b)

df_delta %>%
  group_by(conformity) %>%
  summarize(mean=mean(delta), lowerCI=HPDI(delta)[1], upperCI=HPDI(delta)[2], CV= sd(delta)/mean(delta)) %>% mutate(ref_comparison = mean/mean[conformity=="None"])

df_delta_trans = df_equiv_payoffs_acq_prod %>% filter(NBDA_s_param==10) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(delta=timestep_acquisition_b-lag(timestep_acquisition_b)) %>% filter(!is.na(delta))

df_delta_trans %>%
  group_by(NBDA_conformity) %>%
  summarize(mean=mean(delta), lowerCI=HPDI(delta)[1], upperCI=HPDI(delta)[2], CV= sd(delta)/mean(delta)) %>% mutate(ref_comparison = mean/mean[NBDA_conformity==1])

df_delta_trans %>%
  filter(NBDA_conformity==1) %>%
  group_by(EWA_conformity) %>%
  summarize(mean=mean(delta), lowerCI=HPDI(delta)[1], upperCI=HPDI(delta)[2], CV= sd(delta)/mean(delta)) %>% mutate(ref_comparison = mean/mean[EWA_conformity==1])


load(file="../model_outputs/Rda_files/df_GEN_memory_conformity_acq_prod.Rda")
df = df_conf_acq_prod %>% filter(NBDA_s_param==10) %>% group_by(sim, graph_type, NBDA_s_param, EWA_soc_info_weight, EWA_recent_payoff_weight,EWA_conformity, NBDA_conformity, conformity, memory_window) %>% summarize(timestep=max(timestep_acquisition_b))

df %>%
  group_by(memory_window,conformity) %>%
  summarize(mean=mean(timestep), lowerCI=HPDI(timestep)[1], upperCI=HPDI(timestep)[2], CV= sd(timestep)/mean(timestep)) %>% mutate(ref_comparison = mean/mean[conformity=="None"])

df_delta = df_conf_acq_prod %>% filter(NBDA_s_param==10) %>% mutate(delta=timestep_production_b - timestep_acquisition_b)

df_delta %>%
  group_by(memory_window,conformity) %>%
  summarize(mean=mean(delta), lowerCI=HPDI(delta)[1], upperCI=HPDI(delta)[2], CV= sd(delta)/mean(delta)) %>% mutate(ref_comparison = mean/mean[conformity=="None"])


