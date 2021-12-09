library(tidyverse)
library(ggpubr)
library(ggridges)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####Equivalent attraction scores####
load(file="../model_outputs/Rda_files/df_baseline_P.Rda")
fun_range <- function(x) {                              # Create user-defined function
  (x - min(x)) / (max(x) - min(x))
}

df = df_baseline_P %>% filter(EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative")
summary(df)
df = df %>%
  group_by(sim)%>%
  filter(timestep<=(max(timestep)-45)) %>%
  mutate(scaled_time=fun_range(timestep)) %>%
  arrange(timestep)

df = df %>% mutate(behavior_a=behavior_a/pop_size,behavior_b=behavior_b/pop_size,num_produced_b=num_produced_b/pop_size) %>% pivot_longer(cols=c(behavior_a,behavior_b,num_know_novel,num_produced_b))
df$name = factor(df$name, levels=c("num_know_novel","num_produced_b","behavior_a","behavior_b"))



p1 = ggplot(df %>% filter(), aes(x=scaled_time, color=name))+
  stat_summary_bin(aes(y=value), size=1, geom="errorbar", fun.data="mean_cl_boot", bins=20)+
  stat_summary_bin(aes(y=value), size=2, geom="point", bins=20)+
  stat_summary_bin(aes(y=value), size=1, geom="line", bins=20)+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Scaled time",y="Proportion",color="",title="Production only")+
  scale_color_manual(values=c("black","gray","#5FD84F","#007C76"), labels=c("know novel","produced first novel","freq. established","freq. novel"), drop=FALSE)+
  theme_classic()

#####Baseline Transmission####
load(file="../model_outputs/Rda_files/df_baseline_T.Rda")
fun_range <- function(x) {                              # Create user-defined function
  (x - min(x)) / (max(x) - min(x))
}

df = df_baseline_T %>% filter(EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative")
summary(df)
df = df %>%
  group_by(sim)%>%
  filter(timestep<=(max(timestep)-45)) %>%
  mutate(scaled_time=fun_range(timestep)) %>%
  arrange(timestep)

df = df %>% mutate(behavior_a=behavior_a/pop_size,behavior_b=behavior_b/pop_size,num_produced_b=num_produced_b/pop_size) %>% pivot_longer(cols=c(behavior_a,behavior_b,num_know_novel,num_produced_b))
df$name = factor(df$name, levels=c("num_know_novel","num_produced_b","behavior_a","behavior_b"))

p2 = ggplot(df %>% filter(), aes(x=scaled_time, color=name))+
  stat_summary_bin(aes(y=value), size=1, geom="errorbar", fun.data="mean_cl_boot", bins=20)+
  stat_summary_bin(aes(y=value), size=2, geom="point", bins=20)+
  stat_summary_bin(aes(y=value), size=1, geom="line", bins=20)+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Scaled time",y="Proportion",color="", title="Transmission disconnected from production")+
  scale_color_manual(values=c("black","gray","#5FD84F","#007C76"), labels=c("know novel","produced first novel","freq. established","freq. novel"), drop=FALSE)+
  theme_classic()


ggarrange(p1,p2,labels=LETTERS[1:2], common.legend = T,legend="right")
ggsave("../output/Fig_SX_baseline_diffusions.png",height=5,width=14,scale=2,units="cm")


#### metrics ####

load(file="../model_outputs/Rda_files/df_baseline_p_acq_prod.Rda")
summary(df_baseline_p_acq_prod)
library(rethinking)
df_baseline_p_acq_prod %>%
  filter( EWA_recent_payoff_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative",memory_window==10) %>%
  group_by(sim)%>%
  filter(timestep_production_b==max(timestep_production_b)) %>%
  ungroup() %>%
  group_by(EWA_soc_info_weight) %>%
  summarize(mean(timestep_production_b), HPDI(timestep_production_b))


load(file="../model_outputs/Rda_files/df_baseline_t_acq_prod.Rda")
summary(df_baseline_p_acq_prod)
library(rethinking)
df_baseline_t_acq_prod %>%
  filter( EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative",memory_window==10) %>%
  group_by(sim)%>%
  filter(timestep_acquisition_b==max(timestep_acquisition_b)) %>%
  ungroup() %>%
  summarize(mean(timestep_acquisition_b), HPDI(timestep_acquisition_b))

load(file="../model_outputs/Rda_files/df_baseline_t_acq_prod.Rda")
summary(df_baseline_p_acq_prod)
library(rethinking)
df_baseline_t_acq_prod %>%
  filter( EWA_recent_payoff_weight=="medium", EWA_soc_info_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative",memory_window==10) %>%
  group_by(sim)%>%
  filter(timestep_production_b==max(timestep_production_b)) %>%
  ungroup() %>%
  summarize(mean(timestep_production_b), HPDI(timestep_production_b))


####Preprogrammed attraction scores####
load(file="../model_outputs/Rda_files/df_baseline_P_preprogrammed.Rda")
fun_range <- function(x) {                              # Create user-defined function
  (x - min(x)) / (max(x) - min(x))
}

df = df_baseline_P %>% filter(EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative")
summary(df)
df = df %>%
  group_by(sim)%>%
  filter(timestep<=(max(timestep)-45)) %>%
  mutate(scaled_time=fun_range(timestep)) %>%
  arrange(timestep)

df = df %>% mutate(behavior_a=behavior_a/pop_size,behavior_b=behavior_b/pop_size,num_produced_b=num_produced_b/pop_size) %>% pivot_longer(cols=c(behavior_a,behavior_b,num_know_novel,num_produced_b))
df$name = factor(df$name, levels=c("num_know_novel","num_produced_b","behavior_a","behavior_b"))

p3 = ggplot(df %>% filter(name!="num_know_novel"), aes(x=scaled_time, color=name))+
  stat_summary_bin(aes(y=value), size=1, geom="errorbar", fun.data="mean_cl_boot", bins=20)+
  stat_summary_bin(aes(y=value), size=2, geom="point", bins=20)+
  stat_summary_bin(aes(y=value), size=1, geom="line", bins=20)+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Scaled time",y="Proportion",color="")+
  scale_color_manual(values=c("black","gray","#5FD84F","#007C76"), labels=c("know novel","produced first novel","freq. established","freq. novel"), drop=FALSE)+
  theme_classic()

load(file="../model_outputs/Rda_files/df_baseline_p_acq_prod_preprogrammed.Rda")
summary(df_baseline_p_acq_prod)
library(rethinking)
df_baseline_p_acq_prod %>%
  filter( EWA_recent_payoff_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative",memory_window==10) %>%
  group_by(sim)%>%
  filter(timestep_production_b==max(timestep_production_b)) %>%
  ungroup() %>%
  group_by(EWA_soc_info_weight) %>%
  summarize(mean(timestep_production_b), HPDI(timestep_production_b))