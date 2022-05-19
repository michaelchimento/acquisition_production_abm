library(tidyverse)
library(ggpubr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Figure SX - Deltas for alpha
load(file="../model_outputs/Rda_files/df_maintext_acq_prod.Rda")
summary(df_maintext_acq_prod)
df_maintext_acq_prod = df_maintext_acq_prod %>% mutate(delay=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number()) %>% ungroup()
df_maintext_acq_prod = df_maintext_acq_prod %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_maintext_acq_prod = df_maintext_acq_prod %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(delta_acq=timestep_acquisition_b-lag(timestep_acquisition_b)) %>% filter(!is.na(delta_acq))

df = df_maintext_acq_prod %>% filter( EWA_chi=="linear bias", EWA_sigma=="medium", EWA_rho=="medium", memory_window==10)
df_mean = df %>% group_by(EWA_alpha) %>% summarize(mean_delay = mean(delay), mean_delta_acq=mean(delta_acq))
df = df %>%
  ungroup() %>%
  select(EWA_alpha, order_acquisition, delta_acq,delay) %>%
  pivot_longer(cols=c(delta_acq,delay)) %>%
  mutate(name=as.factor(name))

alpha = ggplot(df, aes(x=order_acquisition, y=value, color=name))+
  facet_grid(~EWA_alpha)+
  stat_summary(fun.data="mean_cl_boot", alpha=0.7)+
  geom_hline(data=df_mean, aes(yintercept=mean_delay), color="#de4968", linetype="dashed")+
  geom_hline(data=df_mean, aes(yintercept=mean_delta_acq), color="#2a788e", linetype="dashed")+
  scale_color_manual(labels=c("Delay","Acquisition interval"), values=c("#de4968","#2a788e"), name="Measure")+
  theme_classic()+
  coord_cartesian(ylim=c(0,17))+
  labs(x="Order of acquisition", y= "Timesteps", title="Risk-appetite")
alpha

#Figure SX - Deltas for memory
df = df_maintext_acq_prod %>% filter(EWA_chi=="linear bias", EWA_sigma=="medium", EWA_rho=="medium", EWA_alpha=="risk-neutral")
df_mean = df %>% group_by(memory_window) %>% summarize(mean_delay = mean(delay), mean_delta_acq=mean(delta_acq))
df = df %>% ungroup() %>% select(memory_window, order_acquisition, delta_acq,delay) %>% pivot_longer(cols=c(delta_acq,delay)) %>% mutate(name=as.factor(name))


mem = ggplot(df, aes(x=order_acquisition, y=value, color=name))+
  facet_grid(~memory_window)+
  stat_summary(fun.data="mean_cl_boot", alpha=0.7)+
  geom_hline(data=df_mean, aes(yintercept=mean_delay), color="#de4968", linetype="dashed")+
  geom_hline(data=df_mean, aes(yintercept=mean_delta_acq), color="#2a788e", linetype="dashed")+
  scale_color_manual(labels=c("Delay","Acquisition interval"), values=c("#de4968","#2a788e"), name="Measure")+
  theme_classic()+
  coord_cartesian(ylim=c(0,17))+
  labs(x="Order of acquisition", y= "Timesteps", title="Memory window")

#Figure SX - Deltas for sigma
df = df_maintext_acq_prod %>% filter(EWA_chi=="linear bias", EWA_rho=="medium", EWA_alpha=="risk-neutral", memory_window==10)
df_mean = df %>% group_by(EWA_sigma) %>% summarize(mean_delay = mean(delay), mean_delta_acq=mean(delta_acq))
df = df %>% ungroup() %>% select(EWA_sigma, order_acquisition, delta_acq,delay) %>% pivot_longer(cols=c(delta_acq,delay)) %>% mutate(name=as.factor(name))

sigma = ggplot(df, aes(x=order_acquisition, y=value, color=name))+
  facet_grid(~EWA_sigma)+
  stat_summary(fun.data="mean_cl_boot", alpha=0.7)+
  geom_hline(data=df_mean, aes(yintercept=mean_delay), color="#de4968", linetype="dashed")+
  geom_hline(data=df_mean, aes(yintercept=mean_delta_acq), color="#2a788e", linetype="dashed")+
  scale_color_manual(labels=c("Delay","Acquisition interval"), values=c("#de4968","#2a788e"), name="Measure")+
  theme_classic()+
  coord_cartesian(ylim=c(0,17))+
  labs(x="Order of acquisition", y= "Timesteps", title="Social information bias")

#Figure SX - Deltas for chi
df = df_maintext_acq_prod %>% filter(EWA_sigma=="medium", EWA_rho=="medium", EWA_alpha=="risk-neutral", memory_window==10)
df_mean = df %>% group_by(EWA_chi) %>% summarize(mean_delay = mean(delay), mean_delta_acq=mean(delta_acq))
df = df %>% ungroup() %>% select(EWA_chi, order_acquisition, delta_acq,delay) %>% pivot_longer(cols=c(delta_acq,delay)) %>% mutate(name=as.factor(name))

chi= ggplot(df, aes(x=order_acquisition, y=value, color=name))+
  facet_grid(~EWA_chi)+
  stat_summary(fun.data="mean_cl_boot", alpha=0.7)+
  geom_hline(data=df_mean, aes(yintercept=mean_delay), color="#de4968", linetype="dashed")+
  geom_hline(data=df_mean, aes(yintercept=mean_delta_acq), color="#2a788e", linetype="dashed")+
  scale_color_manual(labels=c("Delay","Acquisition interval"), values=c("#de4968","#2a788e"), name="Measure")+
  theme_classic()+
  coord_cartesian(ylim=c(0,17))+
  labs(x="Order of acquisition", y= "Timesteps", title="Frequency dependent bias")

#Figure SX - Deltas for rho
df = df_maintext_acq_prod %>% filter(EWA_sigma=="medium", EWA_chi=="linear bias", EWA_alpha=="risk-neutral", memory_window==10)
df_mean = df %>% group_by(EWA_rho) %>% summarize(mean_delay = mean(delay), mean_delta_acq=mean(delta_acq))
df = df %>% ungroup() %>% select(EWA_rho, order_acquisition, delta_acq,delay) %>% pivot_longer(cols=c(delta_acq,delay)) %>% mutate(name=as.factor(name))
rho = ggplot(df, aes(x=order_acquisition, y=value, color=name))+
  facet_grid(~EWA_rho)+
  stat_summary(fun.data="mean_cl_boot", alpha=0.7)+
  geom_hline(data=df_mean, aes(yintercept=mean_delay), color="#de4968", linetype="dashed")+
  geom_hline(data=df_mean, aes(yintercept=mean_delta_acq), color="#2a788e", linetype="dashed")+
  scale_color_manual(labels=c("Delay","Acquisition interval"), values=c("#de4968","#2a788e"), name="Measure")+
  theme_classic()+
  coord_cartesian(ylim=c(0,17))+
  labs(x="Order of acquisition", y= "Timesteps", title="Recent experience bias")

ggarrange(sigma,chi,mem,rho,alpha, labels = c("A","B","C","D","E"), common.legend = T, ncol=2, nrow=3)
ggsave(file="../output/Fig_S4_delay_acq_interval.png",width=15,height=15,scale=2,units="cm")

