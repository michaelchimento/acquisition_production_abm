library(tidyverse)
library(ggpubr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../model_outputs/Rda_files/df_maintext_agents.Rda")
load(file="../model_outputs/Rda_files/df_maintext.Rda")

df_params = df_ABM_maintext %>% group_by(sim,EWA_sigma,EWA_chi,EWA_rho,EWA_alpha,memory_window) %>% summarize()
df_maintext_agents = df_maintext_agents %>% select(-X)

df = left_join(df_maintext_agents,df_params) %>% filter(timestep_knowledgable<=40)

####E_Mat####
df_plot = df %>% filter(EWA_chi=="linear bias", EWA_rho=="medium", EWA_alpha=="risk-neutral", memory_window==10)

p1a=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_e, color=EWA_sigma))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="E(t)", color="", title="Soc. info bias")

p1b=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_i, color=EWA_sigma))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="I(t)", color="")

p1c=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_s, color=EWA_sigma))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="S(t)", color="")

p1d=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_p, color=EWA_sigma))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="Timesteps knowledgable of b", y="P(produce b)", color="")

g1= ggarrange(p1a,p1b,p1c,p1d, nrow=4,ncol=1, common.legend=T,legend="top")

df_plot = df %>% filter(EWA_sigma=="medium", EWA_rho=="medium", EWA_alpha=="risk-neutral", memory_window==10)

p2a=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_e, color=EWA_chi))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="", title="Conformity bias")

p2b=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_i, color=EWA_chi))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="")

p2c=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_s, color=EWA_chi))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="")

p2d=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_p, color=EWA_chi))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="Timesteps knowledgable of b", y="", color="")

g2= ggarrange(p2a,p2b,p2c,p2d, nrow=4,ncol=1, common.legend=T,legend="top")


df_plot = df %>% filter(EWA_sigma=="medium", EWA_chi=="linear bias", EWA_rho=="medium", EWA_alpha=="risk-neutral")

p3a=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_e, color=as.factor(memory_window)))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="", title="Memory")

p3b=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_i, color=as.factor(memory_window)))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="")

p3c=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_s, color=as.factor(memory_window)))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="")

p3d=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_p, color=as.factor(memory_window)))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="Timesteps knowledgable of b", y="", color="")

g3= ggarrange(p3a,p3b,p3c,p3d, nrow=4,ncol=1, common.legend=T,legend="top")


df_plot = df %>% filter(EWA_sigma=="medium", EWA_chi=="linear bias", EWA_alpha=="risk-neutral", memory_window==10)

p4a=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_e, color=EWA_rho))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="", title="Recent exp. bias")

p4b=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_i, color=EWA_rho))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="")

p4c=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_s, color=EWA_rho))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="")

p4d=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_p, color=EWA_rho))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="Timesteps knowledgable of b", y="", color="")

g4= ggarrange(p4a,p4b,p4c,p4d, nrow=4,ncol=1, common.legend=T,legend="top")


df_plot = df %>% filter(EWA_sigma=="medium", EWA_chi=="linear bias", EWA_rho=="medium", memory_window==10)

p5a=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_e, color=EWA_alpha))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="", title="Risk-appetite")

p5b=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_i, color=EWA_alpha))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="")

p5c=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_s, color=EWA_alpha))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="", y="", color="")

p5d=ggplot(df_plot, aes(x=timestep_knowledgable, y = B_p, color=EWA_alpha))+
  stat_summary()+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  coord_cartesian(ylim=c(0,.5))+
  theme_classic()+
  labs(x="Timesteps knowledgable of b", y="", color="")

g5= ggarrange(p5a,p5b,p5c,p5d, nrow=4,ncol=1, common.legend=T,legend="top")


g_all = ggarrange(g1,g2,g3,g4,g5, labels=c("A","B","C","D","E"), ncol=5)

ggsave(g_all, file="../output/Fig_S_EISP.png",width=15,height=15,scale=3,units="cm")

