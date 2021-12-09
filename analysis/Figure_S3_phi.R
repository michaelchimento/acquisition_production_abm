#DIFFUSION CHARTS FOR PHI
#load in dataframe with full values on initialization
library(tidyverse)
library(ggpubr)
library(ggridges)
library(grid)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####make diffusion charts
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoff.Rda")
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% filter(graph_type=="random regular" ,NBDA_s_param==5, memory_window==10, EWA_soc_info_weight=="medium", EWA_tau=="non-conservative", EWA_conformity==1) %>% mutate(timestep=timestep+1)
end_point = max(df_ABM_equiv_payoff$timestep)

df = df_ABM_equiv_payoff %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 5)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_soc_info_weight,EWA_recent_payoff_weight,EWA_conformity,EWA_tau,sim,timestep,num_know_novel, full_diffusion)

p1a = ggplot(df %>% group_by(sim) %>% filter(full_diffusion==T) %>% slice(head=1) %>% ungroup() %>% group_by(EWA_recent_payoff_weight) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density(aes(x=timestep, color=as.factor(EWA_recent_payoff_weight)),show.legend = F, adjust=1.5 )+
  geom_point(aes(group=as.factor(EWA_recent_payoff_weight),x=mean_timestep,y=0, color=as.factor(EWA_recent_payoff_weight)), show.legend = F)+
  scale_color_viridis_d(option = "A", direction=-1, begin=0.1, end=0.6)+
  scale_x_continuous(limits=c(1,end_point))+
  coord_trans(x = "sqrt")+
  labs(x="Time",y="Prop. knowledgable",color="Recent experience bias")+
  theme_void()
p1a
p1b = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(EWA_recent_payoff_weight)))+
  geom_line(aes(group=sim),alpha=0.05)+
  stat_summary(size=1, fun = mean, geom=c("line"))+
  stat_summary(geom="errorbar", fun.data = "mean_cl_boot")+
  stat_summary(geom="point", size=1.5, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "A", direction=-1, begin=0.1, end=0.6)+
  scale_x_continuous(limits=c(1,end_point), breaks=c(1,25,100,200,300,400))+
  coord_trans(x = "sqrt")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Recent experience bias")+
  theme_classic()

p1b

#load in dataframe with full values on initialization
load(file="../model_outputs/Rda_files/df_fullweight.Rda")
summary(df_ABM_fullweight)
df_ABM_fullweight = df_ABM_fullweight %>% filter(graph_type=="random regular" ,NBDA_s_param==10, memory_window==20, EWA_soc_info_weight=="medium", EWA_tau=="non-conservative", EWA_conformity==1) %>% mutate(timestep=timestep+1)
end_point = max(df_ABM_fullweight$timestep)

df = df_ABM_fullweight %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 5)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_soc_info_weight,EWA_recent_payoff_weight,EWA_conformity,EWA_tau,sim,timestep,num_know_novel, full_diffusion)

p2a = ggplot(df %>% group_by(sim) %>% filter(full_diffusion==T) %>% slice(head=1) %>% ungroup() %>% group_by(EWA_recent_payoff_weight) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density(aes(x=timestep, color=as.factor(EWA_recent_payoff_weight)),show.legend = F)+
  geom_point(aes(group=as.factor(EWA_recent_payoff_weight),x=mean_timestep,y=0, color=as.factor(EWA_recent_payoff_weight)), show.legend = F)+
  scale_color_viridis_d(option = "A", direction=-1, begin=0.1, end=0.6)+
  scale_x_continuous(limits=c(1,end_point))+
  coord_trans(x = "sqrt")+
  labs(x="Time",y="Prop. knowledgable",color="Recent experience bias")+
  theme_void()
p2a
p2b = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(EWA_recent_payoff_weight)))+
  geom_line(aes(group=sim),alpha=0.01)+
  stat_summary(size=1, fun = mean, geom=c("line"))+
  stat_summary(geom="errorbar", fun.data = "mean_cl_boot")+
  stat_summary(geom="point", size=1.5, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "A", direction=-1, begin=0.1, end=0.6)+
  scale_x_continuous(limits=c(1,end_point), breaks=c(1,25,100,200,300,400))+
  coord_trans(x = "sqrt")+
  #scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Recent experience bias")+
  theme_classic()


g2 = ggarrange(p1a,p2a,p1b,p2b, ncol=2,nrow=2, heights=c(1,4), legend = "top",align="v", common.legend = T, labels=c("A","B","",""))

ggsave(g2, file="../output/Fig_5_phi.png",width=12,height=6,scale=2,units="cm")

library(rethinking)
df_ABM_fullweight %>% filter(full_diffusion==T) %>% group_by(sim) %>% slice(head=1) %>% ungroup() %>% group_by(EWA_recent_payoff_weight) %>% summarize(mean_timestep=mean(timestep), HPDI(timestep))

