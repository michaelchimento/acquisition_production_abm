library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file="../model_outputs/Rda_files/df_maintext.Rda")

#order: conformity, memory, recent exp bias, risk-appetite

#### Chi ####
df_ABM_maintext = df_ABM_maintext %>% filter(EWA_sigma=="medium", EWA_rho=="medium", EWA_alpha=="risk-neutral", memory_window==10) %>% mutate(timestep=timestep+1)

end_point = max(df_ABM_maintext$timestep)

df = df_ABM_maintext %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 1)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_sigma,EWA_rho,EWA_chi,EWA_alpha,sim,timestep,num_know_novel,num_produced_b,full_diffusion)

p_chi_a = ggplot(df %>% group_by(sim) %>% filter(full_diffusion==T) %>% slice(head=1) %>% ungroup() %>% group_by(EWA_chi) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density(aes(x=timestep, color=as.factor(EWA_chi)),show.legend = F)+
  geom_point(aes(group=as.factor(EWA_chi),x=mean_timestep, y=0, color=as.factor(EWA_chi)), show.legend = F)+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point))+
  coord_trans(x = "sqrt")+
  labs(x="Time",y="Prop. knowledgable",color="Conformity bias")+
  theme_void()

p_chi_b = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(EWA_chi)))+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(size=1, fun = mean, geom=c("line"))+
  stat_summary(geom="errorbar", fun.data = "mean_cl_boot")+
  stat_summary(geom="point", size=1.5, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point), breaks=c(1,25,100,200,350))+
  coord_trans(x = "sqrt")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Conformity bias")+
  theme_classic()
g_chi = ggarrange(p_chi_a,p_chi_b,ncol=1,nrow=2,heights=c(1,4),legend = "right",align="v",common.legend = T)

#### memory ####
load(file="../model_outputs/Rda_files/df_maintext.Rda")

df_ABM_maintext = df_ABM_maintext %>% filter(EWA_chi=="linear bias", EWA_sigma=="medium", EWA_rho=="medium", EWA_alpha=="risk-neutral") %>% mutate(timestep=timestep+1)

summary(df_ABM_maintext)


end_point = max(df_ABM_maintext$timestep)

df = df_ABM_maintext %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 1)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_sigma,EWA_rho,EWA_chi,EWA_alpha,sim,timestep,num_know_novel,full_diffusion)

p_mem_a = ggplot(df %>% group_by(sim) %>% filter(full_diffusion==T) %>% slice(head=1) %>% ungroup() %>% group_by(memory_window) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density(aes(x=timestep, color=as.factor(memory_window)),show.legend = F)+
  geom_point(aes(group=as.factor(memory_window),x=mean_timestep,y=0, color=as.factor(memory_window)), show.legend = F)+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point))+
  coord_trans(x = "sqrt")+
  labs(x="Time",y="Prop. knowledgable",color="Memory window")+
  theme_void()
p_mem_b = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(memory_window)))+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(size=1, fun = mean, geom=c("line"))+
  stat_summary(geom="errorbar", fun.data = "mean_cl_boot")+
  stat_summary(geom="point", size=1.5, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point), breaks=c(1,25,100,200,350))+
  coord_trans(x = "sqrt")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Memory window")+
  theme_classic()
g_m = ggarrange(p_mem_a,p_mem_b,ncol=1,nrow=2,heights=c(1,4),legend = "right",align="v",common.legend = T)

#### Rho ####
load(file="../model_outputs/Rda_files/df_maintext.Rda")

df_ABM_maintext = df_ABM_maintext %>% filter(EWA_chi=="linear bias", EWA_sigma=="medium", EWA_alpha=="risk-neutral", memory_window==10) %>% mutate(timestep=timestep+1)

end_point = max(df_ABM_maintext$timestep)

df = df_ABM_maintext %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 1)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_sigma,EWA_rho,EWA_chi,EWA_alpha,sim,timestep,num_know_novel,full_diffusion)

p_rho_a = ggplot(df %>% group_by(sim) %>% filter(full_diffusion==T) %>% slice(head=1) %>% ungroup() %>% group_by(EWA_rho) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density(aes(x=timestep, color=as.factor(EWA_rho)),show.legend = F)+
  geom_point(aes(group=as.factor(EWA_rho),x=mean_timestep,y=0, color=as.factor(EWA_rho)), show.legend = F)+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point))+
  coord_trans(x = "sqrt")+
  labs(x="Time",y="Prop. knowledgable",color="Recent exp. bias")+
  theme_void()
p_rho_b = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(EWA_rho)))+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(size=1, fun = mean, geom=c("line"))+
  stat_summary(geom="errorbar", fun.data = "mean_cl_boot")+
  stat_summary(geom="point", size=1.5, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point), breaks=c(1,25,100,200,350))+
  coord_trans(x = "sqrt")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Recent exp. bias")+
  theme_classic()
g_rho = ggarrange(p_rho_a,p_rho_b,ncol=1,nrow=2,heights=c(1,4),legend = "right",align="v",common.legend = T)

#### Alpha ####
load(file="../model_outputs/Rda_files/df_maintext.Rda")

df_ABM_maintext = df_ABM_maintext %>% filter(EWA_chi=="linear bias", EWA_rho=="medium", EWA_sigma=="medium", memory_window==10) %>% mutate(timestep=timestep+1)

end_point = max(df_ABM_maintext$timestep)

df = df_ABM_maintext %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 1)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_sigma,EWA_rho,EWA_chi,EWA_alpha,sim,timestep,num_know_novel,full_diffusion)

p_alpha_a = ggplot(df %>% group_by(sim) %>% filter(full_diffusion==T) %>% slice(head=1) %>% ungroup() %>% group_by(EWA_alpha) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density(aes(x=timestep, color=as.factor(EWA_alpha)),show.legend = F)+
  geom_point(aes(group=as.factor(EWA_alpha),x=mean_timestep,y=0, color=as.factor(EWA_alpha)), show.legend = F)+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point))+
  coord_trans(x = "sqrt")+
  labs(x="Time",y="Prop. knowledgable",color="Risk-appetite")+
  theme_void()
p_alpha_b = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(EWA_alpha)))+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(size=1, fun = mean, geom=c("line"))+
  stat_summary(geom="errorbar", fun.data = "mean_cl_boot")+
  stat_summary(geom="point", size=1.5, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point), breaks=c(1,25,100,200,350))+
  coord_trans(x = "sqrt")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Risk-appetite")+
  theme_classic()
g_alpha = ggarrange(p_alpha_a,p_alpha_b,ncol=1,nrow=2,heights=c(1,4),legend = "right",align="v",common.legend = T)

ggarrange(g_chi, g_m, g_rho, g_alpha, labels = c("A","B","C","D"))

ggsave(file="../output/Fig_S_all_production_params.png",width=15,height=13,scale=2,units="cm")
