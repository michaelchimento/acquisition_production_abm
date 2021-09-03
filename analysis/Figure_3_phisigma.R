library(tidyverse)
library(ggpubr)
library(ggridges)
library(magick)
library(grid)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####make diffusion charts
load(file="../concat_data/df_ABM_equiv.Rda")

df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% filter(conformity=="None",graph_type=="small world") %>% mutate(timestep=timestep+1)

end_point = max(df_ABM_equiv_payoff$timestep) + 10

df_ABM_equiv_payoff = df_ABM_equiv_payoff %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 10)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_soc_info_weight,EWA_recent_payoff_weight,conformity,EWA_tau,sim,timestep,num_know_novel) %>%
  mutate(full_diffusion=ifelse(is.na(full_diffusion),FALSE,full_diffusion))


df = df_ABM_equiv_payoff
end_point = max(df$timestep[df$full_diffusion==T])
p1a = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(EWA_soc_info_weight)))+
  geom_line(aes(group=sim),alpha=0.05)+
  stat_summary(data = df %>% filter(full_diffusion==F), size=1, fun = mean, geom=c("line"))+
  stat_summary(data = df %>% filter(full_diffusion==F), size=0.8, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,400), breaks=c(1,25,100,200,300,400))+
  coord_trans(x = "sqrt")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Social info bias")+
  theme_classic()


p1b = ggplot(df %>% filter(full_diffusion==T) %>% group_by(EWA_soc_info_weight) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density_ridges(stat = "binline", bins=50, alpha=0.3, show.legend = F,scale=3, aes(x=timestep, y=as.factor(EWA_soc_info_weight), fill=as.factor(EWA_soc_info_weight)))+
  scale_fill_viridis_d(option = "C", direction=-1, end=0.7)+
  geom_point(aes(group=as.factor(EWA_soc_info_weight),x=mean_timestep,y=as.factor(EWA_soc_info_weight), color=as.factor(EWA_soc_info_weight)), show.legend = F)+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,400))+
  coord_trans(x = "sqrt")+
  labs(x="Time",y="Prop. knowledgable",color="Social info bias")+
  theme_void()

#load in dataframe with full values on initialization
load(file="../concat_data/df_ABM_equiv.Rda")
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% filter(graph_type=="small world",conformity=="None") %>% mutate(timestep=timestep+1)
end_point = max(df_ABM_equiv_payoff$timestep) + 10

df = df_ABM_equiv_payoff %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 10)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_soc_info_weight,EWA_recent_payoff_weight,conformity,EWA_tau,sim,timestep,num_know_novel) %>%
  mutate(full_diffusion=ifelse(is.na(full_diffusion),FALSE,full_diffusion))

p2a = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(EWA_recent_payoff_weight)))+
  geom_line(aes(group=sim),alpha=0.05)+
  stat_summary(data = df %>% filter(full_diffusion==F), size=1, fun = mean, geom=c("line"))+
  stat_summary(data = df %>% filter(full_diffusion==F), size=0.8, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "A", direction=-1, begin=0.1, end=0.6)+
  scale_x_continuous(limits=c(1,400), breaks=c(1,25,100,200,300,400))+
  coord_trans(x = "sqrt")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Recent payoff bias")+
  theme_classic()

p2b = ggplot(df %>% filter(full_diffusion==T) %>% group_by(EWA_recent_payoff_weight) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density_ridges(stat = "binline", bins=50, alpha=0.3, show.legend = F,scale=3, aes(x=timestep, y=as.factor(EWA_recent_payoff_weight), fill=as.factor(EWA_recent_payoff_weight)))+
  scale_fill_viridis_d(option = "A", direction=-1, begin=0.1, end=0.6)+
  geom_point(aes(group=as.factor(EWA_recent_payoff_weight),x=mean_timestep,y=as.factor(EWA_recent_payoff_weight), color=as.factor(EWA_recent_payoff_weight)),show.legend=F)+
  scale_color_viridis_d(option = "A", direction=-1, begin=0.1, end=0.6)+
  scale_x_continuous(limits=c(1,400))+
  labs(x="Time",y="Prop. knowledgable",color="Recent payoff bias")+
  coord_trans(x = "sqrt")+
  theme_void()

#load in dataframe with full values on initialization
load(file="../concat_data/df_ABM_equiv_fullweights.Rda")
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% filter(graph_type=="small world",conformity=="None") %>% mutate(timestep=timestep+1)
end_point = max(df_ABM_equiv_payoff$timestep) + 10

df = df_ABM_equiv_payoff %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 10)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_soc_info_weight,EWA_recent_payoff_weight,conformity,EWA_tau,sim,timestep,num_know_novel) %>%
  mutate(full_diffusion=ifelse(is.na(full_diffusion),FALSE,full_diffusion))

p3a = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(EWA_recent_payoff_weight)))+
  geom_line(aes(group=sim),alpha=0.05)+
  stat_summary(data = df %>% filter(full_diffusion==F), size=1, fun = mean, geom=c("line"))+
  stat_summary(data = df %>% filter(full_diffusion==F), size=0.8, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "A", direction=-1, begin=0.1, end=0.6)+
  scale_x_continuous(limits=c(1,400), breaks=c(1,25,100,200,300,400))+
  coord_trans(x = "sqrt")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Recent payoff bias")+
  theme_classic()

p3b = ggplot(df %>% filter(full_diffusion==T) %>% group_by(EWA_recent_payoff_weight) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density_ridges(stat = "binline", bins=50, alpha=0.3, show.legend = F,scale=3, aes(x=timestep, y=as.factor(EWA_recent_payoff_weight), fill=as.factor(EWA_recent_payoff_weight)))+
  scale_fill_viridis_d(option = "A", direction=-1, begin=0.1, end=0.6)+
  geom_point(aes(group=as.factor(EWA_recent_payoff_weight),x=mean_timestep,y=as.factor(EWA_recent_payoff_weight), color=as.factor(EWA_recent_payoff_weight)),show.legend=F)+
  scale_color_viridis_d(option = "A", direction=-1, begin=0.1, end=0.6)+
  scale_x_continuous(limits=c(1,400))+
  labs(x="Time",y="Prop. knowledgable",color="Recent payoff bias")+
  coord_trans(x = "sqrt")+
  theme_void()

df %>% filter(full_diffusion==T) %>% group_by(EWA_recent_payoff_weight) %>% summarize(mean_timestep=mean(timestep))

g2 = ggarrange(p1b,p1a,ncol=1,nrow=2,heights=c(1,4),legend = "top",align="v",common.legend = T, labels=c("A"))

g3 = ggarrange(p2b,p3b,p2a,p3a,ncol=2,nrow=2,heights=c(1,4),legend = "bottom",align="v",common.legend = T,labels=c("B","C"))

g4 = ggarrange(g2,g3, ncol=1)

ggsave(g4, file="../output/fig3_sigmaphi.png",width=11,height=12,scale=2,units="cm")
