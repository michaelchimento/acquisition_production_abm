library(tidyverse)
library(ggpubr)
library(ggridges)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../concat_data/df_ABM_memory.Rda")

df = df_memory %>% filter(EWA_recent_payoff_weight=="medium",EWA_soc_info_weight=="medium",memory_window==1)
end_point = max(df$timestep) + 10

df = df %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 10)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_soc_info_weight,EWA_recent_payoff_weight,conformity,EWA_tau,sim,timestep,num_know_novel,conformity) %>%
  mutate(full_diffusion=ifelse(is.na(full_diffusion),FALSE,full_diffusion)) %>% ungroup() %>% droplevels()

p1 = ggplot(df, aes(x=timestep+1,y=num_know_novel,color=conformity))+
  stat_summary(data = df %>% filter(full_diffusion==F), size=1, fun = mean, geom=c("line"))+
  stat_summary(data = df %>% filter(full_diffusion==F), size=0.8, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "D", direction=-1, end=0.7, guide=F)+
  scale_x_continuous(limits=c(1,600), breaks=c(1,25,100,250,500,900),trans="sqrt")+
  labs(x="timestep", y = "proportion knowledgeable")+
  theme_classic()

load(file="../concat_data/df_ABM_memory.Rda")
load(file="../concat_data/df_ABM_equiv.Rda")

df_memory = df_memory %>% filter(full_diffusion==T)
df = df_ABM_equiv_payoff %>% filter(full_diffusion==T, EWA_recent_payoff_weight=="medium",EWA_soc_info_weight=="medium")
df_memory = rbind(df,df_memory)
df_memory = df_memory %>% mutate(memory_window=as.factor(memory_window))

df = df_memory

p2 = ggplot(df %>% filter(full_diffusion==T) %>% group_by(memory_window,conformity) %>% mutate(median_timestep=mean(timestep)))+
  geom_density_ridges(stat = "binline", bins=100, alpha=0.3, show.legend = F,scale=1, aes(x=timestep, y=memory_window, fill=conformity))+
  geom_point(aes(group=memory_window,x=median_timestep,y=memory_window, color=conformity), size=3)+
  #geom_line(aes(group=conformity, x=median_timestep,y=memory_window, color=conformity), size=1)+
  scale_color_viridis_d(option = "D", direction=-1, end=0.7)+
  scale_fill_viridis_d(option = "D", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(10,10000), breaks=c(10,33,100,330,1000,3300,10000),trans="log")+
  labs(x="time to diffusion",y="memory window")+
  theme_classic()

summary(df)

ggarrange(p1,p2, labels=c("A","B"), common.legend = T, legend = "bottom")

ggsave(file="../output/fig4_memoryconformity.png",width=17.8,height=8,scale=2,units="cm")



