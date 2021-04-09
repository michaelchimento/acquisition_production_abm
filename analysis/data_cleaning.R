library(tidyverse)
library(ggpubr)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#directory = "../concat_data/"
#file_names <- dir(directory) #where you have your files
#df_ABM <- do.call(rbind,lapply(paste(directory,file_names, sep=""),read.csv))

#summary(df_ABM)


#df_ABM = df_ABM %>% mutate(prop_efficient=(behavior_b_1/(behavior_a_1+behavior_b_1)))
#df_ABM$NBDA_z_jt_type = recode(df_ABM$NBDA_z_jt_type, binary="unidirectional", proportional="bidirectional")
#df_ABM = df_ABM %>% mutate(NBDA_s_param = factor(NBDA_s_param,labels=c("S = 1","S = 5", "S = 10")))
#df_ABM = df_ABM %>% pivot_longer(cols=c(behavior_a_10,behavior_b_20), names_to="behavior", values_to="count")
#save(df_ABM,file="../concat_data/df_ABM.Rda")
load(file="../concat_data/df_ABM.Rda")

df_ABM = df_ABM %>% filter(timestep%%10==0)

p1 = ggplot(df_ABM %>% filter(EWA_conformity==1), aes(x=timestep,y=num_solvers,color=as.factor(EWA_soc_info_weight)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Number knowledgable",colour="Social info bias\n(EWA)",title="Linear copiers")+
  theme_dark()

p2 = ggplot(df_ABM %>% filter(EWA_conformity==2), aes(x=timestep,y=num_solvers,color=as.factor(EWA_soc_info_weight)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Number knowledgable",colour="Social info bias\n(EWA)",title="Conformist copiers")+
  theme_dark()

p3=ggarrange(p1,p2,labels = c("A","B"), common.legend = T)
ggsave(plot=p3,filename="../output/fig1.png", height = 17.8, width=35, units="cm",scale=1.3)

p1 = ggplot(df_ABM %>% filter(EWA_conformity==1), aes(x=timestep,y=num_solvers,color=as.factor(EWA_recent_payoff_weight)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Number knowledgable",colour="Recent payoff bias\n(EWA)",title="Linear copiers")+
  theme_dark()

p2 = ggplot(df_ABM %>% filter(EWA_conformity==2), aes(x=timestep,y=num_solvers,color=as.factor(EWA_recent_payoff_weight)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Number knowledgable",colour="Recent payoff bias\n(EWA)",title="Conformist copiers")+
  theme_dark()

p3=ggarrange(p1,p2,labels = c("A","B"), common.legend = T)
ggsave(plot=p3,filename="../output/fig2.png", height = 17.8, width=35, units="cm",scale=1.3)

p1 = ggplot(df_ABM %>% filter(EWA_conformity==1), aes(x=timestep,y=num_solvers,color=as.factor(EWA_inverse_temp)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Number knowledgable",colour="Inverse Temp\n(EWA)",title="Linear copiers")+
  theme_dark()

p2 = ggplot(df_ABM %>% filter(EWA_conformity==2), aes(x=timestep,y=num_solvers,color=as.factor(EWA_inverse_temp)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Number knowledgable",colour="Inverse Temp\n(EWA)",title="Conformist copiers")+
  theme_dark()

p3=ggarrange(p1,p2,labels = c("A","B"), common.legend = T)
ggsave(plot=p3,file="../output/fig3.png", height = 17.8, width=35, units="cm",scale=1.3)

#bigger differences in longer memory window, higher social learning score

p1 = ggplot(df_ABM %>% filter(EWA_conformity==2, social_memory_window==50), aes(x=timestep,y=num_solvers,color=as.factor(EWA_soc_info_weight)))+
  facet_grid(EWA_soc_info_weight~EWA_recent_payoff_weight)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Number knowledgable",colour="Social info bias\n(EWA)",title="Conformist, large memory learners")+
  theme_dark()

ggsave(plot=p1,file="../output/fig4.png", height = 17.8, width=35, units="cm",scale=1.3)

