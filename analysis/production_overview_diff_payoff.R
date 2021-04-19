library(tidyverse)
library(ggpubr)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file="../concat_data/df_ABM_diff_payoff.Rda")

df_ABM_diff_payoff = df_ABM_diff_payoff %>% filter(EWA_soc_info_weight %in% c(0.1,0.5,0.9),
                                                     EWA_recent_payoff_weight %in% c(0.1,0.5,0.9),
                                                     EWA_inverse_temp != 0.5)


p1 = ggplot(df_ABM_diff_payoff %>% filter(EWA_conformity==1), aes(x=timestep,y=prop_b,color=as.factor(EWA_soc_info_weight)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Proportion of productions as novel behavior",colour="Social info bias\n(EWA)",title="Linear copiers")+
  theme_dark()

p2 = ggplot(df_ABM_diff_payoff %>% filter(EWA_conformity==2), aes(x=timestep,y=prop_b,color=as.factor(EWA_soc_info_weight)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Proportion of productions as novel behavior",colour="Social info bias\n(EWA)",title="Conformist copiers")+
  theme_dark()

p3=ggarrange(p1,p2,labels = c("A","B"), common.legend = T)
ggsave(plot=p3,filename="../output/diff_overview/productions_fig1.png", height = 17.8, width=35, units="cm",scale=1.3)

remove(p1,p2,p3)

p1 = ggplot(df_ABM_diff_payoff %>% filter(EWA_conformity==1), aes(x=timestep,y=prop_b,color=as.factor(EWA_recent_payoff_weight)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Proportion of productions as novel behavior",colour="Recent payoff bias\n(EWA)",title="Linear copiers")+
  theme_dark()

p2 = ggplot(df_ABM_diff_payoff %>% filter(EWA_conformity==2), aes(x=timestep,y=prop_b,color=as.factor(EWA_recent_payoff_weight)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Proportion of productions as novel behavior",colour="Recent payoff bias\n(EWA)",title="Conformist copiers")+
  theme_dark()

p3=ggarrange(p1,p2,labels = c("A","B"), common.legend = T)
ggsave(plot=p3,filename="../output/diff_overview/productions_fig2.png", height = 17.8, width=35, units="cm",scale=1.3)

remove(p1,p2,p3)

p1 = ggplot(df_ABM_diff_payoff %>% filter(EWA_conformity==1), aes(x=timestep,y=prop_b,color=as.factor(EWA_inverse_temp)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Proportion of productions as novel behavior",colour="Inverse Temp\n(EWA)",title="Linear copiers")+
  theme_dark()

p2 = ggplot(df_ABM_diff_payoff %>% filter(EWA_conformity==2), aes(x=timestep,y=prop_b,color=as.factor(EWA_inverse_temp)))+
  facet_grid(social_memory_window~NBDA_s_param)+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(alpha=0.5, size=1.5, fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) },
               fun = median, geom=c("errorbar"))+
  stat_summary(size=2, fun = median, geom=c("point"))+
  scale_color_viridis_d(option = "magma", direction=-1)+
  labs(x="Time",y="Proportion of productions as novel behavior",colour="Inverse Temp\n(EWA)",title="Conformist copiers")+
  theme_dark()

p3=ggarrange(p1,p2,labels = c("A","B"), common.legend = T)
ggsave(plot=p3,file="../output/diff_overview/productions_fig3.png", height = 17.8, width=35, units="cm",scale=1.3)

remove(p1,p2,p3)
