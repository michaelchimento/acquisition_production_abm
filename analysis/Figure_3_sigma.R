library(tidyverse)
library(ggpubr)
library(ggridges)
library(grid)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoff.Rda")

#Panel A: diffusion chart with margin density plot
df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% filter(graph_type=="random regular",EWA_conformity==1, EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", NBDA_s_param==5,memory_window==10) %>% mutate(timestep=timestep+1)

end_point = max(df_ABM_equiv_payoff$timestep) - 45

df = df_ABM_equiv_payoff %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 5)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_soc_info_weight,EWA_recent_payoff_weight,EWA_conformity,EWA_tau,sim,timestep,num_know_novel, full_diffusion)

p1a = ggplot(df %>% group_by(sim) %>% filter(full_diffusion==T) %>% slice(head=1) %>% ungroup() %>% group_by(EWA_soc_info_weight) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density(aes(x=timestep, color=as.factor(EWA_soc_info_weight)),show.legend = F)+
  #geom_density_ridges(stat = "binline", bins=50, alpha=0.3, show.legend = F,scale=5, aes(x=timestep, y="0", color=as.factor(EWA_soc_info_weight)))+
  geom_point(aes(group=as.factor(EWA_soc_info_weight),x=mean_timestep,y=0, color=as.factor(EWA_soc_info_weight)), show.legend = F)+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point))+
  coord_trans(x = "sqrt")+
  labs(x="Time",y="Prop. knowledgable",color="Social info bias")+
  theme_void()
p1b = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(EWA_soc_info_weight)))+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(size=1, fun = mean, geom=c("line"))+
  stat_summary(geom="errorbar", fun.data = "mean_cl_boot")+
  stat_summary(geom="point", size=1.5, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point), breaks=c(1,25,100,200,350))+
  coord_trans(x = "sqrt")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Social info bias")+
  theme_classic()
g1 = ggarrange(p1a,p1b,ncol=1,nrow=2,heights=c(1,4),legend = "right",align="v",common.legend = T, labels=c("A"))

#Panel B: compare avg delta between order of acquisition and production for SIGMA
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod.Rda")
df = df_equiv_payoffs_acq_prod %>% filter() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df = df %>% ungroup() %>% mutate(delta=timestep_production_b- timestep_acquisition_b)
df_summary = df %>% group_by(order_acquisition,order_production, EWA_soc_info_weight) %>% summarize(mean_delay=mean(delta))

p2= ggplot(df_summary, aes(x=order_acquisition,y=order_production, fill=mean_delay))+
  facet_wrap(~EWA_soc_info_weight)+
  geom_tile()+
  labs(x="Order of acquisition", y="Order of production", fill="Avg. delay")+
  scale_fill_viridis_c(option="plasma", trans="sqrt",  direction=-1) +
  scale_x_continuous(breaks=c(1,4,8,12,16,20,24))+
  scale_y_continuous(breaks=c(1,4,8,12,16,20,24))+
  theme_classic()

ggarrange(g1,p2,nrow=2, labels=c("","B"))
ggsave(file="../output/Fig_3_sigma.png",width=12,height=10,scale=2,units="cm")
