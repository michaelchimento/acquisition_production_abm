library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file="../model_outputs/Rda_files/df_maintext.Rda")

summary(df_ABM_maintext)

#Panel A: diffusion chart with margin density plot
df_ABM_maintext = df_ABM_maintext %>% filter(EWA_chi=="linear bias", EWA_rho=="medium", EWA_alpha=="risk-neutral", memory_window==10) %>% mutate(timestep=timestep+1)

end_point = max(df_ABM_maintext$timestep)

df = df_ABM_maintext %>%
  group_by(sim)%>%
  complete(timestep = seq(min(timestep), end_point, 1)) %>%
  arrange(timestep) %>%
  fill(graph_type,memory_window,EWA_sigma,EWA_rho,EWA_chi,EWA_alpha,sim,timestep,num_know_novel,full_diffusion)

p1a = ggplot(df %>% group_by(sim) %>% filter(full_diffusion==T) %>% slice(head=1) %>% ungroup() %>% group_by(EWA_sigma) %>% mutate(mean_timestep=mean(timestep)))+
  geom_density(aes(x=timestep, color=as.factor(EWA_sigma)),show.legend = F, adjust=1.75)+
  #geom_density_ridges(stat = "binline", bins=50, alpha=0.3, show.legend = F,scale=5, aes(x=timestep, y="0", color=as.factor(EWA_sigma)))+
  geom_point(aes(group=as.factor(EWA_sigma),x=mean_timestep,y=0, color=as.factor(EWA_sigma)), show.legend = F)+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point))+
  coord_trans(x = "sqrt")+
  labs(x="Time",y="Prop. knowledgable",color="Social info bias")+
  theme_void()
p1a
p1b = ggplot(df, aes(x=timestep,y=num_know_novel,color=as.factor(EWA_sigma)))+
  geom_line(aes(group=sim),alpha=0.1)+
  stat_summary(size=1, fun = mean, geom=c("line"))+
  stat_summary(geom="errorbar", fun.data = "mean_cl_boot")+
  stat_summary(geom="point", size=1.5, fun.data = "mean_cl_boot")+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  scale_x_continuous(limits=c(1,end_point), breaks=c(1, 9, 25, 48, 81, 121))+
  coord_trans(x = "sqrt")+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Time",y="Prop. knowledgable",color="Social info bias")+
  theme_classic()
g1 = ggarrange(p1a,p1b,ncol=1,nrow=2,heights=c(1,4),legend = "right",align="v",common.legend = T)
g1
ggsave(g1, file="../output/Fig_3_sigma.png",width=12,height=10,scale=1.5,units="cm")


#Panel B: compare avg delta between order of acquisition and production for SIGMA
load(file="../model_outputs/Rda_files/df_maintext_acq_prod.Rda")
df = df_maintext_acq_prod %>% filter() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df = df %>% ungroup() %>% mutate(delta=timestep_production_b- timestep_acquisition_b)
df_summary = df %>% group_by(order_acquisition,order_production, EWA_sigma) %>% summarize(mean_delay=mean(delta))

p2= ggplot(df_summary, aes(x=order_acquisition,y=order_production, fill=mean_delay))+
  facet_wrap(~EWA_sigma)+
  geom_tile()+
  labs(x="Order of acquisition", y="Order of production", fill="Avg. delay")+
  scale_fill_viridis_c(option="plasma", trans="sqrt",  direction=-1) +
  scale_x_continuous(breaks=c(1,4,8,12,16,20,24))+
  scale_y_continuous(breaks=c(1,4,8,12,16,20,24))+
  theme_classic()

ggarrange(g1,p2,nrow=2, labels=c("","B"))
