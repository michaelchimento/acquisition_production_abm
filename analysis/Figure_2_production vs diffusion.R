library(tidyverse)
library(ggpubr)
library(ggridges)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#chart proportions
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoff2.Rda")



fun_range <- function(x) {                              # Create user-defined function
  (x - min(x)) / (max(x) - min(x))
}

df = df_ABM_equiv_payoff %>% filter(EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative")

df = df %>%
  group_by(sim)%>%
  filter(timestep<=(max(timestep)-45)) %>%
  mutate(scaled_time=fun_range(timestep)) %>%
  arrange(timestep)

df = df %>% mutate(behavior_a=behavior_a/pop_size,behavior_b=behavior_b/pop_size,num_produced_b=num_produced_b/pop_size) %>% pivot_longer(cols=c(behavior_a,behavior_b,num_know_novel,num_produced_b))
df$name = factor(df$name, levels=c("num_know_novel","num_produced_b","behavior_a","behavior_b"))

p1 = ggplot(df, aes(x=scaled_time, color=name))+
  stat_summary_bin(aes(y=value), size=1, geom="errorbar", fun.data="mean_cl_boot", bins=15)+
  stat_summary_bin(aes(y=value), size=2, geom="point", bins=15)+
  stat_summary_bin(aes(y=value), size=1, geom="line", bins=15)+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Scaled time",y="Proportion",color="")+
  scale_color_manual(values=c("black","gray","#5FD84F","#007C76"), labels=c("know novel","produced first novel","freq. established","freq. novel"), drop=FALSE)+
  theme_classic()
p1

#compare order of acquisition and production
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod2.Rda")

df_equiv_payoffs_acq_prod %>%
  group_by(sim)%>%
  filter(timestep_acquisition_b==max(timestep_acquisition_b)) %>%
  ungroup() %>%
  summarize(mean(timestep_acquisition_b), HPDI(timestep_acquisition_b))

df_equiv_payoffs_acq_prod %>%
  group_by(sim)%>%
  filter(timestep_production_b==max(timestep_production_b)) %>%
  ungroup() %>%
  summarize(mean(timestep_production_b), HPDI(timestep_production_b))

df_equiv_payoffs_acq_prod %>%
  filter(EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative",memory_window==20) %>%
  group_by(sim)%>%
  filter(timestep_acquisition_b==max(timestep_acquisition_b)) %>%
  ungroup() %>%
  group_by(NBDA_s_param) %>%
  summarize(mean(timestep_acquisition_b), HPDI(timestep_acquisition_b))

df_equiv_payoffs_acq_prod %>%
  filter(EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative",memory_window==20) %>%
  group_by(sim)%>%
  filter(timestep_acquisition_b==max(timestep_acquisition_b)) %>%
  ungroup() %>%
  group_by(graph_type) %>%
  summarize(mean(timestep_acquisition_b), HPDI(timestep_acquisition_b))

df_equiv_payoffs_acq_prod %>%
  group_by(sim)%>%
  filter(timestep_acquisition_b==max(timestep_acquisition_b)) %>%
  ungroup() %>%
  group_by(EWA_tau) %>%
  summarize(mean(timestep_acquisition_b), HPDI(timestep_acquisition_b))
84/54
76/66

summary(df_equiv_payoffs_acq_prod)


df = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())

df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

p2= ggplot(df %>% group_by(order_production,order_acquisition) %>% mutate(count=n()), aes(x=order_acquisition,y=order_production, fill=count))+
  geom_tile() +
  labs(x="Order of acquisition", y="Order of production", fill="count")+
  scale_fill_continuous(type = "viridis",trans="log10", direction=-1) +
  scale_x_continuous(breaks=c(1,4,8,12,16,20,24))+
  scale_y_continuous(breaks=c(1,4,8,12,16,20,24))+
  theme_classic()
p2
df_summary = df %>% group_by(order_acquisition,order_production) %>% summarize(mean_delta=mean(delta))

p3= ggplot(df_summary, aes(x=order_acquisition,y=order_production, fill=mean_delta+1))+
  geom_tile()+
  labs(x="Order of acquisition", y="Order of production", fill="Avg. delta")+
  scale_x_continuous(breaks=c(1,4,8,12,16,20,24))+
  scale_y_continuous(breaks=c(1,4,8,12,16,20,24))+
  scale_fill_viridis_c(option="plasma", direction=-1, trans="log10",  end=0.8, breaks=c(1,10,30,80)) +
  theme_classic()
p3

library(rethinking)
df %>% ungroup() %>% filter(order_acquisition!=1) %>% summarize(n= sum(order_acquisition==order_production)/n())
df %>% ungroup() %>% filter(order_acquisition!=1) %>% group_by(NBDA_s_param) %>% summarize(n= 1-sum(order_acquisition==order_production)/n())
df %>% ungroup() %>% group_by(NBDA_s_param) %>% summarize(mean(delta),HPDI(delta))



df %>% ungroup() %>% filter(order_acquisition!=1) %>% group_by(graph_type) %>% summarize(n=sum(order_acquisition==order_production)/n())
df %>% ungroup() %>% group_by(graph_type) %>% summarize(mean(delta),HPDI(delta))

ggplot(df %>% filter(EWA_tau=="non-conservative", EWA_conformity==1,memory_window==10)%>% group_by(graph_type,order_production,order_acquisition) %>% mutate(count=n()), aes(x=order_acquisition,y=order_production, fill=count))+
  facet_wrap(~graph_type)+
  geom_tile() +
  labs(x="Order of acquisition", y="Order of production", fill="count")+
  scale_fill_continuous(type = "viridis",trans="log10", direction=-1) +
  scale_x_continuous(breaks=c(1,4,6,8,12,16,20,24))+
  scale_y_continuous(breaks=c(1,4,8,6,12,16,20,24))+
  theme_classic()


df %>% filter(EWA_tau=="non-conservative", EWA_conformity==1,memory_window==10) %>% group_by(NBDA_s_param) %>% summarize(n= sum(order_acquisition==order_production)/n())

ggarrange(p1,p2, labels = c("A","B"),nrow=1)
ggsave("../output/Fig_2_separate_processes.png",height=5,width=14,scale=2.1,units="cm")

df = df %>% mutate(NBDA_s_param = as.factor(NBDA_s_param))
summary(df)
levels(df$NBDA_s_param) = c("s=15","s=5","s=10")
df$NBDA_s_param = factor(df$NBDA_s_param, levels=c("s=5","s=10","s=15"))

p3= ggplot(df %>% filter(EWA_tau=="non-conservative", EWA_conformity==1,memory_window==10), aes(x=as.factor(order_acquisition),y=as.factor(order_production)))+
  facet_wrap(~NBDA_s_param)+
  geom_bin2d()+
  labs(x="Order of acquisition", y="Order of production", fill="count")+
  scale_fill_continuous(type = "viridis",trans="log10", direction=-1) +
  theme_classic()
p3
ggsave("../output/Fig_S1_NBDAs_variance.png",height=5,width=14,scale=2,units="cm")




#densities of deltas
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod2.Rda")
df = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

ggplot(df, aes(x=order_acquisition,y=delta,color=EWA_soc_info_weight))+
  stat_summary(geom="errorbar")+
  stat_summary(geom="line")+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  labs(x="Order of acquisition", y="Delta (production - acquisition)", color="Social info bias")+
  theme_classic()


ggplot(df, aes(x=order_acquisition,y=delta,color=EWA_recent_payoff_weight))+
  stat_summary()+
  stat_summary(geom="line")




df = df_equiv_payoffs_acq_prod %>% filter(conformity=="None") %>% mutate(delta=timestep_production_b - timestep_acquisition_b)

ggplot(df, aes(x=delta, color=EWA_soc_info_weight))+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  geom_density()+
  coord_trans(x="sqrt")+
  theme_classic()

ggplot(df, aes(x=delta, color=EWA_recent_payoff_weight))+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  geom_density()+
  coord_trans(x="sqrt")+
  theme_classic()

