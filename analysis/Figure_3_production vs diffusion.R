library(tidyverse)
library(ggpubr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../model_outputs/Rda_files/df_GEN_equiv_payoff.Rda")

fun_range <- function(x) {(x - min(x)) / (max(x) - min(x))}

#Panel A: acquisition and production curves over normalized time
df = df_ABM_equiv_payoff %>% filter(graph_type=="random regular", EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative", NBDA_s_param==5, memory_window==10)

df = df %>%
  group_by(sim)%>%
  filter(timestep<=(max(timestep)-45)) %>%
  mutate(scaled_time=fun_range(timestep)) %>%
  arrange(timestep)

df = df %>% mutate(behavior_a=behavior_a/pop_size,behavior_b=behavior_b/pop_size,num_produced_b=num_produced_b/pop_size) %>% pivot_longer(cols=c(behavior_a,behavior_b,num_know_novel,num_produced_b))
df$name = factor(df$name, levels=c("num_know_novel","num_produced_b","behavior_a","behavior_b"))

p1 = ggplot(df, aes(x=scaled_time, color=name))+
  stat_summary_bin(aes(y=value), size=1, geom="errorbar", fun.data="mean_cl_boot", bins=10)+
  stat_summary_bin(aes(y=value), size=2, geom="point", bins=10)+
  stat_summary_bin(aes(y=value), size=1, geom="line", bins=10)+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Scaled time",y="Proportion",color="")+
  scale_color_manual(values=c("black","gray","#5FD84F","#007C76"), labels=c("know novel","produced first novel","freq. established","freq. novel"), drop=FALSE)+
  theme_classic()

#compare order of acquisition and production
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod.Rda")
df = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

#df = df %>% filter(graph_type=="random regular", EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_conformity==1, EWA_tau=="non-conservative", NBDA_s_param==5, memory_window==10)

p2= ggplot(df %>% group_by(order_production,order_acquisition) %>% mutate(count=n()), aes(x=order_acquisition,y=order_production, fill=count))+
  geom_tile() +
  labs(x="Order of acquisition", y="Order of production", fill="count")+
  scale_fill_continuous(type = "viridis",trans="log10", direction=-1) +
  scale_x_continuous(breaks=c(1,4,8,12,16,20,24))+
  scale_y_continuous(breaks=c(1,4,8,12,16,20,24))+
  theme_classic()

ggarrange(p1,p2, labels = c("A","B"),nrow=1)
ggsave("../output/Fig_3_curves_divergence.png",height=5,width=14,scale=2.1,units="cm")
