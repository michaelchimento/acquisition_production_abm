library(tidyverse)
library(ggpubr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../model_outputs/Rda_files/df_maintext.Rda")

fun_range <- function(x) {(x - min(x)) / (max(x) - min(x))}
rsquare <- function (x, y) cor(x, y) ^ 2


#Panel A: acquisition and production curves over normalized time
df = df_ABM_maintext %>% filter(EWA_sigma=="medium", EWA_rho=="medium", EWA_chi=="linear bias", EWA_alpha=="risk-neutral", memory_window==10)

df = df %>%
  group_by(sim)%>%
  filter(timestep<=(max(timestep+1))) %>%
  mutate(scaled_time=fun_range(timestep+1)) %>%
  arrange(timestep)

df = df %>% mutate(behavior_a=behavior_a/pop_size,behavior_b=behavior_b/pop_size,num_produced_b=num_produced_b/pop_size) %>% pivot_longer(cols=c(behavior_a,behavior_b,num_know_novel,num_produced_b))
df$name = factor(df$name, levels=c("num_know_novel","num_produced_b","behavior_a","behavior_b"))

summary(df)

p1 = ggplot(df, aes(x=scaled_time, color=name))+
  stat_summary_bin(aes(y=value), size=1, geom="errorbar", fun.data="mean_cl_boot", bins=10)+
  stat_summary_bin(aes(y=value), size=2, geom="point", bins=10)+
  stat_summary_bin(aes(y=value), size=1, geom="line", bins=10)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  scale_x_continuous(limits=c(-.1,1.1), breaks=c(0,.25,.5,.75,1))+
  labs(x="Scaled time",y="Proportion",color="")+
  scale_color_manual(values=c("black","gray","#5FD84F","#007C76"), labels=c("know novel","produced first novel","freq. established","freq. novel"), drop=FALSE)+
  theme_classic()
p1

#compare order of acquisition and production
load(file="../model_outputs/Rda_files/df_maintext_acq_prod.Rda")
summary(df_maintext_acq_prod)
df = df_maintext_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

df = df %>% filter(EWA_sigma=="medium", EWA_rho=="medium", EWA_chi=="linear bias", EWA_alpha=="risk-neutral", memory_window==10)

rsquare(df$order_production,df$order_acquisition)

p2= ggplot(df %>% group_by(order_production,order_acquisition) %>% mutate(count=n()), aes(x=order_acquisition,y=order_production, fill=count))+
  geom_tile() +
  labs(x="Order of acquisition", y="Order of production", fill="count")+
  scale_fill_continuous(type = "viridis",trans="log10", direction=1) +
  scale_x_continuous(breaks=c(1,4,8,12,16,20,24))+
  scale_y_continuous(breaks=c(1,4,8,12,16,20,24))+
  theme_classic()

#compare order of acquisition and production
load(file="../model_outputs/Rda_files/df_maintext_acq_prod.Rda")
summary(df_maintext_acq_prod)
df = df_maintext_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

df %>% ungroup() %>% group_by(EWA_sigma, EWA_rho, EWA_chi, EWA_alpha, memory_window) %>% summarize(divergence=1-sum(order_acquisition==order_production)/n()) %>% arrange(desc(divergence))

df = df %>% filter(EWA_sigma=="weak", EWA_rho=="strong", EWA_chi=="conformist bias", EWA_alpha=="risk-averse", memory_window==10)
1-.168
df %>% ungroup() %>% summarize(sum(order_acquisition==order_production)/n())

rsquare(df$order_production,df$order_acquisition)


p3= ggplot(df %>% group_by(order_production,order_acquisition) %>% mutate(count=n()), aes(x=order_acquisition,y=order_production, fill=count))+
  geom_tile() +
  labs(x="Order of acquisition", y="Order of production", fill="count")+
  scale_fill_continuous(type = "viridis",trans="log10", direction=1) +
  scale_x_continuous(breaks=c(1,4,8,12,16,20,24))+
  scale_y_continuous(breaks=c(1,4,8,12,16,20,24))+
  theme_classic()


g1=ggarrange(p2,p3, labels=c("B","C"), common.legend = T, legend="right")
ggarrange(p1,g1, labels = c("A"),nrow=1, widths=c(1,1.6), legend="left")
ggsave("../output/Fig_2_dynamics.png",height=4,width=15,scale=2.1,units="cm")
