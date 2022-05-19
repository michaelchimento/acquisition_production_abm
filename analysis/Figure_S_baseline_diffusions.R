library(tidyverse)
library(ggpubr)
library(rethinking)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

PI = function (samples, prob = 0.92)
{
  x <- sapply(prob, function(p) {
    a <- (1 - p)/2
    quantile(samples, probs = c(a, 1 - a))
  })
  n <- length(prob)
  result <- rep(0, n * 2)
  for (i in 1:n) {
    low_idx <- n + 1 - i
    up_idx <- n + i
    result[low_idx] <- x[1, i]
    result[up_idx] <- x[2, i]
    a <- (1 - prob[i])/2
    names(result)[low_idx] <- concat(round(a * 100, 0), "%")
    names(result)[up_idx] <- concat(round((1 - a) * 100,
                                          0), "%")
  }
  return(result)
}

#####Equivalent attraction scores####
load(file="../model_outputs/Rda_files/df_baseline_P.Rda")


fun_range <- function(x) {                              # Create user-defined function
  (x - min(x)) / (max(x) - min(x))
}

df = df_baseline_P
summary(df)
df = df %>%
  group_by(sim)%>%
  filter(timestep<=(max(timestep))) %>%
  mutate(scaled_time=fun_range(timestep)) %>%
  arrange(timestep)

df = df %>% mutate(behavior_a=behavior_a/pop_size,behavior_b=behavior_b/pop_size,num_produced_b=num_produced_b/pop_size) %>% pivot_longer(cols=c(behavior_a,behavior_b,num_know_novel,num_produced_b))
df$name = factor(df$name, levels=c("num_know_novel","num_produced_b","behavior_a","behavior_b"))



p1 = ggplot(df %>% filter(), aes(x=scaled_time, color=name))+
  stat_summary_bin(aes(y=value), size=1, geom="errorbar", fun.data="mean_cl_boot", bins=10)+
  stat_summary_bin(aes(y=value), size=2, geom="point", bins=10)+
  stat_summary_bin(aes(y=value), size=1, geom="line", bins=10)+
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(-.1,1.1), breaks=c(0,.25,.5,.75,1))+
  labs(x="Scaled time",y="Proportion",color="",title="Production only")+
  scale_color_manual(values=c("black","gray","#5FD84F","#007C76"), labels=c("know novel","produced first novel","freq. established","freq. novel"), drop=FALSE)+
  theme_classic()
p1

#####Baseline Transmission####
load(file="../model_outputs/Rda_files/df_baseline_T.Rda")
fun_range <- function(x) {                              # Create user-defined function
  (x - min(x)) / (max(x) - min(x))
}

df = df_baseline_T %>% filter(EWA_sigma=="medium", EWA_rho=="medium", EWA_chi=="linear bias", EWA_alpha=="risk-neutral", memory_window==10)
summary(df)
df = df %>%
  group_by(sim)%>%
  filter(timestep<=(max(timestep))) %>%
  mutate(scaled_time=fun_range(timestep)) %>%
  arrange(timestep)

df = df %>% mutate(behavior_a=behavior_a/pop_size,behavior_b=behavior_b/pop_size,num_produced_b=num_produced_b/pop_size) %>% pivot_longer(cols=c(behavior_a,behavior_b,num_know_novel,num_produced_b))
df$name = factor(df$name, levels=c("num_know_novel","num_produced_b","behavior_a","behavior_b"))

p2 = ggplot(df %>% filter(), aes(x=scaled_time, color=name))+
  stat_summary_bin(aes(y=value), size=1, geom="errorbar", fun.data="mean_cl_boot", bins=10)+
  stat_summary_bin(aes(y=value), size=2, geom="point", bins=10)+
  stat_summary_bin(aes(y=value), size=1, geom="line", bins=10)+
  scale_x_continuous(limits=c(-.1,1.1), breaks=c(0,.25,.5,.75,1))+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Scaled time",y="Proportion",color="", title="Acquisition disconnected from production")+
  scale_color_manual(values=c("black","gray","#5FD84F","#007C76"), labels=c("know novel","produced first novel","freq. established","freq. novel"), drop=FALSE)+
  theme_classic()


ggarrange(p1,p2,labels=LETTERS[1:2], common.legend = T,legend="right")
ggsave("../output/Fig_S_baseline_diffusions.png",height=5,width=14,scale=2,units="cm")


#### metrics ####

load(file="../model_outputs/Rda_files/df_baseline_p_acq_prod.Rda")
summary(df_baseline_p_acq_prod)
library(rethinking)
df_baseline_p_acq_prod %>%
  group_by(sim)%>%
  filter(timestep_production_b==max(timestep_production_b)) %>%
  ungroup() %>%
  group_by(EWA_sigma) %>%
  summarize(mean(timestep_production_b), PI(timestep_production_b))


load(file="../model_outputs/Rda_files/df_baseline_t_acq_prod.Rda")
summary(df_baseline_p_acq_prod)
df_baseline_t_acq_prod %>%
  #filter( EWA_rho=="medium", EWA_sigma=="medium", EWA_chi=="linear bias", EWA_alpha=="risk-neutral",memory_window==10) %>%
  group_by(sim)%>%
  filter(timestep_acquisition_b==max(timestep_acquisition_b)) %>%
  ungroup() %>%
  summarize(mean(timestep_acquisition_b), PI(timestep_acquisition_b))

df_baseline_t_acq_prod %>%
  #filter( EWA_rho=="medium", EWA_sigma=="medium", EWA_chi=="linear bias", EWA_alpha=="risk-neutral",memory_window==10) %>%
  group_by(sim)%>%
  filter(timestep_production_b==max(timestep_production_b)) %>%
  ungroup() %>%
  summarize(mean(timestep_production_b), PI(timestep_production_b))


####Preprogrammed attraction scores####
load(file="../model_outputs/Rda_files/df_baseline_P_preprogrammed.Rda")
fun_range <- function(x) {                              # Create user-defined function
  (x - min(x)) / (max(x) - min(x))
}

df = df_baseline_P %>% filter(EWA_sigma=="medium", EWA_rho=="medium", EWA_chi=="linear bias", EWA_alpha=="risk-neutral")
summary(df)
df = df %>%
  group_by(sim)%>%
  filter(timestep<=(max(timestep)-45)) %>%
  mutate(scaled_time=fun_range(timestep)) %>%
  arrange(timestep)

df = df %>% mutate(behavior_a=behavior_a/pop_size,behavior_b=behavior_b/pop_size,num_produced_b=num_produced_b/pop_size) %>% pivot_longer(cols=c(behavior_a,behavior_b,num_know_novel,num_produced_b))
df$name = factor(df$name, levels=c("num_know_novel","num_produced_b","behavior_a","behavior_b"))

p3 = ggplot(df %>% filter(name!="num_know_novel"), aes(x=scaled_time, color=name))+
  stat_summary_bin(aes(y=value), size=1, geom="errorbar", fun.data="mean_cl_boot", bins=20)+
  stat_summary_bin(aes(y=value), size=2, geom="point", bins=20)+
  stat_summary_bin(aes(y=value), size=1, geom="line", bins=20)+
  scale_y_continuous(limits=c(0,1))+
  labs(x="Scaled time",y="Proportion",color="")+
  scale_color_manual(values=c("black","gray","#5FD84F","#007C76"), labels=c("know novel","produced first novel","freq. established","freq. novel"), drop=FALSE)+
  theme_classic()

load(file="../model_outputs/Rda_files/df_baseline_p_acq_prod_preprogrammed.Rda")
summary(df_baseline_p_acq_prod)
library(rethinking)
df_baseline_p_acq_prod %>%
  filter( EWA_rho=="medium", EWA_chi=="linear bias", EWA_alpha=="risk-neutral",memory_window==10) %>%
  group_by(sim)%>%
  filter(timestep_production_b==max(timestep_production_b)) %>%
  ungroup() %>%
  group_by(EWA_sigma) %>%
  summarize(mean(timestep_production_b), PI(timestep_production_b))