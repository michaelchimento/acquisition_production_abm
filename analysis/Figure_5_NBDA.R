library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../model_outputs/Rda_files/NBDA_figure_data_binary.Rda")
df$diffusion = factor(df$diffusion, levels=c("social diffusion","asocial diffusion"))
df_sanity = df %>% filter(est_s<10,feeder_data=="acquisition")
df_sanity$facet="Idealized data"

load(file="../model_outputs/Rda_files/NBDA_figure_data_proportional.Rda")
df$diffusion = factor(df$diffusion, levels=c("social diffusion","asocial diffusion"))
df_reality = df %>% filter(est_s<10,feeder_data=="first production")
df_reality$facet="Realistic data - transmission x production"
df_combined = bind_rows(df_sanity,df_reality)

p1 = ggplot(df_combined,aes(x=diffusion,y=aicc_asocial-aicc_social))+
  facet_wrap(~facet)+
  geom_hline(yintercept = 0, linetype="dashed")+
  labs(x="Diffusion type",y="Support for social transmission (AICC_a - AICC_s)", color="Estimated s")+
  geom_jitter(aes(color=est_s), alpha=0.6)+
  geom_boxplot(aes(fill=feeder_data), alpha=0.6, scale="width",outlier.alpha = 0)+
  scale_fill_viridis_d(begin=0.5, guide=F)+
  theme_classic()

ggplot(df_combined,aes(x=diffusion,y=aicc_asocial-aicc_social))+
  facet_wrap(~facet)+
  geom_hline(yintercept = 0, linetype="dashed")+
  labs(x="Diffusion type",y="Support for social transmission (AICC_a - AICC_s)", color="Estimated s")+
  geom_jitter(aes(color=EWA_soc_info_weight), alpha=0.6)+
  geom_boxplot(aes(fill=feeder_data), alpha=0.6, scale="width",outlier.alpha = 0)+
  scale_fill_viridis_d(begin=0.5, guide=F)+
  theme_classic()

ggplot(df_combined,aes(x=diffusion,y=aicc_asocial-aicc_social))+
  facet_wrap(~facet)+
  geom_hline(yintercept = 0, linetype="dashed")+
  labs(x="Diffusion type",y="Support for social transmission (AICC_a - AICC_s)", color="Estimated s")+
  geom_jitter(aes(color=as.factor(memory_window)), alpha=0.6)+
  geom_boxplot(aes(fill=feeder_data), alpha=0.6, scale="width",outlier.alpha = 0)+
  scale_fill_viridis_d(begin=0.5, guide=F)+
  theme_classic()

ggplot(df_combined,aes(x=diffusion,y=aicc_asocial-aicc_social))+
  facet_wrap(~facet)+
  geom_hline(yintercept = 0, linetype="dashed")+
  labs(x="Diffusion type",y="Support for social transmission (AICC_a - AICC_s)", color="Estimated s")+
  geom_jitter(aes(color=as.factor(EWA_recent_payoff_weight)), alpha=0.6)+
  geom_boxplot(aes(fill=feeder_data), alpha=0.6, scale="width",outlier.alpha = 0)+
  scale_fill_viridis_d(begin=0.5, guide=F)+
  theme_classic()


ggplot(df_combined,aes(x=diffusion,y=aicc_asocial-aicc_social))+
  facet_wrap(~facet)+
  geom_hline(yintercept = 0, linetype="dashed")+
  labs(x="Diffusion type",y="Support for social transmission (AICC_a - AICC_s)", color="Estimated s")+
  geom_jitter(aes(color=as.factor(memory_window)), alpha=0.6)+
  geom_boxplot(aes(fill=feeder_data), alpha=0.6, scale="width",outlier.alpha = 0)+
  scale_fill_viridis_d(begin=0.5, guide=F)+
  theme_classic()

load(file="../model_outputs/Rda_files/NBDA_figure_data_proportional.Rda")
df$diffusion = factor(df$diffusion, levels=c("social diffusion","asocial diffusion"))
social_temp = df %>% ungroup() %>% filter(feeder_data=="first production", diffusion == "social diffusion") %>% mutate(correct_support=aicc_social<aicc_asocial)
load(file="../model_outputs/Rda_files/df_NBDA_SP.Rda")

df_div = df_NBDA_SP %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% ungroup() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_div = df_div %>% select(sim,delta,order_acquisition,order_production) %>% group_by(sim) %>% summarize(avgdelta=mean(delta), manhattan=sum(abs(delta)),manhattan_divergence=sum(abs(order_acquisition-order_production)), divergence = 1-sum(order_acquisition==order_production)/n())
df_social = left_join(df_div,social_temp)
summary(df_div)

asocial_temp = df %>% ungroup() %>% filter(feeder_data=="first production", diffusion == "asocial diffusion") %>% mutate(correct_support=aicc_social>aicc_asocial)
load(file="../model_outputs/Rda_files/df_NBDA_AP.Rda")
df_div = df_NBDA_AP %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% ungroup() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_div %>% summarize(avgdelta=mean(delta), divergence = 1-sum(order_acquisition==order_production)/n())
df_div = df_div %>% select(sim,delta,order_acquisition,order_production) %>% group_by(sim) %>% summarize(avgdelta=mean(delta), manhattan_delay=sum(abs(delta)), manhattan_divergence=sum(abs(order_acquisition-order_production)), divergence = 1-sum(order_acquisition==order_production)/n())
df_asocial = left_join(df_div,asocial_temp)
df_combined = bind_rows(df_social, df_asocial)

summary(df_combined)

p2 = ggplot(df_combined, aes(x=correct_support,y=avgdelta))+
  facet_wrap(~diffusion)+
  geom_jitter(alpha=0.6)+
  geom_boxplot(alpha=0.6, fill="yellow", outlier.alpha = 0)+
  labs(x="Correct model supported", y= "Avg. delay (first production - acquisition)")+
  theme_classic()

p3 = ggplot(df_combined, aes(x=correct_support,y=divergence))+
  facet_wrap(~diffusion)+
  geom_jitter(alpha=0.6)+
  geom_boxplot(alpha=0.6, fill="yellow", outlier.alpha = 0)+
  labs(x="Correct model supported", y= "Avg. delay (first production - acquisition)")+
  theme_classic()

p4 = ggplot(df_combined, aes(x=correct_support,y=manhattan_delay))+
  facet_wrap(~diffusion)+
  geom_jitter(alpha=0.6)+
  geom_boxplot(alpha=0.6, fill="yellow", outlier.alpha = 0)+
  labs(x="Correct model supported", y= "Manhattan distance")+
  theme_classic()

p5 = ggplot(df_combined, aes(x=correct_support,y=manhattan_divergence))+
  facet_wrap(~diffusion)+
  geom_jitter(alpha=0.6)+
  geom_boxplot(alpha=0.6, fill="yellow", outlier.alpha = 0)+
  labs(x="Correct model supported", y= "Manhattan distance")+
  theme_classic()

ggplot(df_combined,aes(x=manhattan_delay,y=manhattan_divergence))+
  geom_point()

ggarrange(p1,p4,widths=c(2,1), labels=c("A","B"))
ggsave("../output/Fig_5_NBDA.png", width=13,height=6,scale=2,units="cm")



