library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Panel A: relative model support per condition
load(file="../model_outputs/Rda_files/df_NBDA_figure_data_binary.Rda")
summary(df)
df$diffusion = factor(df$diffusion, levels=c("social diffusion","asocial diffusion"))
levels(df$diffusion)[2] <- "asocial learning"
df_sanity = df %>% filter(feeder_data=="acquisition")
df_sanity$facet="Idealized data"

load(file="../model_outputs/Rda_files/df_NBDA_figure_data_proportional.Rda")
df$diffusion = factor(df$diffusion, levels=c("social diffusion","asocial diffusion"))
levels(df$diffusion)[2] <- "asocial learning"
df_reality = df %>% filter(feeder_data=="first production")
df_reality$facet="Realistic data"
df_combined = bind_rows(df_sanity,df_reality)

summary(df_combined)
p1 = ggplot(df_combined,aes(x=diffusion,y=aicc_asocial-aicc_social))+
  facet_wrap(~facet)+
  geom_hline(yintercept = 0, linetype="dashed")+
  labs(x="Diffusion type",y="Support for social transmission (AICC_a - AICC_s)", color="Estimated s")+
  geom_jitter(alpha=0.4)+
  #geom_jitter(data= df_combined %>% filter(est_s>=10), color="blue", alpha=0.6)+
  #geom_jitter(data= df_combined %>% filter(est_s<10), aes(fill=est_s), alpha=1)+
  geom_boxplot(aes(fill=feeder_data), alpha=0.6,outlier.alpha = 0)+
  scale_fill_viridis_d(begin=0.5, guide=F)+
  theme_classic()

library(rethinking)
df_combined %>% group_by(facet,diffusion) %>% mutate(support_social=aicc_asocial-aicc_social) %>% summarize(mean(support_social),HPDI(support_social))

#Panel B and C: delay and divergence correlates with type 1 and 2 errors
load(file="../model_outputs/Rda_files/df_NBDA_figure_data_proportional.Rda")
df$diffusion = factor(df$diffusion, levels=c("social diffusion","asocial diffusion"))
levels(df$diffusion)[2] <- "asocial learning"
social_temp = df %>% ungroup() %>% filter(feeder_data=="first production", diffusion == "social diffusion") %>% mutate(correct_support=aicc_social<aicc_asocial)
load(file="../model_outputs/Rda_files/df_NBDA_SP.Rda")

df_div = df_NBDA_SP %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% ungroup() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_div = df_div %>% select(sim,delta,order_acquisition,order_production) %>% group_by(sim) %>% summarize(avgdelta=mean(delta), manhattan_delay=sum(abs(delta))/100, manhattan_divergence=sum(abs(order_acquisition-order_production))/99, divergence = 1-sum(order_acquisition==order_production)/n())
df_social = left_join(df_div,social_temp)
summary(df_div)

asocial_temp = df %>% ungroup() %>% filter(feeder_data=="first production", diffusion == "asocial learning") %>% mutate(correct_support=aicc_social>aicc_asocial)
load(file="../model_outputs/Rda_files/df_NBDA_AP.Rda")
df_div = df_NBDA_AP %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% ungroup() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_div %>% summarize(avgdelta=mean(delta), divergence = 1-sum(order_acquisition==order_production)/n())
df_div = df_div %>% select(sim,delta,order_acquisition,order_production) %>% group_by(sim) %>% summarize(avgdelta=mean(delta), manhattan_delay=sum(abs(delta))/100, manhattan_divergence=sum(abs(order_acquisition-order_production))/99, divergence = 1-sum(order_acquisition==order_production)/n())
df_asocial = left_join(df_div,asocial_temp)
df_combined = bind_rows(df_social, df_asocial)

p2 = ggplot(df_combined, aes(x=correct_support,y=manhattan_delay))+
  facet_wrap(~diffusion)+
  geom_jitter(alpha=0.4)+
  geom_boxplot(alpha=0.6, fill="yellow", outlier.alpha = 0)+
  labs(x="Correct model supported", y= "Delay score")+
  theme_classic()

p3 = ggplot(df_combined, aes(x=correct_support,y=manhattan_divergence))+
  facet_wrap(~diffusion)+
  geom_jitter(alpha=0.4)+
  geom_boxplot(alpha=0.6, fill="yellow", outlier.alpha = 0)+
  labs(x="Correct model supported", y= "Divergence score")+
  theme_classic()

ggarrange(p1,p2,p3,widths=c(1,1,1), labels=c("A","B", "C"), ncol=3)
ggsave("../output/Fig_4_NBDA.png", width=15,height=6,scale=2,units="cm")



