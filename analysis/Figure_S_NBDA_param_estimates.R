library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


load(file="../model_outputs/Rda_files/df_NBDA_figure_data_binary.Rda")
df = df_ideal %>% mutate(support=aicc_asocial-aicc_social)
summary(df)

p1=ggplot(df %>% filter(condition=="social transmission"), aes(x=est_s,y=est_rate))+
  geom_point(aes(color=support),alpha=0.9)+
  geom_vline(xintercept = 5, color="blue",linetype="dashed")+
  geom_hline(yintercept = 0.05, color="red",linetype="dashed")+
  xlim(c(0,10))+
  scale_color_continuous(limits=c(-5,60), breaks=c(-5,0,25,50))+
  labs(x="Estimated s", y="Estimated base rate", title="Social transmission (idealized data)", color="support\nsocial")+
  theme_classic()

p2=ggplot(df %>% filter(condition=="asocial learning"), aes(x=est_s,y=est_rate))+
  geom_point(aes(color=support),alpha=0.5)+
  geom_vline(xintercept = 0, color="blue",linetype="dashed")+
  geom_hline(yintercept = 0.05, color="red",linetype="dashed")+
  xlim(c(0,10))+
  scale_color_continuous(limits=c(-5,60), breaks=c(-5,0,25,50))+

  labs(x="Estimated s", y="Estimated base rate", title="Asocial learning (idealized data)", color="support\nsocial")+
  theme_classic()

load(file="../model_outputs/Rda_files/df_NBDA_figure_data_proportional.Rda")
df = df_realistic %>% mutate(support=aicc_asocial-aicc_social) %>% filter(model_type=="weighted")
summary(df)
p3=ggplot(df %>% filter(data_type=="time_of_first_production", condition=="social transmission"), aes(x=est_s,y=est_rate))+
  geom_point(aes(color=support),alpha=0.5)+
  geom_vline(xintercept = 5, color="blue",linetype="dashed")+
  geom_hline(yintercept = 0.05, color="red",linetype="dashed")+
  xlim(c(0,10))+
  scale_color_continuous(limits=c(-5,60), breaks=c(-5,0,25,50))+

  labs(x="Estimated s", y="Estimated base rate", title="Social transmission (realistic data)", color="support\nsocial")+
  theme_classic()

p4=ggplot(df %>% filter(data_type=="time_of_first_production", condition=="asocial learning"), aes(x=est_s,y=est_rate))+
  geom_point(aes(color=support),alpha=0.5)+
  geom_vline(xintercept = 0, color="blue",linetype="dashed")+
  geom_hline(yintercept = 0.05, color="red",linetype="dashed")+
  xlim(c(0,10))+
  scale_color_continuous(limits=c(-5,60), breaks=c(-5,0,25,50))+

  labs(x="Estimated s", y="Estimated base rate", title="Asocial learning (realistic data)", color="support\nsocial")+
  theme_classic()

ggarrange(p1,p2,p3,p4, labels=c("A","B","C","D"), common.legend = T, legend = "right")
ggsave("../output/Fig_S_NBDA_param_estimates.png", width=12,height=10,scale=2,units="cm")

