library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


load(file="../model_outputs/Rda_files/df_NBDA_figure_data_binary.Rda")
df = df %>% mutate(support=aicc_asocial-aicc_social)
summary(df)
p1=ggplot(df %>% filter(feeder_data=="acquisition", diffusion=="social diffusion"), aes(x=est_s,y=est_rate))+
  geom_point(aes(color=support),alpha=0.9)+
  geom_vline(xintercept = 5, color="blue",linetype="dashed")+
  geom_hline(yintercept = 0.05, color="red",linetype="dashed")+
  xlim(c(0,10))+
  scale_colour_continuous(limits=c(-50,300),breaks=c(-50,0,50,100,150,200,250,300))+
  labs(x="Estimated s", y="Estimated base rate", title="Social diffusion (idealized data)", color="support\nsocial")+
  theme_classic()

p2=ggplot(df %>% filter(feeder_data=="acquisition", diffusion=="asocial diffusion"), aes(x=est_s,y=est_rate))+
  geom_point(aes(color=support),alpha=0.5)+
  geom_vline(xintercept = 0, color="blue",linetype="dashed")+
  geom_hline(yintercept = 0.05, color="red",linetype="dashed")+
  xlim(c(0,10))+
  scale_color_continuous(limits=c(-50,300), breaks=c(-50,0,50,100,150,200,250,300))+

  labs(x="Estimated s", y="Estimated base rate", title="Asocial diffusion (idealized data)", color="support\nsocial")+
  theme_classic()

load(file="../model_outputs/Rda_files/df_NBDA_figure_data_proportional.Rda")
df = df %>% mutate(support=aicc_asocial-aicc_social)
summary(df)
p3=ggplot(df %>% filter(feeder_data=="first production", diffusion=="social diffusion"), aes(x=est_s,y=est_rate))+
  geom_point(aes(color=support),alpha=0.5)+
  geom_vline(xintercept = 5, color="blue",linetype="dashed")+
  geom_hline(yintercept = 0.05, color="red",linetype="dashed")+
  xlim(c(0,10))+
  scale_color_continuous(limits=c(-50,300), breaks=c(-50,0,50,100,150,200,250,300))+

  labs(x="Estimated s", y="Estimated base rate", title="Social diffusion (realistic data)", color="support\nsocial")+
  theme_classic()

p4=ggplot(df %>% filter(feeder_data=="first production", diffusion=="asocial diffusion"), aes(x=est_s,y=est_rate))+
  geom_point(aes(color=support),alpha=0.5)+
  geom_vline(xintercept = 0, color="blue",linetype="dashed")+
  geom_hline(yintercept = 0.05, color="red",linetype="dashed")+
  xlim(c(0,10))+
  scale_color_continuous(limits=c(-50,300), breaks=c(-50,0,50,100,150,200,250,300))+

  labs(x="Estimated s", y="Estimated base rate", title="Asocial diffusion (realistic data)", color="support\nsocial")+
  theme_classic()

ggarrange(p1,p2,p3,p4, labels=c("A","B","C","D"), common.legend = T, legend = "right")
ggsave("../output/Fig_S5_NBDA_s_lambda.png", width=12,height=10,scale=2,units="cm")

