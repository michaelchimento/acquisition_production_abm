library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


load(file="../model_outputs/Rda_files/NBDA_figure_data_binary.Rda")
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
p1
p2=ggplot(df %>% filter(feeder_data=="acquisition", diffusion=="asocial diffusion"), aes(x=est_s,y=est_rate))+
  geom_point(aes(color=support),alpha=0.5)+
  geom_vline(xintercept = 0, color="blue",linetype="dashed")+
  geom_hline(yintercept = 0.05, color="red",linetype="dashed")+
  xlim(c(0,10))+
  scale_color_continuous(limits=c(-50,300), breaks=c(-50,0,50,100,150,200,250,300))+

  labs(x="Estimated s", y="Estimated base rate", title="Asocial diffusion (idealized data)", color="support\nsocial")+
  theme_classic()

load(file="../model_outputs/Rda_files/NBDA_figure_data_proportional.Rda")
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
ggsave("../output/Fig_SX_NBDA_s_lambda.png", width=12,height=10,scale=2,units="cm")



p5=ggplot(df_summary_AB_AC, aes(x=est_s,y=est_rate))+
  geom_point(aes(color=est_s*est_rate),alpha=0.5)+
  geom_vline(xintercept = df$NBDA_s_param[1], color="blue",linetype="dashed")+
  geom_hline(yintercept = df$NBDA_basehazard[1], color="red",linetype="dashed")+
  xlim(c(0,10))+
  labs(x="Estimated s", y="Estimated base rate", title="Asocial diffusion: acquisition data")+
  theme_classic()

p7=ggplot(df_summary_AB_FP, aes(x=est_s,y=est_rate))+
  geom_point(aes(color=est_s*est_rate),alpha=0.5)+
  geom_vline(xintercept = df$NBDA_s_param[1], color="blue",linetype="dashed")+
  geom_hline(yintercept = df$NBDA_basehazard[1], color="red",linetype="dashed")+
  xlim(c(0,10))+
  labs(x="Estimated s", y="Estimated base rate", title="Asocial diffusion: production data")+
  theme_classic()



p1=ggplot(df_summary_SP_AC, aes(x=est_s,y=est_rate))+
  geom_point(aes(color=est_s*est_rate),alpha=0.5)+
  geom_vline(xintercept = df$NBDA_s_param[1], color="blue",linetype="dashed")+
  geom_hline(yintercept = df$NBDA_basehazard[1], color="red",linetype="dashed")+
  xlim(c(0,10))+
  labs(x="Estimated s", y="Estimated base rate", title="Social diffusion: acquisition data")+
  theme_classic()

p2=ggplot(df_summary_SP_AC,aes(x=aicc_social,aicc_asocial)) +
  geom_point()+
  scale_color_viridis_d(option="E", end=.7)+
  geom_abline(linetype="dashed",color="darkgreen") +
  ylim(c(0,1000))+
  xlim(c(0,1000))+
  labs(x="AICC social model", y="AICC asocial model")+
  theme_classic()

p3=ggplot(df_summary_SP_FP, aes(x=est_s,y=est_rate))+
  geom_point(aes(color=est_s*est_rate),alpha=0.5)+
  geom_vline(xintercept = df$NBDA_s_param[1], color="blue",linetype="dashed")+
  geom_hline(yintercept = df$NBDA_basehazard[1], color="red",linetype="dashed")+
  xlim(c(0,10))+
  labs(x="Estimated s", y="Estimated base rate", title="Social diffusion: production data")+
  theme_classic()

p4=ggplot(df_summary_SP_FP,aes(x=aicc_social,aicc_asocial)) +
  geom_point() +
  scale_color_viridis_d(option="E", end=.7)+
  geom_abline(linetype="dashed",color="darkgreen") +
  ylim(c(0,1000))+
  xlim(c(0,1000))+
  labs(x="AICC social model", y="AICC asocial model")+
  theme_classic()

p5=ggplot(df_summary_AP_AC, aes(x=est_s,y=est_rate))+
  geom_point(aes(color=est_s*est_rate),alpha=0.5)+
  geom_vline(xintercept = df$NBDA_s_param[1], color="blue",linetype="dashed")+
  geom_hline(yintercept = df$NBDA_basehazard[1], color="red",linetype="dashed")+
  xlim(c(0,10))+
  labs(x="Estimated s", y="Estimated base rate", title="Asocial diffusion: acquisition data")+
  theme_classic()

p6=ggplot(df_summary_AP_AC,aes(x=aicc_social,aicc_asocial)) +
  geom_point() +
  scale_color_viridis_d(option="E", end=.7)+
  geom_abline(linetype="dashed",color="darkgreen") +
  ylim(c(0,1000))+
  xlim(c(0,1000))+
  labs(x="AICC social model", y="AICC asocial model")+
  theme_classic()

p7=ggplot(df_summary_AP_FP, aes(x=est_s,y=est_rate))+
  geom_point(aes(color=est_s*est_rate),alpha=0.5)+
  geom_vline(xintercept = df$NBDA_s_param[1], color="blue",linetype="dashed")+
  geom_hline(yintercept = df$NBDA_basehazard[1], color="red",linetype="dashed")+
  xlim(c(0,10))+
  labs(x="Estimated s", y="Estimated base rate", title="Asocial diffusion: production data")+
  theme_classic()

p8=ggplot(df_summary_AP_FP,aes(x=aicc_social,aicc_asocial)) +
  geom_point() +
  scale_color_viridis_d(option="E", end=.7)+
  geom_abline(linetype="dashed",color="darkgreen") +
  ylim(c(0,1000))+
  xlim(c(0,1000))+
  labs(x="AICC social model", y="AICC asocial model")+
  theme_classic()
