library(tidyverse)
library(NBDA)
library(igraph)
library(sna)
library(magrittr)
library(reader)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


p1=ggplot(df_summary_SB_AC, aes(x=est_s,y=est_rate))+
  geom_point(aes(color=est_s*est_rate),alpha=0.5)+
  geom_vline(xintercept = df$NBDA_s_param[1], color="blue",linetype="dashed")+
  geom_hline(yintercept = df$NBDA_basehazard[1], color="red",linetype="dashed")+
  xlim(c(0,10))+
  labs(x="Estimated s", y="Estimated base rate", title="Social diffusion: acquisition data")+
  theme_classic()

p3=ggplot(df_summary_SB_FP, aes(x=est_s,y=est_rate))+
  geom_point(aes(color=est_s*est_rate),alpha=0.5)+
  geom_vline(xintercept = df$NBDA_s_param[1], color="blue",linetype="dashed")+
  geom_hline(yintercept = df$NBDA_basehazard[1], color="red",linetype="dashed")+
  xlim(c(0,10))+
  labs(x="Estimated s", y="Estimated base rate", title="Social diffusion: production data")+
  theme_classic()

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
