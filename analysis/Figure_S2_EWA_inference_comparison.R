library(tidyverse)
library(ggpubr)
library(ggridges)
library(latex2exp)
library(rethinking)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


load("../../concat_data/df_vanilla_inference.Rda")

p1 = ggplot(data=df_vanilla, aes(y=as.factor(true_phi),x=post_phi, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(y=expression(paste("true ",phi)), x="")+
  theme_classic()

p2 = ggplot(data=df_vanilla, aes(y=as.factor(true_gamma),x=post_gamma, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(y=expression(paste("true ",sigma)), x="")+
  theme_classic()

p3 = ggplot(data=df_vanilla, aes(y=as.factor(true_f),x=post_f, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,5), breaks=c(1,2,3,4,5))+
  labs(y=TeX("True $f_{SI}$"), x="")+
  theme_classic()

p4 = ggplot(data=df_vanilla, aes(y=as.factor(true_tau),x=post_tau, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,5), breaks=c(1,2,3,4,5))+
  labs(y=expression(paste("true ",tau)), x="")+
  theme_classic()

load("../../concat_data/df_diffusion_inference_2.Rda")

p5 = ggplot(data=df_diffusion, aes(y=as.factor(true_phi),x=post_phi, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(y=expression(paste("true ",phi)), x="")+
  theme_classic()

p6 = ggplot(data=df_diffusion, aes(y=as.factor(true_gamma),x=post_gamma, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(y=expression(paste("true ",sigma)), x="")+
  theme_classic()

p7 = ggplot(data=df_diffusion, aes(y=as.factor(true_f),x=post_f, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,5), breaks=c(1,2,3,4,5))+
  labs(y=TeX("True $f_{SI}$"), x="")+
  theme_classic()

p8 = ggplot(data=df_diffusion, aes(y=as.factor(true_tau),x=post_tau, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,5), breaks=c(1,2,3,4,5))+
  labs(y=expression(paste("true ",tau)), x="")+
  theme_classic()


load(file="../../concat_data/df_asocial_inference.Rda")

p9 = ggplot(data=df_asocial_inference, aes(y=as.factor(true_phi),x=post_phi, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(y=expression(paste("true ",phi)), x="")+
  theme_classic()

p10 = ggplot(data=df_asocial_inference, aes(y=as.factor(true_gamma),x=post_gamma, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(y=expression(paste("true ",sigma)), x="")+
  theme_classic()

p11 = ggplot(data=df_asocial_inference, aes(y=as.factor(true_f),x=post_f, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,5), breaks=c(1,2,3,4,5))+
  labs(y=TeX("True $f_{SI}$"), x="")+
  theme_classic()

p12 = ggplot(data=df_asocial_inference, aes(y=as.factor(true_tau),x=post_tau, fill=sim))+
  geom_density_ridges(alpha=0.3, show.legend = F)+
  scale_x_continuous(limits=c(0,5), breaks=c(1,2,3,4,5))+
  labs(y=expression(paste("true ",tau)), x="")+
  theme_classic()

g1 = ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12, align = "v", ncol=4,nrow=3,labels=c("A", "", "", "", "B","","","","C","","",""))

annotate_figure(g1,
                bottom = text_grob("posterior density estimates", color = "black",
                                   vjust = -1,size = 12),

)

ggsave("../../output/fig_S2_EWA_inference.png", width=17.8,height=10, scale=2,units="cm")
