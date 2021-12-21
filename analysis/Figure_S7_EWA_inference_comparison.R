library(tidyverse)
library(ggpubr)
library(ggridges)
library(latex2exp)
library(rethinking)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("../model_outputs/Rda_files/df_homogeneous_inference.Rda")

df_vanilla$true_phi_name = as.factor(df_vanilla$true_phi)
levels(df_vanilla$true_phi_name) = c("phi=0.25","phi=0.5","phi=0.75")
df_vanilla$true_sigma_name = as.factor(df_vanilla$true_sigma)
levels(df_vanilla$true_sigma_name) = c("sigma=0.25","sigma=0.5","sigma=0.75")
df_vanilla = df_vanilla %>% group_by(sim) %>% mutate(prior=logistic(rnorm(n(), mean=0, sd=1)))
df = df_vanilla %>% pivot_longer(cols=c(post_phi,post_sigma))

p1 = ggplot(data=df)+
  facet_grid(true_sigma_name~true_phi_name)+
  geom_density(aes(x=value, fill=name), alpha=0.3)+
  geom_density(data = df %>% filter(name=="post_phi"),aes( x=prior),alpha=0.3, linetype="dotted", fill="white")+
  geom_vline(aes(xintercept=true_phi), alpha=1, color="red")+
  geom_vline(aes(xintercept=true_sigma), alpha=1,color="blue")+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(x="estimated parameter value", fill="posterior density", title="Homogeneous repertoires")+
  theme_classic()

load("../model_outputs/Rda_files/df_social_inference.Rda")
df_diffusion$true_phi_name = as.factor(df_diffusion$true_phi)
levels(df_diffusion$true_phi_name) = c("phi=0.25","phi=0.5","phi=0.75")
df_diffusion$true_sigma_name = as.factor(df_diffusion$true_sigma)
levels(df_diffusion$true_sigma_name) = c("sigma=0.25","sigma=0.5","sigma=0.75")
df_diffusion = df_diffusion %>% group_by(sim) %>% mutate(prior=logistic(rnorm(n(), mean=0, sd=1)))
df = df_diffusion %>% pivot_longer(cols=c(post_phi,post_sigma))

p2 = ggplot(data=df)+
  facet_grid(true_sigma_name~true_phi_name)+
  geom_density(aes(x=value, fill=name), alpha=0.3)+
  geom_density(data = df %>% filter(name=="post_phi"),aes( x=prior),alpha=0.3, linetype="dotted", fill="white")+
  geom_vline(aes(xintercept=true_phi), alpha=1, color="red")+
  geom_vline(aes(xintercept=true_sigma), alpha=1,color="blue")+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(x="estimated parameter value", fill="posterior density", title="Social diffusion")+
  theme_classic()


load("../model_outputs/Rda_files/df_asocial_inference.Rda")

df_asocial_inference$true_phi_name = as.factor(df_asocial_inference$true_phi)
levels(df_asocial_inference$true_phi_name) = c("phi=0.25","phi=0.5","phi=0.75")
df_asocial_inference$true_sigma_name = as.factor(df_asocial_inference$true_sigma)
levels(df_asocial_inference$true_sigma_name) = c("sigma=0.25","sigma=0.5","sigma=0.75")
df_asocial_inference = df_asocial_inference %>% group_by(sim) %>% mutate(prior=logistic(rnorm(n(), mean=0, sd=1)))
df = df_asocial_inference %>% pivot_longer(cols=c(post_phi,post_sigma))

p3 = ggplot(data=df)+
  facet_grid(true_sigma_name~true_phi_name)+
  geom_density(aes(x=value, fill=name), alpha=0.3)+
  geom_density(data = df %>% filter(name=="post_phi"),aes( x=prior),alpha=0.3, linetype="dotted", fill="white")+
  geom_vline(aes(xintercept=true_phi), alpha=1, color="red")+
  geom_vline(aes(xintercept=true_sigma), alpha=1,color="blue")+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(x="estimated parameter value", fill="posterior density", title="Asocial diffusion")+
  theme_classic()

g1 = ggarrange(p1,p2,p3, ncol=1, align = "v", common.legend = T)

ggsave("../output/fig_S7_EWA_inference.png", width=16,height=20, scale=2,units="cm")
