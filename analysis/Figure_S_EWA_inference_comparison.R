library(tidyverse)
library(ggpubr)
library(rethinking)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("../model_outputs/Rda_files/df_EWA_posterior_homogeneous_inference_10sim.Rda")
df_vanilla$true_rho_name = as.factor(df_vanilla$true_rho)
levels(df_vanilla$true_rho_name) = c("rho=0.25","rho=0.5","rho=0.75")
df_vanilla$true_sigma_name = as.factor(df_vanilla$true_sigma)
levels(df_vanilla$true_sigma_name) = c("sigma=0.25","sigma=0.5","sigma=0.75")
summary(df_vanilla)
df_vanilla %>% group_by(true_sigma_name, true_rho_name, sim) %>% summarize(n())
df_prior = df_vanilla %>% group_by(true_sigma_name, true_rho_name) %>% summarize(prior=logistic(rnorm(3000, mean=0, sd=1)))
df = df_vanilla %>% pivot_longer(cols=c(post_rho,post_sigma)) %>% mutate(value=logistic(value)) %>% mutate(grouping=paste0(sim,name))
df$name = as.factor(df$name)

p1 = ggplot(data=df)+
  facet_grid(true_sigma_name~true_rho_name, scales="free_y")+
  geom_density(aes(x=value, color=name, group=grouping), alpha=0.1)+
  geom_density(data = df_prior,aes( x=prior),alpha=0.3, linetype="dotted", fill="white")+
  geom_vline(aes(xintercept=true_rho), alpha=1, color="red")+
  geom_vline(aes(xintercept=true_sigma), alpha=1,color="blue")+
  scale_color_manual(values=c("red","blue"))+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(x="estimated parameter value", fill="posterior density", title="Homogeneous repertoires")+
  theme_classic()
p1
load("../model_outputs/Rda_files/df_EWA_posterior_social_inference_10sim.Rda")
df_diffusion$true_rho_name = as.factor(df_diffusion$true_rho)
levels(df_diffusion$true_rho_name) = c("rho=0.25","rho=0.5","rho=0.75")
df_diffusion$true_sigma_name = as.factor(df_diffusion$true_sigma)
levels(df_diffusion$true_sigma_name) = c("sigma=0.25","sigma=0.5","sigma=0.75")
summary(df_diffusion)
df_diffusion %>% group_by(true_sigma_name, true_rho_name, sim) %>% summarize(n())
df_prior = df_diffusion %>% group_by(true_sigma_name, true_rho_name) %>% summarize(prior=logistic(rnorm(3000, mean=0, sd=1)))
df = df_diffusion %>% pivot_longer(cols=c(post_rho,post_sigma)) %>% mutate(value=logistic(value)) %>% mutate(grouping=paste0(sim,name))
df$name = as.factor(df$name)

p2 = ggplot(data=df)+
  facet_grid(true_sigma_name~true_rho_name, scales="free_y")+
  geom_density(aes(x=value, color=name, group=grouping), alpha=0.1)+
  geom_density(data = df_prior,aes( x=prior),alpha=0.3, linetype="dotted", fill="white")+
  geom_vline(aes(xintercept=true_rho), alpha=1, color="red")+
  geom_vline(aes(xintercept=true_sigma), alpha=1,color="blue")+
  scale_color_manual(values=c("red","blue"))+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(x="estimated parameter value", fill="posterior density", title="Social transmission")+
  theme_classic()

p2
load("../model_outputs/Rda_files/df_EWA_posterior_asocial_inference_10sim.Rda")
summary(df_asocial_inference)
df_asocial_inference$true_rho_name = as.factor(df_asocial_inference$true_rho)
levels(df_asocial_inference$true_rho_name) = c("rho=0.25","rho=0.5","rho=0.75")
df_asocial_inference$true_sigma_name = as.factor(df_asocial_inference$true_sigma)
levels(df_asocial_inference$true_sigma_name) = c("sigma=0.25","sigma=0.5","sigma=0.75")
summary(df_asocial_inference)
df_asocial_inference %>% group_by(true_sigma_name, true_rho_name, sim) %>% summarize(n())
df_prior = df_asocial_inference %>% group_by(true_sigma_name, true_rho_name) %>% summarize(prior=logistic(rnorm(3000, mean=0, sd=1)))
df = df_asocial_inference %>% pivot_longer(cols=c(post_rho,post_sigma)) %>% mutate(value=logistic(value)) %>% mutate(grouping=paste0(sim,name))
df$name = as.factor(df$name)

p3 = ggplot(data=df)+
  facet_grid(true_sigma_name~true_rho_name, scales="free_y")+
  geom_density(aes(x=value, color=name, group=grouping), alpha=0.1)+
  geom_density(data = df_prior,aes( x=prior),alpha=0.3, linetype="dotted", fill="white")+
  geom_vline(aes(xintercept=true_rho), alpha=1, color="red")+
  geom_vline(aes(xintercept=true_sigma), alpha=1,color="blue")+
  scale_color_manual(values=c("red","blue"))+
  scale_x_continuous(limits=c(0,1), breaks=c(0,.25,.5,.75,1))+
  labs(x="estimated parameter value", fill="posterior density", title="Asocial learning")+
  theme_classic()

g1 = ggarrange(p1,p2,p3, ncol=1, align = "v", common.legend = T)
g1
ggsave("../output/Fig_S_EWA_inference.png", width=16,height=20, scale=2,units="cm")

