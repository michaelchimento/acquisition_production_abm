library(tidyverse)
library(rethinking)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(grid)
library(gridGraphics)
library(gridExtra)

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

load(file="../model_outputs/Rda_files/df_EWA_posterior_homogeneous_inference.Rda")
df=df_vanilla %>% group_by(sim) %>% summarize(rho=true_rho[1],sigma=true_sigma[1]) %>% mutate(value=paste0("rho:",rho,"; sigma:",sigma)) %>% arrange(rho,sigma)
j=1
for (i in df$sim){
  fit=loadRData(paste0("fits/fit",i,"_homogeneous.RDA"))
  par(oma=c(1,.75,.75,.75))
  trankplot(fit, n_cols=2)
  grid.echo()
  assign(paste0("p",j),grid.grab())
  dev.off()
  j=j+1
}

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow=9, labels=df$value)
ggsave("../output/trankplots_homogeneous.png", width=10, height=18, scale=2, units="cm")

load(file="../model_outputs/Rda_files/df_EWA_posterior_social_inference.Rda")
df=df_vanilla %>% group_by(sim) %>% summarize(rho=true_rho[1],sigma=true_sigma[1]) %>% mutate(value=paste0("rho:",rho,"; sigma:",sigma)) %>% arrange(rho,sigma)
j=1
for (i in df$sim){
  fit=loadRData(paste0("fits/fit",i,"_social.RDA"))
  par(oma=c(1,.75,.75,.75))
  trankplot(fit, n_cols=2)
  grid.echo()
  assign(paste0("p",j),grid.grab())
  dev.off()
  j=j+1
}

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow=9, labels=df$value)
ggsave("../output/trankplots_social.png", width=10, height=18, scale=2, units="cm")

load(file="../model_outputs/Rda_files/df_EWA_posterior_asocial_inference.Rda")
df=df_vanilla %>% group_by(sim) %>% summarize(rho=true_rho[1],sigma=true_sigma[1]) %>% mutate(value=paste0("rho:",rho,"; sigma:",sigma)) %>% arrange(rho,sigma)
j=1
for (i in df$sim){
  fit=loadRData(paste0("fits/fit",i,"_asocial.RDA"))
  par(oma=c(1,.75,.75,.75))
  trankplot(fit, n_cols=2)
  grid.echo()
  assign(paste0("p",j),grid.grab())
  dev.off()
  j=j+1
}

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow=9, labels=df$value)
ggsave("../output/trankplots_asocial.png", width=10, height=18, scale=2, units="cm")
