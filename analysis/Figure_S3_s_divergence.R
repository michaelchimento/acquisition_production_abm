library(tidyverse)
library(ggpubr)
library(ggridges)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#compare order of acquisition and production
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod.Rda")


df = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

df = df %>% mutate(NBDA_s_param = as.factor(NBDA_s_param))
levels(df$NBDA_s_param) = c("s=15","s=5","s=10")
df$NBDA_s_param = factor(df$NBDA_s_param, levels=c("s=5","s=10","s=15"))

p3= ggplot(df %>% filter(EWA_soc_info_weight=="medium", EWA_recent_payoff_weight=="medium", EWA_tau=="non-conservative", EWA_conformity==1,memory_window==10) %>% group_by(order_production,order_acquisition) %>% mutate(count=n()), aes(x=order_acquisition,y=order_production, fill=count))+
  facet_wrap(~NBDA_s_param)+
  geom_tile()+
  labs(x="Order of acquisition", y="Order of production", fill="count")+
  scale_fill_continuous(type = "viridis",trans="log10", direction=-1) +
  scale_x_continuous(breaks=c(1,4,8,12,16,20,24))+
  scale_y_continuous(breaks=c(1,4,8,12,16,20,24))+
  theme_classic()
p3
ggsave("../output/Fig_S3_s_divergence.png",height=5,width=14,scale=2,units="cm")

