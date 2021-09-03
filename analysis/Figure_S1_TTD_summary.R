library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../concat_data/df_ABM_equiv.Rda")

df_ABM_equiv_payoff = df_ABM_equiv_payoff %>% filter(conformity=="None")

df = df_ABM_equiv_payoff %>%
  filter(full_diffusion==T) %>%
  group_by(graph_type,conformity,EWA_soc_info_weight,EWA_recent_payoff_weight,EWA_tau) %>%
  summarize(mean=mean(timestep), CV= sd(timestep)/mean(timestep), Q1=quantile(timestep,.25), Q3=quantile(timestep,.75), IQR=IQR(timestep))

df = df %>% ungroup() %>% group_by(graph_type,conformity,EWA_tau) %>% mutate(mean_fill=mean/mean[EWA_recent_payoff_weight=="medium" & EWA_soc_info_weight=="medium"])

p1 = ggplot(df %>% filter(conformity=="None"), aes(x=EWA_soc_info_weight,y=EWA_recent_payoff_weight))+
  facet_grid(EWA_tau~graph_type)+
  geom_tile(aes(fill=mean_fill))+
  geom_text(aes(label=paste0(mean,"\n",round(mean_fill,2),"\n(",round(CV,2),")")))+
  scale_fill_gradient2(low="blue",high="red", trans="sqrt",midpoint = 1, guide = F)+
  labs(x="Social info. bias",y="Recent payoff bias",fill="Ratio")+
  ggtitle("No conformity")+
  theme_classic()

p2 = ggplot(df %>% filter(conformity=="EWA"), aes(x=EWA_soc_info_weight,y=EWA_recent_payoff_weight))+
  facet_grid(EWA_tau~graph_type)+
  geom_tile(aes(fill=mean_fill))+
  geom_text(aes(label=paste0(mean,"\n",round(mean_fill,2),"\n(",round(CV,2),")")))+
  scale_fill_gradient2(low="blue",high="red", trans="sqrt",midpoint = 1, guide = F)+
  labs(x="Social info. bias",y="Recent payoff bias",fill="Ratio")+
  ggtitle("EWA conformity")+
  theme_classic()

p3 = ggplot(df %>% filter(conformity=="NBDA"), aes(x=EWA_soc_info_weight,y=EWA_recent_payoff_weight))+
  facet_grid(EWA_tau~graph_type)+
  geom_tile(aes(fill=mean_fill))+
  geom_text(aes(label=paste0(mean,"\n",round(mean_fill,2),"\n(",round(CV,2),")")))+
  scale_fill_gradient2(low="blue",high="red", trans="sqrt",midpoint = 1, guide = F)+
  labs(x="Social info. bias",y="Recent payoff bias",fill="Ratio")+
  ggtitle("NBDA conformity")+
  theme_classic()

p4 = ggplot(df %>% filter(conformity=="EWA & NBDA"), aes(x=EWA_soc_info_weight,y=EWA_recent_payoff_weight))+
  facet_grid(EWA_tau~graph_type)+
  geom_tile(aes(fill=mean_fill))+
  geom_text(aes(label=paste0(mean,"\n",round(mean_fill,2),"\n(",round(CV,2),")")))+
  scale_fill_gradient2(low="blue",high="red", trans="sqrt",midpoint = 1, guide = F)+
  labs(x="Social info. bias",y="Recent payoff bias",fill="Ratio")+
  ggtitle("EWA & NBDA conformity")+
  theme_classic()

ggarrange(p1,p2,p3,p4,ncol=1, labels=c("A","B","C","D"))

ggsave("../output/fig_S1.png", width=17.8, height=27, units="cm", scale=2)