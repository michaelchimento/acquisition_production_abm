library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#compare order of acquisition and production
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod.Rda")


df = df_equiv_payoffs_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number()) %>% ungroup()

df = df %>% ungroup() %>% group_by(sim, graph_type, NBDA_s_param, memory_window, EWA_soc_info_weight, EWA_recent_payoff_weight, EWA_tau, EWA_conformity) %>% summarize(TTD=max(timestep_acquisition_b), TTFP=max(timestep_production_b), avgdelta=mean(delta), manhattan_delay=sum(abs(delta)), manhattan_divergence=sum(abs(order_acquisition-order_production)), divergence = 1-sum(order_acquisition==order_production)/n())

df = df %>% group_by(NBDA_s_param,EWA_soc_info_weight) %>% summarize(mean_TTD=mean(TTD), mean_div= mean(manhattan_divergence), mean_delay=mean(manhattan_delay))

p1 = ggplot(df, aes(x=EWA_soc_info_weight,y=NBDA_s_param))+
  geom_tile(aes(fill=mean_TTD))+
  geom_text(aes(label=paste0("TTD: ",round(mean_TTD,2),"\nDiv: ",round(mean_div,2),"\nDelay: ", round(mean_delay,2) )))+
  scale_fill_gradient2(low="blue",high="red", midpoint = 85.21, guide = F)+
  labs(x="Social info. bias",y="Strength of social transmission",fill="Ratio")+
  theme_classic()
p1


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