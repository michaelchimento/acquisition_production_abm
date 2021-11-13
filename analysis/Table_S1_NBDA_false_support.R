#Table S1: Production rules influence NBDA inference
library(tidyverse)
library(kableExtra)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../model_outputs/Rda_files/NBDA_figure_data_proportional.Rda")
df$diffusion = factor(df$diffusion, levels=c("social diffusion","asocial diffusion"))
df_reality = df %>% filter(feeder_data=="first production")
summary(df_reality)
df_reality %>% group_by(diffusion) %>% summarize(support= mean( aicc_asocial - aicc_social))

df_reality %>% mutate(support=aicc_asocial - aicc_social)%>% filter(diffusion=="social diffusion", support <=0) %>% group_by(graph_type,EWA_soc_info_weight,EWA_recent_payoff_weight,EWA_tau,EWA_conformity, memory_window) %>% summarize(n())

#false negatives
t1 = df_reality %>% mutate(support=aicc_asocial - aicc_social, false_negative=ifelse(diffusion=="social diffusion" & support <=0,TRUE,FALSE), false_positive=ifelse(diffusion=="asocial diffusion" & support >0,TRUE,FALSE)) %>% group_by(graph_type) %>% summarize(type="graph_type", false_negatives = sum(false_negative), false_positive=sum(false_positive), avg_support = mean(support), sd_support=sd(support)) %>% mutate(value=graph_type) %>% mutate(value=as.character(value))

t2 = df_reality %>% mutate(support=aicc_asocial - aicc_social, false_negative=ifelse(diffusion=="social diffusion" & support <=0,TRUE,FALSE), false_positive=ifelse(diffusion=="asocial diffusion" & support >0,TRUE,FALSE)) %>% group_by(EWA_soc_info_weight) %>% summarize(type="EWA_soc_info",  false_negatives = sum(false_negative), false_positive=sum(false_positive), avg_support = mean(support), sd_support=sd(support)) %>% mutate(value=EWA_soc_info_weight) %>% mutate(value=as.character(value))

t3 = df_reality %>% mutate(support=aicc_asocial - aicc_social, false_negative=ifelse(diffusion=="social diffusion" & support <=0,TRUE,FALSE), false_positive=ifelse(diffusion=="asocial diffusion" & support >0,TRUE,FALSE)) %>% group_by(EWA_recent_payoff_weight) %>% summarize(type="EWA_recent_payoff",false_negatives = sum(false_negative), false_positive=sum(false_positive), avg_support = mean(support), sd_support=sd(support))  %>% mutate(value=EWA_recent_payoff_weight) %>% mutate(value=as.character(value))

t4 = df_reality %>% mutate(support=aicc_asocial - aicc_social, false_negative=ifelse(diffusion=="social diffusion" & support <=0,TRUE,FALSE), false_positive=ifelse(diffusion=="asocial diffusion" & support >0,TRUE,FALSE)) %>% group_by(EWA_tau) %>% summarize(type="EWA_tau",false_negatives = sum(false_negative), false_positive=sum(false_positive), avg_support = mean(support), sd_support=sd(support)) %>% mutate(value=EWA_tau) %>% mutate(value=as.character(value))

t5 = df_reality %>% mutate(support=aicc_asocial - aicc_social, false_negative=ifelse(diffusion=="social diffusion" & support <=0,TRUE,FALSE), false_positive=ifelse(diffusion=="asocial diffusion" & support >0,TRUE,FALSE)) %>% group_by(EWA_conformity) %>% summarize(type="EWA_conformity",false_negatives = sum(false_negative), false_positive=sum(false_positive), avg_support = mean(support), sd_support=sd(support)) %>% mutate(value=EWA_conformity) %>% mutate(value=as.character(value))

t6 = df_reality %>% mutate(support=aicc_asocial - aicc_social, false_negative=ifelse(diffusion=="social diffusion" & support <=0,TRUE,FALSE), false_positive=ifelse(diffusion=="asocial diffusion" & support >0,TRUE,FALSE)) %>% group_by(memory_window) %>% summarize(type="memory_window",false_negatives = sum(false_negative), false_positive=sum(false_positive), avg_support = mean(support), sd_support=sd(support)) %>% mutate(value=memory_window) %>% mutate(value=as.character(value))


t = bind_rows(t1,t2,t3,t4,t5,t6) %>% select(type, value, false_negatives, false_positive) %>% mutate( false_negatives= round(false_negatives,2), false_positive = round(false_positive,2))

kable(t[2:ncol(t)],booktabs = TRUE,
      col.names = c('value', 'false negatives', 'false_positives'),
      align = "rcccc",
      format="latex") %>%
  kable_styling(full_width = F) %>%
  pack_rows(index=c("graph type"=4,"Social info. bias"=3, "Recent exp. bias"=3, "Conservatism"=2, "Production conformity"=2, "Memory window"=3))