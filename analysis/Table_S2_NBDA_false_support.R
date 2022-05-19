#Table S1: Production rules influence NBDA inference
library(tidyverse)
library(kableExtra)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../model_outputs/Rda_files/df_NBDA_figure_data_proportional.Rda")
df_realistic = df_realistic %>% filter(data_type=="time_of_first_production", model_type=="weighted") %>% droplevels()

summary(df_realistic)
df = df_realistic %>% mutate(support= aicc_asocial - aicc_social)
summary(df)
df = df %>% mutate(false_negative=ifelse(condition=="social transmission" & support <=0,TRUE,FALSE), false_positive=ifelse(condition=="asocial learning" & support > 0 ,TRUE,FALSE)) %>% ungroup()

summary(df)

df_sum=df %>% ungroup() %>% summarize(FN_sum = sum(false_negative), FP_sum=sum(false_positive))
FN_sum=df_sum$FN_sum[1]
FP_sum=df_sum$FP_sum[1]
sum_param=810

t2 = df %>%
  group_by(EWA_sigma) %>%
  summarize(type="EWA_soc_info",
            false_negatives = sum(false_negative),
            percent_param_FN=sum(false_negative)/sum_param,
            percent_all_FN=sum(false_negative)/FN_sum,
            false_positive=sum(false_positive),
            percent_param_FP = sum(false_positive)/sum_param,
            percent_all_FP = sum(false_positive)/FP_sum) %>%
  mutate(value=EWA_sigma) %>% mutate(value=as.character(value))

t3 = df %>%
  group_by(EWA_rho) %>%
  summarize(type="EWA_recent_payoff",
            false_negatives = sum(false_negative),
            percent_param_FN=sum(false_negative)/sum_param,
            percent_all_FN=sum(false_negative)/FN_sum,
            false_positive=sum(false_positive),
            percent_param_FP = sum(false_positive)/sum_param,
            percent_all_FP = sum(false_positive)/FP_sum) %>% mutate(value=EWA_rho) %>% mutate(value=as.character(value))

t4 = df %>%  group_by(EWA_alpha) %>%
  summarize(type="EWA_alpha",
            false_negatives = sum(false_negative),
            percent_param_FN=sum(false_negative)/sum_param,
            percent_all_FN=sum(false_negative)/FN_sum,
            false_positive=sum(false_positive),
            percent_param_FP = sum(false_positive)/sum_param,
            percent_all_FP = sum(false_positive)/FP_sum) %>%
  mutate(value=EWA_alpha) %>%
  mutate(value=as.character(value))

t5 = df %>%  group_by(EWA_chi) %>%
  summarize(type="EWA_chi",
            false_negatives = sum(false_negative),
            percent_param_FN=sum(false_negative)/sum_param,
            percent_all_FN=sum(false_negative)/FN_sum,
            false_positive=sum(false_positive),
            percent_param_FP = sum(false_positive)/sum_param,
            percent_all_FP = sum(false_positive)/FP_sum) %>% mutate(value=EWA_chi) %>% mutate(value=as.character(value))

t6 = df %>%
  group_by(memory_window) %>%
  summarize(type="memory_window",
            false_negatives = sum(false_negative),
             percent_param_FN=sum(false_negative)/sum_param,
             percent_all_FN=sum(false_negative)/FN_sum,
             false_positive=sum(false_positive),
             percent_param_FP = sum(false_positive)/sum_param,
             percent_all_FP = sum(false_positive)/FP_sum) %>%
  mutate(value=memory_window) %>%
  mutate(value=as.character(value))

t = bind_rows(t2,t3,t4,t5,t6) %>% select(type, value, false_negatives, percent_param_FN, percent_all_FN, false_positive, percent_param_FP, percent_all_FP) %>% mutate(percent_param_FN=percent_param_FN*100, percent_all_FN=percent_all_FN*100, percent_param_FP=percent_param_FP*100, percent_all_FP=percent_all_FP*100)


kable(t[2:ncol(t)],booktabs = TRUE,
      col.names = c('value', 'false negatives', '% sims w/ value', '% all FN', 'false positives', '% sims w/ value', '% all FP'),
      align = "rcccccc",
      format="pipe", digits=2) %>%
  kable_styling(full_width = F) %>%
  pack_rows(index=c("Social info. bias"=3, "Recent exp. bias"=3, "Risk-appetite"=3, "Production conformity"=3, "Memory window"=3))
