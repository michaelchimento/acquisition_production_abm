library(tidyverse)
library(latex2exp)
library(rethinking)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("../model_outputs/Rda_files/df_homogeneous_inference.Rda")
load("../model_outputs/Rda_files/df_social_inference.Rda")
load("../model_outputs/Rda_files/df_asocial_inference.Rda")

df_vanilla$type="a"
df_diffusion$type="b"
df_asocial_inference$type="c"

library(kableExtra)
df = rbind(df_vanilla,df_diffusion,df_asocial_inference)

t1 = df %>% group_by(type,true_phi) %>% summarize(variable="phi",mean=round(mean(post_phi),2), HPDI_89 = paste0("[",round(HPDI(post_phi)[1],2),",",high = round(HPDI(post_phi)[2],2),"]"))%>% rename(value = true_phi)

t2 = df %>% group_by(type,true_sigma) %>% summarize(variable="sigma",mean=round(mean(post_sigma),2), HPDI_89 = paste0("[",round(HPDI(post_sigma)[1],2),",",high = round(HPDI(post_sigma)[2],2),"]")) %>% rename(value = true_sigma)

t3 = df %>% group_by(type,true_f) %>% summarize(variable="f_SI",mean=round(mean(post_f),2), HPDI_89 = paste0("[",round(HPDI(post_f)[1],2),",",high = round(HPDI(post_f)[2],2),"]")) %>% rename(value = true_f)

t4 = df %>% group_by(type,true_tau) %>% summarize(variable="tau",mean=round(mean(post_tau),2),HPDI_89 = paste0("[",round(HPDI(post_tau)[1],2),",",high = round(HPDI(post_tau)[2],2),"]"))%>% rename(value = true_tau)

t = bind_rows(t1,t2)
t = t %>% pivot_wider(names_from = type, values_from = c(mean,HPDI_89)) %>% relocate(variable) %>% select(variable,value,mean_a, HPDI_89_a, mean_b, HPDI_89_b, mean_c, HPDI_89_c)

kable(t[2:ncol(t)],booktabs = TRUE,
      col.names = c('true value', 'est.', 'HPDI', 'est.', 'HPDI','est.', 'HPDI'),
      align = "rcccccc",
      format="latex") %>%
  kable_styling(full_width = F) %>%
  pack_rows(index=c("phi"=3,"sigma"=3)) %>%
  add_header_above(c(" "=1,"Homogenous"=2,"Heterogenous:\nsocial learning"=2,"Heterogenous:\nasocial learning"=2))
