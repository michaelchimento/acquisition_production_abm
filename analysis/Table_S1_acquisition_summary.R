library(tidyverse)
library(knitr)
library(kableExtra)
library(rethinking)



PI <- function (samples, prob = 0.92)
{
  x <- sapply(prob, function(p) {
    a <- (1 - p)/2
    quantile(samples, probs = c(a, 1 - a))
  })
  n <- length(prob)
  result <- rep(0, n * 2)
  for (i in 1:n) {
    low_idx <- n + 1 - i
    up_idx <- n + i
    result[low_idx] <- x[1, i]
    result[up_idx] <- x[2, i]
    a <- (1 - prob[i])/2
    names(result)[low_idx] <- concat(round(a * 100, 0), "%")
    names(result)[up_idx] <- concat(round((1 - a) * 100,
                                          0), "%")
  }
  return(result)
}

SE <- function (samples){
  result = sd(samples)/sqrt(length((samples)))
  return(result)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load(file="../model_outputs/Rda_files/df_supptext_acq_prod.Rda")
df = df_supptext_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% ungroup() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

df %>% ungroup() %>% filter(order_acquisition!=1) %>% summarize(divergence = sum(order_acquisition==order_production)/n())
summary(df)
df = df %>% ungroup() %>% group_by(sim, graph_type, NBDA_s_param, NBDA_basehazard, asocial_learning) %>% summarize(TTD=max(timestep_acquisition_b), TTFP=max(timestep_production_b), divergence = 1-sum(order_acquisition==order_production)/n(), manhattan_divergence=sum(abs(order_acquisition-order_production))/23, manhattan_delay=sum(abs(delta))/24)

summary(df)

####S1 effect of network architecture####
t1 = df %>%
  filter(NBDA_s_param=="5", NBDA_basehazard=="0.05", asocial_learning=="0") %>%
  group_by(graph_type) %>%
  summarize(variable = "network architecture", meanTTD=mean(TTD),
            seTTD=SE(TTD),
            CI_TTD= paste0("[",round(PI(TTD)[1],2), ",", upperCI_TTD=round(PI(TTD)[2],2),"]"),
            meanTTFP=mean(TTFP),
            seTTFP=SE(TTFP),
            CI_TTFP= paste0("[",round(PI(TTFP)[1],2), ",", upperCI_TTD=round(PI(TTFP)[2],2),"]"),
            mean_perc_div = mean(divergence)*100,
            mean_d_order = mean(manhattan_divergence),
            se_d_order=SE(manhattan_divergence),
            CI_d_order= paste0("[",round(PI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(PI(manhattan_divergence)[2],2),"]"),
            mean_d_time = mean(manhattan_delay),
            se_d_time=SE(manhattan_delay),
            CI_d_time= paste0("[",round(PI(manhattan_delay)[1],2), ",", upperCI_TTD=round(PI(manhattan_delay)[2],2),"]")) %>%
  rename(value=graph_type) %>%
  mutate(value=as.character(value))

####S1 effect of baseline rate####

df
t2 = df %>%
  filter(graph_type=="random regular", NBDA_s_param=="5", asocial_learning=="0") %>%
  group_by(NBDA_basehazard) %>%
  summarize(variable = "base learning rate", meanTTD=mean(TTD),
            seTTD=SE(TTD),
            CI_TTD= paste0("[",round(PI(TTD)[1],2), ",", upperCI_TTD=round(PI(TTD)[2],2),"]"),
            meanTTFP=mean(TTFP),
            seTTFP=SE(TTFP),
            CI_TTFP= paste0("[",round(PI(TTFP)[1],2), ",", upperCI_TTD=round(PI(TTFP)[2],2),"]"),
            mean_perc_div = mean(divergence)*100,
            mean_d_order = mean(manhattan_divergence),
            se_d_order=SE(manhattan_divergence),
            CI_d_order= paste0("[",round(PI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(PI(manhattan_divergence)[2],2),"]"),
            mean_d_time = mean(manhattan_delay),
            se_d_time=SE(manhattan_delay),
            CI_d_time= paste0("[",round(PI(manhattan_delay)[1],2), ",", upperCI_TTD=round(PI(manhattan_delay)[2],2),"]")) %>%
  rename(value= NBDA_basehazard) %>% mutate(value=as.character(value))

t2

#### 3.2 s param ####
t3 = df %>%
  filter(graph_type=="random regular", asocial_learning=="0", NBDA_basehazard=="0.05") %>%
  group_by(NBDA_s_param) %>%
  summarize(variable = "NBDA_s_param", meanTTD=mean(TTD),
            seTTD=SE(TTD),
            CI_TTD= paste0("[",round(PI(TTD)[1],2), ",", upperCI_TTD=round(PI(TTD)[2],2),"]"),
            meanTTFP=mean(TTFP),
            seTTFP=SE(TTFP),
            CI_TTFP= paste0("[",round(PI(TTFP)[1],2), ",", upperCI_TTD=round(PI(TTFP)[2],2),"]"),
            mean_perc_div = mean(divergence)*100,
            mean_d_order = mean(manhattan_divergence),
            se_d_order=SE(manhattan_divergence),
            CI_d_order= paste0("[",round(PI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(PI(manhattan_divergence)[2],2),"]"),
            mean_d_time = mean(manhattan_delay),
            se_d_time=SE(manhattan_delay),
            CI_d_time= paste0("[",round(PI(manhattan_delay)[1],2), ",", upperCI_TTD=round(PI(manhattan_delay)[2],2),"]")) %>%
  rename(value= NBDA_s_param) %>%
  mutate(value=as.character(value))

#### S1 asocial learning ####
t4 = df %>%
  filter(graph_type=="random regular", NBDA_s_param=="5", NBDA_basehazard=="0.05") %>%
  group_by(asocial_learning) %>%
  summarize(variable = "asocial learning", meanTTD=mean(TTD),
            seTTD=SE(TTD),
            CI_TTD= paste0("[",round(PI(TTD)[1],2), ",", upperCI_TTD=round(PI(TTD)[2],2),"]"),
            meanTTFP=mean(TTFP),
            seTTFP=SE(TTFP),
            CI_TTFP= paste0("[",round(PI(TTFP)[1],2), ",", upperCI_TTD=round(PI(TTFP)[2],2),"]"),
            mean_perc_div = mean(divergence)*100,
            mean_d_order = mean(manhattan_divergence),
            se_d_order=SE(manhattan_divergence),
            CI_d_order= paste0("[",round(PI(manhattan_divergence)[1],2), ",", upperCI_TTD=round(PI(manhattan_divergence)[2],2),"]"),
            mean_d_time = mean(manhattan_delay),
            se_d_time=SE(manhattan_delay),
            CI_d_time= paste0("[",round(PI(manhattan_delay)[1],2), ",", upperCI_TTD=round(PI(manhattan_delay)[2],2),"]")) %>% rename(value= asocial_learning) %>% mutate(value=as.character(value))

t = bind_rows(t1,t3,t2,t4)  %>% relocate(variable)

t = t %>% mutate(mean_perc_div = round(mean_perc_div,2), mean_d_order = round(mean_d_order,2), mean_d_time = round(mean_d_time,2))

t = t %>% mutate(meanTTD=paste0(meanTTD,"$\\pm$",round(seTTD,2)),
                 meanTTFP=paste0(meanTTFP,"$\\pm$",round(seTTFP,2)),
                 mean_d_order=paste0(mean_d_order,"$\\pm$",round(se_d_order,2)),
                 mean_d_time=paste0(mean_d_time,"$\\pm$",round(se_d_time,2))) %>%
  select(-c(seTTD,seTTFP,se_d_order, se_d_time))

kable(t[2:ncol(t)], booktabs=T,
      col.names = c('value', 'mean', 'PI', 'mean', 'PI', '\\% divergent', 'mean', 'PI', 'mean', "HDPI"),
      align = "rccccccccc",
      format="latex", escape = F, digits=2) %>%
  kable_styling(full_width = F, latex_options = c("scale_down")) %>%
  pack_rows(index=c("Network architecture"=4, "Social trans. rate ($s$)"=4, "Baseline learning rate ($\\lambda_b$)"=3, "Asocial learning ($A$)"=2), escape = F) %>%
  add_header_above(c(" "=1,"TTD"=2,"TTFP"=2,"divergence $d_{order}$"=3, "delay $d_{time}$"=2), escape=F)
