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
load(file="../model_outputs/Rda_files/df_maintext_acq_prod.Rda")

summary(as.factor(df_maintext_acq_prod$asocial_learning))
df = df_maintext_acq_prod %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

df %>% ungroup() %>% filter(order_acquisition!=1) %>% summarize(divergence = sum(order_acquisition==order_production)/n())

df = df %>% ungroup() %>% group_by(sim, graph_type, memory_window, EWA_sigma, EWA_rho, EWA_alpha, EWA_chi) %>% summarize(TTD=max(timestep_acquisition_b), TTFP=max(timestep_production_b), divergence = 1-sum(order_acquisition==order_production)/n(), manhattan_divergence=sum(abs(order_acquisition-order_production))/23, manhattan_delay=sum(abs(delta))/24)


#### 3.2 Sigma ####
t3 = df %>%
  filter(memory_window==10, EWA_rho=="medium", EWA_alpha=="risk-neutral", EWA_chi=="linear bias") %>%
  group_by(EWA_sigma) %>%
  summarize(variable = "social info. bias",
            meanTTD=mean(TTD),
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
  rename(value= EWA_sigma) %>%
  mutate(value=as.character(value))

t3

#### 3.2 Production conformity ####
t4 = df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_sigma=="medium", EWA_rho=="medium", EWA_alpha=="risk-neutral") %>%
  group_by(EWA_chi) %>%
  summarize(variable = "production conformity",
            meanTTD=mean(TTD),
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
  rename(value= EWA_chi) %>%
  mutate(value=as.character(value))

df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_sigma=="strong", EWA_rho=="medium", EWA_alpha=="risk-neutral") %>%
  group_by(EWA_chi) %>%
  summarize(meanTTD=mean(TTD), lowerCI_TTD=PI(TTD)[1], upperCI_TTD=PI(TTD)[2], CV= sd(TTD)/mean(TTD), mean_div = mean(divergence), mean_Manhattan_div = mean(manhattan_divergence), lowerCI_div=PI(manhattan_divergence)[1], upperCI_div=PI(manhattan_divergence)[2], mean_Manhattan_delay = mean(manhattan_delay), lowerCI_del=PI(manhattan_delay)[1], upperCI_del=PI(manhattan_delay)[2]) %>% mutate(ref_TTD = meanTTD/meanTTD[EWA_chi=="linear bias"])

#### 3.2 Memory window ####
t5 = df %>%
  filter(graph_type=="random regular", EWA_chi=="linear bias", EWA_sigma=="medium", EWA_rho=="medium", EWA_alpha=="risk-neutral") %>%
  group_by(memory_window) %>%
  summarize(variable = "memory window",
            meanTTD=mean(TTD),
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
  rename(value= memory_window) %>%
  mutate(value=as.character(value))

#### 3.2 Rho ####
t6 = df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_chi=="linear bias", EWA_sigma=="medium", EWA_alpha=="risk-neutral") %>%
  group_by(EWA_rho) %>%
  summarize(variable = "recent experience bias",
            meanTTD=mean(TTD),
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
  rename(value= EWA_rho) %>%
  mutate(value=as.character(value))

#### 3.2 Tau ####
t7 = df %>%
  filter(graph_type=="random regular", memory_window==10, EWA_chi=="linear bias", EWA_sigma=="medium", EWA_rho=="medium") %>%
  group_by(EWA_alpha) %>%
  summarize(variable = "risk-appetite",
            meanTTD=mean(TTD),
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
  rename(value= EWA_alpha) %>%
  mutate(value=as.character(value))
t7

t = bind_rows(t3,t4,t5,t6,t7)  %>% relocate(variable)


t = t %>% mutate(mean_perc_div = round(mean_perc_div,2), mean_d_order = round(mean_d_order,2), mean_d_time = round(mean_d_time,2))

t = t %>% mutate(meanTTD=paste0(meanTTD,"\\pm",round(seTTD,2)),
             meanTTFP=paste0(meanTTFP,"\\pm",round(seTTFP,2)),
             mean_d_order=paste0(mean_d_order,"\\pm",round(se_d_order,2)),
             mean_d_time=paste0(mean_d_time,"\\pm",round(se_d_time,2))) %>%
  select(-c(seTTD,seTTFP,se_d_order, se_d_time))
t
kable(t[2:ncol(t)],booktabs = TRUE,
      col.names = c('value', 'mean', 'PI', 'mean', 'PI', '% divergent', 'mean', 'PI', 'mean', "PI"),
      align = "rccccccccc",
      format="latex", escape = F) %>%
  kable_styling(full_width = F,  latex_options = c("scale_down")) %>%
  #collapse_rows(1, latex_hline = "none", row_group_label_position = c("identity")) %>%
  pack_rows(index=c("Soc. info. bias ($\\sigma$)"=3, "Conformity bias ($\\chi$)"=3, "Memory ($m$)"=3, "Recent exp. bias ($\\rho$)"=3, "Risk appetite ($\\alpha$)"=3), escape=F)%>%
  add_header_above(c(" "=1,"TTD"=2,"TTFP"=2,"divergence $d_{order}$"=3, "delay $d_{time}$"=2), escape=F)
