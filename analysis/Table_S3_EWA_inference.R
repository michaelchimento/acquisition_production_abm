library(tidyverse)
library(latex2exp)
library(rethinking)

HPDI = function (samples, prob = 0.92)
{
  coerce.list <- c("numeric", "matrix", "data.frame", "integer",
                   "array")
  if (inherits(samples, coerce.list)) {
    samples <- coda::as.mcmc(samples)
  }
  x <- sapply(prob, function(p) coda::HPDinterval(samples,
                                                  prob = p))
  n <- length(prob)
  result <- rep(0, n * 2)
  for (i in 1:n) {
    low_idx <- n + 1 - i
    up_idx <- n + i
    result[low_idx] <- x[1, i]
    result[up_idx] <- x[2, i]
    names(result)[low_idx] <- concat("|", prob[i])
    names(result)[up_idx] <- concat(prob[i], "|")
  }
  return(result)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("../model_outputs/Rda_files/df_EWA_posterior_homogeneous_inference_10sim.Rda")
load("../model_outputs/Rda_files/df_EWA_posterior_social_inference_10sim.Rda")
load("../model_outputs/Rda_files/df_EWA_posterior_asocial_inference_10sim.Rda")

df_vanilla$type="a"
df_diffusion$type="b"
df_asocial_inference$type="c"

library(kableExtra)
df = rbind(df_vanilla,df_diffusion,df_asocial_inference) %>% mutate(post_rho=logistic(post_rho), post_sigma=logistic(post_sigma))

summary(df)

t1 = df %>% group_by(type,true_rho) %>%
  summarize(variable="rho",
            estimate = paste0(round(mean(post_rho),2)," [",round(HPDI(post_rho)[1],2),",",high = round(HPDI(post_rho)[2],2),"]"),
            rhat=paste0(round(mean(rhat_rho),3)," [",round(min(rhat_rho),3),",",round(max(rhat_rho),3),"]"),
            neff=paste0(paste0(as.integer(mean(neff_rho))," [",min(as.integer(neff_rho)),",",max(as.integer(neff_rho)),"]"))) %>%
  rename(value = true_rho)

t2 = df %>% group_by(type,true_sigma) %>%
  summarize(variable="sigma",
            estimate = paste0(round(mean(post_sigma),2), " [",round(HPDI(post_sigma)[1],2),",",high = round(HPDI(post_sigma)[2],2),"]"),
            rhat=paste0(round(mean(rhat_sigma),3)," [",round(min(rhat_sigma),3),",",round(max(rhat_sigma),3),"]"),
            neff=paste0(paste0(as.integer(mean(neff_sigma))," [",min(as.integer(neff_sigma)),",",max(as.integer(neff_sigma)),"]"))) %>%
  rename(value = true_sigma)

t = bind_rows(t1,t2) %>% ungroup() %>% arrange(type) %>% relocate(variable) %>% select(-type)
t
t = t %>%
  HPDIvot_wider(names_from = type, values_from = c(estimate, rhat, neff)) %>%
  relocate(variable) %>%
  select(variable, value,
          estimate_a, rhat_a, neff_a,
          estimate_b, rhat_b, neff_b,
          estimate_c, rhat_c, neff_c)

kable(t,booktabs = T,
      col.names = c('parameter','true value', 'estimate', 'rhat', 'effective sample size'),
      align = "rllll",
      format="latex") %>%
  kable_styling(full_width = F) %>%
  pack_rows(index=c("Homogenous"=6, "Heterogenous (social transmission)"=6,"Heterogenous (asocial learning)"=6 ))%>%
  column_spec(column = 1, bold = T) %>%
  collapse_rows(columns = 1, valign = "top")
t
