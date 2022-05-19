library(tidyverse)
library(latex2exp)
library(rethinking)
library(kableExtra)




HPDI <- function( samples , prob=0.92 ) {
  # require(coda)
  coerce.list <- c( "numeric" , "matrix" , "data.frame" , "integer" , "array" )
  if ( inherits(samples, coerce.list) ) {
    # single chain for single variable
    samples <- coda::as.mcmc( samples )
  }
  x <- sapply( prob , function(p) coda::HPDinterval( samples , prob=p ) )
  # now order inside-out in pairs
  n <- length(prob)
  result <- rep(0,n*2)
  for ( i in 1:n ) {
    low_idx <- n+1-i
    up_idx <- n+i
    # lower
    result[low_idx] <- x[1,i]
    # upper
    result[up_idx] <- x[2,i]
    # add names
    names(result)[low_idx] <- concat("|",prob[i])
    names(result)[up_idx] <- concat(prob[i],"|")
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

df = rbind(df_vanilla,df_diffusion,df_asocial_inference) %>% mutate(post_rho = logistic(post_rho), post_sigma=logistic(post_sigma))

str(df)

t = df %>% group_by(type, true_rho, true_sigma, sim) %>%
  summarize(est_rho=round(mean(post_rho),2),
            HPDI_low_rho = round(HPDI(post_rho)[1],2),
            HPDI_high_rho = round(HPDI(post_rho)[2],2),
            est_sigma=round(mean(post_sigma),2),
            HPDI_low_sigma = round(HPDI(post_sigma)[1],2),
            HPDI_high_sigma = round(HPDI(post_sigma)[2],2)) %>%
  mutate(in_HPDI_rho = HPDI_low_rho <= true_rho & true_rho <= HPDI_high_rho,
         in_HPDI_sigma = HPDI_low_sigma <= true_sigma & true_sigma <= HPDI_high_sigma) %>%
  ungroup() %>%
  group_by(type, true_rho, true_sigma) %>%
  summarize(rho_in_HPDI = sum(in_HPDI_rho), perc_rho=sum(in_HPDI_rho)/n()*100, sigma_in_HPDI=sum(in_HPDI_sigma), perc_sigma=sum(in_HPDI_sigma)/n()*100)

kable(t[2:ncol(t)],booktabs = TRUE,
      col.names = c('rho', 'sigma', 'true rho in HPDI', '%', 'true sigma in HPDI', '%'),
      format="latex",
      digits=3) %>%
  kable_styling(full_width = F) %>%
  pack_rows(index=c("Homogeneous repertoires"=9,"Heterogeneous repertoires (social transmission)"=9, "Heterogeneous repertoires (asocial innovation)"=9)) %>%
  add_header_above(c("True values"=2,"true $rho$ in HPDI"=2,"true $sigma$ in HPDI"=2))

180-75
t=df %>% group_by(type,true_rho,true_sigma) %>%
  summarize(est_rho=round(mean(post_rho),2),
            HPDI_rho = paste0("[",round(HPDI(post_rho)[1],2),",",round(HPDI(post_rho)[2],2),"]"),
            rhat_rho=rhat_rho[1],
            neff_rho=as.integer(neff_rho[1]),
            est_sigma=round(mean(post_sigma),2),
            HPDI_sigma = paste0("[",round(HPDI(post_sigma)[1],2),",",round(HPDI(post_sigma)[2],2),"]"),
            rhat_sigma=rhat_sigma[1],
            neff_sigma=as.integer(neff_sigma[1]))

kable(t[2:ncol(t)],booktabs = TRUE,
      col.names = c('rho', 'sigma', 'mean', 'HPDI', 'rhat','N eff.','mean', 'HPDI', 'rhat','N eff.'),
      format="latex",
      align=c("ccrlccrlcc"),
      digits=3) %>%
  kable_styling(full_width = F) %>%
  pack_rows(index=c("Homogeneous repertoires"=9,"Heterogeneous repertoires (social transmission)"=9, "Heterogeneous repertoires (asocial innovation)"=9)) %>%
  add_header_above(c("True values"=2,"Estimated rho"=4,"Estimated sigma"=4))
