library(tidyverse)
library(rethinking)
options(mc.cores = parallel::detectCores())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("../concat_data/EWA_inference_social_agents.Rda")

df_EWA = df_EWA %>%
  arrange(sim,agent,timestep) %>%
  group_by(sim,agent) %>%
  mutate(sim=sim+1,sum_obs=obs_a+obs_b, timestep=timestep+1, agent=agent+1,choice=as.numeric(as.factor(choice))) %>%
  ungroup()

df_EWA = df_EWA %>% mutate(grouping =as.numeric(as.factor(paste0(EWA_soc_info_weight,"_",EWA_recent_payoff_weight,"_",EWA_conformity,"_",EWA_inverse_temp))))

df_EWA = df_EWA %>% group_by(grouping) %>% mutate(agent = as.integer(as.factor(paste0(sim,"_",agent)))) %>% ungroup()
summary(df_EWA)

df_EWA %>% group_by(sim) %>% summarize(max(timestep))

for (i in unique(df_EWA$grouping)){
  print(paste("INFERRING VALUES FROM SIMULATION",i))
  df = df_EWA %>%
    filter(grouping==i) %>%
    select(agent,timestep,choice,yield_a,yield_b,obs_a,obs_b,EWA_soc_info_weight,EWA_recent_payoff_weight,EWA_conformity,EWA_inverse_temp) %>%
    arrange(agent,timestep)

  #turn simulated data to a list for r-stan
  ds <- list(
    N = nrow(df),
    J = length( unique(df$agent)),
    K=length(unique(df$choice)),
    tech = df$choice,
    y = cbind(df$yield_a, df$yield_b),
    s = cbind(df$obs_a, df$obs_b),
    id = df$agent,
    bout = df$timestep ,
    N_effects=4
  )
  parlist <- c("mu", "log_lik" ,"PrPreds","phi" , "gamma", "fc", "lambda")

  #fit model in stan
  fit <- stan( file = 'stan_code/stan_model_noRE.stan', data = ds ,
                     iter = 2500, warmup=1000, chains=5, cores=10, pars=parlist,
                     control=list( adapt_delta=0.9 ))

  post <- extract(fit)

  #extract posterior densities
  post_phi = logistic(post$mu[,1])
  post_gamma = logistic(post$mu[,2])
  post_f = exp(post$mu[,3])
  post_tau = exp(post$mu[,4])

  new = data.frame(post_phi,post_gamma,post_f,post_tau)

  new = new %>% mutate(
    true_phi=df$EWA_recent_payoff_weight[1],
    true_gamma=df$EWA_soc_info_weight[1],
    true_f=df$EWA_conformity[1],
    true_tau=df$EWA_inverse_temp[1],
    #est_phi=precis(fit, depth=2, pars='phi')[1,1],
    #est_gamma=precis(fit, depth=2, pars='gamma')[1,1],
    #est_f=precis(fit, depth=2, pars='fc')[1,1],
    #est_tau=precis(fit, depth=2, pars='lambda')[1,1],
    sim=i
  )

  data = rbind(data,new)

}

data$sim = as.factor(data$sim)
df_diffusion = data

save(df_diffusion,file="../concat_data/df_social_inference.Rda")


