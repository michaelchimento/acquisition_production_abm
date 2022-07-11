library(tidyverse)
library(rethinking)
options(mc.cores = parallel::detectCores())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


####Homogeneous condition ####
load("../model_outputs/Rda_files/df_EWA_homogeneous_agents.Rda")
df_EWA = df_EWA %>%
  arrange(sim,agent,timestep) %>%
  group_by(sim,agent) %>%
  mutate(sim=sim+1,sum_obs=obs_a+obs_b, timestep=timestep+1, agent=agent+1,choice=as.numeric(as.factor(choice))) %>%
  ungroup()

df_EWA = df_EWA %>% mutate(grouping =as.numeric(as.factor(paste0(EWA_soc_info_weight,"_",EWA_recent_payoff_weight))))

df_EWA = df_EWA %>% group_by(sim) %>% mutate(agent = as.integer(as.factor(paste0(sim,"_",agent)))) %>% ungroup()
summary(df_EWA)

data= data.frame(post_rho=numeric(),
                 rhat_rho=numeric(),
                 neff_rho=numeric(),
                 post_sigma=numeric(),
                 rhat_sigma=numeric(),
                 neff_sigma=numeric(),
                 true_rho=numeric(),
                 true_sigma=numeric(),
                 sim=numeric(),
                 grouping=numeric())

for (i in unique(df_EWA$sim)){
    print(paste("INFERRING VALUES FROM SIMULATION",i))
    df = df_EWA %>%
      filter(sim==i) %>%
      select(sim, grouping, agent,timestep,choice,yield_a,yield_b,obs_a,obs_b,EWA_soc_info_weight,EWA_recent_payoff_weight,EWA_conformity,EWA_inverse_temp) %>%
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
      N_effects=2
    )
    parlist <- c("rho", "sigma")

    #fit model in stan
    fit <- stan( file = 'stan_code/stan_model_noRE_rhosigma.stan', data = ds ,
                       iter = 4000, warmup=1000, chains=5, cores=5, pars=parlist,
                       control=list( adapt_delta=0.95 ))

    sim=df$sim[1]
    group=df$grouping[1]
    #assign(paste0("fit",sim),fit)
    model_summary = precis(fit, depth=2)
    post <- extract(fit)
    remove(fit)

    #extract posterior densities
    post_rho = post$mu[,1]
    rhat_rho = model_summary$Rhat4[1]
    neff_rho = model_summary$n_eff[1]
    post_sigma = post$mu[,2]
    rhat_sigma = model_summary$Rhat4[2]
    neff_sigma = model_summary$n_eff[2]
    true_rho=df$EWA_recent_payoff_weight[1]
    true_sigma=df$EWA_soc_info_weight[1]


    new = data.frame(post_rho,rhat_rho,neff_rho,post_sigma,rhat_sigma,neff_sigma,true_rho,true_sigma, sim, group)


    data = rbind(data,new)
  }

data$sim = as.factor(data$sim)
df_vanilla = data

summary(df_vanilla)

save(df_vanilla,file="../model_outputs/Rda_files/df_EWA_posterior_homogeneous_inference_10sim.Rda")

####SOCIAL DIFFUSION####
load("../model_outputs/Rda_files/df_EWA_heterogeneous_social_agents.Rda")

df_EWA %>% group_by(sim) %>% summarize(max = max(timestep))
d = df_EWA %>% group_by(sim) %>% summarize(max = max(timestep))
mean(d$max)

df_EWA = df_EWA %>%
  arrange(sim,agent,timestep) %>%
  group_by(sim,agent) %>%
  mutate(sim=sim+1,sum_obs=obs_a+obs_b, timestep=timestep+1, agent=agent+1,choice=as.numeric(as.factor(choice))) %>%
  ungroup()

df_EWA = df_EWA %>% mutate(grouping =as.numeric(as.factor(paste0(EWA_soc_info_weight,"_",EWA_recent_payoff_weight))))

df_EWA = df_EWA %>% group_by(sim) %>% mutate(agent = as.integer(as.factor(paste0(sim,"_",agent)))) %>% ungroup()
summary(df_EWA)

data= data.frame(post_rho=numeric(),
                 rhat_rho=numeric(),
                 neff_rho=numeric(),
                 post_sigma=numeric(),
                 rhat_sigma=numeric(),
                 neff_sigma=numeric(),
                 true_rho=numeric(),
                 true_sigma=numeric(),
                 sim=numeric(),
                 grouping=numeric())


for (i in unique(df_EWA$sim)){
  print(paste("INFERRING VALUES FROM SIMULATION",i))
  df = df_EWA %>%
    filter(sim==i) %>%
    select(sim, grouping, agent,timestep,choice,yield_a,yield_b,obs_a,obs_b,EWA_soc_info_weight,EWA_recent_payoff_weight,EWA_conformity,EWA_inverse_temp) %>%
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
    N_effects=2
  )
  parlist <- c("rho", "sigma")

  #fit model in stan
  fit <- stan( file = 'stan_code/stan_model_noRE_rhosigma.stan', data = ds ,
               iter = 4000, warmup=1000, chains=5, cores=5, pars=parlist,
               control=list( adapt_delta=0.95 ))

  sim=df$sim[1]
  group=df$grouping[1]
  #assign(paste0("fit",sim),fit)
  model_summary = precis(fit, depth=2)
  post <- extract(fit)
  remove(fit)

  #extract posterior densities
  post_rho = post$mu[,1]
  rhat_rho = model_summary$Rhat4[1]
  neff_rho = model_summary$n_eff[1]
  post_sigma = post$mu[,2]
  rhat_sigma = model_summary$Rhat4[2]
  neff_sigma = model_summary$n_eff[2]
  true_rho=df$EWA_recent_payoff_weight[1]
  true_sigma=df$EWA_soc_info_weight[1]


  new = data.frame(post_rho,rhat_rho,neff_rho,post_sigma,rhat_sigma,neff_sigma,true_rho,true_sigma, sim, group)


  data = rbind(data,new)
}

data$sim = as.factor(data$sim)
df_diffusion = data
save(df_diffusion,file="../model_outputs/Rda_files/df_EWA_posterior_social_inference_10sim.Rda")

####ASOCIAL DIFFUSION ####

load("../model_outputs/Rda_files/df_EWA_heterogeneous_asocial_agents.Rda")
df_EWA %>% group_by(sim) %>% summarize(max = max(timestep))

df_EWA = df_EWA %>%
  arrange(sim,agent,timestep) %>%
  group_by(sim,agent) %>%
  mutate(sim=sim+1,sum_obs=obs_a+obs_b, timestep=timestep+1, agent=agent+1,choice=as.numeric(as.factor(choice))) %>%
  ungroup()

df_EWA = df_EWA %>% mutate(grouping =as.numeric(as.factor(paste0(EWA_soc_info_weight,"_",EWA_recent_payoff_weight))))

df_EWA = df_EWA %>% group_by(sim) %>% mutate(agent = as.integer(as.factor(paste0(sim,"_",agent)))) %>% ungroup()
summary(df_EWA)

data= data.frame(post_rho=numeric(),
                 rhat_rho=numeric(),
                 neff_rho=numeric(),
                 post_sigma=numeric(),
                 rhat_sigma=numeric(),
                 neff_sigma=numeric(),
                 true_rho=numeric(),
                 true_sigma=numeric(),
                 sim=numeric(),
                 grouping=numeric())


for (i in unique(df_EWA$sim)){
  print(paste("INFERRING VALUES FROM SIMULATION",i))
  df = df_EWA %>%
    filter(sim==i) %>%
    select(sim, grouping, agent,timestep,choice,yield_a,yield_b,obs_a,obs_b,EWA_soc_info_weight,EWA_recent_payoff_weight,EWA_conformity,EWA_inverse_temp) %>%
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
    N_effects=2
  )
  parlist <- c("rho", "sigma")

  #fit model in stan
  fit <- stan( file = 'stan_code/stan_model_noRE_rhosigma.stan', data = ds ,
               iter = 4000, warmup=1000, chains=5, cores=5, pars=parlist,
               control=list( adapt_delta=0.95 ))

  sim=df$sim[1]
  group=df$grouping[1]
  #assign(paste0("fit",sim),fit)
  model_summary = precis(fit, depth=2)
  post <- extract(fit)
  remove(fit)

  #extract posterior densities
  post_rho = post$mu[,1]
  rhat_rho = model_summary$Rhat4[1]
  neff_rho = model_summary$n_eff[1]
  post_sigma = post$mu[,2]
  rhat_sigma = model_summary$Rhat4[2]
  neff_sigma = model_summary$n_eff[2]
  true_rho=df$EWA_recent_payoff_weight[1]
  true_sigma=df$EWA_soc_info_weight[1]


  new = data.frame(post_rho,rhat_rho,neff_rho,post_sigma,rhat_sigma,neff_sigma,true_rho,true_sigma, sim, group)


  data = rbind(data,new)
}


data$sim = as.factor(data$sim)
df_asocial_inference = data
save(df_asocial_inference,file="../model_outputs/Rda_files/df_EWA_posterior_asocial_inference_10sim.Rda")
