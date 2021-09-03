library(tidyverse)
library(rethinking)
library(ggridges)
options(mc.cores = parallel::detectCores())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
directory = "../concat_data/EWA_inference/"

load("../concat_data/EWA_inference_asocial_agents.Rda")

df_EWA = df_EWA %>%
  arrange(sim,agent,timestep) %>%
  group_by(sim,agent) %>%
  mutate(sim=sim+1,sum_obs=obs_a+obs_b, timestep=timestep+1, agent=agent+1,choice=as.numeric(as.factor(choice))) %>%
  ungroup()

df_EWA = df_EWA %>% mutate(grouping =as.numeric(as.factor(paste0(EWA_soc_info_weight,"_",EWA_recent_payoff_weight,"_",EWA_conformity,"_",EWA_inverse_temp))))

df_EWA = df_EWA %>% group_by(grouping) %>% mutate(agent = as.integer(as.factor(paste0(sim,"_",agent)))) %>% ungroup()
summary(df_EWA)

df_EWA %>% group_by(grouping) %>% summarize(max(timestep))

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
                     iter = 2500, warmup=1000, chains=5, cores=5, pars=parlist,
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
    sim=i
  )

  save(new,file=paste0("scratch/df",i,".Rda"))
  remove(new)

}

setwd("scratch/")


load("df1.Rda")
df1 = new
load("df2.Rda")
df2 = new
load("df3.Rda")
df3 = new
load("df4.Rda")
df4 = new
load("df5.Rda")
df5 = new
load("df6.Rda")
df6 = new
load("df7.Rda")
df7 = new
load("df8.Rda")
df8 = new
load("df9.Rda")
df9 = new




df_asocial_inference = bind_rows(df1,df2,df3,df4,df5,df6,df7,df8,df9)
df_asocial_inference$sim = as.factor(df_asocial_inference$sim)
save(df_asocial_inference,file="../../concat_data/EWA_inference/df_asocial_inference.Rda")

summary(df_asocial_inference)

ggplot(data=df_asocial_inference, aes(y=as.factor(true_phi),x=post_phi, fill=sim))+
  geom_density_ridges(alpha=0.3)+
  theme_classic()

ggplot(data=df_asocial_inference, aes(y=as.factor(true_gamma),x=post_gamma, fill=sim))+
  geom_density_ridges(alpha=0.3)+
  theme_classic()

ggplot(data=df_asocial_inference, aes(y=as.factor(true_f),x=post_f, fill=sim))+
  geom_density_ridges(alpha=0.3)+
  theme_classic()

ggplot(data=df_asocial_inference, aes(y=as.factor(true_tau),x=post_tau, fill=sim))+
  geom_density_ridges(alpha=0.3)+
  theme_classic()


summary(ds$bout)
