library(tidyverse)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#PROPORTIONAL Z_JT: heatmap of divergence
load(file="../model_outputs/Rda_files/NBDA_figure_data_proportional.Rda")
df$diffusion = factor(df$diffusion, levels=c("social diffusion","asocial diffusion"))
social_temp = df %>% ungroup() %>% filter(feeder_data=="first production", diffusion == "social diffusion") %>% mutate(correct_support=aicc_social<aicc_asocial)
load(file="../model_outputs/Rda_files/df_NBDA_SP.Rda")
df_div = df_NBDA_SP %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% ungroup() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_social = left_join(df_div,social_temp)
asocial_temp = df %>% ungroup() %>% filter(feeder_data=="first production", diffusion == "asocial diffusion") %>% mutate(correct_support=aicc_social>aicc_asocial)
load(file="../model_outputs/Rda_files/df_NBDA_AP.Rda")
df_div = df_NBDA_AP %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% ungroup() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_asocial = left_join(df_div,asocial_temp)
df_joined = bind_rows(df_asocial,df_social)
ggplot(df_joined %>% group_by(diffusion,correct_support,order_production,order_acquisition) %>% mutate(count=n()), aes(x=order_acquisition,y=order_production, fill=count))+
  facet_grid(correct_support~diffusion)+
  geom_tile() +
  labs(x="Order of acquisition", y="Order of production", fill="count")+
  scale_fill_continuous(type = "viridis",trans="log10", direction=-1) +
  theme_classic()
ggsave("../output/Fig_SX_NBDA_divergence.png", width=10,height=8,scale=2,units="cm")


#BINARY Z_JT: heatmap of divergence
load(file="../model_outputs/Rda_files/NBDA_figure_data.Rda")
summary(df)
df$diffusion = factor(df$diffusion, levels=c("social diffusion","asocial diffusion"))
social_temp = df %>% ungroup() %>% filter(feeder_data=="first production", diffusion == "social diffusion") %>% mutate(correct_support=aicc_social<aicc_asocial)
load(file="../model_outputs/Rda_files/df_NBDA_SB.Rda")
df_div = df_NBDA_SB %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% ungroup() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_social = left_join(df_div,social_temp)
asocial_temp = df %>% ungroup() %>% filter(feeder_data=="first production", diffusion == "asocial diffusion") %>% mutate(correct_support=aicc_social>aicc_asocial)
load(file="../model_outputs/Rda_files/df_NBDA_AB.Rda")
df_div = df_NBDA_AB %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% ungroup() %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df_div = df_div %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())
df_asocial = left_join(df_div,asocial_temp)
df_joined = bind_rows(df_asocial,df_social)
ggplot(df_joined %>% group_by(diffusion,correct_support,order_production,order_acquisition) %>% mutate(count=n()), aes(x=order_acquisition,y=order_production, fill=count))+
  facet_grid(correct_support~diffusion)+
  geom_tile() +
  labs(x="Order of acquisition", y="Order of production", fill="count")+
  scale_fill_continuous(type = "viridis",trans="log10", direction=-1) +
  theme_classic()
ggsave("../output/Fig_SX_NBDA_divergence_binary.png", width=10,height=8,scale=2,units="cm")