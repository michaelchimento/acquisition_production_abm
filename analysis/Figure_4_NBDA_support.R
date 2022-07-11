library(tidyverse)
library(rethinking)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

PI = function (samples, prob = 0.92)
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





#Panel A: relative model support per condition
load(file="../model_outputs/Rda_files/df_NBDA_figure_data_binary.Rda")
df_ideal$condition = factor(df_ideal$condition, levels=c("social transmission","asocial learning"))
df_ideal = df_ideal %>% filter(data_type=="time_of_acquisition")
df_ideal$facet = "Idealized data"

load(file="../model_outputs/Rda_files/df_NBDA_figure_data_proportional.Rda")
df_realistic$condition = factor(df_realistic$condition, levels=c("social transmission","asocial learning"))
df_realistic = df_realistic %>% filter(data_type=="time_of_first_production", model_type=="weighted")
df_realistic$facet = "Realistic data"
df_combined = bind_rows(df_ideal,df_realistic) %>% mutate(facet=as.factor(facet), delta_aicc=aicc_asocial-aicc_social)

df_combined %>% group_by(facet,condition) %>% summarize(median = median(delta_aicc), Q2=PI(delta_aicc)[1], Q3=PI(delta_aicc)[2])

summary(df_combined)
p1 = ggplot(df_combined,aes(x=condition,y=delta_aicc, fill=facet))+
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_jitter(alpha=0.3, position=position_jitterdodge(dodge.width=.75), color="gray23")+
  geom_boxplot( alpha=0.6,outlier.alpha = 0, color="black")+
  scale_fill_viridis_d(begin=0.5)+
  labs(x="Acquisition mechanism",y="Support for social transmission (delta AICC)", fill="Data type")+
  theme_classic()

p1

#Panel B and C: delay and divergence correlates with type 1 and 2 errors
load(file="../model_outputs/Rda_files/df_NBDA_figure_data_proportional.Rda")
summary(df_realistic)
df_realistic$condition = factor(df_realistic$condition, levels=c("social transmission","asocial learning"))
social_temp = df_realistic %>%
  ungroup() %>%
  filter(data_type=="time_of_first_production", condition == "social transmission", model_type=="weighted") %>%
  mutate(inferred_mechanism=ifelse(aicc_social>aicc_asocial, "asocial learning", "social transmission"))

load(file="../model_outputs/Rda_files/df_NBDA_SP.Rda")
df_div = df_NBDA_SP %>%
  ungroup() %>%
  mutate(delay=timestep_production_b - timestep_acquisition_b) %>%
  group_by(sim) %>%
  arrange(timestep_acquisition_b) %>%
  mutate(order_acquisition=row_number(),
         delta_acq=timestep_acquisition_b-lag(timestep_acquisition_b))
df_div = df_div %>%
  group_by(sim) %>%
  arrange(timestep_production_b) %>%
  mutate(order_production=row_number())
df_div = df_div %>%
  select(sim,delay,order_acquisition,order_production, delta_acq) %>%
  group_by(sim) %>%
  summarize(avgdelay=mean(delay),
            divergence = 1-sum(order_acquisition==order_production)/n(),
            mean_delta_acq = mean(delta_acq, na.rm=T),
            manhattan_delay=sum(abs(delay))/72,
            manhattan_divergence=sum(abs(order_acquisition-order_production))/71,
            ratio=manhattan_delay/manhattan_divergence)

df_social = left_join(df_div,social_temp)

asocial_temp = df_realistic %>%
  ungroup() %>%
  filter(data_type=="time_of_first_production", condition == "asocial learning", model_type=="weighted") %>%
  mutate(inferred_mechanism=ifelse(aicc_social>aicc_asocial, "asocial learning", "social transmission"))

load(file="../model_outputs/Rda_files/df_NBDA_AP.Rda")
df_div = df_NBDA_AP %>%
  ungroup() %>%
  mutate(delay=timestep_production_b - timestep_acquisition_b) %>%
  group_by(sim) %>%
  arrange(timestep_acquisition_b) %>%
  mutate(order_acquisition=row_number(),
         delta_acq=timestep_acquisition_b-lag(timestep_acquisition_b))
df_div = df_div %>%
  group_by(sim) %>%
  arrange(timestep_production_b) %>%
  mutate(order_production=row_number())
df_div = df_div %>%
  select(sim,delay,order_acquisition,order_production, delta_acq) %>%
  group_by(sim) %>%
  summarize(avgdelay=mean(delay),
            divergence = 1-sum(order_acquisition==order_production)/n(),
            mean_delta_acq = mean(delta_acq, na.rm=T),
            manhattan_delay=sum(abs(delay))/72,
            manhattan_divergence=sum(abs(order_acquisition-order_production))/71,
            ratio=manhattan_delay/manhattan_divergence)

df_asocial = left_join(df_div,asocial_temp)
df_combined = bind_rows(df_social, df_asocial)

df_plot = df_combined %>%
  pivot_longer(cols=c(manhattan_delay, manhattan_divergence), names_to = "measure") %>%
  mutate(measure = as.factor(measure), inferred_mechanism=factor(inferred_mechanism, levels=c("social transmission", "asocial learning")))

levels(df_plot$measure) = c("delay","divergence")


df_plot = df_plot %>% mutate(label=case_when(
  inferred_mechanism=="social transmission" & condition=="social transmission" ~ "TP",
  inferred_mechanism=="asocial learning" & condition=="social transmission" ~ "FN",
  inferred_mechanism=="social transmission" & condition=="asocial learning" ~ "FP",
  inferred_mechanism=="asocial learning" & condition=="asocial learning" ~ "TN",
))

p2 = ggplot(df_plot, aes(x="", y=value))+
  facet_grid(rows=vars(condition), cols=vars(inferred_mechanism), switch="y")+
  geom_jitter(aes(fill=measure), alpha=0.4, position=position_jitterdodge(dodge.width=.75), color="gray25")+
  geom_boxplot(alpha=0.6, outlier.alpha = 0, aes(fill=measure))+
  geom_label(data = df_plot %>% group_by(inferred_mechanism,condition) %>% slice(n=1), aes(label=label), x=1,y=10, show.legend = F)+
  labs(x="Inferred acquisition mechanism", y= "True acquisition mechanism", fill="Measure")+
  scale_fill_viridis_d(option="C", end=0.5)+
  scale_x_discrete(position = "top")+
  theme_classic2()

library(cowplot)
p_graphs = plot_grid(p1 + theme(legend.position="none"),p2 + theme(legend.position="none"), labels=c("A","B"), align = "h", axis="bt")

legend1 <- get_legend(p1 + theme(legend.position = "bottom"))

legend2 <- get_legend(p2 + theme(legend.position = "bottom"))

legend = plot_grid(legend1,legend2, ncol = 2, nrow=1)

plot_grid(p_graphs, legend, rel_heights = c(3, .4), ncol=1,nrow=2)

ggsave("../output/Fig_S_NBDA_support.png", width=15,height=6,scale=2,units="cm")


df_plot %>% group_by(label,measure) %>% summarize(n(), mean = mean(value), Q2=PI(value)[1], Q3=PI(value)[2])
