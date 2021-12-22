#Figure SX - Deltas for SIGMA
load(file="../model_outputs/Rda_files/df_GEN_equiv_payoffs_acq_prod.Rda")
df = df_equiv_payoffs_acq_prod %>% filter(EWA_conformity==1, EWA_tau=="non-conservative", NBDA_s_param==5,memory_window==30) %>% mutate(delta=timestep_production_b - timestep_acquisition_b) %>% group_by(sim) %>% arrange(timestep_acquisition_b) %>% mutate(order_acquisition=row_number())
df = df %>% group_by(sim) %>% arrange(timestep_production_b) %>% mutate(order_production=row_number())

p1=ggplot(df, aes(x=delta, color=EWA_soc_info_weight))+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  geom_density(adjust=2)+
  coord_trans(x="sqrt")+
  labs(y="Density", x="Delay (production - acquisition)", color="Social info bias")+
  theme_classic()

p2=ggplot(df, aes(x=order_acquisition,y=delta,color=EWA_soc_info_weight))+
  stat_summary(geom="errorbar")+
  stat_summary(geom="line")+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  labs(x="Order of acquisition", y="Delay (production - acquisition)", color="Social info bias")+
  scale_x_continuous(breaks=c(1,8,16))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12))+
  theme_classic()

p3=ggplot(df, aes(x=order_production,y=delta,color=EWA_soc_info_weight))+
  stat_summary(geom="errorbar")+
  stat_summary(geom="line")+
  scale_color_viridis_d(option = "C", direction=-1, end=0.7)+
  labs(x="Order of production", y="Delay (production - acquisition)", color="Social info bias")+
  scale_x_continuous(breaks=c(1,8,16))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12))+
  theme_classic()

ggarrange(p1,p2,p3, labels = c("A","B","C"), common.legend = T, nrow=1)

ggsave("../output/Fig_S3_simga_delta.png",scale=2,width=12,height=5,units="cm")
