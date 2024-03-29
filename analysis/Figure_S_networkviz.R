library(tidyverse)
library(ggpubr)
library(magick)
library(grid)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rr = image_read("../output/network_diagrams/Random_regular.png")
rr = rr %>% image_quantize(colorspace = 'gray')
rr = rasterGrob(rr,height=1)
sw = image_read("../output/network_diagrams/small_world.png")
sw = sw %>% image_quantize(colorspace = 'gray')
sw = rasterGrob(sw,height=1)
er = image_read("../output/network_diagrams/Erdos.png")
er = er %>% image_quantize(colorspace = 'gray')
er = rasterGrob(er,height=1)
ba = image_read("../output/network_diagrams/Barabasi.png")
ba = ba %>% image_quantize(colorspace = 'gray')
ba = rasterGrob(ba,height=1)

g1 = ggarrange(rr,sw,er,ba, nrow=2, ncol=2, labels=c("random regular", "small world", "Erdos-Renyi","Barabasi-Albert"))
g1
ggsave(file="../output/Fig_S1_networkviz.png",width=16,height=16,scale=2.1,units="cm")
