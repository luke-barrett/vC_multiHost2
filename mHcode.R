setwd("~/ARC/ARC work/within-population/2018 analysis/Data/")

library(tidyverse)

multiHostDat<-read_csv("legume_weights_rhizobia_20str.csv")

mHdFac<-multiHostDat %>% 
  mutate(Strain=as.factor(Strain))

ggplot(data=mHdFac, aes(x=weight))+
  geom_histogram(binwidth = 0.1)

ggplot(data=mHdFac, aes(x=weight, y=Strain))+
  geom_point()+
  facet_grid(~Plant, scales = "free_x")

##try again tomorrow