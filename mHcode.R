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

hostStrainSum<-mHdFac %>% 
  group_by(Plant,Strain) %>% 
  summarize(mnWt=mean(weight, na.rm = TRUE)) %>% 
  arrange(-mnWt) %>% 
  mutate(rank=row_number())

AdRank<-hostStrainSum %>% 
  filter(Plant=="Ad") %>% 
  select(Strain, rank)

ord1<-AdRank$Strain

hostSsOrd<-mHdFac %>% 
  mutate(Strain = factor(Strain, levels = ord1))

hostSsOrd$Strain

ggplot(data=hostSsOrd, aes(x=weight, y=Strain))+
  geom_point()+
  facet_grid(~Plant, scales = "free_x")
