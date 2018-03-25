library(tidyverse)

#read data
multiHostDat<-read_csv("legume_weights_rhizobia_20str.csv")

#covert strain to factor
mHdFac<-multiHostDat %>% 
  mutate(Strain=as.factor(Strain))

#plot histogram of individual observations
ggplot(data=mHdFac, aes(x=weight))+
  geom_histogram(binwidth = 0.1)

#calculate summary statistics
hostStrainSum<-mHdFac %>% 
  group_by(Plant,Strain) %>% 
  summarize(mnWt=mean(weight, na.rm = TRUE),
            n=n(),
            SEM=sd(weight/sqrt(n), na.rm=TRUE)) %>% 
  arrange(-mnWt) %>% 
  mutate(rank=row_number())

#calculate rank of strains o A.dealbata to order results
AdRank<-hostStrainSum %>% 
  filter(Plant=="Ad") %>% 
  select(Strain, rank)

#reorder by A.dealbata strain rank
ord1<-AdRank$Strain

hostSsOrd<-hostStrainSum %>% 
  mutate(Strain = factor(Strain, levels = ord1))

mHdFacOrd<-mHdFac %>% 
  mutate(Strain = factor(Strain, levels = ord1))

### plot ordered results
########################

#plot all data points
ggplot(data=mHdFacOrd, aes(x=weight, y=Strain))+
  geom_point()+
  facet_grid(~Plant, scales = "free_x")

#plot means
ggplot(data=hostSsOrd, aes(x=Strain, y=mnWt))+
  geom_bar(stat="identity")+
  facet_grid(~Plant, scales = "free_x")+
  coord_flip()

#plot both
ggplot(data=hostSsOrd, aes(x=Strain, y=mnWt))+
  geom_bar(stat="identity", alpha=0.5)+
  geom_point(data=mHdFacOrd, aes(x=Strain, y=weight))+
  facet_grid(~Plant, scales = "free_x")+
  coord_flip()
  