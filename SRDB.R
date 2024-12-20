library(readr)
library(tidyverse)
library(ggplot2)
library(patchwork)
srdb_data <- read_csv("Data/SRDB_V5_1827/data/srdb-data-V5.csv")

srdb_data %>% group_by(Species) %>% count() %>% print(n=100)
srdb_data %>% filter(Species == "Pinus taeda") %>% group_by(Biome, Ecosystem_state, Leaf_habit) %>% count()
srdb_data %>% group_by(Ecosystem_state) %>% count() %>% print(n=24)

srbd_narrowed<-srdb_data %>% filter(Ecosystem_type == "Forest" &
                       Biome == "Temperate" &
                       #Ecosystem_state == "Managed" & 
                       Leaf_habit == "Evergreen" &
                       Country == "USA") 
srbd_narrowed %>% group_by(Species) %>% count() %>% print(n=200)
srbd_lob<-srbd_narrowed %>% filter(Species == "Pinus taeda")
srbd_lob %>% group_by(Country, Region) %>% count()
nrow(srbd_lob)
write_csv(srbd_lob, "srbd_lob.csv")
srbd_lob %>% group_by(Manipulation) %>% count()



ggplot(data=srbd_lob) +
  geom_histogram(aes(x=Rs_annual))

srbd_lob %>% filter(!is.na(C_CR)) %>% group_by(Ecosystem_state) %>% count() #coarse root
srbd_lob %>% filter(!is.na(C_FR)) %>% group_by(Ecosystem_state) %>% count() #fine root
srbd_lob %>% filter(!is.na(C_litter)) %>% group_by(Ecosystem_state) %>% count() #litter
srbd_lob %>% filter(!is.na(C_soilmineral)) %>% group_by(Ecosystem_state) %>% count() #mineral soil 
srbd_lob %>% filter(!is.na(C_BG)) %>% group_by(Ecosystem_state) %>% count()

srbd_lobL<-srbd_lob %>% select(c(Record_number, YearsOfData, C_CR, C_FR, C_litter, C_BG, C_soilmineral, C_soildepth)) %>% 
  pivot_longer(cols = !c(Record_number, YearsOfData, C_soildepth), names_to = "C_measurement")

srbd_lobL$graphscale<-ifelse(srbd_lobL$C_measurement=="C_soilmineral", 2, 1)
srbd_lobL$pool_clean<-ifelse(srbd_lobL$C_measurement=="C_soilmineral", "SOM", 
                             ifelse(srbd_lobL$C_measurement=="C_CR", "Coarse Roots",
                                    ifelse(srbd_lobL$C_measurement=="C_FR", "Fine Roots",
                                           ifelse(srbd_lobL$C_measurement=="C_litter", "Litter", "Belowground Vegetation"))))

srbd_lobL %>% filter(pool_clean=="SOM") %>% filter(!is.na(value)) %>% summarize(mean=mean(value),
                                                                                median=median(value), 
                                                                                minimum=min(value),
                                                                                maximum=max(value))
srbd_lobL %>% filter(pool_clean=="SOM") %>% filter(!is.na(value)) %>% count()
srbd_lobL %>% filter(pool_clean=="SOM") %>% filter(!is.na(value)) %>%
  ggplot()+
  geom_histogram(aes(x=value), bins=15)

srbd_lobL %>% filter(pool_clean=="Coarse Roots") %>% filter(!is.na(value)) %>% summarize(mean=mean(value),
                                                                                median=median(value), 
                                                                                minimum=min(value),
                                                                                maximum=max(value))
srbd_lobL %>% filter(pool_clean=="Coarse Roots") %>% filter(!is.na(value)) %>% count()
srbd_lobL %>% filter(pool_clean=="Coarse Roots") %>% filter(!is.na(value)) %>%
  ggplot()+
  geom_histogram(aes(x=value), bins=15)

srbd_lobL %>% filter(pool_clean=="Fine Roots") %>% filter(!is.na(value)) %>% summarize(mean=mean(value),
                                                                                         median=median(value), 
                                                                                         minimum=min(value),
                                                                                         maximum=max(value))
srbd_lobL %>% filter(pool_clean=="Fine Roots") %>% filter(!is.na(value)) %>% count()
srbd_lobL %>% filter(pool_clean=="Fine Roots") %>% filter(!is.na(value)) %>%
  ggplot()+
  geom_histogram(aes(x=value), bins=15)

srbd_lobL %>% filter(pool_clean=="Litter") %>% filter(!is.na(value)) %>% summarize(mean=mean(value),
                                                                                       median=median(value), 
                                                                                       minimum=min(value),
                                                                                       maximum=max(value))
srbd_lobL %>% filter(pool_clean=="Litter") %>% filter(!is.na(value)) %>% count()
srbd_lobL %>% filter(pool_clean=="Litter") %>% filter(!is.na(value)) %>%
  ggplot()+
  geom_histogram(aes(x=value), bins=15)

srbd_lobL %>% filter(C_measurement=="C_BG") %>% filter(!is.na(value)) %>% summarize(mean=mean(value),
                                                                                   median=median(value), 
                                                                                   minimum=min(value),
                                                                                   maximum=max(value))
srbd_lobL %>% filter(C_measurement=="C_BG") %>% filter(!is.na(value)) %>% count()
srbd_lobL %>% filter(C_measurement=="C_BG") %>% filter(!is.na(value)) %>%
  ggplot()+
  geom_histogram(aes(x=value), bins=15)



####GRAPHS####

SRBD.plot1<-srbd_lobL %>% filter(!is.na(value) & graphscale==1) %>% 
  ggplot()+
  geom_boxplot(aes(x=value, y=pool_clean))+
  geom_point(aes(x=value, y=pool_clean)) +
  theme_bw() + 
  labs(x="Total Carbon (g/m^2)", y="C Pool", title="Carbon Pool Densities", subtitle="From SRBD")

SRBD.plot2<-srbd_lobL %>% filter(!is.na(value) & graphscale==2) %>% 
  ggplot()+
  geom_boxplot(aes(x=value, y=pool_clean))+
  geom_point(aes(x=value, y=pool_clean))+
  theme_bw() + 
  labs(x="Total Carbon (g/m^2)", y="C Pool") #no depth reported

SRBD.plot1/SRBD.plot2 

srbd_lobL %>% filter(!is.na(value) & graphscale==2) %>% select(value)
