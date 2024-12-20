library(readr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(patchwork)

####LAYER DATA####
#ISCN Layer data
ISCNLayer <- read_excel("Data/ISCNData.xlsx", sheet = "Layer")
#ISCNLayer<-ISCNLayer[ ,-1]
colnames(ISCNLayer)<-c('dataset_name_sub', 'dataset_name_soc', 'lat', 'long', 'datum', 
                       'site_name', 'observation_date', 'profile_name', 'layer_name', 
                       'layer_top_cm', 'layer_bot_cm', 'hzn','hzn_desgn','vegclass_local',
                       'bd_method', 'bd_samp_g_cm3', 'bd_tot_g_cm3', 'bd_whole_g_cm3', 'bd_other_g_cm3',
                       'c_method', 'c_tot_percent', 'oc_percent', 'soc_g_cm2','soc_carbon_flag','soc_method')
vegtotals<-ISCNLayer %>% group_by(vegclass_local) %>% count()
view(vegtotals)
pinecol.iscn.layer<-grep("loblolly", ISCNLayer$vegclass_local)
ISCNLayer$vegclass_local[pinecol.iscn.layer] #no loblolly observations specified by this column
#surface_veg column is not available for download for Layer data, best I can tell 

####PROFILE DATA####
#ISCN Profile data
ISCNData <- read_excel("Data/ISCNData.xlsx")
#ISCNData<-ISCNData[ ,-1]
colnames(ISCNData)<-c('dataset_name_sub', 'dataset_name_soc', 'lat', 'long', 'datum', 
                      'state', 'observation_date', 'site_name', 'profile_name', 
                      'layer_top_cm', 'layer_bot_cm', 'total_lcount', 'carbon_lcount', 'soc_lcount',
                      'soc_depth_cm', 'soc_g_cm2', 'soc_method', 'soc_spatial_flag', 'soc_carbon_flag', 
                      'vegclass_local', 'surface_veg')

vegtotals<-ISCNData %>% group_by(vegclass_local) %>% count()
ISCNData %>% group_by(surface_veg) %>% count() %>% view()
view(vegtotals)
pinecol.iscn<-grep("pine", ISCNData$vegclass_local)
ISCNData$vegclass_local[pinecol.iscn]  #no loblolly observations specified by this column

pinelist<-c("PITA", "Pinus taeda")
emptylist<-list()
for (w in pinelist){
  emptylist[[w]]<-grep(get("w"), ISCNData$surface_veg) %>% print()
}
str(emptylist)

emptylist.df<-tibble(emptylist) #empty list from the standard loop above that has row numbers connected to each term
for (i in 1:length(emptylist.df)) { 
  row_indices <- unlist(emptylist.df[, i])
  row_indices.list<-as.numeric(row_indices)
  pinerows <- ISCNData[row_indices.list, ]
}

write.csv(pinerows, "ISCN.loblollyprofiles.csv")

pinerows %>% filter(!is.na(soc_g_cm2)) %>% summarize(mean=mean(soc_g_cm2),
                                                     median=median(soc_g_cm2), 
                                                     minimum=min(soc_g_cm2),
                                                     maximum=max(soc_g_cm2))
pinerows %>% filter(!is.na(soc_g_cm2)) %>% count()
pinerows %>% filter(!is.na(soc_g_cm2))
pinerows %>% filter(!is.na(soc_g_cm2)) %>% summarize(minimum=min(soc_depth_cm),
                                                     maximum=max(soc_depth_cm))


#using profiles identified as loblolly to subset layers with same profile identifiers
LLprofiles<-as.list(pinerows$profile_name)

emptylist2<-list()
for (w in LLprofiles){
  emptylist2[[w]]<-pinecol.iscn<-grep(get("w"), ISCNLayer$profile_name) %>% print()
}
str(emptylist2)
emptylist2.df<-tibble(emptylist2) #empty list from the standard loop above that has row numbers connected to each term
for (i in 1:length(emptylist2.df)) { 
  row_indices2 <- unlist(emptylist2.df[, i])
  row_indices2.list<-as.numeric(row_indices2)
  LLlayers <- ISCNLayer[row_indices2.list, ]
}

write.csv(LLlayers, "ISCN.loblollylayers.csv")

view(LLlayers)

LLlayers$layer_width_cm<-LLlayers$layer_bot_cm - LLlayers$layer_top_cm
LLlayers$layer_midpoint_cm<-(LLlayers$layer_bot_cm + LLlayers$layer_top_cm)/2

midpoint_intervals<-c("0-5", "5.1-15", "15.1-30", "30.1-50", "50.1-100", "100.1-200", "200+")
LLlayers$layer_mpinterval_cm<-ifelse(LLlayers$layer_midpoint_cm <=5.0, "0-5",
                                     ifelse(LLlayers$layer_midpoint_cm <=15.0 & LLlayers$layer_midpoint_cm>5, "5.1-15", 
                                            ifelse(LLlayers$layer_midpoint_cm <=30.0 & LLlayers$layer_midpoint_cm>15, "15.1-30",
                                                   ifelse(LLlayers$layer_midpoint_cm <=50.0 & LLlayers$layer_midpoint_cm>30, "30.1-50",
                                                          ifelse(LLlayers$layer_midpoint_cm <=100 & LLlayers$layer_midpoint_cm>50, "50.1-100",
                                                                 ifelse(LLlayers$layer_midpoint_cm <=200 & LLlayers$layer_midpoint_cm>100, "100.1-200", "200+"))))))


LLlayers %>% filter(!is.na(soc_g_cm2)) %>% summarize(mean=mean(soc_g_cm2),
                                                median=median(soc_g_cm2), 
                                                minimum=min(soc_g_cm2),
                                                maximum=max(soc_g_cm2))
LLlayers %>% filter(!is.na(soc_g_cm2)) %>% count()
LLlayers %>% filter(!is.na(soc_g_cm2)) %>% count(layer_top_cm, layer_bot_cm)
LLlayers %>% filter(!is.na(soc_g_cm2)) %>% summarize(minimum=min(soc_depth_cm),
                                                     maximum=max(soc_depth_cm))
LLlayers %>% filter(!is.na(soc_g_cm2)) %>%
  ggplot()+
  geom_histogram(aes(x=soc_g_cm2), bins=30)

####GRAPHS####
ggplot(pinerows) +
  geom_histogram(aes(x=soc_g_cm2), bins=30) +
  labs(x="SOC (g/cm^2)", title="SOC for complete profile", subtitle = "From ISCN")+
  theme_bw()

ggplot(pinerows)+
  geom_histogram(aes(x=layer_bot_cm), bins=30) +
  theme_bw() +
  labs(x="Total Profile Depth (cm)", title="Frequency of Measured Profile Depths", subtitle="From ISCN")

ggplot(pinerows)+
  geom_point(aes(x=soc_depth_cm, y=soc_g_cm2))

bluerange<-c("lightskyblue2","#87CEFF", "steelblue1","dodgerblue","dodgerblue3", "dodgerblue4")
ggplot(LLlayers)+
  geom_point(aes(x=layer_top_cm, y=layer_bot_cm, color=layer_width_cm)) + 
  theme_bw()+
  labs(x="Top of Layer (cm)", y="Bottom of Layer (cm)", title="Layer Depth Intervals", subtitle="From ISCN", color="Layer Width (cm)") +
  binned_scale(aesthetics = "color",
               scale_name = "stepsn",
               palette = function(x) bluerange,
               breaks = c(5, 10, 20, 50, 100),
               limits = c(0, 183),
               show.limits = TRUE,
               guide = "colorsteps")

ggplot(LLlayers)+
  geom_point(aes(x=layer_midpoint_cm, y=soc_g_cm2))

LLlayers %>% mutate(across(layer_mpinterval_cm, factor, levels=midpoint_intervals)) %>% 
  ggplot()+
  geom_boxplot(aes(x=soc_g_cm2, y=layer_mpinterval_cm), show.legend = F) +
  geom_point(aes(x=soc_g_cm2, y=layer_mpinterval_cm, color=layer_width_cm))+
  theme_bw()+
  labs(x="SOC (g/cm2)", y="Layer Midpoint (cm)", title = "SOC Concentrations sorted by Midpoint Depth of Soil Layer", 
       subtitle="From ISCN (Total C * BD)", color="Layer Width (cm)")+
  binned_scale(aesthetics = "color",
               scale_name = "stepsn",
               palette = function(x) c("firebrick2", "coral", "goldenrod1", "green3", "dodgerblue", "violet"),
               breaks = c(5, 10, 20, 50, 100),
               limits = c(0, 183),
               show.limits = TRUE,
               guide = "colorsteps")
  #scale_fill_brewer(palette='Pastel2')

LLlayers %>% mutate(across(layer_mpinterval_cm, factor, levels=midpoint_intervals)) %>% 
  ggplot()+
  geom_boxplot(aes(x=c_tot_percent, y=layer_mpinterval_cm), show.legend = F) +
  geom_point(aes(x=c_tot_percent, y=layer_mpinterval_cm, color=layer_width_cm))+
  theme_bw()+
  labs(x="Total Carbon (%)", y="Layer Midpoint (cm)", title = "Total Carbon Percentages sorted by Midpoint Depth of Soil Layer", 
       subtitle="From ISCN", color="Layer Width (cm)") + 
  #scale_fill_brewer(palette='Pastel2')+
  binned_scale(aesthetics = "color",
               scale_name = "stepsn",
               palette = function(x) c("firebrick2", "coral", "goldenrod1", "green3", "dodgerblue", "violet"),
               breaks = c(5, 10, 20, 50, 100),
               limits = c(0, 183),
               show.limits = TRUE,
               guide = "colorsteps")

#OC only has 5 observations - not really worth a figure
# LLlayers %>% mutate(across(layer_mpinterval_cm, factor, levels=midpoint_intervals)) %>% 
#   ggplot()+
#   geom_boxplot(aes(x=oc_percent, y=layer_mpinterval_cm, fill=layer_mpinterval_cm), show.legend = F) +
#   geom_point(aes(x=oc_percent, y=layer_mpinterval_cm))+
#   theme_bw()+
#   labs(x="OC (%)", y="Layer Midpoint (cm)", title = "OC Concentrations sorted by Midpoint Depth of Soil Layer", 
#        subtitle="From ISCN") + 
#   scale_fill_brewer(palette='Pastel2')

####COMBINING ISCN AND SRBD####
SRBDSOM<-srbd_lobL %>% filter(!is.na(value) & graphscale==2) %>% select(value) %>% mutate(value/(100*100))
colnames(SRBDSOM)<-c('SOM_g_m2', 'SOM_g_cm2')
SRBDSOM$database<-"SRBD"

ISCNSOM<-tibble(SOM_g_m2=pinerows$soc_g_cm2*(100*100),
                SOM_g_cm2=pinerows$soc_g_cm2,
                database="ISCN")

FIASOM<-tibble(SOM_g_m2= allstates_pita$carbon_soil_org/.044609, #correct conversion?
               SOM_g_cm2= allstates_pita$carbon_soil_org/44.609, 
               database="FIA")

SOM_combo<-rbind(SRBDSOM, ISCNSOM, FIASOM)
combo1<-SOM_combo %>% filter(!is.na(SOM_g_cm2)) %>% 
  ggplot()+
  geom_histogram(aes(x=SOM_g_cm2), bins=100) + 
  facet_wrap(~database, scales = "free")+
  theme_bw()+
  labs(x="SOC (g/cm^2)", y="Density", title="SOC values for total profiles of all depths")

combo2<-SOM_combo %>% filter(!is.na(SOM_g_cm2)) %>% 
  ggplot()+
  geom_histogram(aes(x=SOM_g_cm2), bins=200) + 
  theme_bw()+
  labs(x="SOC (g/cm^2)", y="Density", title="SOC values for total profiles of all depths", subtitle= "From ISCN, SRBD, and FIA")

combo2/combo1
