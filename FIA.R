library(readr)
library(tidyverse)
library(ggplot2)
library(patchwork)
#remotes::install_github("ncx-co/tidyFIA")
library(tidyFIA)
citation("tidyFIA")
####COND####

allstates_data <- tidyFIA::tidy_fia(
  states = c("GA", "SC", "TX", "LA", "AL", "AR", "MO", "FL", "VA", "MD", "DE", "TN", "OK", "NC", "MS"),
  postgis = F,
  table_names = c("plot", "sitetree", "cond"), 
  file_dir = '~/Desktop/Virginia Tech/Thesis/',
)
allstates_cond<-allstates_data[["cond"]]
allstates_pita<-allstates_cond %>% filter(fldtypcd==161)

allstates_cond %>% filter(fortypcd==161) %>% nrow()
allstates_cond %>% filter(fldtypcd==161) %>% nrow()
allstates_cond %>% filter(fortypcd==161 & fldtypcd==161) %>% nrow()
allstates_pita %>% nrow()

write.csv(allstates_pita, "FIA_COND_PITA.csv")

{allstates_pita %>% 
  ggplot()+
  geom_histogram(aes(x=carbon_litter), bins=50) +
  theme_bw()+
  labs(x="Litter Carbon (tons/acre)", y="Count", title="Litter Carbon Concentrations", subtitle="From FIA")
#install.packages("pwr")
library(pwr)

allstates_pita %>% filter(!is.na(carbon_litter)) %>% nrow()#summarize(mean(carbon_litter), 
                                                                             # sd(carbon_litter))
3.57 * 1.004
(3.58428- 3.57)/1.2
pwr.t.test(n=79933, d=0.0119, sig.level=0.1, power=NULL, type="one.sample", alternative="two.sided")


allstates_pita %>% 
  ggplot()+
  geom_histogram(aes(x=carbon_soil_org), bins=50) +
  theme_bw()+
  labs(x="SOC (tons/acre)", y="Count", title="SOC Concentrations", subtitle="From FIA")

allstates_pita %>% filter(!is.na(carbon_soil_org)) %>% summarize(mean=mean(carbon_soil_org),
                                                                median=median(carbon_soil_org), 
                                                                minimum=min(carbon_soil_org),
                                                                maximum=max(carbon_soil_org))
allstates_pita %>% filter(!is.na(carbon_soil_org)) %>% count()

allstates_pita %>% filter(!is.na(carbon_litter)) %>% summarize(mean=mean(carbon_litter),
                                                                 median=median(carbon_litter), 
                                                                 minimum=min(carbon_litter),
                                                                 maximum=max(carbon_litter))
allstates_pita %>% filter(!is.na(carbon_litter)) %>% count()

allstates_pita %>% filter(!is.na(carbon_down_dead)) %>% summarize(mean=mean(carbon_down_dead),
                                                               median=median(carbon_down_dead), 
                                                               minimum=min(carbon_down_dead),
                                                               maximum=max(carbon_down_dead))
allstates_pita %>% filter(!is.na(carbon_down_dead)) %>% count()
allstates_pita %>% ggplot()+
  geom_histogram(aes(x=carbon_down_dead), bins=100)
} #preliminary data exploration

####SOILS_LAB ALL####
#devtools::install_github('hunter-stanke/rFIA')
library(rFIA)
citation("rFIA")

GAsoils<-getFIA(states = 'GA', 
       tables='SOILS_LAB', 
       dir=NULL) 
GAsoils<-GAsoils$SOILS_LAB
SCsoils<-getFIA(states = 'SC', 
                tables='SOILS_LAB', 
                dir=NULL) 
SCsoils<-SCsoils$SOILS_LAB
TXsoils<-getFIA(states = 'TX', 
                tables='SOILS_LAB', 
                dir=NULL)
TXsoils<-TXsoils$SOILS_LAB
LAsoils<-getFIA(states = 'LA', 
                tables='SOILS_LAB', 
                dir=NULL)
LAsoils<-LAsoils$SOILS_LAB
ALsoils<-getFIA(states = 'AL', 
                tables='SOILS_LAB', 
                dir=NULL)
ALsoils<-ALsoils$SOILS_LAB
ARsoils<-getFIA(states = 'AR', 
                tables='SOILS_LAB', 
                dir=NULL)
ARsoils<-ARsoils$SOILS_LAB
MOsoils<-getFIA(states = 'MO', 
                tables='SOILS_LAB', 
                dir=NULL)
MOsoils<-MOsoils$SOILS_LAB
FLsoils<-getFIA(states = 'FL', 
                tables='SOILS_LAB', 
                dir=NULL)
FLsoils<-FLsoils$SOILS_LAB
VAsoils<-getFIA(states = 'VA', 
                tables='SOILS_LAB', 
                dir=NULL)
VAsoils<-VAsoils$SOILS_LAB
MDsoils<-getFIA(states = 'MD', 
                tables='SOILS_LAB', 
                dir=NULL)
MDsoils<-MDsoils$SOILS_LAB
DEsoils<-getFIA(states = 'DE', 
                tables='SOILS_LAB', 
                dir=NULL)
DEsoils<-DEsoils$SOILS_LAB
TNsoils<-getFIA(states = 'TN', 
                tables='SOILS_LAB', 
                dir=NULL)
TNsoils<-TNsoils$SOILS_LAB
OKsoils<-getFIA(states = 'OK', 
                tables='SOILS_LAB', 
                dir=NULL)
OKsoils<-OKsoils$SOILS_LAB
MOsoils <- read_csv("MO_SOILS_LAB.csv")
soils_all<-rbind(GAsoils,SCsoils,TXsoils,LAsoils, ALsoils, ARsoils,MOsoils, FLsoils, VAsoils, MDsoils, DEsoils, TNsoils, OKsoils, MOsoils)

loblollyplots<-allstates_pita$plt_cn %>% as.numeric()
loblollyplots_unique<-sapply(loblollyplots, unique)
soils_all %>% filter(PLT_CN %in% loblollyplots_unique) %>% nrow()

unique_pita<-allstates_pita %>% select(c(invyr, statecd, countycd, plot))
head(unique_pita)
unique_pita2<-unique_pita[!duplicated(unique_pita), ]
soils_all%>% select(c(INVYR, STATECD, COUNTYCD, PLOT)) %>% head()

empty_test<-tibble()
for (m in 1:nrow(unique_pita2)){
  currentconditions<-unique_pita2[m, ]
  keeper<-soils_all %>% filter(INVYR==currentconditions$invyr &
                         STATECD==currentconditions$statecd &
                         COUNTYCD==currentconditions$countycd &
                         PLOT==currentconditions$plot)
  empty_test<-rbind(empty_test, keeper)
}
nrow(empty_test)
write.csv(empty_test, "FIA_SOILSLAB_PITA.csv")

soils_org_pctL<-empty_test %>% select(c(PLT_CN, LAYER_TYPE, BULK_DENSITY, 
                                        COARSE_FRACTION_PCT, C_ORG_PCT, C_INORG_PCT, C_TOTAL_PCT)) %>% 
  pivot_longer(cols=!c(PLT_CN, LAYER_TYPE, BULK_DENSITY,COARSE_FRACTION_PCT), names_to= "POOL", values_to = "CARBON_PCT")

soils_org_pctL %>% filter(POOL=="C_TOTAL_PCT" & !is.na(BULK_DENSITY)) %>% count()
soils_org_pctL %>% filter(POOL=="C_ORG_PCT" & !is.na(BULK_DENSITY)) %>% count()
soils_org_pctL %>% filter(POOL=="C_INORG_PCT" & !is.na(BULK_DENSITY)) %>% count()

org.test<-soils_org_pctL %>% filter(LAYER_TYPE!="FF_TOTAL")
org.test$C_gpercm2<-org.test$BULK_DENSITY*org.test$CARBON_PCT*10

ggplot(org.test)+
  geom_boxplot(aes(x=C_gpercm2, y=POOL))

soils_org_pctL %>% filter(!is.na(CARBON_PCT)) %>% #filter(LAYER_TYPE == 'FF_TOTAL') %>% 
  ggplot()+
  geom_histogram(aes(x=CARBON_PCT), bins=100) + 
  facet_wrap(LAYER_TYPE~POOL, scales = "free") + 
  theme_bw()

soils_org_pctL %>% filter(LAYER_TYPE == 'FF_TOTAL') %>% count(CARBON_PCT)
soils_org_pctL %>% filter(POOL == 'C_INORG_PCT') %>% count(CARBON_PCT)
empty_test$C.O_tot.diff<-empty_test$C_ORG_PCT-empty_test$C_TOTAL_PCT
empty_test %>% group_by(LAYER_TYPE) %>% filter(LAYER_TYPE == 'FF_TOTAL') %>%count(C.O_tot.diff)

empty_test %>% filter(LAYER_TYPE != 'FF_TOTAL') %>%ggplot()+
  geom_histogram(aes(x=C.O_tot.diff))

####CWD####

state_list<-c("GA", "SC", "TX", "LA", "AL", "AR", "MO", "FL", "VA", "MD", "TN", "OK", "NC", "DE")
empty_test2<-tibble()
for(s in state_list){
  cwd_temp_m<-getFIA(states = s, 
                     tables='DWM_COARSE_WOODY_DEBRIS', 
                     dir=NULL)
  cwd_temp_m2<-cwd_temp_m$DWM_COARSE_WOODY_DEBRIS
  empty_test2<-rbind(empty_test2, cwd_temp_m2)
}


cwd_pita<-empty_test2 %>% filter(PLT_CN %in% loblollyplots_unique)

cwd_pita$diffCcols<-(cwd_pita$CARBON * cwd_pita$LPA_UNADJ) - cwd_pita$CARBON_AC_UNADJ
cwd_pita %>% filter(!is.na(CARBON_AC_UNADJ)) %>% nrow()
  # ggplot()+
  # geom_histogram(aes(x=diffCcols), bins=100)

write_csv(cwd_pita, "FIA_CWD_PITA.csv")

cwd_pita %>% 
  ggplot()+
  geom_histogram(aes(x=CARBON_AC_UNADJ), bins=200) + 
  theme_bw() + 
  labs(x="Carbon (pounds/acre)", y="Count", title="Carbon in Coarse Woody Debris", subtitle= "From FIA")


####TREE ALL####
empty_test3<-tibble()
for(s in state_list){
  filename<-paste("Data/FIA_tree/", s, "_TREE.csv", sep="")
  temp_tree<-read_csv(filename) %>% select(c('PLT_CN', 'CARBON_BG', 'TPA_UNADJ'))
  empty_test3<-rbind(empty_test3, temp_tree)
  message("TREE file for", paste(s), "imported")
}
#forgot DE the first time around but now it's in the csv file :)
## DE_tree_temp<-read_csv("Data/FIA_tree/DE_TREE.csv") %>% select(c('CARBON_BG','PLT_CN'))
## empty_test3<-rbind(empty_test3, DE_tree_temp)
tree_pita<-empty_test3 %>% filter(PLT_CN %in% loblollyplots_unique)



write.csv(tree_pita, "FIA_TREE_PITA.csv")

####PILES####
empty_test4<-tibble()
for(s in state_list){
  pile_temp_m<-getFIA(states = s, 
                     tables='DWM_RESIDUAL_PILE', 
                     dir=NULL)
  pile_temp_m4<-pile_temp_m$DWM_RESIDUAL_PILE
  empty_test4<-rbind(empty_test4, pile_temp_m4)
}

pile_pita<-empty_test4 %>% filter(PLT_CN %in% loblollyplots_unique)
write.csv(pile_pita, "FIA_PILE_PITA.csv")

####FWD####
empty_test5<-tibble()
for(s in state_list){
  fwd_temp_m<-getFIA(states = s, 
                      tables='COND_DWM_CALC', 
                      dir=NULL)
  fwd_temp_m5<-fwd_temp_m$COND_DWM_CALC
  empty_test5<-rbind(empty_test5, fwd_temp_m5)
}

empty_test6<-tibble()
for (m in 1:nrow(unique_pita2)){
  currentconditions<-unique_pita2[m, ]
  keeper<-empty_test5 %>% filter(INVYR==currentconditions$invyr &
                                 STATECD==currentconditions$statecd &
                                 COUNTYCD==currentconditions$countycd &
                                 PLOT==currentconditions$plot)
  empty_test6<-rbind(empty_test6, keeper)
}
nrow(empty_test6)
fwd_pita<-empty_test5 %>% filter(PLT_CN %in% loblollyplots_unique)
write.csv(fwd_pita, "FIA_FWD_PITA.csv")
nrow(fwd_pita)
nrow(empty_test)
FFtobind<-empty_test %>% filter(LAYER_TYPE=="FF_TOTAL" &
                                  !is.na(C_TOTAL_PCT))
12572-186
fwd_pita %>% count(is.na(FWD_LG_CARBON_UNADJ), 
                   is.na(FWD_MD_CARBON_UNADJ))
FF_FWD_test<-left_join(FFtobind, empty_test5, by=c('INVYR', 'STATECD', 'COUNTYCD', 'PLOT','PLT_CN'), relationship = "many-to-many")

FF_FWD_test %>% count(is.na(FWD_LG_CARBON_UNADJ), 
                      is.na(FWD_MD_CARBON_UNADJ), 
                      is.na(C_TOTAL_PCT), 
                      is.na(OVEN_DRY_SOIL_WT))
ggplot(FF_FWD_test)+
  geom_histogram(aes(x=(C_TOTAL_PCT*(OVEN_DRY_SOIL_WT/(3.14*15.24)))/10+FWD_MD_CARBON_UNADJ*0.0001120869+FWD_LG_CARBON_UNADJ*0.0001120869), bins=100) + 
  labs(x="FF + MD+LG FWD C kg/m2")

empty_test %>% filter(LAYER_TYPE=="FF_TOTAL" &
                                !is.na(C_TOTAL_PCT)) %>% ggplot() + 
  geom_histogram(aes(x=(C_TOTAL_PCT*(OVEN_DRY_SOIL_WT/(3.14*15.24)))/10), bins=100) + 
  labs(x="FF kg/m2")

FF_FWD_test %>% filter(is.na(FWD_MD_CARBON_UNADJ)) %>% ggplot() + 
  geom_histogram(aes(x=(C_TOTAL_PCT*(OVEN_DRY_SOIL_WT/(3.14*15.24)))/10), bins=100) + 
  labs(x="FF kg/m2")
colnames(fwd_pita)
fwd_pita %>% ggplot() + 
  geom_histogram(aes(x=CWD_CARBON_UNADJ*(16*28.35)/(1000*4046.86)), bins=100)
fwd_pita %>% ggplot() + 
  geom_histogram(aes(x=FWD_SM_CARBON_UNADJ*(16*28.35)/(1000*4046.86)), bins=100)
fwd_pita %>% ggplot() + 
  geom_histogram(aes(x=PILE_CARBON_UNADJ*(16*28.35)/(1000*4046.86)), bins=100)
fwd_pita %>% ggplot() + 
  geom_histogram(aes(x=FUEL_CARBON*(16*28.35)/(1000*4046.86)), bins=100) #no fuels data anyways!
fwd_pita %>% ggplot() + 
  geom_histogram(aes(x=LITTER_CARBON*(16*28.35)/(1000*4046.86)), bins=100)
fwd_pita %>% ggplot() + 
  geom_histogram(aes(x=DUFF_CARBON*(16*28.35)/(1000*4046.86)), bins=100)

####ANALYSIS TEST####
library(lme4)
lmer(carbon_soil_org ~ stdage + (1|plt_cn), data=allstates_pita) 



