library(readr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(patchwork)
#remotes::install_github("ncx-co/tidyFIA")
library(tidyFIA)
library(pwr)
#citation("WebPower")
#install.packages("WebPower")
library(WebPower)
#install.packages("moments")
library(moments)
#library(viridis)
library(dplyr)
library(ggpubr)



####LOADING DATA IN####
#International Soil Carbon Network #not specified what management type is
ISCN_layers <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/ISCN.loblollylayers.csv")
#ISCN_layers<-ISCN_layers[, -1]
ISCN_profiles <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/ISCN.loblollyprofiles.csv")
#ISCN_profiles<-ISCN_profiles[, -1]

#Pinemap #all managed according to experiment design
pinemap_tier2 <- read_excel("~/Desktop/Virginia Tech/Thesis/Data/pinemap_tier2.xlsx")
#pinemap_tier2<-pinemap_tier2[, -c(20:21)]
pinemap_sc<- read_excel("~/Desktop/Virginia Tech/Thesis/Data/pinemap_tier2.xlsx", 
                        sheet = "soil carbon")
#pinemap_sc<-pinemap_sc[, -c(11:14)]


#Soil Respiration Database #only available data is from managed (not natural) stands
srbd_lob <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/srbd_lob.csv")
srbd_lob<-srbd_lob %>% filter(Country=="USA")

#Forest Inventory and Analysis
FIA_COND_PITA <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/FIA_COND_PITA.csv")
FIA_COND_PITA<-FIA_COND_PITA[, -1]
FIA_SOILSLAB_PITA <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/FIA_SOILSLAB_PITA.csv")
FIA_SOILSLAB_PITA<-FIA_SOILSLAB_PITA[, -1]
FIA_CWD_PITA <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/FIA_CWD_PITA.csv")
FIA_TREE_PITA <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/FIA_TREE_PITA.csv")
FIA_TREE_PITA<-FIA_TREE_PITA[, -1]
FIA_FWD_PITA <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/FIA_FWD_PITA.csv")
FIA_FWD_PITA<-FIA_FWD_PITA[, -1]
fia_pile<- read_csv("~/Desktop/Virginia Tech/Thesis/Data/FIA_PILE_PITA.csv")
fia_pile<-fia_pile[, -1]

#LTSP #all managed according to experiment design
LTSP_lob <- read_csv("~/Desktop/Virginia Tech/Thesis/LTSP.lob.csv")
LTSP_ff_lob<-read_csv("~/Desktop/Virginia Tech/Thesis/LTSP.lob.ff.csv")

#####FIA#####
fia_pile$Carbon_poundsperacre<-fia_pile$CARBON * fia_pile$PPA_UNADJ
fia_pile$carbon_kgperm2<-fia_pile$Carbon_poundsperacre * (16*28.35)/(1000*4046.86)
fia_pile$definition_notes<-"transect C weight * piles per acre"
fia_pile$pool<-"pile"
fia_pile$table<-"DWM_RESIDUAL_PILE"
fia_pile$year<-fia_pile$MEASYEAR

FIA_COND_PITA %>% group_by(stdorgcd) %>% count()
fia_condL<-FIA_COND_PITA %>% dplyr::select(c(plt_cn, carbon_down_dead, carbon_litter, carbon_soil_org)) %>% 
  pivot_longer(cols=!"plt_cn", names_to = "pool", values_to = "carbon_tonsperacre_modeled")
colnames(fia_condL)<-c("PLT_CN", "pool", "carbon_tonsperacre_modeled", "carbon_kgperm2")
fia_condL$carbon_kgperm2<-fia_condL$carbon_tonsperacre_modeled * 0.2241738
fia_condL$definition_notes<-"modeled using Smith and Heath 2008"
fia_condL$table<-"COND"
#FIA_COND_PITA %>% filter(plt_cn %in% problem.mineral$PLT_CN) %>% group_by(statecd) %>% count()
#problem.mineral<-fia_condL %>% filter(pool=="carbon_soil_org" &
                 #                       carbon_tonsperacre_modeled<18.8)
#FIA_COND_PITA %>% filter(plt_cn %in% problem.mineral$PLT_CN) %>% group_by(statecd) %>% count()
#problem.mineral %>% nrow()
#problem.mineral$PLT_CN[duplicated(problem.mineral$PLT_CN)] #no duplicates - why are there 9699 of this one value? (4.19 kgperm2 or 18.69086 tonsperarcre)

CEAR.list<-FIA_COND_PITA %>% filter(stdorgcd==1) %>% dplyr::select(plt_cn) %>% as.list() 

fia_soilsL<-FIA_SOILSLAB_PITA %>% dplyr::select(c(PLT_CN, LAYER_TYPE, BULK_DENSITY, OVEN_DRY_SOIL_WT, 
                                                  C_ORG_PCT, C_INORG_PCT, C_TOTAL_PCT, MEASYEAR)) %>% 
  pivot_longer(cols=!c(PLT_CN, LAYER_TYPE, BULK_DENSITY, OVEN_DRY_SOIL_WT, MEASYEAR), names_to = "pool", values_to = "carbon_pct")

fia_soils_org<-FIA_SOILSLAB_PITA %>% dplyr::select(c(PLT_CN, LAYER_TYPE, BULK_DENSITY, OVEN_DRY_SOIL_WT, C_ORG_PCT, C_TOTAL_PCT, MEASYEAR))
fia_soils_org$thickness<-ifelse(FIA_SOILSLAB_PITA$LAYER_TYPE=="FF_TOTAL", NA, 10)
fia_soils_org$carbon_MGperha<-ifelse(fia_soils_org$LAYER_TYPE=="FF_TOTAL", fia_soils_org$C_TOTAL_PCT * (fia_soils_org$OVEN_DRY_SOIL_WT/(3.14*15.42)), #PROBLEM- g/cm2 but if you keep % as *100, you're at Mg/ha
                                     fia_soils_org$C_ORG_PCT * fia_soils_org$BULK_DENSITY * fia_soils_org$thickness) #removed the *100 because the percents are already in full number form, but is 100 needed to get from g/cm2 to Mg/ha

# FIA_SOILSLAB_PITA %>% filter(!is.na(C_INORG_PCT)) %>% summarize(min(C_INORG_PCT), 
#                                                                 max(C_INORG_PCT), 
#                                                                 mean(C_INORG_PCT))

fia_soils_org$carbon_kgperm2<-fia_soils_org$carbon_MGperha/10
fia_soils_org$pool<-fia_soils_org$LAYER_TYPE
fia_soils_org$definition_notes<-ifelse(fia_soils_org$LAYER_TYPE=="FF_TOTAL", "total C pct * (dry soil weight/sample area)", 
                                       "BD * organic C pct * thickness")
fia_soils_org$table<-"SOILS_LAB"
fia_soils_org$year<-fia_soils_org$MEASYEAR

FIA_FWD_PITA$FWD_MD_kgperm2<-FIA_FWD_PITA$FWD_MD_CARBON_UNADJ*0.0001120869
FIA_FWD_PITA$FWD_LG_kgperm2<-FIA_FWD_PITA$FWD_LG_CARBON_UNADJ*0.0001120869
FFtobind<-fia_soils_org %>% filter(LAYER_TYPE=="FF_TOTAL")
FF_FWD<-left_join(FFtobind, FIA_FWD_PITA, by=c('PLT_CN', 'MEASYEAR'), relationship = "many-to-many")
FF_FWD<-FF_FWD[!duplicated(FF_FWD), ]

FF_FWD$ff_carbon_kgperm2<-FF_FWD$carbon_kgperm2
FF_FWD$carbon_kgperm2<-FF_FWD$FWD_MD_kgperm2 + FF_FWD$FWD_LG_kgperm2 + FF_FWD$ff_carbon_kgperm2
FF_FWD$pool<-"FF+FWD"
FF_FWD$table<-"SOILS_LAB+COND_DWM_CALC"
FF_FWD %>% filter(!is.na(ff_carbon_kgperm2)) %>% nrow()
fia_soils_org %>% filter(LAYER_TYPE=="FF_TOTAL") %>% filter(!is.na(carbon_kgperm2)) %>% summarise(mean(carbon_kgperm2))

FIA_FWD_PITA$CWD_kgperm2<-FIA_FWD_PITA$CWD_CARBON_UNADJ*0.0001120869
FIA_FWD_PITA$pile_kgperm2<-FIA_FWD_PITA$PILE_CARBON_UNADJ*0.0001120869
FIA_FWD_PITA$litter_kgperm2<-FIA_FWD_PITA$LITTER_CARBON*0.0001120869
FIA_FWD_PITA$year<-FIA_FWD_PITA$MEASYEAR

FIA_COND_DWM_CALC_l<-FIA_FWD_PITA %>% dplyr::select(PLT_CN, CWD_kgperm2, pile_kgperm2, litter_kgperm2, MEASYEAR) %>% pivot_longer(cols=!c(PLT_CN, MEASYEAR), names_to = "pooltemp", values_to = "carbon_kgperm2")
FIA_COND_DWM_CALC_l$pool<-ifelse(FIA_COND_DWM_CALC_l$pooltemp=="CWD_kgperm2", "coarsewoodydebris", 
                                 ifelse(FIA_COND_DWM_CALC_l$pooltemp=="pile_kgperm2", "pile", "litter"))
FIA_COND_DWM_CALC_l$table<-"COND_DWM_CALC"
FIA_COND_DWM_CALC_l$definition_notes<-"based on plot transect length, unadjusted"
FIA_COND_DWM_CALC_l$year<-FIA_COND_DWM_CALC_l$MEASYEAR

fia_cwdL<-FIA_CWD_PITA %>% dplyr::select(c(PLT_CN, CARBON_AC_UNADJ, MEASYEAR))
colnames(fia_cwdL)<-c("PLT_CN", "carbon_poundsperacre", "year")
fia_cwdL$pool<-"coarsewoodydebris"
fia_cwdL$carbon_kgperm2<- fia_cwdL$carbon_poundsperacre * 0.0001120869
fia_cwdL$definition_notes<-"based on plot transect length, unadjusted"
fia_cwdL$table<-"DWM_COARSE_WOODY_DEBRIS"

#FIA_TREE_PITA %>% filter(!is.na(TPA_UNADJ)) %>% summarize(min(TPA_UNADJ))
# FIA_TREE_PITA$carbon_poundsperacre<-FIA_TREE_PITA$CARBON_BG*FIA_TREE_PITA$TPA_UNADJ
# fia_treeL<-FIA_TREE_PITA %>% dplyr::select(c("PLT_CN", "carbon_poundsperacre"))
# fia_treeL$pool<-"roots"
# fia_treeL$carbon_kgperm2<- fia_treeL$carbon_poundsperacre * 0.0001120869
# fia_treeL$definition_notes<-"carbon weight * TPA_unadjusted"
# fia_treeL$table<-"TREE"

fia_condL_sel<-fia_condL %>% dplyr::select(c('PLT_CN', 'pool', 'carbon_kgperm2','definition_notes', 'table'))
fia_soils_orgL_sel<-fia_soils_org %>% filter(LAYER_TYPE!="FF_TOTAL") %>% dplyr::select(PLT_CN, pool, carbon_kgperm2,definition_notes, table, year)
fia_cwdL_sel<-fia_cwdL %>% dplyr::select(PLT_CN, pool, carbon_kgperm2,definition_notes, table, year)
# fia_treeL_sel<-fia_treeL %>% dplyr::select(PLT_CN, pool, carbon_kgperm2,definition_notes, table)
fia_pile_sel<-fia_pile %>% dplyr::select(PLT_CN, pool, carbon_kgperm2,definition_notes, table, year)
fia_ff_sel<-FF_FWD %>% filter(!is.na(carbon_kgperm2)) %>% dplyr::select(PLT_CN, pool, carbon_kgperm2,definition_notes, table, year)
fia_condcwdcalc_sel<-FIA_COND_DWM_CALC_l %>% dplyr::select(PLT_CN, pool, carbon_kgperm2,definition_notes, table, year)

FIA_long<-rbind(#fia_condL_sel, 
  fia_soils_orgL_sel, fia_cwdL_sel, #fia_treeL_sel, #REMOVING FIA ROOT DATA BECAUSE IT IS ALLOMETRIC
  fia_pile_sel, fia_ff_sel, fia_condcwdcalc_sel)
FIA_long$database<-"FIA"
FIA_long %>% group_by(pool, table) %>% count()

FIA_long_nodups<-distinct(FIA_long, PLT_CN, pool, carbon_kgperm2, .keep_all = T)
nrow(FIA_long_nodups)

FIA_long_nodups$forest.management <- ifelse(FIA_long_nodups$PLT_CN %in% CEAR.list$plt_cn, "planted", "natural")

FIA_long_nodups_planted<-FIA_long_nodups %>% filter(PLT_CN %in% CEAR.list$plt_cn)

planted_and_nat.count<-FIA_long_nodups %>% group_by(pool) %>% count()
planted.count<-FIA_long_nodups_planted %>% group_by(pool) %>% count()

FIA_tobind<-FIA_long_nodups %>% dplyr::select(c(pool, carbon_kgperm2, database, definition_notes, table, forest.management, year))
FIA_tobind$calculation<-ifelse(FIA_tobind$pool=="roots", "0.5*biomass", 
                               ifelse(FIA_tobind$pool=="carbon_down_dead", "modeled", 
                                      ifelse(FIA_tobind$pool=="carbon_litter", "modeled",
                                             ifelse(FIA_tobind$pool=="carbon_soil_org", "modeled","observed"))))
FIA_tobind$depth_cm<-ifelse(FIA_tobind$pool=="carbon_soil_org", "0-100", 
                            ifelse(FIA_tobind$pool=="MIN_1", "0-10",
                                   ifelse(FIA_tobind$pool=="MIN_2", "10-20",NA)))


#####ISCN##### 
library(stringr)
ISCN_profiles$depth_cm<-paste("0-", ISCN_profiles$soc_depth_cm, sep="")
ISCN_profiles$forest.management<-ifelse(ISCN_profiles$surface_veg=="Pinus taeda canopy, Liriodendron/Liquidambar/Acer/Quercus understory", "natural", "planted")
ISCN_profiles$year <- str_extract(ISCN_profiles$observation_date, "^\\d{4}")

ISCN_planted<-ISCN_profiles %>% dplyr::select(c(profile_name, forest.management, year))
ISCN_layers_withmanagement<-unique(left_join(ISCN_layers, ISCN_planted, by="profile_name", relationship = "many-to-many"))
ISCN_layers_withmanagement$depth_cm<-paste(ISCN_layers_withmanagement$layer_top_cm, "-", ISCN_layers_withmanagement$layer_bot_cm, sep="")
ISCN_layersL<-ISCN_layers_withmanagement %>% dplyr::select(c(c_tot_percent, oc_percent, soc_g_cm2, depth_cm, soc_method, forest.management, year)) %>% 
  pivot_longer(cols=!c(depth_cm, soc_method, forest.management, year), names_to = "pool", values_to = "c_value")
ISCN_layersL$table<-"layer"

ISCN_profilesL<-ISCN_profiles %>% dplyr::select(c(soc_g_cm2, depth_cm, soc_method, forest.management, year)) %>% pivot_longer(cols=soc_g_cm2, names_to = "pool", values_to = "c_value")
ISCN_profilesL$table<-"profile"
ISCNL<-rbind(ISCN_layersL, ISCN_profilesL)
ISCNL$unit<-ifelse(ISCNL$pool=='soc_g_cm2', "gpercm2", "percent")

ISCNL_nona<-ISCNL %>% filter(!is.na(c_value))

ISCN_MassperArea<-ISCNL_nona %>% filter(unit=="gpercm2")
ISCN_MassperArea$carbon_kgperm2<-ISCN_MassperArea$c_value * 10 
ISCN_MassperArea$pool<-"carbon_soil_org"
ISCN_MassperArea$database<-"ISCN"
ISCN_MassperArea$calculation<-ISCN_MassperArea$soc_method
ISCN_tobind<-ISCN_MassperArea %>% dplyr::select(c(pool, carbon_kgperm2, database, depth_cm, calculation, table, forest.management, year))
ISCN_tobind$definition_notes<-"BD * C pct"



#####PINEMAP####
pinemap_pools<-pinemap_tier2 %>% dplyr::select(c('SITEID','roots<5mm', 'soil organic layer', 'CWD', 'soil detritus', 'Detritus'))
colnames(pinemap_pools)<-c('SITEID', 'fine_roots', 'o_horizon', 'coarsewoodydebris', 'soil_detritus', 'total_detritus')
pinemap_pools$o_horizon<-as.numeric(pinemap_pools$o_horizon)

pinemapL<-pinemap_pools %>% pivot_longer(cols=!SITEID, names_to = "pool", values_to = "c_MGperha")
pinemapL$carbon_kgperm2<-pinemapL$c_MGperha * 0.1
pinemapL$depth_cm<-ifelse(pinemapL$pool=="fine_roots", "0-100", 
                          ifelse(pinemapL$pool=="soil_detritus", "0-100", NA))
pinemapL$database<-"PINEMAP"
pinemapL$calculation<-ifelse(pinemapL$pool=="total_detritus", "soil organic layer+CWD+soil detritus", "observed")
pinemapL$table<-"detritus+vegetation"
pinemapL$definition_notes<-ifelse(pinemapL$pool=="total_detritus", "soil organic layer+CWD+soil detritus",
                                  ifelse(pinemapL$pool=="fine_roots", "<5mm", 
                                         ifelse(pinemapL$pool=="o_horizon", "detrital material collected from the surface to the mineral soil interface", 
                                                ifelse(pinemapL$pool=="coarsewoodydebris", ">7.5cm diameter", 
                                                       ifelse(pinemapL$pool=="soil_detritus", "fragmented wood, dead roots and char", NA)))))
pinemapL$forest.management<-"planted"
pinemapL$year <- 2015 #study went from 2012-2015
pinemap_tobind<-pinemapL %>% dplyr::select(c(pool, carbon_kgperm2, database, depth_cm, calculation, definition_notes, table, forest.management, year))


pinemap_soilc<-pinemap_sc %>% dplyr::select(c(Depth, soilC))
pinemap_soilc$pool<-"total carbon"
pinemap_soilc$calculation<-"observed"
colnames(pinemap_soilc)<-c('depth_cm', 'c_Mgperha', 'pool', 'calculation')
pinemap_soilc$database<-"PINEMAP"
pinemap_soilc$carbon_kgperm2<-pinemap_soilc$c_Mgperha * 0.1
pinemap_soilc$definition_notes<-"soil carbon"
pinemap_soilc$table<-"soil carbon"
pinemap_soilc$forest.management<-"planted"
pinemap_soilc$year <- 2015
pinemap_soilc_tobind<-pinemap_soilc %>% dplyr::select(c('depth_cm', 'carbon_kgperm2', 'pool', 'calculation', 'database', 'definition_notes', 'table', 'forest.management', 'year'))


#####SRDB#####
srdb_pools<-srbd_lob %>% dplyr::select(c(Site_ID, C_BG, C_CR, C_FR, C_litter, C_soilmineral, C_soildepth, Study_midyear))
colnames(srdb_pools)<-c('Site_ID', 'belowground_carbon', 'coarse_roots', 'fine_roots', 'carbon_litter', 'carbon_soil_org', 'depth_cm', 'year')
srdbL<-srdb_pools %>% pivot_longer(cols=!c(Site_ID, depth_cm, year), values_to = "c_gperm2", names_to = "pool")
srdbL$carbon_kgperm2<-srdbL$c_gperm2/1000
srdbL$calculation<-"0.5*biomass"
srdbL$database<-"SRDB"
srdbL$definition_notes<-ifelse(srdbL$pool=="belowground_carbon", "total carbon in belowground vegetation",NA)
srdbL$table<-"SRDBver5"
srdbL$forest.management<-"planted"
srdb_tobind<-srdbL %>% dplyr::select(c(pool, carbon_kgperm2, database,depth_cm,calculation,definition_notes, table, forest.management, year)) %>% filter(!is.na(carbon_kgperm2))


#####LTSP#####
LTSP_sel<-LTSP_lob %>% dplyr::select('soil_layer_bottom', 'soil_layer_top', 'thickness', 'tsc_kgperm2', 'observation_year')
LTSP_sel$depth_cm<-paste(LTSP_sel$soil_layer_top, "-", LTSP_sel$soil_layer_bottom, sep="")
LTSP_sel$pool<-"total carbon"
LTSP_sel$database<-"LTSP"
LTSP_sel$calculation<-"BD * C pct"
LTSP_sel$table<-"loblollyLTSPobs"
LTSP_sel$definition_notes<-"Soil Total Carbon not acid treated to remove inorganic C"
LTSP_sel$carbon_kgperm2<-LTSP_sel$tsc_kgperm2
LTSP_sel$forest.management<-"planted"
LTSP_sel$year<-LTSP_sel$observation_year
LTSP_tsc_tobind<-LTSP_sel %>% dplyr::select(!c(soil_layer_bottom, soil_layer_top, thickness, tsc_kgperm2, observation_year))


LTSP_ff_lob$depth_cm<-NA
LTSP_ff_lob$pool<-"o_horizon"
LTSP_ff_lob$database<-"LTSP"
LTSP_ff_lob$calculation<-"biomass * C pct"
LTSP_ff_lob$table<-"loblollyLTSPobs"
LTSP_ff_lob$definition_notes<-"Humus and litter"
LTSP_ff_lob$forest.management<-"planted"
LTSP_ff_lob$year<-LTSP_ff_lob$observation_year
LTSP_ff_tobind<-LTSP_ff_lob %>% dplyr::select(c(pool, carbon_kgperm2, database,depth_cm,calculation,definition_notes, table, forest.management, year))

####BIND####
kgperm2.planted<-rbind(ISCN_tobind, FIA_tobind, pinemap_tobind, srdb_tobind, pinemap_soilc_tobind, LTSP_tsc_tobind, LTSP_ff_tobind)
kgperm2.planted_nona<-kgperm2.planted %>% filter(!is.na(carbon_kgperm2)) 
kgperm2.planted_nona %>% group_by(pool) %>% count()

kgperm2.planted_nona$pool_group<-ifelse(kgperm2.planted_nona$pool=="roots", "Roots", 
                                ifelse(kgperm2.planted_nona$pool=="fine_roots", "Roots", 
                                       ifelse(kgperm2.planted_nona$pool=="coarse_roots", "Roots", 
                                              ifelse(kgperm2.planted_nona$pool=="MIN_2", "Mineral Soil", 
                                                     ifelse(kgperm2.planted_nona$pool=="MIN_1", "Mineral Soil", 
                                                            ifelse(kgperm2.planted_nona$pool=="carbon_soil_org", "Mineral Soil", 
                                                                   ifelse(kgperm2.planted_nona$pool=="total carbon", "Mineral Soil", 
                                                                          ifelse(kgperm2.planted_nona$pool=="total_detritus", "Detritus", 
                                                                                 ifelse(kgperm2.planted_nona$pool=="soil_detritus", "Detritus", 
                                                                                        ifelse(kgperm2.planted_nona$pool=="belowground_carbon", "Total Belowground", "Surface"))))))))))

kgperm2.planted_nona$pool2<-ifelse(kgperm2.planted_nona$pool=="MIN_2", "organic carbon", 
                           ifelse(kgperm2.planted_nona$pool=="MIN_1", "organic carbon", 
                                  ifelse(kgperm2.planted_nona$pool=="carbon_soil_org", "organic carbon", 
                                         ifelse(kgperm2.planted_nona$pool=="FF+FWD", "forest floor",kgperm2.planted_nona$pool))))

prettybins<-c('0-10', '0-15', '10-20', '0-20', '20-30','20-40', '0-40','20-50','0-50', '50-100','0-100' )

kgperm2.planted_nona$poolgroup2<-ifelse(kgperm2.planted_nona$pool=="roots", "Coarse Roots", 
                                ifelse(kgperm2.planted_nona$pool=="fine_roots", "Fine Roots", 
                                       ifelse(kgperm2.planted_nona$pool=="coarse_roots", "Coarse Roots", 
                                              ifelse(kgperm2.planted_nona$pool=="MIN_2", "Mineral Soil", 
                                                     ifelse(kgperm2.planted_nona$pool=="MIN_1", "Mineral Soil", 
                                                            ifelse(kgperm2.planted_nona$pool=="carbon_soil_org", "Mineral Soil", 
                                                                   ifelse(kgperm2.planted_nona$pool=="total carbon", "Mineral Soil", 
                                                                          ifelse(kgperm2.planted_nona$pool=="total_detritus", "TOOBROAD", 
                                                                                 ifelse(kgperm2.planted_nona$pool=="soil_detritus", "Belowground Detritus", 
                                                                                        ifelse(kgperm2.planted_nona$pool=="belowground_carbon", "TOOBROAD", 
                                                                                               ifelse(kgperm2.planted_nona$pool=="pile", "Harvest Pile",
                                                                                                      ifelse(kgperm2.planted_nona$pool=="carbon_down_dead", "Woody Down Dead", 
                                                                                                             ifelse(kgperm2.planted_nona$pool=="coarsewoodydebris", "Coarse Woody Material", "O Horizon")))))))))))))

poolgroup2.list<-c(#"Harvest Pile", #"Woody Down Dead", 
  "Coarse Woody Material", "O Horizon", "Coarse Roots", "Fine Roots","Belowground Detritus", "Mineral Soil")
numbercount.p<-list()
for (p in poolgroup2.list){
  numbercount.p[[p]]<-kgperm2.planted_nona %>% filter(forest.management=="planted") %>%  filter(poolgroup2==p) %>% count()
}

planted.counts<-kgperm2.planted_nona %>% filter(poolgroup2 %in% poolgroup2.list) %>% group_by(poolgroup2, forest.management) %>% count()
planted.counts.W<-planted.counts %>% pivot_wider(id_cols=poolgroup2, names_from=forest.management, names_prefix = "n.", values_from = "n")
planted.counts.W$n.natural<-planted.counts.W$n.natural %>% replace_na(0)
planted.counts.W$n.total<-planted.counts.W$n.planted+planted.counts.W$n.natural
planted.counts.W$percent.planted<-(planted.counts.W$n.planted/planted.counts.W$n.total)*100

histogramstats.p<-kgperm2.planted_nona %>% filter(poolgroup2 %in% poolgroup2.list & forest.management=="planted") %>% group_by(poolgroup2) %>% summarize(mean=mean(carbon_kgperm2), 
                                                                                                                median=median(carbon_kgperm2), 
                                                                                                                standard_deviation=sd(carbon_kgperm2))
hist_count.p<-kgperm2.planted_nona %>% filter(poolgroup2 %in% poolgroup2.list & forest.management=="planted") %>% group_by(poolgroup2) %>% count()
histogramstats2.p<-left_join(histogramstats.p, hist_count.p, by='poolgroup2') %>% arrange(factor(poolgroup2, levels=poolgroup2.list))
write.csv(histogramstats2.p, "~/Desktop/Virginia Tech/Thesis/VT_thesis/Written_tables/table1.planted.csv")
table.counts.all<-kgperm2.planted_nona %>% filter(poolgroup2 %in% poolgroup2.list) %>% group_by(poolgroup2, forest.management) %>% summarize(mean=mean(carbon_kgperm2), 
                                                                                                                           median=median(carbon_kgperm2), 
                                                                                                                           standard_deviation=sd(carbon_kgperm2)) 


table1<-left_join(table.counts.all, planted.counts, by=c("poolgroup2", "forest.management"))
table1.W<-table1 %>% pivot_wider(id_cols=poolgroup2, names_from=forest.management, values_from = c(mean, median, standard_deviation, n), names_sep = ".")
table1.W.narm<-table1.W %>% replace_na(list(mean.natural=0, median.natural=0, standard_deviation.natural=0, n.natural=0))
table1.wTotal<-left_join(table1.W.narm, hist_count.p, by="poolgroup2")
table1.wTotal$percent.planted<-(table1.wTotal$n.planted/table1.wTotal$n)*100
table1.wTotal$percent.natural<-(table1.wTotal$n.natural/table1.wTotal$n)*100

colnames(table1.wTotal)<-c("poolgroup2", "planted.mean", "natural.mean", "planted.median", "natural.median", "planted.sd", "natural.sd", "planted.n", "natural.n", "total.n", "planted.percentoftotal", "natural.percentoftotal")
# write.csv(table1.wTotal, "~/Desktop/Virginia Tech/Thesis/VT_thesis/Written_tables/table1.plantedvsnat.csv")


####GRAPHS####
kgperm2.planted_nona<-kgperm2.planted_nona %>% separate(col=depth_cm, into=c("top_depth_cm", "bottom_depth_cm"), sep="-", remove=F)
kgperm2.planted_nona$depth_increment_cm<-as.numeric(kgperm2.planted_nona$bottom_depth_cm) - as.numeric(kgperm2.planted_nona$top_depth_cm)
min.counts<-kgperm2.planted_nona %>% filter(pool_group=="Mineral Soil") %>% filter(depth_cm %in% prettybins) %>% group_by(depth_cm, forest.management) %>% count()

kgperm2.planted_nona %>% filter(pool_group=="Mineral Soil" & forest.management=="planted" & !is.na(depth_cm)) %>% group_by(depth_cm) %>% count()

dat_text <- data.frame(
  label= paste("n=", min.counts$n), 
  forest.management = min.counts$forest.management, 
  x = min.counts$depth_cm, 
  y = 15
)
man.names<-c(
  `planted` = "Planted",
  `natural` = "Natural"
)

kgperm2.planted_nona %>% 
  filter(pool_group=="Mineral Soil") %>% filter(depth_cm %in% prettybins) %>% 
  mutate(across(depth_cm, factor, levels=rev(prettybins))) %>% 
  ggplot()+
  geom_point(aes(x=depth_cm, y=carbon_kgperm2, color=database, shape=database), 
             stat="identity", position="jitter", show.legend = T)+
  geom_boxplot(aes(depth_cm, y=carbon_kgperm2), fill=NA)+
  theme_bw() + theme(legend.position = "bottom") + 
  facet_wrap(~forest.management, scales="free_y",labeller = as_labeller(man.names)) + 
  #legend.box.background = element_rect(fill="grey80")) + 
  scale_color_manual(values=c("red", "purple", "dodgerblue3", "green3"))+
  scale_shape_manual(values=c(0,1, 2, 3))+
  labs(x="Layer Depth (cm)", y=expression(paste("Carbon (", kg/m^2, paste(")"))))+#,
  #color="Database", shape="Database")+#, title="Mineral Soil Carbon")+
  coord_flip() +
  geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label))
ggsave(plot=last_plot(), filename = "mineral.comparison.png", path = "~/Desktop/Virginia Tech/Thesis/VT_thesis/Figures/Planted/", width = 6.5, height = 4, units = "in", dpi = 600)

min.depth.box1.p<-kgperm2.planted_nona %>% filter(forest.management=="planted") %>%
  filter(pool_group=="Mineral Soil") %>% filter(depth_cm %in% prettybins) %>% 
  mutate(across(depth_cm, factor, levels=rev(prettybins))) %>% 
  ggplot()+
  geom_point(aes(x=depth_cm, y=carbon_kgperm2, color=database, shape=database), 
             stat="identity", position="jitter", show.legend = F)+
  geom_boxplot(aes(depth_cm, y=carbon_kgperm2), fill=NA)+
  theme_bw() + #theme(legend.position = "bottom", text = element_text(size = 15)) + 
  scale_color_manual(values=c("red", "purple", "dodgerblue3", "green3"))+
  scale_shape_manual(values=c(0,1, 2, 3))+
  labs(x="Layer Depth (cm)", y=expression(paste("Carbon (", kg/m^2, paste(")"))), color="Database", shape="Database", title="Mineral Soil Carbon")+
  coord_flip() +
  annotate("text", label="n=470", y=15, x="0-10")+
  annotate("text", label="n=4", y=15, x="0-100")+
  annotate("text", label="n=1", y=15, x="0-15")+
  annotate("text", label="n=9", y=15, x="0-20")+
  annotate("text", label="n=9", y=15, x="0-40")+
  annotate("text", label="n=2", y=15, x="0-50")+
  annotate("text", label="n=464", y=15, x="10-20")+
  annotate("text", label="n=54", y=15, x="20-30")+
  annotate("text", label="n=9", y=15, x="20-40")+
  annotate("text", label="n=295", y=15, x="20-50")+
  annotate("text", label="n=295", y=15, x="50-100")

depth_names<-c(
  `10` = "Depth Increment: 10cm",
  `20` = "Depth Increment: 20cm",
  `50` = "Depth Increment: 50cm"
)
min.depth.box2.p<-kgperm2.planted_nona %>% filter(pool_group=="Mineral Soil") %>% filter(depth_cm %in% prettybins) %>% filter(forest.management=="planted") %>%
  filter(depth_increment_cm %in% c(10, 20, 50)) %>% 
  mutate(across(depth_cm, factor, levels=rev(prettybins))) %>% ggplot()+
  geom_point(aes(x=depth_cm, y=carbon_kgperm2, color=database, shape=database), 
             stat="identity", position="jitter", alpha=0.6)+
  geom_boxplot(aes(depth_cm, y=carbon_kgperm2), fill=NA)+
  theme_bw() + theme(legend.position = "bottom")+ 
                        #  text = element_text(size = 12)) + 
  scale_color_manual(values=c("red", "purple", "dodgerblue3", "green3"))+
  scale_shape_manual(values=c(0,1, 2, 3))+
  labs(x="Layer Depth (cm)", y=expression(paste("Carbon (", kg/m^2, paste(")"))),
       color="Database", shape="Database")+ #, title="Mineral Soil Carbon")+
  coord_flip() + 
  facet_wrap(~depth_increment_cm, scales="free_y", labeller = as_labeller(depth_names), ncol=3) +
  guides(shape = guide_legend(ncol = 4)) + guides(color = guide_legend(ncol = 4))
ggsave(plot=last_plot(), filename = "min.facetedbydepth.png", path = "~/Desktop/Virginia Tech/Thesis/VT_thesis/Figures/Planted/", width = 6.5, height = 2.75, units = "in", dpi = 600)

min.depth.box1.p/min.depth.box2.p

totalplanted.count<-kgperm2.planted_nona %>% filter(forest.management=="planted" & poolgroup2 %in% poolgroup2.list) %>% group_by(poolgroup2)  %>% mutate(poolgroup2 = factor(poolgroup2, levels = poolgroup2.list)) %>% count()
hist.annotations<-data.frame(
  label=paste("n =", totalplanted.count$n), 
  poolgroup2=totalplanted.count$poolgroup2, 
  x = c(11, 325, 0.6, 5.5, 0.4, 50), 
  y= c(375, 200, 0.23, 5, 5, 50)
)
pool_names<-c(
  `O Horizon` = "Organic Horizon",
  `Coarse Woody Material` = "Coarse Woody Material",
  `Coarse Roots` = "Coarse Roots", 
  `Fine Roots` = "Fine Roots",
  `Belowground Detritus` = "Belowground Detritus",
  `Mineral Soil` = "Mineral Soil Horizons"
)
kgperm2.planted_nona %>% filter(forest.management=="planted") %>% 
  filter(poolgroup2 %in% poolgroup2.list) %>% 
  mutate(poolgroup2 = factor(poolgroup2, levels = c("Coarse Woody Material", "O Horizon", "Mineral Soil", "Belowground Detritus", "Fine Roots", "Coarse Roots"))) %>%
  ggplot()+
  geom_histogram(aes(x=carbon_kgperm2), bins=100) + 
  facet_wrap(~poolgroup2, scales="free", ncol = 3) + 
  theme_bw() + 
  labs(x=expression(paste("Carbon (", kg/m^2, paste(")"))), y="Count") + #, title="Histograms of Carbon Pool observations", subtitle="From FIA, ISCN, SRDB, and Pinemap")
  geom_text(data = hist.annotations, mapping = aes(x = x, y = y, label = label)) 
ggsave(plot=last_plot(), filename = "histograms.png", path = "~/Desktop/Virginia Tech/Thesis/VT_thesis/Figures/Planted/", width = 6.5, height = 4, units = "in", dpi = 600)

kgperm2.planted_nona %>% filter(forest.management=="natural") %>% 
  filter(poolgroup2 %in% poolgroup2.list) %>% 
  mutate(poolgroup2 = factor(poolgroup2, levels = poolgroup2.list)) %>%
  ggplot()+
  geom_histogram(aes(x=carbon_kgperm2), bins=100) + 
  facet_wrap(~poolgroup2, scales="free", ncol = 3) + 
  theme_bw() + 
  labs(x=expression(paste("Carbon (", kg/m^2, paste(")"))), y="Count")

kgperm2.planted_nona %>% filter(forest.management=="planted") %>% 
  filter(poolgroup2=="Coarse Woody Material") %>% ggplot()+
  geom_histogram(aes(x=carbon_kgperm2), bins=30) + 
  theme_linedraw() + theme(text = element_text(size = 20)) + theme(panel.background = element_rect(fill="grey80", linetype = "solid"),
                                                                   panel.grid.major = element_line(colour = "grey40", linewidth = 0.1), 
                                                                   panel.grid.minor = element_line(colour = "grey40", linewidth = 0.1),
                                                                   plot.background = element_rect(fill = "grey80")) +
  labs(x=expression(paste("Carbon (", kg/m^2, paste(")"))), y="Count") +
  scale_x_continuous(limits=c(-0.1,1), breaks=c(0,0.25,0.5,0.75,1))
ggsave(plot=last_plot(), filename = "CWD.hist.LT.png", path = "~/Desktop/Virginia Tech/Thesis/VT_thesis/Figures/Planted/", width = 4, height = 4, units = "in", dpi = 600)

kgperm2.planted_nona %>% filter(forest.management=="planted") %>% 
  filter(poolgroup2=="O Horizon") %>% ggplot()+
  geom_histogram(aes(x=carbon_kgperm2), bins=30) + 
  theme_linedraw() + theme(text = element_text(size = 20)) + theme(panel.background = element_rect(fill="grey80", linetype = "solid"),
                                                                   panel.grid.major = element_line(colour = "grey40", linewidth = 0.1), 
                                                                   panel.grid.minor = element_line(colour = "grey40", linewidth = 0.1),
                                                                   plot.background = element_rect(fill = "grey80")) +
  labs(x=expression(paste("Carbon (", kg/m^2, paste(")"))), y="Count") +
  scale_x_continuous(limits=c(-0.25,10), breaks=c(0,2.5, 5, 7.5,10))
ggsave(plot=last_plot(), filename = "FF.hist.LT.png", path = "~/Desktop/Virginia Tech/Thesis/VT_thesis/Figures/Planted/", width = 4, height = 4, units = "in", dpi = 600)

kgperm2.planted_nona %>% filter(forest.management=="planted") %>% 
  filter(poolgroup2=="Mineral Soil") %>% ggplot()+
  geom_histogram(aes(x=carbon_kgperm2), bins=30) + 
  theme_linedraw() + theme(text = element_text(size = 20)) + theme(panel.background = element_rect(fill="grey80", linetype = "solid"),
                                                                   panel.grid.major = element_line(colour = "grey40", linewidth = 0.25), 
                                                                   panel.grid.minor = element_line(colour = "grey40", linewidth = 0.1),
                                                                   plot.background = element_rect(fill = "grey80")) +
  labs(x=expression(paste("Carbon (", kg/m^2, paste(")"))), y="Count") +
  scale_x_continuous(limits=c(-0.5,10), breaks=c(0,2,4,6,8,10)) 
ggsave(plot=last_plot(), filename = "TSC.hist.LT.p.png", path = "~/Desktop/Virginia Tech/Thesis/VT_thesis/Figures/Planted/", width = 4, height = 4, units = "in", dpi = 600)

meansbyyear<-kgperm2.planted_nona %>% group_by(poolgroup2, year, forest.management) %>% summarize(mean.c=mean(carbon_kgperm2), 
                                                                                                  median.c=median(carbon_kgperm2))

meansbyyear %>% filter(forest.management=="planted") %>% filter(poolgroup2 %in% poolgroup2.list) %>% ggplot() + 
  geom_line(aes(y=mean.c, x=as.numeric(year), color=poolgroup2))+
  geom_point(aes(y=mean.c, x=as.numeric(year), color=poolgroup2))+
  geom_line(aes(y=median.c, x=as.numeric(year), color=poolgroup2), linetype="dashed")+
  geom_point(aes(y=median.c, x=as.numeric(year), color=poolgroup2), shape=2)+
  theme_minimal() +
  labs(y="Carbon (kg/m^2)", x="Year", title="Mean and Median Carbon over time", subtitle="Circle and solid line = median; triangle/dashed line = median") +
  scale_color_manual(values=colpal.poolgroup5, name = "Pool")

kgperm2.planted_nona %>% filter(forest.management=="planted") %>% 
  filter(poolgroup2 %in% poolgroup2.list) %>% 
  mutate(poolgroup2 = factor(poolgroup2, levels = c("Coarse Woody Material", "O Horizon", "Mineral Soil", "Belowground Detritus", "Fine Roots", "Coarse Roots"))) %>%
  ggplot()+
  geom_histogram(aes(x=carbon_kgperm2, fill=year), bins=100) + 
  facet_wrap(~poolgroup2, scales="free", ncol = 3) + 
  theme_bw() + 
  labs(x=expression(paste("Carbon (", kg/m^2, paste(")"))), y="Count") + #, title="Histograms of Carbon Pool observations", subtitle="From FIA, ISCN, SRDB, and Pinemap")
  geom_text(data = hist.annotations, mapping = aes(x = x, y = y, label = label)) 

##QQ Plots
plots.p<-NULL
shapiro.wilk.p<-NULL
for(p in poolgroup2.list){
  temp.data<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted")
  plots.p[[p]]<-ggqqplot(temp.data$carbon_kgperm2, add = "qqline", title = paste(p))
  if(p=="Coarse Woody Debris"){
    temp.data<-dplyr::sample_n(temp.data, 1000)}
  shapiro.wilk.p[[p]]<-shapiro.test(temp.data$carbon_kgperm2)
}
qqCWD.p<-plots.p$`Coarse Woody Material`
qqO.p<-plots.p$`O Horizon` 
qqCR.p<-plots.p$`Coarse Roots`
qqFR.p<-plots.p$`Fine Roots`
qqBGD.p<-plots.p$`Belowground Detritus`
qqTSC.p<-plots.p$`Mineral Soil`

(qqCWD.p|qqO.p|qqCR.p)/(qqFR.p+qqBGD.p+qqTSC.p)

plotsTSC.p<-NULL
shapiro.wilkTSC.p<-NULL
for(b in prettybins){
  temp.data<-kgperm2.planted_nona %>% filter(poolgroup2=="Mineral Soil" & depth_cm==b & forest.management=="planted")
  temp.title<-paste("Depth = ", b, "cm")
  plotsTSC.p[[b]]<-ggqqplot(temp.data$carbon_kgperm2, add = "qqline", title = temp.title)
  if(nrow(temp.data)>=3){
    shapiro.wilkTSC.p[[b]]<-shapiro.test(temp.data$carbon_kgperm2) }
}

(plotsTSC.p$`0-10`|plotsTSC.p$`10-20`|plotsTSC.p$`0-20`|plotsTSC.p$`20-50`)/(plotsTSC.p$`0-40`|plotsTSC.p$`20-40`|plotsTSC.p$`20-50`|plotsTSC.p$`50-100`)
(plotsTSC.p$`0-10`|plotsTSC.p$`0-20`|plotsTSC.p$`0-40`|plotsTSC.p$`20-50`)/(plotsTSC.p$`10-20`|plotsTSC.p$`20-40`|plotsTSC.p$`20-30`|plotsTSC.p$`50-100`)
ggsave(plot=last_plot(), filename = "qqplots.tsc.p.png", path = "~/Desktop/Virginia Tech/Thesis/VT_thesis/Figures/Planted/", width = 12, height = 7, units = "in", dpi = 600)


####CALCULATIONS####
output.norm.p<-list()
output.nonnorm.p<-list()
for(p in poolgroup2.list){
  #statistics for tests
  n.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% nrow()
  mu0.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(mean(carbon_kgperm2)) %>% as.numeric()
  sd.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(sd(carbon_kgperm2)) %>% as.numeric()
  mu1.temp<-mu0.temp * 1.004
  d.temp<-(mu1.temp-mu0.temp)/sd.temp
  pool.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p)
  skew.temp<-skewness(pool.temp$carbon_kgperm2)
  kurt.temp<-kurtosis(pool.temp$carbon_kgperm2)
  
  #tests
  output.norm.p[[p]]<-pwr.t.test(n=n.temp, d=d.temp, sig.level=0.1, power=NULL, type="one.sample", alternative="two.sided")
  #output.nonnorm[[p]]<-wp.mc.t(n=n.temp, R0=10000, R1=1000, mu0=mu0.temp, mu1=mu1.temp, sd=sd.temp, skewness=skew.temp, kurtosis=kurt.temp, alpha=0.1, type="one.sample", alternative="two.sided")
  
  #curious
  if(skew.temp<2 & kurt.temp<7)
    message(paste(p), " is normally distributed.")
  if(skew.temp>2 | kurt.temp>7)
    message(paste(p), " is NOT normally distributed.")
}
str(output.norm.p)
t.test.powers.p<-data.frame(do.call(rbind,output.norm.p))
t.test.powers.p$poolgroup2<-poolgroup2.list



{output.norm2.p<-list()
  for(p in poolgroup2.list){
    #statistics for tests
    #n.temp<-kgperm2_nona %>% filter(pool2==p) %>% nrow()
    mu0.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(mean(carbon_kgperm2)) %>% as.numeric()
    sd.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(sd(carbon_kgperm2)) %>% as.numeric()
    mu1.temp<-mu0.temp * 1.004
    d.temp<-(mu1.temp-mu0.temp)/sd.temp
    
    #tests
    output.norm2.p[[p]]<-pwr.t.test(n=NULL, d=d.temp, sig.level=0.1, power=0.8, type="one.sample", alternative="two.sided")
    
  }
  t.test.samplesize.p<-data.frame(do.call(rbind,output.norm2.p))
  t.test.samplesize.p$currentn<-t.test.powers.p$n
  t.test.samplesize.p$poolgroup2<-poolgroup2.list
  t.test.samplesize.p$n<-unlist(t.test.samplesize.p$n)
  str(t.test.samplesize.p)
  
  t.test.samplesize.clean.p<-tibble(neededn=t.test.samplesize.p$n,
                                  currentn=unlist(t.test.samplesize.p$currentn),
                                  cohensd=unlist(t.test.samplesize.p$d),
                                  alpha=unlist(t.test.samplesize.p$sig.level),
                                  desiredpower=unlist(t.test.samplesize.p$power),
                                  alternative=unlist(t.test.samplesize.p$alternative), 
                                  poolgroup2=t.test.samplesize.p$poolgroup2)
  
  t.test.samplesize.clean.p$ndeficit<-t.test.samplesize.clean.p$neededn-t.test.samplesize.clean.p$currentn
  t.test.samplesize.clean.p$satisfied<-ifelse(t.test.samplesize.clean.p$ndeficit<0, "Satisfied", "Not Satisfied")
}



{output.norm3.p<-list()
necessarystats.p<-NULL
for(p in poolgroup2.list){
  
  #statistics for tests
  mu0.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(mean(carbon_kgperm2)) %>% as.numeric()
  median.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(median(carbon_kgperm2)) %>% as.numeric()
  sd.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(sd(carbon_kgperm2)) %>% as.numeric()
  n.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% nrow()
  # 
  temptibble<-tibble(poolgroup2=p,
                     mu0=mu0.temp, 
                     sd=sd.temp, 
                     median=median.temp)
  necessarystats.p<-dplyr::bind_rows(necessarystats.p, temptibble)
  
  #tests
  output.norm3.p[[p]]<-pwr.t.test(n=n.temp, d=NULL, sig.level=0.1, power=0.8, type="one.sample", alternative="two.sided")
}
t.test.detectchange.p<-data.frame(do.call(rbind,output.norm3.p))
t.test.detectchange.p$poolgroup2<-poolgroup2.list
t.test.detectchange2.p<-left_join(t.test.detectchange.p, necessarystats.p, by='poolgroup2')
t.test.detectchange2.p$percentchange<-100*(as.numeric(t.test.detectchange2.p$d)*t.test.detectchange2.p$sd)/t.test.detectchange2.p$mu0
t.test.detectchange2.df.p<-tibble(pool=t.test.detectchange2.p$poolgroup2,
                                sample_size=as.numeric(t.test.detectchange2.p$n),
                                standard_deviation=t.test.detectchange2.p$sd,
                                percentchange=t.test.detectchange2.p$percentchange,
                                power="0.8",
                                significance="0.1")
}



size.list<-seq(from=100, to=100000, by=1000)
alpha.list<-c(0.05, 0.1, 0.2)
output.norm4.p<-list()
powerrangers.p<-NULL
for(p in poolgroup2.list){
  for(a in alpha.list){
    for(i in size.list){
      
      #statistics for tests
      #normal
      mu0.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(mean(carbon_kgperm2)) %>% as.numeric()
      sd.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(sd(carbon_kgperm2)) %>% as.numeric()
      mu1.temp<-mu0.temp * 1.004
      d.temp<-(mu1.temp-mu0.temp)/sd.temp

      #tests
      output.norm4.p[[i]]<-pwr.t.test(n=i, d=d.temp, sig.level=a, power=NULL, type="one.sample", alternative="two.sided")
      
    }
    pool.powerrange<-data.frame(do.call(rbind,output.norm4.p))
    pool.powerrange$poolgroup2<-paste(p)
    pool.powerrange$alpha<-paste(a)
    powerrangers.p<-rbind(powerrangers.p, pool.powerrange)
    
    message(p, " at ", a, " DONE")
  }
  message(p, " DONE")
}
powerrangers.p<-powerrangers.p %>% unnest(cols = c(n, d, sig.level, power, alternative, note, method, poolgroup2, alpha))

powerrangers.p %>% mutate(across(poolgroup2, factor, levels=poolgroup2.list)) %>% ggplot() + 
  geom_line(aes(x=n, y=power, linetype=alpha)) + 
  facet_wrap(~poolgroup2, ncol=3) + 
  theme_bw() + theme(legend.position = "bottom", 
                     legend.background = element_rect(linetype = 1, color="black"), 
                     text = element_text(size = 15), 
                     axis.text.x.bottom = element_text(angle = 45, hjust = 1), 
                     axis.text.x.top = element_text(angle = 0, hjust = 0.5), 
                     plot.margin = margin(r = 20, l=10, t=10, b=10)) + 
  labs(x="Sample Size", y="Power")+#, title="A.")+ #title="Increases in Power corresponding with Sample Size", subtitle="Red line indicates Power of 0.8")+ 
  geom_hline(yintercept = 0.8, color="red") +
  scale_linetype(name="Significance Level") +
  #geom_text(data = hist.annotations, mapping = aes(x = 11000, y = 0.9, label = label)) + 
  scale_x_continuous(name = "Total Sample Size",  labels = label_comma(), 
    sec.axis = sec_axis(~ . / 13000, name = "Sample Density (n/1,000 ha)"))
ggsave(plot=last_plot(), "powerincreasewithincreasingsamplesize.planted.png", path="~/Desktop/Virginia Tech/Thesis/VT_thesis/Figures/Planted/", width=9, height=6, units = "in", dpi=600)

size.list2<-seq(from=50, to=20000, by=5)
output.norm5.p<-list()
powerrangers5.p<-NULL
t.test.detectchange5.p<-NULL
necessarystats5.p<-NULL
for(p in poolgroup2.list){
  #statistics for tests
  mu0.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(mean(carbon_kgperm2)) %>% as.numeric()
  sd.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p  & forest.management=="planted") %>% summarize(sd(carbon_kgperm2)) %>% as.numeric()
  
  temptibble<-tibble(poolgroup2=p,
                     mu0=mu0.temp, 
                     sd=sd.temp)
  necessarystats5.p<-dplyr::bind_rows(necessarystats5.p, temptibble)
  
  for(a in alpha.list){
    for(i in size.list2){
      #tests
      output.norm5.p[[i]]<-pwr.t.test(n=i, d=NULL, sig.level=a, power=0.8, type="one.sample", alternative="two.sided")
    }
    pool.powerrange<-data.frame(do.call(rbind,output.norm5.p))
    pool.powerrange$poolgroup2<-paste(p)
    pool.powerrange$alpha<-paste(a)
    t.test.detectchange5.p<-left_join(pool.powerrange, necessarystats5.p, by='poolgroup2', relationship = "many-to-many")
    t.test.detectchange5.p$percentchange<-100*(as.numeric(t.test.detectchange5.p$d)*t.test.detectchange5.p$sd)/t.test.detectchange5.p$mu0
    powerrangers5.p<-rbind(powerrangers5.p, t.test.detectchange5.p)
    message(p, " at ", a, " DONE")
  }
  message(p, " DONE")
}
powerrangers5.p<-powerrangers5.p %>% unnest(cols = c(n, d, sig.level, power, alternative, note, method, poolgroup2, alpha, percentchange))

powerrangers5.w.p<-powerrangers5.p %>% dplyr::select(!c('power', 'alternative', 'note', 'method', 'alpha', 'd', 'mu0', 'sd')) %>% 
  pivot_wider(id_cols=c(poolgroup2, n), names_from = sig.level, values_from = percentchange, names_prefix = "alpha")
powerrangers5.w.p$nper10000ha<-powerrangers5.w.p$n/1300
poolgroup2.list
powerrangers5.w.p %>% group_by(poolgroup2) %>% summarize(min(alpha0.2), 
                                                         min(alpha0.1), 
                                                         min(alpha0.05))
colpal.poolgroup5<-c("firebrick1", "orange2", "dodgerblue1", "orchid", "green3", "darkgreen")
powerrangers5.w.p %>% mutate(across(poolgroup2, factor, levels=poolgroup2.list)) %>% ggplot() + 
  geom_ribbon(aes(x=n, ymax=alpha0.05, ymin=alpha0.2, fill=poolgroup2, color=poolgroup2), alpha=0.3) + 
  geom_line(aes(x=n, y=alpha0.1, color=poolgroup2, linetype=poolgroup2)) + 
  theme_bw() + theme(#axis.text.x = element_text(angle = 45, hjust = 0), 
                     legend.position=c(0.775, 0.7), 
                     text = element_text(size = 17), 
                     legend.background = element_rect(fill="white"),
                     plot.margin = margin(r = 30, l=10, t=10, b=10))+
  # caption="Upper bound represents change detectable at alpha=0.05; \nMiddle line represents change detectable at alpha=0.1;\nLower bound represents change detectable at alpha=0.2.")+#, title="B.")+ #title="Detectable Percent Change in C Stock with Increasing Sample Size")+ 
  #scale_x_continuous(trans='log10', breaks = c(100, 500, 1000, 5000, 10000), expand=c(0,0)) +  #13 million hectares of planted PITA
  scale_x_continuous(trans='log10', breaks = c(50, 100, 500, 1000, 5000, 10000), expand=c(0,0), name = "Sample Size (Log-Transformed)", 
                     sec.axis = dup_axis(name = "Sampling Density (n/Million ha)", labels=c(3.9, 7.7, 38.5, 76.9, 384.6, 769.2)))+
  scale_y_continuous(expand=c(0,0), breaks = c(20,40,60,80,100), name="Theoretical Detectable Change\nin C Stock (%)")+
  scale_color_manual(values=colpal.poolgroup5, name="Pool", breaks=c("Coarse Woody Material", "O Horizon", "Mineral Soil", "Belowground Detritus", "Fine Roots", "Coarse Roots"))+
  scale_fill_manual(values=colpal.poolgroup5, name="Pool", breaks=c("Coarse Woody Material", "O Horizon", "Mineral Soil", "Belowground Detritus", "Fine Roots", "Coarse Roots"))+
  scale_linetype_manual(values=c(1:6), name="Pool", breaks=c("Coarse Woody Material", "O Horizon", "Mineral Soil", "Belowground Detritus", "Fine Roots", "Coarse Roots")) # + 
  # geom_vline(aes(xintercept=75), color="darkgreen", linetype="twodash", alpha=0.7)+
  # geom_vline(aes(xintercept=1110), color="orchid", linetype="dotdash", alpha=0.7)+
  # geom_vline(aes(xintercept=755), color="green3", linetype="longdash", alpha=0.7)+
  # geom_vline(aes(xintercept=2135), color="dodgerblue1", linetype="dotted", alpha=0.7)+
  # geom_vline(aes(xintercept=4990), color="orange2", linetype="dashed", alpha=0.7)
ggsave(plot=last_plot(), file="percentchangewithincreasingsamplesize.ribbons.png", path="~/Desktop/Virginia Tech/Thesis/VT_thesis/Figures/Planted/", width=8, height=6, units = "in", dpi=600)

powerrangers5.w.p %>% filter(alpha0.1<10) %>% group_by(poolgroup2) %>% summarize(min(n))

###tables to write
alpha.list2<-c(0.001, 0.05, 0.10, 0.2, 0.5)
output.norm6.p<-list()
powerrangers6.p<-NULL
temp_results6.p<-NULL
t.test.detectchange6.p<-NULL
for(a in alpha.list2){
  necessarystats6.p<-NULL
  for(p in poolgroup2.list){
    n.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% nrow()
    mu0.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% 
      summarize(mean(carbon_kgperm2)) %>% as.numeric()
    sd.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% 
      summarize(sd(carbon_kgperm2)) %>% as.numeric()
    
    temptibble<-tibble(mu0=mu0.temp, 
                       sd=sd.temp)
    necessarystats6.p<-dplyr::bind_rows(necessarystats6.p, temptibble)
    
    output.norm6.p[[p]]<-pwr.t.test(n=n.temp, d=NULL, 
                                  sig.level=a, power=0.8, type="one.sample", alternative="two.sided")
  }
  necessarystats6.p$poolgroup2<-poolgroup2.list
  temp_results6.p <- data.frame(do.call(rbind, output.norm6.p))
  temp_results6.p$poolgroup2<-poolgroup2.list
  t.test.detectchange6.p<-left_join(temp_results6.p, necessarystats6.p, by='poolgroup2')
  t.test.detectchange6.p$percentchange<-100*(as.numeric(t.test.detectchange6.p$d)*t.test.detectchange6.p$sd)/t.test.detectchange6.p$mu0
  powerrangers6.p<-rbind(powerrangers6.p, t.test.detectchange6.p)
}
powerrangers6.p<-powerrangers6.p %>% unnest(cols = c(n, d, sig.level, power, alternative, note, method, poolgroup2))
powerrangers6.pretty.p<-powerrangers6.p %>% dplyr::select(c("sig.level", "poolgroup2", "percentchange"))
powerrangers6.pretty.p<-powerrangers6.pretty.p %>% 
  pivot_wider(id_cols = poolgroup2,
              names_from = sig.level, values_from = percentchange, 
              names_prefix = "alpha=")


output.norm7.p<-NULL
powerrangers7.p<-NULL
t.test.detectchange7.p<-NULL
for(a in alpha.list2){
  necessarystats7<-NULL
  for(p in poolgroup2.list){
    #n.temp<-kgperm2_nona %>% filter(poolgroup2==p) %>% nrow()
    mu0.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(mean(carbon_kgperm2)) %>% as.numeric()
    sd.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted") %>% summarize(sd(carbon_kgperm2)) %>% as.numeric()
    mu1.temp<-mu0.temp * 1.004
    d.temp<-(mu1.temp-mu0.temp)/sd.temp
    
    temptibble<-tibble(poolgroup2=p,
                       mu0=mu0.temp, 
                       sd=sd.temp)
    necessarystats7<-dplyr::bind_rows(necessarystats7, temptibble)
    
    output.norm7[[p]]<-pwr.t.test(n=NULL, d=d.temp, 
                                  sig.level=a, power=0.8, type="one.sample", alternative="two.sided")
    
  }
  temp_results <- data.frame(do.call(rbind, output.norm7))
  temp_results$poolgroup2<-poolgroup2.list
  temp_results$alpha<-paste(a)
  t.test.detectchange7.p<-left_join(temp_results, necessarystats7, by='poolgroup2')
  powerrangers7.p<-rbind(powerrangers7.p, t.test.detectchange7.p)
  
}
powerrangers7.p<-powerrangers7.p %>% unnest(cols = c(n, d, sig.level, power, alternative, note, method, poolgroup2, alpha))
powerrangers7.pretty.p<-powerrangers7.p %>% dplyr::select(c("sig.level", "poolgroup2", "n"))
powerrangers7.pretty.p$nper1000ha<-powerrangers7.pretty.p$n/13000
powerrangers7.pretty.w.p<-powerrangers7.pretty.p %>% 
  pivot_wider(id_cols = poolgroup2,
              names_from = sig.level, values_from = c(n, nper1000ha), 
              names_prefix = "alpha=")
powerrangers7.pretty.w.p$current_n<-as.numeric(totalplanted.count$n)

changeandnbyalpha<-left_join(powerrangers6.pretty.p, powerrangers7.pretty.w.p, by="poolgroup2")
write.csv(changeandnbyalpha, "~/Desktop/Virginia Tech/Thesis/VT_thesis/Written_tables/changeandnbyalpha.planted.csv")

depth.list<-unique(kgperm2.planted_nona$depth_cm)
{
  output.norm8.p<-list()
  necessarystats8.p<-NULL
  depthsincluded<-NULL
  for(d in depth.list){
    #statistics for tests
    mu0.temp<-kgperm2.planted_nona %>% filter(poolgroup2=="Mineral Soil" & forest.management=="planted" & depth_cm==d) %>% summarize(mean(carbon_kgperm2)) %>% as.numeric()
    median.temp<-kgperm2.planted_nona %>% filter(poolgroup2=="Mineral Soil" & forest.management=="planted" & depth_cm==d) %>% summarize(median(carbon_kgperm2)) %>% as.numeric()
    sd.temp<-kgperm2.planted_nona %>% filter(poolgroup2=="Mineral Soil" & forest.management=="planted" & depth_cm==d) %>% summarize(sd(carbon_kgperm2)) %>% as.numeric()
    n.temp<-kgperm2.planted_nona %>% filter(poolgroup2=="Mineral Soil" & forest.management=="planted" & depth_cm==d) %>% nrow()
    # 
    temptibble<-tibble(depth_cm=d,
                       mu0=mu0.temp, 
                       sd=sd.temp, 
                       median=median.temp)
    necessarystats8.p<-dplyr::bind_rows(necessarystats8.p, temptibble)
    
    #tests
    if(n.temp>3){
      output.norm8.p[[d]]<-pwr.t.test(n=n.temp, d=NULL, sig.level=0.1, power=0.8, type="one.sample", alternative="two.sided")
      depthsincluded<-c(depthsincluded, d)
      }
  }
}
minsoil.detectchange.p<-data.frame(do.call(rbind,output.norm8.p))
minsoil.detectchange.p$depth_cm<-depthsincluded
minsoil.detectchange.p<-left_join(minsoil.detectchange.p, necessarystats8.p, by='depth_cm')
minsoil.detectchange.p$percentchange<-100*(as.numeric(minsoil.detectchange.p$d)*minsoil.detectchange.p$sd)/minsoil.detectchange.p$mu0
minsoil.detectchange.p<-tibble(depth_cm=minsoil.detectchange.p$depth_cm,
                                  sample_size=as.numeric(minsoil.detectchange.p$n),
                                  standard_deviation=minsoil.detectchange.p$sd,
                                  percentchange=minsoil.detectchange.p$percentchange,
                                  power="0.8",
                                  significance="0.1")
#### Student's t ####
library(stats)
for(p in poolgroup2.list){
  planted.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="planted")
  natural.temp<-kgperm2.planted_nona %>% filter(poolgroup2==p & forest.management=="natural")
  ifelse(nrow(natural.temp)>0, print(t.test(x=planted.temp$carbon_kgperm2, y=natural.temp$carbon_kgperm2), alternative = "two.sided"), print(c(paste(p), "has no natural data")))
}

