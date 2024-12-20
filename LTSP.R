library(readr)
library(tidyverse)
library(ggplot2)

loblollyLTSPloc <- read_csv("loblollyLTSPobs/loblollyLTSPloc.csv")


LTSPobs <- read_csv("loblollyLTSPobs/loblollyLTSPobs.csv")
#loblolly only because all sites except LA sites were not planted with loblolly before LTSP start year
loblollyLTSPobs<-LTSPobs %>% filter(installation_code %in% c('MS1', 'MS2', 'MS3', 'NC1', 'NC2', 'NC3', 'TX1', 'TX2', 'TX3')) %>% filter(installation_year>0)

loblollyLTSPobs$ff_c_gperm2<-loblollyLTSPobs$biomass_stock *loblollyLTSPobs$ff_carbon
loblollyLTSPobs %>% filter(!is.na(ff_c_gperm2)) %>% summarize(min(ff_c_gperm2), 
                                                              max(ff_c_gperm2))
loblollyLTSPobs$carbon_kgperm2<-loblollyLTSPobs$ff_c_gperm2/1000
ltsp.ff.nona<-loblollyLTSPobs %>% filter(!is.na(carbon_kgperm2))

loblollyLTSPobs %>% filter(!is.na(bd_bulk) &
                             !is.na(soil_carbon_total)) %>% count()
#soil depths are 0-10, 10-20, 20-30 cm
loblollyLTSPobs %>% filter(!is.na(bd_bulk) &
                             !is.na(som_loi)) %>% count()
loblollyLTSPobs %>% filter(!is.na(bd_bulk) &
                             !is.na(som_other)) %>% count()
loblollyLTSPobs %>% filter(!is.na(bd_bulk) &
                             !is.na(som_WalkleyBlack)) %>% count()

ltsp.TSC.nona<-loblollyLTSPobs %>% filter(!is.na(bd_bulk) &
                             !is.na(soil_carbon_total))
ltsp.TSC.nona$thickness<-ltsp.TSC.nona$soil_layer_bottom-ltsp.TSC.nona$soil_layer_top
ltsp.TSC.nona$tsc_gpercm2<-(ltsp.TSC.nona$soil_carbon_total/100)*ltsp.TSC.nona$bd_bulk*ltsp.TSC.nona$thickness
ltsp.TSC.nona %>% summarize(mean(tsc_gpercm2), 
                            min(tsc_gpercm2), 
                            max(tsc_gpercm2))
ltsp.TSC.nona$tsc_kgperm2<-ltsp.TSC.nona$tsc_gpercm2 * (100*100)/1000
ltsp.TSC.nona %>% summarize(mean(tsc_kgperm2), 
                            min(tsc_kgperm2), 
                            max(tsc_kgperm2))

ggplot(ltsp.TSC.nona) +
  geom_histogram(aes(x=tsc_kgperm2), bins=30)

ggplot(ltsp.ff.nona) +
  geom_histogram(aes(x=carbon_kgperm2), bins=30)

write_csv(ltsp.TSC.nona, "LTSP.lob.csv")
write_csv(ltsp.ff.nona, "LTSP.lob.ff.csv")
