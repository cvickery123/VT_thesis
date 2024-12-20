library(readr)
library(tidyverse)

####LAYER DATA####
#ISCN Layer data
ISCNLayer <- read_excel("Data/ISCNData.xlsx", sheet = "Layer")
ISCNLayer<-ISCNLayer[ ,-1] #remove first column that has no data in it
colnames(ISCNLayer)<-c('dataset_name_sub', 'dataset_name_soc', 'lat', 'long', 'datum', #rename columns with prettier names
                       'site_name', 'observation_date', 'profile_name', 'layer_name', 
                       'layer_top_cm', 'layer_bot_cm', 'hzn','hzn_desgn','vegclass_local',
                       'bd_method', 'bd_samp_g_cm3', 'bd_tot_g_cm3', 'bd_whole_g_cm3', 'bd_other_g_cm3',
                       'c_method', 'c_tot_percent', 'oc_percent', 'soc_g_cm2','soc_carbon_flag','soc_method')
vegtotals<-ISCNLayer %>% group_by(vegclass_local) %>% count() 
view(vegtotals) #see what veg covers are included in the dataset 

####PROFILE DATA####
#ISCN Profile data
ISCNData <- read_excel("Data/ISCNData.xlsx")
ISCNData<-ISCNData[ ,-1] #remove first column that has no data in it
colnames(ISCNData)<-c('dataset_name_sub', 'dataset_name_soc', 'lat', 'long', 'datum', #rename columns with prettier names
                      'state', 'observation_date', 'site_name', 'profile_name', 
                      'layer_top_cm', 'layer_bot_cm', 'total_lcount', 'carbon_lcount', 'soc_lcount',
                      'soc_depth_cm', 'soc_g_cm2', 'soc_method', 'soc_spatial_flag', 'soc_carbon_flag', 
                      'vegclass_local', 'surface_veg')

vegtotals<-ISCNData %>% group_by(vegclass_local) %>% count() 
view(vegtotals) #see what veg covers are included in the dataset 
ISCNData %>% group_by(surface_veg) %>% count() %>% view() #see what veg covers are included in the dataset (but different column)

####Subsetting data for veg of interest####
pinelist<-c("PITA", "Pinus taeda") #FILL WITH YOUR VEGETATION OF INTEREST - MUST MATCH OUTPUT IN THE vegtotals$vegclass_local OR surface_veg COLUMN
emptylist<-list()
for (w in pinelist){
  emptylist[[w]]<-grep(get("w"), ISCNData$surface_veg) %>% print() #if you are looking for veg in the surface_veg column
  #if you are looking for veg in the vegclass_local column, change ISCNData$surface_veg to ISCNData$vegclass_local in the line above
}

emptylist.df<-tibble(emptylist) #list from the standard loop above that has row numbers connected to each term
for (i in 1:length(emptylist.df)) { 
  row_indices <- unlist(emptylist.df[, i])
  row_indices.list<-as.numeric(row_indices)
  pinerows <- ISCNData[row_indices.list, ] #selecting the rows with your veg of interest
}


#using profiles identified as veg of interest to subset layers with same profile identifiers
LLprofiles<-as.list(pinerows$profile_name)

emptylist2<-list()
for (w in LLprofiles){
  #select row numbers for the layers within the profiles that you have identified as veg of interest
  emptylist2[[w]]<-pinecol.iscn<-grep(get("w"), ISCNLayer$profile_name) %>% print() 
}
str(emptylist2)
emptylist2.df<-tibble(emptylist2) #list from the standard loop above that has row numbers connected to each term
for (i in 1:length(emptylist2.df)) { 
  row_indices2 <- unlist(emptylist2.df[, i])
  row_indices2.list<-as.numeric(row_indices2)
  LLlayers <- ISCNLayer[row_indices2.list, ] #selecting the layers with your veg of interest
}