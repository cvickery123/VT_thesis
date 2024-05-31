library(readr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(patchwork)
#remotes::install_github("ncx-co/tidyFIA")
library(tidyFIA)

####LOADING DATA IN####
#International Soil Carbon Network
ISCN_layers <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/ISCN.loblollylayers.csv")
ISCN_profiles <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/ISCN.loblollyprofiles.csv")

#Pinemap
pinemap_tier2 <- read_excel("~/Desktop/Virginia Tech/Thesis/Data/pinemap_tier2.xlsx")
pinemap_sc<- read_excel("~/Desktop/Virginia Tech/Thesis/Data/pinemap_tier2.xlsx", 
                          sheet = "soil carbon")

#Soil Respiration Database
srbd_lob <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/srbd_lob.csv")

#Forest Inventory and Analysis
FIA_COND_PITA <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/FIA_COND_PITA.csv")
FIA_SOILSLAB_PITA <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/FIA_SOILSLAB_PITA.csv")
FIA_CWD_PITA <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/FIA_CWD_PITA.csv")
FIA_TREE_PITA <- read_csv("~/Desktop/Virginia Tech/Thesis/Data/FIA_TREE_PITA.csv")

git test

#testing commit
####DATA FORMATTING####
#select columns of interest
#pivot
#rename columns and add identifying column for database
#bind
