# Title: 3a_FuelTyping.R

# Objective:
# This script assigns dominant fuel types for each plot based on upper strata (by occupied volume above 10cm), 
# all strata (by occupied volume of the whole plot), 
# and 0-10 strata (by biomass)  
#
# Authors: Deborah Nemens 
# Date: 11.14.2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# First run these scripts:
# 1_BiomassDataPrep.R
# 2_PointCloudProcessing.R
# Change input folder to the filepath for the occupied voxel data file downloaded onto your machine 
# run script
# Create an output folder for csv 
#
#
# Required libraries:
library(tidyverse)
#
# Input Folder Path:
input <-  "C:/Users/dnemens/Box/UW FLAME Lab/PROJECTS/3D FUELS/_Common_files/global_inputs/field/"

# Output folder path:
output <- input

#### DATA PREP #### 
#import file of field occupied voxel surveys
survey <- read.csv(paste0(input, "/occupiedVoxels_ALL.csv"))

#remove 100+ stratum
survey <- survey %>%
  filter(StratumHeight != "100-110 cm") %>%
  mutate(Occupied = 1-Empty)

#clean up ClipPlot names 
survey$ClipPlot <- survey$ClipPlot %>% str_replace_all(pattern=c("V"= "", "G"="", "F"=""))

#clean up site names to match lidar
survey$Site <- survey$Site %>% str_replace_all(pattern=c("Sycan Marsh F"="Sycan Forest I", 
                                                         "Sycan Marsh G" ="Sycan Grassland",
                                                         "Sycan Marsh 2A" = 'Sycan Forest II',
                                                         "Hanna Hammock" = "Hannah Hammock",
                                                         "Methow Wildlife Area" = "Methow"))
#save to Box as csv
write.csv(survey, file= paste0(output, "Field.Occupied.Voxels.byStratum_AllSites.csv"), row.names = F)

#####FUEL TYPING BY OCCUPIED VOLUME -- all plots treated the same way ######

#### Group fuel elements into Fuel Types #####
occvol <- survey %>%
  group_by(Site, Plot, ClipPlot, Visit, StratumHeight, Voxel) %>%
  mutate(Shrub = sum(WoodyLive_D,WoodyLive_E,WoodyLive_OD,WoodyLive_OE,WoodyLive_OM,WoodyLive_EM,WoodyLive_DM, WoodyLive_M, StandingDead, ConiferLiveSeedSap,Vines)) %>%
  mutate(Herb = sum(c(Grass_W,Grass_B,Grass_WB,OtherGram,Forbs))) %>%
  mutate(TimberLitter = sum(WoodyLitter_D,WoodyLitter_E,WoodyLitter_OD,WoodyLitter_OE,WoodyLitter_OM,WoodyLitter_EM,WoodyLitter_DM,WoodyLitter_M,
                            Fuel1hr,Fuel10hr,Fuel100hr,Fuel1000hr,Cones,ConiferLitter,Needles_L,Needles_S,Needles_LS,Other))

#remove unneeded columns
occvol <- occvol[,c(1, 3:6,42, 49:51)]

#make all occupied voxels = 1 for calculation of % occupied by fuel type
occvol$Shrub[occvol$Shrub > 0] <- 1
occvol$Herb[occvol$Herb > 0] <- 1
occvol$TimberLitter[occvol$TimberLitter > 0] <- 1

#remove unoccupied voxels
occvol <- occvol %>% filter(Empty == 0)

###create df of % occupied volume for each coarse Fuel Type by plot for whole plot (0-100cm)
#math is number of voxels occupied by fuelx/total # of *occupied* voxels (not all 250)
occvol.byplot.0 <- occvol %>% dplyr::group_by(Site, ClipPlot, Visit) %>%
  dplyr::summarize(OccVol_Total = n(),
                   Shrub = sum(Shrub)/OccVol_Total ,
                   Herb = sum(Herb)/OccVol_Total,
                   Timber_Litter = sum(TimberLitter)/OccVol_Total) %>% 
                    as.data.frame()

###create df of % occupied volume for each coarse Fuel Type by plot for upper strata (10-100cm)
#math is number of voxels occupied by fuelX/total # of *occupied* voxels (not all 225)
occvol.byplot.10 <- occvol %>% filter(!StratumHeight %in% "0-10 cm") %>%
  dplyr::group_by(Site, ClipPlot, Visit) %>%
  dplyr::summarize(OccVol_Total = n(),
                   Shrub = sum(Shrub)/OccVol_Total ,
                   Herb = sum(Herb)/OccVol_Total,
                   Timber_Litter = sum(TimberLitter)/OccVol_Total) %>% 
                  as.data.frame()

#remove na's - these are caused by plots that have no occupied voxels above 10 cm
#occvol.byplot.10 <- na.omit(occvol.byplot.10) ## or keep na's?

### calculate dominant fuel type for whole plot 
occvol.byplot.0$DominantFuel <- (colnames(occvol.byplot.0[,c(5:7)])
                                  [max.col(occvol.byplot.0[,c(5:7)],
                                           ties.method="first")])

###calculate dominant fuel type for 10-100 cm
occvol.byplot.10$DominantFuel <- (colnames(occvol.byplot.10[,c(5:7)])
                                    [max.col(occvol.byplot.10[,c(5:7)],
                                             ties.method="first")])

#####################################
#Fuel Typing by Occupied volume for plots that have material above 10cm
#####################################

#subset plots with biomass above 10cm for fuel typing by Occupied Volume

biomass <- read.csv(paste0(input, "prepped_biomass.csv"))

### select plots that have biomass above 10cm #### 
biomass0.10 <- biomass %>% 
   filter(biomass_10to100cm > 0) 
biomass.top10 <- biomass %>% 
  filter(biomass_10to100cm == 0) 

#use list of plots to filter occupancy-based fuel type list, keep only plots that have biomass > 10cm
occvol.Dom0.10plus <- semi_join(occvol.byplot.0, biomass0.10, join_by(Site, ClipPlot, Visit)) 
occvol.Dom10.10plus <- semi_join(occvol.byplot.10, biomass0.10, join_by(Site, ClipPlot, Visit)) 

#standardize column names
occvol.Dom0.10plus <- occvol.Dom0.10plus %>% rename("Per_Shrub_Occ_WP"="Shrub", "Per_TimberLitter_Occ_WP"="Timber_Litter", "Per_Herb_Occ_WP"="Herb", DominantFuel_Occ_WP = DominantFuel)
occvol.Dom10.10plus <- occvol.Dom10.10plus %>% rename("Per_Shrub_Occ_10"="Shrub", "Per_TimberLitter_Occ_10"="Timber_Litter", "Per_Herb_Occ_10"="Herb", DominantFuel_Occ_10 = DominantFuel)


#for plots that are typed based on 10 cm + strata only
#make all plots with >33% occupied shrub voxels shrub-dominated, overriding % occvol fuel typing
occ.vol.10plus <- occvol.Dom10.10plus %>%
 mutate(DominantFuel_Occ_10 = dplyr::case_when (Per_Shrub_Occ_10 > 0.33 ~ "Shrub", 
                                  .default = DominantFuel_Occ_10))

count(occ.vol.10plus, DominantFuel_Occ_10)

#save occ vol typed based on 10 cm + strata only
write.csv(occ.vol.10plus, paste0(output, "Fuel_Type_OccupiedVol_above10cm.csv"), row.names = F)

#save occ vol typed by whole plot
write.csv(occvol.Dom0.10plus, paste0(output, "Fuel_Type_OccupiedVol_wholeplot.csv"), row.names = F)

#################
#Fuel typing by BIOMASS for plots with material in 0-10 only ####
#################
#### DATA PREP #######
# identical to prep script in 1_BiomassDataPrep
#####
#correct Site names for later merging
# #read in summed by stratum biomass data for subsetting
biomass <- read.csv(paste0(input, "/Biomass_Raw_AllSites.csv"), header = T)

biomass$unitName <- biomass$unitName %>% str_replace_all(pattern = c("Sycan Marsh F"="Sycan Forest I", 
                                                                     "Sycan Marsh G" ="Sycan Grassland",
                                                                     "Sycan Marsh 2A" = 'Sycan Forest II',
                                                                     "Hanna Hammock" = "Hannah Hammock",
                                                                     "Methow Wildlife Area" = "Methow"))
#correct plot names for later merging
biomass$clipplotName  <-  biomass$clipplotName %>% str_replace_all(pattern=c("F"="", "V"="", "G"=""))
#remove strata above 100cm
biomass <-  biomass %>% filter (!stratumHt %in% "100-110 cm")

##REMOVE DUFF (fuel element 16), missing elements###
biomass <- biomass %>%
  rename(Site = unitName, ClipPlot = clipplotName, Visit = visitType) %>%
  filter (netWt_g>0, !fuelElementID %in% 16) 
biomass$stratumID <- as.numeric(biomass$stratumID)

#sum net.wts of fuel elements
biomass.less10 <- biomass %>%
  group_by(Site, ClipPlot, Visit, fuelElement) %>%
  summarise(netWt_g = sum(netWt_g)) 

biomass.less10$fuelElement <- as.factor(biomass.less10$fuelElement)
levels(biomass.less10$fuelElement)

#combine fuel elements into coarse fuel types
biomass.less10 <- biomass.less10 %>%
  mutate(fuelType = case_when(
    fuelElement %in%  c("1-hr fuel","10-hr fuel","100-hr fuel", "1000-hr+ fuel","Acorns/acorn caps",
                        "Conifer litter (bark flakes, pollen cones, other needles)", "Moss/lichen","Pine cone",
                        "Pine needles","Woody leaves/litter","Pupal Case","Scat","Spanish moss","Gatorback","Mushroom") ~ "Timber_Litter" , 
    fuelElement %in% c("Vines (and vine litter)", "Woody live shrubs/trees", "Woody standing dead shrubs/trees including conifers","Conifer live seedling/saplings") ~ "Shrub", 
    fuelElement %in% c("Wiregrass/bunchgrass", "Other graminoids", "Forbs (and forb litter)") ~ "Herb"))

#sum net.wts of coarse types
biomass.wtSum <- biomass.less10 %>%
  group_by(Site, ClipPlot, Visit, fuelType) %>%
  summarise(netWt_g = sum(netWt_g))

biomass.less10.wide <- pivot_wider(biomass.wtSum, names_from = fuelType, values_from = netWt_g)
biomass.less10.wide <- biomass.less10.wide[,c(1:6)]
biomass.less10.wide <- biomass.less10.wide %>% replace(is.na(.), 0) %>% as.data.frame()

#use net wt to calculate dominant fuel type
biomass.less10.wide$DominantFuel <- (colnames(biomass.less10.wide[,c(4:6)])
                        [max.col(biomass.less10.wide[,c(4:6)],
                                 ties.method="first")])

biomass.less10.wide$DominantFuel <- as.factor(biomass.less10.wide$DominantFuel)

#use list of plots to filter occupancy-based fuel type list, keep only plots that have biomass < 10cm
biomass.less10.wide <- left_join(biomass.top10, biomass.less10.wide) 

#change total biomass to % biomass per fuel type
biomass.percent <- biomass.less10.wide %>%
  mutate(Biomass_Total = sum(Herb, Shrub, Timber_Litter)) %>%
  transmute(Site=Site, ClipPlot=ClipPlot, Visit=Visit, Per_Herb_Bio = Herb/Biomass_Total, Per_Shrub_Bio=Shrub/Biomass_Total, 
            Per_TimberLitter_Bio=Timber_Litter/Biomass_Total, DominantFuel_Biomass = DominantFuel) 


write.csv(biomass.percent, paste0(output, "Fuel_Type_Biomass_under10cm.csv"), row.names = F)

##########################
#look at herb-dominated plots####

FuelType.herb <- FuelType %>% filter(DominantFuel == "Herb") %>% as.data.frame()

count(FuelType.herb, Site)

# Site  n
# 1               Aucilla 33
# 2            Blackwater  2
# 3          Fort Stewart 15
# 4      Glacial Heritage 40
# 5        Hannah Hammock 10
# 6            Hitchiti A  3
# 7            Hitchiti B 12
# 8           LANL Forest 11
# 9        LANL Grassland 30
# 10             Lubrecht 19
# 11 Methow Wildlife Area  8
# 12              Osceola  3
# 13         Sycan Forest  5
# 14      Sycan Grassland 22
# 15       Sycan Marsh 2A  3
# 16         Tates Hell B  3
# 17            Tenalquot 36

################################################
# #####Fuel Typing categorization using P. Eagle fuel typing csv ####
# #D.Nemens 11/8/2023
# 
# library(data.table)
# library(purrr)
# library(tidyverse)
# 
# #import Paige Eagle's csv of dominant fuel by clip plot
# survey <- read.csv("U:/3D Fuels/Analysis/Dominant Fuel Elements_PE.csv")
# 
# colnames(survey)
# 
# survey <- survey %>%
#   rename("ClipPlot"= "clipplotName") %>%
#   rename("Site"= "unitName") %>%
#   rename("Visit"= "visit") 
# 
# count(survey, Site)
# 
# ##### group fuel elements by simplified fuel type #####
# count(survey, fuelElement)
# 
# occvol <- survey %>%
#   mutate(fuelType.10.PE = case_when(
#          fuelElement %in%  c("1-hr fuel","1000-hr+ fuel", "Woody leaves/litter", "Pine needles") ~ "Timber_Litter" , 
#          fuelElement %in% c("Woody live shrubs/trees", "Conifer live seedling/saplings", "Vines (and vine litter)", "Woody standing dead shrubs/trees including conifers") ~ "Shrub", 
#          fuelElement %in% c("Wiregrass/bunchgrass", "Other graminoids", "Forbs (and forb litter)") ~ "Herb"))
# 
# occvol <- occvol %>% dplyr::select(Site, ClipPlot, Visit, fuelType.10.PE)
# 
# #save csv of fuel typed plots
# write.csv(occvol, "C:/Users/dnemens/Box/External 3D Fuels/analysis/TLS.PC_Metrics/csv inputs/Fuel_typing.csv", row.names = F)
# 
# 
