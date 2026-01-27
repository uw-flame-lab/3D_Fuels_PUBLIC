# Title: 3_FieldStructualMetricsPrep.R

# Objective:
# This script inputs occupied voxel data from field ocular surveys and calculates field structural metrics 
# for all voxel plots to be used in subsequent analyses (see numbered scripts in this folder)
#
# Authors: Gina Cova, Deborah Nemens
# Date: May 2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
## first run script 2_FuelTyping.R
# Change input folder to the filepath for the occupied voxel data file downloaded onto your machine 
# run script
# Create an output folder for csv of field ocular survey metrics
#
# 
# Required libraries:
library(data.table)
library(purrr)
library(tidyverse)

# Input Folder Path:
input <-  "C:/Users/dnemens/Box/UW FLAME Lab/PROJECTS/3D FUELS/_Common_files/global_inputs/field/"

# Output folder path:
output <- input

######
#read in saved csv
survey <- read.csv(paste0(input, "Field.Occupied.Voxels.byStratum_AllSites.csv"), header=T)

#remove non-destructive plots (no biomass data)
survey <- survey %>% filter(destructiveYN == "Y")

occvol <- survey %>% dplyr::select(Site, ClipPlot, Visit, Voxel, Occupied) %>%
  filter(Occupied == 1) %>%
  as.data.frame()

#field fuel height calculation
#add xyz columns to data frame
xyz <- read.csv(paste0(input, "voxel.height.schema.csv"), header=T)

# create factor for stratum height by voxel 
#create factor for replacement
height <- occvol$Vox
row_count <- nrow(xyz)

for (i in 1:row_count) {
  search_keyword <- xyz[i,1]
  replace_keyword <- xyz[i,2]
  
  height <- sapply(height, function(x){
    x[x==search_keyword] <- replace_keyword
    return(x)
  })
}
#create column in occvol dataframe and insert height
occvol$z <- height

#create factor for x coordinate for voxel location
#create factor for replacement
voxelx <- occvol$Vox

for (i in 1:row_count) {
  search_keyword <- xyz[i,1]
  replace_keyword <- xyz[i,3]
  
  voxelx <- sapply(voxelx, function(x){
    x[x==search_keyword] <- replace_keyword
    return(x)
  })
}
occvol$X <- voxelx

#create factor for y coordinate for voxel location
#create factor for replacement
voxely <- occvol$Vox

for (i in 1:row_count) {
  search_keyword <- xyz[i,1]
  replace_keyword <- xyz[i,4]
  
  voxely <- sapply(voxely, function(x){
    x[x==search_keyword] <- replace_keyword
    return(x)
  })
}
occvol$Y <- voxely

### calculate fuel height max and mean ####
# calculate occ volume and max ht per column of voxels
fuelheightprep <- occvol %>% group_by(Site, ClipPlot, Visit, X, Y) %>%
  summarize(FieldHt = max(z), fieldOv = sum(Occupied)/250) %>%
  as.data.frame()

fuelheight <- fuelheightprep %>% group_by(Site, ClipPlot, Visit) %>%
  summarize(FieldHtMax = max(FieldHt), FieldHeight_avg = (sum(FieldHt)/25), fieldOv = sum(fieldOv)) %>%
  as.data.frame()

### Merge in fuel type for each plot ####
#read in fuel typing - upper strata (by occ vol above 10), all strata (by occvol whole plot), and 0-10 (by biomass) df's
typed.uppervol <- read.csv(paste0(input, "Fuel_Type_OccupiedVol_above10cm.csv"), header = T)
typed.wholevol <- read.csv(paste0(input, "Fuel_Type_OccupiedVol_wholeplot.csv"), header = T)
typed.biomass <- read.csv(paste0(input, "Fuel_Type_Biomass_under10cm.csv"), header = T)

## object richness - count number of fuel types per plot for *each* typing method
typed.richness.10 <- typed.uppervol %>%
  rowwise() %>%
  mutate(Occ10_objectrichness = sum(c_across(Per_Shrub_Occ_10:Per_TimberLitter_Occ_10)!=0))
typed.richness.whole <- typed.wholevol %>%
  rowwise() %>%
  mutate(OccWP_objectrichness = sum(c_across(Per_Shrub_Occ_WP:Per_TimberLitter_Occ_WP)!=0))
typed.richness.biomass <- typed.biomass %>%
  rowwise() %>%
  mutate(Bio_objectrichness = sum(c_across(Per_Herb_Bio:Per_TimberLitter_Bio)>0))

##############

 #combine each fuel typing and richness into single df
 typed.rich <- merge(typed.richness.10, typed.richness.whole, by = c("Site", "ClipPlot", "Visit"))
 typed_all <- typed.rich %>% dplyr::select(Site, ClipPlot, Visit, "DominantFuel_Occ_10", "DominantFuel_Occ_WP", Occ10_objectrichness, OccWP_objectrichness) %>%
   mutate(DominantFuel_Biomass = NA, Bio_objectrichness = NA)
 typed_bio <- typed.richness.biomass %>% dplyr::select(Site, ClipPlot, Visit, DominantFuel_Biomass, Bio_objectrichness) %>%
   mutate(DominantFuel_Occ_10 = NA, DominantFuel_Occ_WP = NA, Occ10_objectrichness=NA, OccWP_objectrichness=NA)
 typed_all <- rbind(typed_all, typed_bio)

# ### Merge fuel typing and richness with height
 ht.type.richness <- merge(fuelheight, typed_all, by = c("Site", "ClipPlot", "Visit"))
count(ht.type.richness, Site)


write.csv(ht.type.richness, file = paste0(output, "FieldStructualMetrics.csv"), row.names = F)




