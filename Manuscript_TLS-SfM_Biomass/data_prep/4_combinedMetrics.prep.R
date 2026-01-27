# title: 4_combinedMetrics.prep.R

# Objective: 
# This script prepares a combined dataframe of TLS point cloud metrics, SfM point cloud metrics, field survey variables, 
# dominant fuel types, and biomass values for use in analyses
# 
#
# Author(s): Deborah Nemens
# Date: 7/29/2025
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script: 
# 1st run these scripts: 1_BiomassDataPrep.r;  1b_TLSPointCloudProcessing.r, 1c_SFMPointCloudProcessing.r, 
# 2_FuelTyping.R, and 3_FieldStructualMetricsPrep.R 
# set file paths for local input file locations

# Required libraries: 
library(tidyverse)

# Input folder path: 
input <- "C:/Users/dnemens/Box/UW FLAME Lab/PROJECTS/3D FUELS/_Common_files/global_inputs/"

# Output folder path: 
output <- paste0(input, "/biomass_PC_comp/")

# csv inputs
sfm <- read.csv(paste0(input,"photogram/WholePlotStructuralMetrics_SFM.csv"), header = T)
tls <- read.csv(paste0(input, "/tls/WholePlotStructuralMetrics.csv"), header = T)
fielddata <- read.csv(paste0(input, "field/FieldStructualMetrics.csv"))
biomass   <- read.csv(paste0(input, "field/prepped_biomass.csv"))
cwd <- read.csv(paste0(input, "/field/Field.Occupied.Voxels.byStratum_AllSites.csv"), header = T)

######
# prep for joining into large dataframe
# remove non-destructive plots
tls <- tls %>% filter (destructiveYN == "Y")

#remove unneeded columns and rename remaining cols
tls <- tls[, c(1:3, 11:17)]
colnames(tls) <- c("Site","ClipPlot","Visit","tlsmaxheight", "tlsavheight","tlsov", "tlsPAD", "tlsPADV", "tlsSA", "tlshullvol")
sfm <- sfm[, c(1:3, 11:17)]
colnames(sfm) <- c("Site","ClipPlot","Visit","sfmmaxheight", "sfmavheight", "sfmov", "sfmPAD", "sfmPADV", "sfmSA", "sfmhullvol")

#combine tls & sfm datasheets
pc.data <- full_join(sfm, tls, join_by(Site, ClipPlot, Visit))

#check on sites missing at least one metric type
miss <- anti_join(sfm, tls) 

pc.data$ClipPlot <- gsub("_", "-", pc.data$ClipPlot)

count(biomass, Site)

# merge biomass with sfm variables 
data <- left_join(biomass, pc.data)

#######
# attribute sites with cwd

cwd <- cwd %>% dplyr::group_by(Site, ClipPlot, Visit) %>%
  summarize(totalcwd = sum(Fuel1000hr)) %>% as.data.frame()
head(cwd)

cwd$cwd <- ifelse(cwd$totalcwd > 0, "cwd", "no")
cwd <- dplyr::select(cwd, !"totalcwd")
#create cols to match data
cwd$destructiveYN <- "Y"

#merge with "data" df
data.cwd <- left_join(data, cwd)

head(data.cwd)

#outlier removal
data.cwd <- data.cwd %>% filter(cwd %in% "no")

#########
# bring in field structural metrics

#merge with rest of data
data.cwd.field <- left_join(data.cwd[,1:19], fielddata[,1:8])
# put field metrics into the same units as other metrics
data.cwd.field$FieldHtMax <- data.cwd.field$FieldHtMax/100
data.cwd.field$FieldHeight_avg <- data.cwd.field$FieldHeight_avg/100

count(data.cwd.field, Site)

#combine Sycan Forest I and II (these share 1 plot name, so renaming plots for Sycan Forest II)
data.cwd.field$Site <- data.cwd.field$Site %>% str_replace("Sycan Forest I", "Sycan Forest")
data.cwd.field$Site <- data.cwd.field$Site %>% str_replace("Sycan ForestI", "Sycan Forest")

#create and fill in column for region/veg type category based on Site
data.cwd.field  <- data.cwd.field  %>%
  mutate(SiteType = case_when(
    Site %in% c('Lubrecht','Methow','LANL Forest','Sycan Forest','Sycan Forest') ~ "Western pine",
    Site %in% c("Aucilla",'Blackwater','Tates Hell A','Tates Hell B',"Fort Stewart", "Osceola") ~ "SE flatwood",
    Site %in% c('Hitchiti A', "Hitchiti B", 'Hannah Hammock') ~ "SE Loblolly-sweetgum",
    Site %in% c('LANL Grassland',"Glacial Heritage","Tenalquot",'Sycan Grassland') ~ "Western grassland" ))

#save combined dataframe
write.csv(data.cwd.field, paste0(output, "combined_Metrics.csv"), row.names = F)
