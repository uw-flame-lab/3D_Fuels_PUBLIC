# Title: 1a_BiomassDataPrep.R

# Objective:
# This script inputs raw biomass data from field collection and prepares a dataframe with summed biomass values for each
# clip plot in two categories: summed biomass in the 0-10cm stratum, and summed biomass in the 10-100cm strata 
# these values are summarized by clip plot for each Site to be used in subsequent analyses (see numbered scripts in this folder)
#
# Authors: Gina Cova, Deborah Nemens
# Date: May 2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# Change input folder to the filepath for the raw biomass data file downloaded onto your machine 
# Create an output folder for prepared biomass csv -- "csv inputs"

# Required libraries:
library(tidyverse)

# Input Path:
wd <- "C:/Users/dnemens/Box/UW FLAME Lab/PROJECTS/3D FUELS/_Common_files/global_inputs/"
biomass <- read.csv(paste0(wd, "/field/Biomass_Raw_AllSites.csv"))
#
# Output folder path:
output <- paste0(wd, "/field/")

# Additional information: 
# 
#####
#correct Site names for later merging
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

# create dataframe of 0 - 100cm strata biomass
biomass0to100 <- biomass %>%
  group_by(Site, ClipPlot, Visit) %>%
  summarise(biomass0to100 = sum(netWt_g)) %>%
  as.data.frame()

#calculate biomass for 10-100 strata only
biomass10to100 <- biomass %>% 
            filter(!stratumHt %in% "0-10 cm") %>%
            dplyr::group_by(Site, ClipPlot, Visit) %>%
            summarize(biomass_10to100cm = sum(netWt_g)) %>% 
            as.data.frame()

# merge together
biomass1 <- left_join(biomass0to100, biomass10to100, by = c("Site", "ClipPlot", "Visit"))

#replace NA's with 0 for plots that did not have biomass above 10 cm
biomass1$biomass_10to100cm[is.na(biomass1$biomass_10to100cm)] <- 0

write.csv(biomass1, paste0(output, "prepped_biomass.csv"), row.names = F)
