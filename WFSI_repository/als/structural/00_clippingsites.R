# Title: clippingsites.R
#
# Objective: 
# use shapefiles of 3D Fuels sites to clip out needed lidar point clouds from als acquisitions
#
# Author(s): Deborah Nemens, Bryce BG
# Date: October 2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# put site .laz files in C:/data, grouped by site
#
# Required libraries: 
library(terra)
library(lidR)
library(sf)
library(tidyverse)

# Input folder path: 
v = vect("C:/Users/dnemens/Box/External 3D Fuels/_Common_files/spatial/3dFuels_Site_Extents.shp")
indata = "C:/Users/dnemens/Box/External 3D Fuels/"

# Output folder path: indata, "/", site, "/data/als/clipped_area/"


list_of_sites =  c('aucilla', 'blackwater', 'lubrecht', "glacial", "tenalquot", "hitchiti_a", 'lanl_forest', 'lanl_grass', 'methow', 'sycan_2a', 'sycan_forest', 'sycan_grass', 'tates_hell_a', 'tates_hell_b', 'ttrs_hannah_hammock')
#aucilla = 16
for(i in 1:nrow(v)) {
  print(i)
  thisv = v[i,]
  thissite = tolower(thisv$Site)
  site = which(list_of_sites == thissite)
  site = list_of_sites[site]
  if(!length(site)) {
    if (length(grep("bwsf_l1", thissite)))
      site = "blackwater"
    else if(length(grep("bwsf_l3", thissite)))
      site = "blackwater"
    else if(length(grep("bwsf_l4", thissite)))
      site = "blackwater"
    else if (thissite == "tates_a")
      site = "tates_hell_a"
    else if (thissite == "tates_b")
      site = "tates_hell_b"
    else if (thissite == "hannah")
      site = "ttrs_hannah_hammock"
    else if(thissite %in% c("Ft_Stewart_A", "Osceola", "Hitchiti_B"))
      next
    else
      stop("Site not found")
  }
  f = paste0(indata, "/", site, "/als/")
  .f = list.files(f)
  w = which(.f == "transformed")
    if(!length(w))
    w = which(.f == "raw_sub")
  if(!length(w))
    w = which(.f == "raw")
  f = list.files(f, full.names = T)[w]
  cat = readLAScatalog(f)
  thisv = project(thisv, crs(cat))
  clip = clip_polygon(cat, geom(thisv)[,3], geom(thisv)[,4])
  writeLAS(clip, paste0(indata, "/", site, "/data/als/clipped_area/", thissite, "_clipped.laz"))
} 


########
# for left out sites
######
list_of_sites = c("ft_stewart", "osceola", "hitchiti_b")

for(i in 1:nrow(v)) {
  print(i)
  thisv = v[i,]
  thissite = tolower(thisv$Site)
  site = which(list_of_sites == thissite)
  if(!length(site)) {
    if(length(grep("ft_stewart_a", thissite)))
        site = "ft_stewart"
    else if(thissite %in% c('aucilla', "bwsf_l1", "bwsf_l3","bwsf_l4", 'lubrecht', "glacial", "tenalquot", "hitchiti_a", 'lanl_forest', 'lanl_grass', 'methow', 'sycan_2a', 'sycan_forest', 'sycan_grass', 'tates_a', 'tates_b', 'hannah'))
        next
    else
      stop("Site not found")
  }
  f = paste0(indata, "/", site, "/data/als/")
  .f = list.files(f)
  w = which(.f == "transformed")
  if(!length(w))
    w = which(.f == "raw_sub")
  if(!length(w))
    w = which(.f == "raw")
  f = list.files(f, full.names = T)[w]
  cat = readLAScatalog(f)
  crs <- st_crs(26917)
  lidR::projection(cat) <- crs
  thisv = project(thisv, crs(cat))
  clip = clip_polygon(cat, geom(thisv)[,3], geom(thisv)[,4])
  writeLAS(clip, paste0(indata, "/", site, "/data/als/clipped_area/", thissite, "_clipped.laz"))
}