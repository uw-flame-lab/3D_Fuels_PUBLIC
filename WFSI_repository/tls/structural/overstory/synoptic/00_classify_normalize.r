# Title: 00_classify_normalize.r
#
# Objective: 
# Classify and normalize TLS Synoptic data using LAStools, also create a digital terrain model
#
# Author(s): Ben Bright
# Date: Oct 2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# put tiles in input folder for each site
# set list_of_sites to the site you want to process
# or uncomment the list to process all sites

# Example input folder: aucilla/data/tls/synoptic/tiles
# Example output folder: aucilla/outputs/tls/syn_metrics/canopy_metrics/01_basics

# script will create folders in each site:
# <site>\outputs\tls\syn_metrics\canopy_metrics\01_basics\class
# <site>\outputs\tls\syn_metrics\canopy_metrics\01_basics\dtm
# <site>\outputs\tls\syn_metrics\canopy_metrics\01_basics\norm
#
# Required libraries/modules: 
# LAStools

# How to create tiles (example for Aucilla):
# copy files aucilla/data/tls/synoptic/scans/ from Box to local machine
# run lastile.exe, 50m with 5m buffer, to D:\tiles
# delete tiles smaller than 1000K
# run lasground_new.exe (wilderness, fine, compute height, replace z), output to D:\tiles_lasground_new_fine
# tiles_lasground_new_fine folders to aucilla/data/tls/synoptic/tiles
# delete local folders

# Input folder path (change this): 
setwd("D:/filter_rerun/TLS_synoptic/")
input_folder = "D:/filter_rerun/TLS_synoptic/"

# Output folder path: 
# output_folder: based on input_folder.


# Additional information: 
# assumes LAStools is installed to C:\LAStools\

#loop file read-in

# set to single site, or uncomment list to do multiple sites
list_of_sites <- c('aucilla')
# c('aucilla',
#   'blackwater',
#   'lubrecht',
#   "glacial",
#   "tenalquot",
#   "ft_stewart",
#   'hitchiti_a',
#   "hitchiti_b",
#   'lanl_forest',
#   'lanl_grass',
#   'methow',
#   "osceola",
#   'sycan_2a',
#   'sycan_forest',
#   'sycan_grass',
#   'tates_hell_a',
#   'tates_hell_b',
#   'ttrs_hannah_hammock')

for (site in list_of_sites) {
  
  print (site)
  
#  laz_folder <- paste0(input_folder, site, "/data/tls/synoptic/tiles/") 
  laz_folder <- paste0(input_folder, site, "/01_basics/norm/") 
#  output_folder <- paste0(input_folder, site, "/outputs/tls/syn_metrics/canopy_metrics") 
  output_folder <- paste0(input_folder, site)
  # Create directories following Carlos' convention
  class.dir = paste0(output_folder,"/01_basics/class/")
  dtm.dir = paste0(output_folder,"/01_basics/dtm/")
#  norm.dir = paste0(output_folder,"/01_basics/norm/")
  dir.create(class.dir, recursive = TRUE)
  dir.create(dtm.dir, recursive = TRUE)
#  dir.create(norm.dir, recursive = TRUE)
  
  # Classify returns as ground or nonground (might need to shorten laz_folder path)
#  print(paste0("C:/LAStools/bin/lasground_new -i ", laz_folder, "*.laz -wilderness -olaz -odir ", class.dir))
#  system(paste0("C:/LAStools/bin/lasground_new -i ", laz_folder, "*.laz -wilderness -olaz -odir ", class.dir))
  
  # Create a digital terrain model (overwrites same dtm.tif for each file... no need to do this)
  # system(paste0("C:/LAStools/bin/blast2dem -i ", class.dir, "*.laz -o ", dtm.dir, "dtm.tif"))
  
  # Normalize to heights above ground
#  print(paste0("C:/LAStools/bin/lasheight -i ", class.dir, "*.laz -replace_z -olaz -odir ", norm.dir))
#  system(paste0("C:/LAStools/bin/lasheight -i ", class.dir, "*.laz -replace_z -olaz -odir ", norm.dir))
#  print('break point here if not doing all sites')
#  print('will get Error: file not found otherwise.')
}

