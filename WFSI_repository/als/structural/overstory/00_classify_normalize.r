# Title: 00_classify_normalize.r
#
# Objective: 
# Classify and normalize ALS data using LAStools, also create a digital terrain model
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

# Example input folder: aucilla/data/als/tiles
# Example output folder: aucilla/outputs/als/01_basics

# script will create folders in each site:
# <site>\outputs\als\01_basics\class
# <site>\outputs\als\01_basics\dtm
# <site>\outputs\als\01_basics\norm
#
# Required libraries/modules: 
# LAStools

# Input folder path (change this): 
#setwd("D:/temp/")
#input_folder = "C:/Users/dnemens/Documents/temp_3DF_ALS/"
input_folder = "D:/filter_rerun/ALS/"
site = 'aucilla'
# Output folder path: 
# output_folder: paste0("C:/Users/dnemens/Documents/temp_3DF_ALS/", site)


# Additional information: 
# assumes LAStools is installed to C:\LAStools\

#loop file read-in

# set to single site, or uncomment list to do multiple sites

list_of_sites <- 
 c('aucilla'
   #,
  #'blackwater',
  #'lubrecht')
  # "glacial",
  # "tenalquot",
  # "ft_stewart",
  # 'hitchiti_a',
  # "hitchiti_b",
  # 'lanl_forest',
  # 'lanl_grass',
  # 'methow',
  # "osceola",
  # 'sycan_2a',
  # 'sycan_forest',
  # 'sycan_grass',
  # 'tates_hell_a',
  # 'tates_hell_b',
  # 'ttrs_hannah_hammock'
)

for (site in list_of_sites) {
  
  print (site)
  
  laz_folder <- paste0(input_folder, site, "/tiles/") 
  output_folder <- paste0(input_folder, site, "/01_basics/") 
  
  # Create directories following Carlos' convention
  class.dir = paste0(output_folder,"/class/")
  dtm.dir = paste0(output_folder,"/dtm/")
  norm.dir = paste0(output_folder,"/norm/")
  dir.create(class.dir, recursive = TRUE)
  dir.create(dtm.dir, recursive = TRUE)
  #dir.create(norm.dir, recursive = TRUE)
  
  # Classify returns as ground or nonground
  system(paste0("C:/LAStools/bin/lasground_new -i ", laz_folder, "*.laz -wilderness -olaz -odir ", class.dir))
  
  # Create a digital terrain model
  #system(paste0("C:/LAStools/bin/blast2dem -i ", class.dir, "*.laz -o ", dtm.dir, "dtm.tif"))
  
  # Normalize to heights above ground
  print(paste0("C:/LAStools/bin/lasheight -i ", class.dir, "*.laz -replace_z -olaz -odir ", norm.dir))
  system(paste0("C:/LAStools/bin/lasheight -i ", class.dir, "*.laz -replace_z -olaz -odir ", norm.dir))
#  print('break point here if not doing all sites')
#  print('will get Error: file not found otherwise.')
}

