# Title: 01_canopy_metrics_grid.R
#
# Objective: 
# Calculate grid metrics using the lidR package
#
# Author(s): Carlos Alberto Silva, Ben Bright
# Date: Oct 2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# NOTE: run 00_classify_normalize_als.r first 
# this script will read files from  D:\temp\<site>\outputs\als\01_basics\norm\
# script will create a folders in each site subfolder
# D:\temp\<site>\outputs\als\01_basics\chm
# D:\temp\<site>\outputs\als\02_canopy_metrics
#
# Required libraries/modules: 
#
install.packages("pacman")
library(lidR)
library(terra)
library(stringr)
library(TreeLS)
#devtools::install_github('tiagodc/TreeLS', force = T)

site = 'blackwater'

# Input folder path: 
input_folder = "C:/Users/dnemens/Documents/temp_3DF_ALS/"

# Output folder path: 
output_folder= paste0("C:/Users/dnemens/Documents/temp_3DF_ALS/", site)

# Additional information: 

###--------------------------------------------------------------------------------------------------------###
#----------------------------------------------------------------------------------------------------------###
#Carlos Alberto Silva, Ph.D.  
#Assistant Professor of Quantitative Forest Science  
#Forest Biometrics and Remote Sensing Lab (Silva Lab)  
#School of Forest, Fisheries, and Geomatics Sciences (SFFGS)  
#Institute of Food & Agricultural Sciences (IFAS)  
#University of Florida (UF)  
#1745 McCarty Drive / 138 Newins-Ziegler Hall  
#PO Box 110410 Gainesville, FL 32611-0410 
#Office: +1 (352) 294-6885 
#c.silva@ufl.edu 
#Silva lab: https://carlos-alberto-silva.github.io/silvalab/home.html 
#----------------------------------------------------------------------------------------------------------###
#----------------------------------------------------------------------------------------------------------###

#----------------------------------------------------------------------------------------------------------###
#                                         Calculating grid metrics using the lidR package
#----------------------------------------------------------------------------------------------------------###


# set to a single site or a list of sites... 
list_of_sites <- "hitchiti_b"
#list_of_sites <-
c(#'aucilla',
  #'blackwater',
  #'lubrecht',
  #"glacial",
  #"ft_stewart",
 # 'hitchiti_a',
 # "hitchiti_b",
 # 'lanl_forest',
#  'lanl_grass',
 # 'methow',
  # "osceola",
 # 'sycan_2a',
 # 'sycan_forest',
 #  'sycan_grass',
 # 'tates_hell_a',
 #  'tates_hell_b',
  "tenalquot",
  'ttrs_hannah_hammock')

for (site in list_of_sites) {
  
  	print (site)
	# 02 - setting work directory
	output_folder <- paste0(input_folder, list_of_sites[i]) 

	# 02a - set/create directories
	norm.dir = paste0(output_folder,"/01_basics/norm/")
	chm.dir = paste0(output_folder,"/01_basics/chm/")
	gm.dir = paste0(output_folder,"/02_canopy_metrics/")
	# dir.create(norm.dir[i], recursive = T)
	# dir.create(chm.dir[i], recursive = TRUE)
	# dir.create(gm.dir[i], recursive = TRUE)

	# 03 - list of normalized laz files
	lazfiles_names<-list.files(norm.dir,".laz")
  laz_file_paths <- list.files(norm.dir,".laz", full.names = T)

	# 04 Canopy metrics function
	f_metrics <- function(Z,n) {  
	  #Zcov
	  Zcov = length(Z[Z>=2 & n==1])/length(Z[n==1])
	  #quantil 98
	  p98 = quantile(Z,0.98)
  
	  #making a list for return of functions
	  list_metrics = list(
		H98TH = p98, # canopy height
		COV = Zcov,  # canopy cover
		Hmean = mean(Z), # mean height
		HSD = sd(Z) # standard deviation of height
	  )
	  return(list_metrics)
	}

	#  Defining spatial resolution
	res_grid=5

	#  laz processing ## 
	for ( i in 1:length(lazfiles_names)){

	  print ( paste( i, "out of",length(lazfiles_names) ))
  
	  # Reading normalized lidar data
	  laz_i <- lidR::readLAS(laz_file_paths[i])
	  #plot(laz_i)
	  
	  ## Nearest neighbor noise filter ## 
	  laz_filter <- TreeLS::nnFilter(laz_i, d = 1, n=5)
	  #will need to choose d (diameter), and n (number of points within diameter))
	  
	  # filter out points below 0 ## 
	  laz_filter = filter_poi(laz_filter, Z >= 0L)
	  
	  # save filtered point cloud ## 
	  writeLAS(laz_filter,paste0(output_folder,"//01_basics//norm//", gsub("_norm.laz","_filtered.laz",lazfiles_names[i])))
	  
	  #  Canopy height model
	  chm_i <- rasterize_canopy(laz_filter, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5)))
	  #windows()
	  #plot(chm_i, col=inferno(10))
  
	  terra::writeRaster(chm_i, filename=paste0(chm.dir, gsub(".laz","_chm.tiff",lazfiles_names[i])), overwrite=T)
	  
	  # computing grid metrics
	  canopy_metrics_grid = lidR::grid_metrics(laz_filter, func = ~f_metrics(Z,ReturnNumber),res_grid)

	  #  exporting grid metrics 
	  terra::writeRaster(canopy_metrics_grid,paste0(gm.dir, gsub(".laz","_gridmetrics.tiff",lazfiles_names[i])), overwrite=T)

	}  
}
