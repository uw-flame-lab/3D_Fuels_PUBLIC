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
# copy .laz files from D:\temp\<site>\data\tls\plots\5x5\biomass\norm to D:\temp\<site>\outputs\tls\5x5_metrics\canopy_metrics\01_basics\norm
# (this skips step 00_classify_normalize.R, which is not needed for this script)
#
# this script will read files from  D:\temp\<site>\outputs\tls\5x5_metrics\canopy_metrics\01_basics\norm\
# script will create a folders in each site subfolder
# D:\temp\<site>\outputs\tls\5x5_metrics\canopy_metrics\01_basics\chm
# D:\temp\<site>\outputs\tls\5x5_metrics\canopy_metrics\02_canopy_metrics
#
# Required libraries/modules: 
#
install.packages("pacman")
require(pacman)                         # load package to manage the R packages
p_load(lidR, terra, sf, RCSF, viridis)


# Input folder path: 
input_folder = "D:/filter_rerun/5x5/"

# Output folder path: 
# output_folder: based on input_folder. Example: D:/temp/<site>/outputs/tls/5x5_metrics/canopy_metrics/


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
list_of_sites <- c('aucilla')
# list_of_sites <-
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
	# 02 - setting work directory
  #output_folder <- paste0(input_folder, site, "/outputs/tls/5x5_metrics/canopy_metrics") 
  output_folder <- paste0(input_folder, site)
  
	# 02a - set/create directories
#	norm.dir = paste0(input_folder, site, "/data/tls/plots/5x5/biomass/norm")
	norm.dir = paste0(input_folder, site, "/01_basics/norm")
	#	norm.dir = paste0(input_folder, site, "/norm/")
	chm.dir = paste0(output_folder,"/01_basics/chm/")
	gm.dir = paste0(output_folder,"/02_canopy_metrics/")
	dir.create(chm.dir, recursive = TRUE)
	dir.create(gm.dir, recursive = TRUE)

	# 03 - list of normalized laz files
	lazfiles_names<-list.files(norm.dir,".laz")


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

	# 06 Defining spatial resolution
	res_grid=5

	# 05 - laz processing
	for ( i in 1:length(lazfiles_names)){

	  print ( paste( i, "out of",length(lazfiles_names) ))
  
	  # 4.1 Reading raw lidar data
	  #laz_i <- lidR::readLAS(paste0(laz_folder, "\\",lazfiles_names[i]))
	  #plot(laz_i)
	  # 4.2 Classifying noise points by Statistical Outliers Removal (SOR)
	  #laz_i <- classify_noise(laz_i, algorithm=sor(k = 10, m = 3, quantile = FALSE))
  
	  # 4.3 Filtering out the noise points (Class 18)
	  #laz_i <- filter_poi(laz_i, Classification != 18)
  
	  # 4.4 Ground point filtering using Cloth Simulation Filter (CSF, see Zhang et al. (2016))
	  #laz_i <- classify_ground(laz_i, csf(cloth_resolution=2, rigidness=3L))
  
	  # 4.5 Interpolating ground points into a DTM using TIN
	  #dtm <- rasterize_terrain(laz_i, res=1, algorithm = knnidw(k = 6L, p = 2))
	  #plot_dtm3d(dtm, bg = "white") 
  
	  # 4.6 Exporting dtm
	  #terra::writeRaster(dtm,paste0(output_folder,"\\01_basics\\",gsub(".laz","_dtm.tiff",lazfiles_names[i])),overwrite=TRUE)

	  # 4.7 Normalization to height aboveground
	  #las_norm <- normalize_height(laz_i, algorithm=dtm)
	  #writeLAS(las_norm,paste0(output_folder,"\\01_basics\\",gsub(".laz","_norm.laz",lazfiles_names[i])))
  
	  # 4.8 Canopy height model
	  las_norm <- lidR::readLAS(paste0(norm.dir, "\\",lazfiles_names[i])) # Read in normalized las file

	  ## adding noise filter based on HannahR's advice (1/10/2025)
	  ## next two code lines are copied from als version of 01_canopy_metrics_grid.R
	  
	  ## Nearest neighbor noise filter ## 
	  las_norm <- TreeLS::nnFilter(las_norm, d = 1, n=5)
	  #will need to choose d (diameter), and n (number of points within diameter))
	  
	  # filter out points below 0 ## 
	  las_norm = filter_poi(las_norm, Z >= 0L)
	  
	  ## end of code copied from als version of 01_canopy_metrics_grid.R
	  
	  chm_i <- rasterize_canopy(las_norm, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5)))
	  #windows()
	  #plot(chm_i, col=inferno(10))
  
	  terra::writeRaster(chm_i,paste0(chm.dir, gsub(".laz","_chm.tiff",lazfiles_names[i])), overwrite=T)
  
	  # 4.9 computing grid metrics
	  canopy_metrics_grid = lidR::grid_metrics(las_norm, func = ~f_metrics(Z,ReturnNumber),res_grid)
	  #windows()
	  #plot(canopy_metrics_grid, col=viridis(10))

	  # 4.10 exporting grid metrics 
	  terra::writeRaster(chm_i,paste0(gm.dir, gsub(".laz","_gridmetrics.tiff",lazfiles_names[i])), overwrite=T)

	}  
}
