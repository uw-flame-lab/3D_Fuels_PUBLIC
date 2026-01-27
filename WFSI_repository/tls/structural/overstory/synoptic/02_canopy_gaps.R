# Title: 02_canopy_gaps.R
#
# Objective: 
# Calculate canopy gaps
#
# Author(s): Carlos Alberto Silva, Ben Bright
# Date: Oct 2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# NOTE: run 00_classify_normalize.r and 01_canopy_metrics_grid.R first 
# this script will read files from  D:\temp\<site>\outputs\tls\syn_metrics\canopy_metrics\01_basics\chm\
# script will create a folders in each site subfolder
# D:\temp\<site>\outputs\tls\syn_metrics\canopy_metrics\03_forest_gaps
#
# Required libraries/modules: 
#
#install.packages("pacman")
require(pacman) # load package to manage the R packages
#p_load(lidR, terra, sf, RCSF, rgeos, ForestGapR, raster, rgdal)
p_load(lidR, terra, sf, RCSF, ForestGapR, raster, rgdal)


# Input folder path: 
input_folder = "D:/filter_rerun/TLS_synoptic/"

# Output folder path: 
# output_folder: set below based on input_folder. Example: D:/temp/<site>/


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
#                                         Calculating canopy gaps
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

#loop file read-in
for (site in list_of_sites) {
  
	# 02 - setting work directory
	output_folder <- paste0(input_folder, site) 
  
	# 02a - set/create directories
#	chm.dir = paste0(output_folder,"/outputs/tls/syn_metrics/canopy_metrics/01_basics/chm/") 
#	gap.dir = paste0(output_folder,"/outputs/tls/syn_metrics/canopy_metrics/03_forest_gaps/")
	chm.dir = paste0(output_folder,"/01_basics/chm/") 
	gap.dir = paste0(output_folder,"/03_forest_gaps/")
	dir.create(gap.dir, recursive = TRUE)

	# 03 - list of laz file (change PRE/POST if needed)
	chmfiles_names<-list.files(chm.dir,"_chm.tiff$")  # this gets only the .tiff files
	#chmfiles_names<-list.files(chm.dir,"_POST_chm.tiff$")  # this gets only the .tiff files
	
	# 04 - Setting height thresholds (e.g. 10 meters) and gap size 
	threshold<-10
	size<-c(1,100000) # setting minimum and maximum gap sizes (m2)

	# 06 - gap detection
	for ( i in 1:length(chmfiles_names)){
		print ( paste( i, "out of",length(chmfiles_names) ))
		# 6.1 Reading raw lidar data
		chm_i <- raster(paste0(chm.dir, "\\",chmfiles_names[i]))

		# 6.2 Detecting forest gaps
		gaps_i<-getForestGaps(chm_layer=chm_i, threshold=threshold, size=size)

		# 6.3 Plotting gaps
		#windows()
		#plot(chm_i,col=viridis(10))
		#plot(gaps_i, col="red", add=TRUE, main="Forest Canopy Gap", legend=FALSE)

		# 6.4 Computing basic statistics of forest gap
		gaps_stats<-GapStats(gap_layer=gaps_i, chm_layer=chm_i)
		write.table(gaps_stats,paste0(gap.dir, gsub("_chm.tiff","_gaps.csv",chmfiles_names[i])), row.names=F, sep=",")
		#write.table(gaps_stats,paste0(gap.dir, gsub("_POST_chm.tiff","_gaps_POST.csv",chmfiles_names[i])), row.names=F, sep=",")
		
		# 6.5 Converting raster layer to SpatialPolygonsDataFrame
		gaps_spdf<-GapSPDF(gap_layer=gaps_i)
		#plot(gaps_spdf, col="red", add=TRUE, main="Forest Canopy Gap", legend=FALSE)

		# 6.6 exporting gaps as shapefiles
		#writeOGR(gaps_spdf, paste0(gap.dir, gsub("_chm.tiff","_gaps.shp",chmfiles_names[i])), driver= "ESRI Shapefile", layer=1 ,overwrite=TRUE)
		sf::st_write(as(gaps_spdf, "sf"), paste0(gap.dir, gsub("_chm.tiff","_gaps.shp",chmfiles_names[i])), driver="ESRI Shapefile", append=FALSE) 
		#sf::st_write(as(gaps_spdf, "sf"), paste0(gap.dir, gsub("POST_chm.tiff","_gaps_POST.shp",chmfiles_names[i])), driver="ESRI Shapefile", append=FALSE) 
	}  
}
