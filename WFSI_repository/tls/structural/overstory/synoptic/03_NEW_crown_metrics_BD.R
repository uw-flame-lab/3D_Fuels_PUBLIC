# Title: 03_ITC_CrownMetrics.R
#
# Objective: 
# Calculate crown metrics 
#
# Author(s): Hannah Redford: hredford@uw.edu
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# NOTE: run following scripts first:
# 00_classify_normalize.R
# 01_canopy_metrics_grid.R 
# 02_canopy_gaps.R
#
#install.packages("rgdal", repos="http://R-Forge.R-project.org")

# Required libraries/modules: 
require(pacman)

p_load(lidR,
       sf,  
       terra,
       dplyr, 
       mixtools, 
       e1071,
       splancs,
       rgdal) 

#site-specific parameters
chm_res= 1 # CHM grid cell size

# set max_cr_factor by site location... 
#max_cr_factor = 0.6 # Maximum value of a crown diameter given as a proportion of the tree height. Default is 0.6, meaning 60% of the tree height 
max_cr_factor = 0.4 # SE sites (Aucilla, blackwater, HH, THA, THB, FS, HitA, HitB, osceola, ttrs_hh)
#max_cr_factor = 0.35 # Western sites (glacial, Lubrecht, lanlF, lanlG, sycan_2a, sycan_forest, methow, tenalquot)

# Input folder path: 
base_folder = "D:/filter_rerun/TLS_synoptic/"

# Set current site: 
site = "aucilla"
list_of_sites = "aucilla"
#list_of_sites <- 
  c(#'aucilla',
    #'blackwater',
    'lubrecht',
    "glacial",
    "ft_stewart",
     'hitchiti_a',
     "hitchiti_b",
     'lanl_forest',
      'lanl_grass',
     'methow',
     "osceola",
     'sycan_2a',
     'sycan_forest',
      'sycan_grass',
     'tates_hell_a',
      'tates_hell_b',
    "tenalquot",
    'ttrs_hannah_hammock')
 
#loop file read-in
for (site in list_of_sites) {
  
  # Input folder path:
  input_folder = paste0(base_folder, site, "/01_basics/norm/")
  
  # Output folder path: 
  output_folder <- paste0(base_folder, site, "/04_crown_metrics/")
  
  dir.create(output_folder, recursive = TRUE)
   
  #list of laz files
  laz_files <- list.files(input_folder,"filtered.laz", full.names = T)
#  laz_files <- list.files(input_folder,".laz", full.names = T)  # brian test of non-filtered files
  lazfiles_names<-list.files(input_folder,"filtered.laz")
  
  for ( i in 1:length(laz_files)){
    print ( paste( i, "out of",length(laz_files) ))
    
    # Read in normalized filtered lidar file
    laz_norm_i <- lidR::readLAS(laz_files[i])
    
    ## BD test... do filter on norm files instead of _filtered.laz
    ## Nearest neighbor noise filter ## 
    #laz_norm_i <- TreeLS::nnFilter(laz_norm_i, d = 1, n=5)
    #will need to choose d (diameter), and n (number of points within diameter))
    
    # filter out points below 0 ## 
    #laz_norm_i = filter_poi(laz_norm_i, Z >= 0L)
    
    ###### Calculate Crown Metrics #####
    las = laz_norm_i
    
    ker <- matrix(1,3,3)
    vegrast<- grid_canopy(las, res = chm_res, pitfree(c(0,2,5,10,15), c(0, 1.5)))
    #plot(vegrast)
    vegrast <- raster::focal(vegrast, w = ker, fun = mean, na.rm = TRUE)
    
    ttops <- locate_trees(las, lmf(7, 1.3, shape = "circular"))
    ttops <- dplyr::filter(ttops, Z > 2L) #remove 'trees' under 2m
    
    veght2 <- segment_trees(las, silva2016(vegrast, 
                                           ttops, 
                                           max_cr_factor = max_cr_factor,
                                           ID = "treeID"))
    
    plot(veght2, color = "treeID")
    
    #removes 'trees' that have fewer than 20 laser returns
    tree_counts <- veght2@data %>%
      dplyr::group_by(treeID) %>%
      dplyr::tally() %>%
      dplyr::filter(n > 20) 
    tree_counts <- na.omit(tree_counts)
    filtered_las <- filter_poi(veght2, treeID %in% tree_counts$treeID)
    
    #export crown metrics into polygon shapefile and df
    metrics <- crown_metrics(
      filtered_las,
      .stdmetrics_z,
      geom = "convex",
      attribute = "treeID"
    )
    
    plot(metrics["zmax"], pal = hcl.colors)
    
    #create crown points shapefile 
    metrics_points = crown_metrics(
      filtered_las,
      .stdmetrics_z,
      geom = "point",
      attribute = "treeID")
    
    plot(metrics_points["zmax"], pal = hcl.colors, pch = 19)
    
    #------------------------- Function for remaining metrics ---------------------------------#
    
    calculate_CBH <- function(las) {
      # Check if the input is a valid LAS object
      if (!inherits(las, "LAS")) {
          stop("Input must be a LAS object.")
      }
    
      # Extract the point data from the LAS object
      point_data <- las@data
      
      # Ensure the required columns exist
      if (!all(c("X", "Y", "Z", "treeID") %in% names(point_data))) {
          stop("The LAS file must contain columns: X, Y, Z, and treeID.")
        }
    
      cbh_results <- point_data %>%
          group_by(treeID) %>%
          summarize(
        
        CBH = tryCatch({
                  # Use Gaussian Mixture Model (GMM) with 2 components
                  model <- mixtools::normalmixEM(x = Z, k = 2)
                  # Identify the Gaussian component with the higher mean
                  index.higher <- which.max(model$mu)
          
          # Calculate CBH using the higher mean and its associated standard deviation
          CBH <- model$mu[index.higher] - model$sigma[index.higher] * 1.5
          
        }, error = function(err) {
                  # If GMM fails, fallback to using 25th percentile of height points
                  return(quantile(Z, 0.25, na.rm = TRUE))
                }),
        
    # Fallback in case CBH is less than 2 meters
              CBH = ifelse(CBH < 2, max(Z, na.rm = TRUE) / 2, CBH),
        
    # Number of points for this tree
              n_points = n(),
        
    # Height metrics
              h_max = max(Z, na.rm = TRUE), 
              CL = h_max - CBH,
              CRatio = CBH / h_max,
              CDens = (sum(Z >= CBH, na.rm = TRUE) / n()) * 100,
              CA = tryCatch({
              hull_indices <- chull(X, Y)
              CA <- areapl(cbind(X[hull_indices], Y[hull_indices]))
                }, error = function(err) { return(NA) }),
              CRad = sqrt(CA / pi),
            )
    # Return the final CBH results as a dataframe
      return(cbh_results)
    }
    
    results = calculate_CBH(filtered_las)
    
    #calculate remaining metrics ####
    results$CRT<-results$CL/results$CBH
    results$CFI<-results$CL/(results$CRad*2)
    results$CTI<-(results$CRad*2)/results$CL
    results$CSR<-(results$CRad*2)/results$CBH
    
    #merge all metrics into one df
    final.metrics <- merge(results, metrics, by = 'treeID')
    #remove geometry
    final.metrics <- dplyr::select(final.metrics, -geometry)
    #add column for site name
    final.metrics$Site <- lazfiles_names[i]
    
    colnames(ttops)
    
    #setwd("C:/Users/dnemens/Documents/")
    #getwd()
    
    writeLAS(filtered_las, paste0(output_folder, gsub("_filtered", "_trees", lazfiles_names[i])))
    # this_name <- gsub("_filtered.laz", "_tree_tops.shp", lazfiles_names)
    this_name <- gsub("_filtered.laz", "_tree_tops.shp", lazfiles_names[i])  # BD correction... added [i]

    #this_name2 <- paste0(output_folder, site, "_crown_metrics_point.shp")

    
    
    # bd fix: added ", layer_options = "SHPT=POINTZ"" to following line to avoid error: 
#     Writing layer `tile_226400_3353350_tree_tops' to data source 
#   `D:/filter_rerun/TLS_synoptic/aucilla/04_crown_metrics/tile_226400_3353350_tree_tops.shp' using driver `ESRI Shapefile'
# Creating or updating layer tile_226400_3353350_tree_tops failed.
# Error in eval(ei, envir) : Write error.
# In addition: Warning messages:
# 1: There are 5644493 points flagged 'withheld'. 
# 2: In CPL_write_ogr(obj, dsn, layer, driver, as.character(dataset_options),  :
#   GDAL Error 6: Geometry type of `3D Point' not supported in shapefiles.  Type can be overridden with a layer creation option of SHPT=POINT/ARC/POLYGON/MULTIPOINT/POINTZ/ARCZ/POLYGONZ/MULTIPOINTZ/MULTIPATCH.

#    sf::st_write(ttops, dsn = paste0(output_folder, this_name), layer=this_name, driver="ESRI Shapefile", layer_options = "SHPT=POINTZ")

    
    
    sf::st_write(ttops, dsn = paste0(output_folder, this_name), layer=this_name, driver="ESRI Shapefile")
    sf::st_write(metrics_points,paste0(output_folder, site, "_crown_metrics_point.shp"), paste0(site, "_crown_metrics_point.shp"), driver ="ESRI Shapefile", append=T)
    
    #sf::st_write(as(ttops, "sf"), paste0(output_folder, paste0("treetop_",outname,"_tws_",tws,"_sws_",sws)), driver="ESRI Shapefile", append=FALSE) 
    # sf::st_write(as(ttops_metrics, "sf"), paste0(outdir, paste0("crown_metrics_point_",outname,"_tws_",tws,"_sws_",sws)), driver="ESRI Shapefile", append=FALSE) 
    # sf::st_write(as(crown_metrics, "sf"), paste0(outdir, paste0("crown_metrics_poly_",outname,"_tws_",tws,"_sws_",sws)), driver="ESRI Shapefile", append=FALSE) 
    
    # writeOGR(metrics, (paste0(output_folder, gsub("_filtered.laz", "_crown_metrics_poly.shp", lazfiles_names))), drive="ESRI Shapefile", overwrite=TRUE)
    writeOGR(metrics, (paste0(output_folder, gsub("_filtered.laz", "_crown_metrics_poly.shp", lazfiles_names[i]))), drive="ESRI Shapefile", overwrite=TRUE)

    #write.csv(final.metrics, paste0(output_folder, gsub("_filtered.laz", "_crown_metrics.csv", lazfiles_names[i])), row.names = F)
  }
}