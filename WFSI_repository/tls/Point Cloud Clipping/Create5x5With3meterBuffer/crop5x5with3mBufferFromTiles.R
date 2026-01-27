library(lidR)
library(sf)
library(dplyr)


clip5x5with3mBuffer <- function(laspath, outpath, site, numbers, csv_pp, site_crs) {
  
  csvpath = paste0('C:/Users/bdrye/Box/UW FLAME Lab/PROJECTS/3D FUELS/', site, '/data/spatial/poles')
  
  csvFiles = list.files(csvpath, pattern = "\\.csv$")
  
  if (!dir.exists(outpath)) {
    dir.create(outpath, recursive = TRUE)
    dir.create(file.path(outpath, "norm"), recursive = TRUE)
    dir.create(file.path(outpath, "shapefiles"), recursive = TRUE)
    dir.create(file.path(outpath, "textfiles"), recursive = TRUE)
  }
  
  #loop through numbers
  for (n in numbers)
  {
    csvFileName <- paste0(csv_pp, n, "_5x5.csv")
    if((site == "lanl_forest" || site == "lanl_grass" || site == "sycan_2a" || site == "sycan_grass" || site == "sycan_forest" || site == "tates_hell_a" || site == "tates_hell_b") && csv_pp == "_ScanOnly") {
      csvFileName <- paste0(n, csv_pp, "_5x5.csv")
    }
    
    #laspath <- "path/to/las_directory"  # Replace with your LAS directory
    
    # Read the CSV file
    csvInfo <- read.csv(file.path(csvpath, csvFileName), sep = ",")
    print(csvInfo)
    
    # Extract coordinates for each corner
    NE_X <- csvInfo %>% filter(PlotName == "NE", PlotType == "Pole") %>% pull(X)
    NE_Y <- csvInfo %>% filter(PlotName == "NE", PlotType == "Pole") %>% pull(Y)
    NW_X <- csvInfo %>% filter(PlotName == "NW", PlotType == "Pole") %>% pull(X)
    NW_Y <- csvInfo %>% filter(PlotName == "NW", PlotType == "Pole") %>% pull(Y)
    SW_X <- csvInfo %>% filter(PlotName == "SW", PlotType == "Pole") %>% pull(X)
    SW_Y <- csvInfo %>% filter(PlotName == "SW", PlotType == "Pole") %>% pull(Y)
    SE_X <- csvInfo %>% filter(PlotName == "SE", PlotType == "Pole") %>% pull(X)
    SE_Y <- csvInfo %>% filter(PlotName == "SE", PlotType == "Pole") %>% pull(Y)
    
    # Check for missing data
    if (length(NE_X) == 0 || length(NE_Y) == 0 || 
        length(NW_X) == 0 || length(NW_Y) == 0 || 
        length(SW_X) == 0 || length(SW_Y) == 0 || 
        length(SE_X) == 0 || length(SE_Y) == 0) {
      stop("Error: CSV file is missing a pole. Edit CSV: change Clip to Pole")
    }
    
    # Write coordinates to a text file
    corners_file <- file.path(outpath, "textfiles", paste0(csv_pp, n, "_corners.txt"))
    if((site == "lanl_forest" || site == "lanl_grass" || site == "sycan_2a" || site == "sycan_grass" || site == "sycan_forest" || site == "tates_hell_a" || site == "tates_hell_b") && csv_pp == "_ScanOnly") {
      corners_file <- file.path(outpath, "textfiles", paste0(n, csv_pp, "_corners.txt"))
    }
    writeLines(c(
      paste(NE_X[1] + 3, NE_Y[1] + 3),
      paste(NW_X[1] - 3, NW_Y[1] + 3),
      paste(SW_X[1] - 3, SW_Y[1] - 3),
      paste(SE_X[1] + 3, SE_Y[1] - 3),
      paste(NE_X[1] + 3, NE_Y[1] + 3)  # Close the polygon
    ), corners_file)
    
    cat("Corners written to:", corners_file, "\n")
    
    # Create a polygon and save as a shapefile
    polygon_coords <- matrix(c(
      NE_X[1] + 3, NE_Y[1] + 3,
      NW_X[1] - 3, NW_Y[1] + 3,
      SW_X[1] - 3, SW_Y[1] - 3,
      SE_X[1] + 3, SE_Y[1] - 3,
      NE_X[1] + 3, NE_Y[1] + 3  # Close the polygon
    ), ncol = 2, byrow = TRUE)
    
    polygon <- st_polygon(list(polygon_coords)) %>%
      st_sfc(crs = site_crs)
    
    laz_files <- list.files(path = laspath, pattern = "\\.laz$", full.names = TRUE)
    catalog <- readLAScatalog(laspath)
    
    polygon <- st_polygon(list(polygon_coords)) %>%
      st_sfc(crs = st_crs(catalog))  # Match the CRS of the catalog
    
    shapefile_path <- file.path(outpath, "shapefiles", paste0(csv_pp, n, "_polygon.shp"))
    if((site == "lanl_forest" || site == "lanl_grass" || site == "sycan_2a" || site == "sycan_grass" || site == "sycan_forest" || site == "tates_hell_a" || site == "tates_hell_b") && csv_pp == "_ScanOnly") {
      shapefile_path <- file.path(outpath, "shapefiles", paste0(n, csv_pp, "_polygon.shp"))
    }
    st_write(polygon, shapefile_path, delete_layer = TRUE)
    
    cat("Polygon shapefile saved to:", shapefile_path, "\n")
    #polygon <- st_read(paste0(laspath, "/polygon.shp"))  # Replace with your polygon file
    
    plot(catalog, main = "LAS Catalog with Polygon Overlay")
    plot(polygon, add = TRUE, border = "red", lwd = 2)
    
    catalog_filtered <- catalog_intersect(catalog, polygon)
    
    las <- readLAS(catalog_filtered)
    las_clipped <- clip_roi(las, polygon)
    outfilename = paste0(csv_pp, n, "_5x5_3m.laz")
    if((site == "lanl_forest" || site == "lanl_grass" || site == "sycan_2a" || site == "sycan_grass" || site == "sycan_forest" || site == "tates_hell_a" || site == "tates_hell_b") && csv_pp == "_ScanOnly") {
      outfilename = paste0(n, csv_pp, "_5x5_3m.laz")
    }
    writeLAS(las_clipped, file.path(outpath, "norm", outfilename))
    cat("Filtered and clipped LAS data saved successfully!")
  }
}

# site = 'aucilla'
# #numbers = [4,9,12,13,16,17,19,23,24] # aucilla CP
# #numbers = [2,3,5,6,10,14,15,20,21] # aucilla SO
# 
# site_crs = 26917 # utm zone 17
# laspath = "D:/5x5/aucilla_laz"
# outpath = "D:/5x5/aucilla"
# numbers = c(4,9,12,13,16,17,19,23,24)
# csv_pp <- "CP_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(2,3,5,6,10,14,15,20,21)
# csv_pp <- "SO_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# site = 'blackwater'
# site_crs = 26916 # this may not matter, gets replaced by catalog CRS
# laspath = "D:/5x5/bw_laz"
# outpath = "D:/5x5/bw"
# 
# # blackwater L1
# numbers = c(6,14)
# csv_pp <- "L1_CP_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# # blackwater L3
# numbers = c(2,13)
# csv_pp <- "L3_CP_"  
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# # blackwater L4
# numbers = c(8,20)
# csv_pp <- "L4_CP_"  
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# site = 'ft_stewart'
# # numbers = [1,9,20,23] #fort stewart PostFire_SO (no csv files, skipped)
# # numbers = [3,5,8,13,15,17,19,21,25] #fort stewart Pre_Dest_CP (no csv for 17,19,21,25)
# # numbers = [3,5,8,13,15] #fort stewart Pre_Dest_CP (no csv for 17,19,21,25 -- skipped these)
# # numbers = [6,7,10,12,14,16,18,22,24] #fort stewart Pre_NonDest_CP (no csv for 6)
# # numbers = [7,10,12,14,16,18,22,24] #fort stewart Pre_NonDest_CP (no csv for 6)
# # numbers = [1,9,20,23] #fort stewart SO (no csvs)
# site_crs = 26917 # utm zone 17
# laspath = "D:/5x5/ft_stewart_laz"
# outpath = "D:/5x5/ft_stewart"
# 
# numbers = c(3,5,8,13,15)
# csv_pp <- "Pre_Dest_CP_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(7,10,12,14,16,18,22,24)
# csv_pp <- "Pre_NonDest_CP_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'glacial'
# site_crs = 26910 # utm zone 10
# laspath = "D:/5x5/glacial_laz"
# outpath = "D:/5x5/glacial"
# 
# #numbers = [5,6,9,10,14,16,17,19,20] # glacial CP
# #numbers = [4,13,18,21] # glacial SO
# 
# numbers = c(5,6,9,10,14,16,17,19,20)
# csv_pp <- "CP_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(4,13,18,21)
# csv_pp <- "SO_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)


# site = 'hitchiti_a'
# #numbers = [1,3,5,9,12,14,15,17,20] 
# #numbers = [7,11,23] # SO
# site_crs = 26917 # utm zone 17
# laspath = "D:/5x5/hitchiti_a_laz"
# outpath = "D:/5x5/hitchiti_a"
# numbers = c(1,3,5,9,12,14,15,17,20)
# csv_pp <- "CP_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(7,11,23)
# csv_pp <- "SO_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'hitchiti_b'
# site_crs = 26917 # utm zone 17
# laspath = "D:/5x5/hitchiti_b_laz"
# outpath = "D:/5x5/hitchiti_b"
# numbers = c(1,3,7,8,17,19,23,24,25)
# csv_pp <- "CP_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(10,12,20,21)
# csv_pp <- "SO_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)


# site = 'lanl_forest'
# #numbers = [2,6,9,10,14,15,18,20] # CP
# #numbers = [3,12,16,17] # SO
# site_crs = 26913 # utm zone 13
# laspath = "D:/5x5/lanl_f_laz"
# outpath = "D:/5x5/lanl_f"
# numbers = c(2,6,9,10,14,15,18,20)
# csv_pp <- ""
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(3,12,16,17)
# csv_pp <- "_ScanOnly"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'lanl_grass'
# #numbers = [1,6,8,10,14,15,17,20,22] # CP
# #numbers = [5,12,21,23] # SO
# site_crs = 26913 # utm zone 13
# laspath = "D:/5x5/lanl_g_laz"
# outpath = "D:/5x5/lanl_g"
# numbers = c(1,6,8,10,14,15,17,20,22)
# csv_pp <- ""
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(5,12,21,23)
# csv_pp <- "_ScanOnly"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'lubrecht'
# #numbers = [1,2,3,4,5,6,7,8,9] # V
# #numbers = [1,2,3,4,5,6,7,8,9] # S
# site_crs = 26912 # utm zone 12
# laspath = "D:/5x5/lubrecht_laz"
# outpath = "D:/5x5/lubrecht"
# numbers = c(1,2,3,4,5,6,7,8,9)
# csv_pp <- "V"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(1,2,3,4,5,6,7,8,9)
# csv_pp <- "S"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'methow'
# #numbers = [4,6,7,8,13,14,15,18,19] # CP
# #numbers = [12,17,22,24] # SO
# site_crs = 26910 # utm zone 10
# laspath = "D:/5x5/methow_laz"
# outpath = "D:/5x5/methow"
# numbers = c(4,6,7,8,13,14,15,18,19)
# csv_pp <- "CP_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(12,17,22,24)
# csv_pp <- "SO_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# osceola (skipped)

# site = 'sycan_forest'
# # numbers = [1,8,11,15,16,18,19,22,23] # '' 
# # numbers = [2,6,9,13,14,17,20,24,25] # SO
# # numbers = [1,2,9,11,13,16,17,18,19,22,23,24] # Post_Dest
# site_crs = 26910 # utm zone 10
# laspath = "D:/5x5/sycan_forest_laz"
# outpath = "D:/5x5/sycan_forest"
# numbers = c(1,8,11,15,16,18,19,22,23)
# csv_pp <- ""
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# numbers = c(2,6,9,13,14,17,20,24,25)
# csv_pp <- "_ScanOnly"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# numbers = c(1,2,9,11,13,16,17,18,19,22,23,24)
# csv_pp <- "Post_Dest_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'sycan_2a'
# # numbers = [2,9,20,22,24] # 
# # numbers = [3,4,10] # 
# site_crs = 26910 # utm zone 10
# laspath = "D:/5x5/sycan_2a_laz"
# outpath = "D:/5x5/sycan_2a"
# numbers = c(2,9,20,22,24)
# csv_pp <- ""
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(3,4,10)
# csv_pp <- "_ScanOnly"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'sycan_grass'
# # numbers = [5,8,9,11,13,17,19,21,24] # Pre 
# # numbers = [3,6,7,14,15,18,20,22,25] # SO
# # numbers = [1,2,3,4,6,7,10,12,16,17,18] # Post (didn't run, not sure what to do)
# # numbers = [5,8,9,13,19,24] # Post Dest
# # numbers = [14,15,20,25] # Post SO N/S (didn't run, not sure what to do)
# site_crs = 26910 # utm zone 10
# laspath = "D:/5x5/sycan_grass_laz"
# outpath = "D:/5x5/sycan_grass"
# numbers = c(5,8,9,11,13,17,19,21,24)
# csv_pp <- ""
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(3,6,7,14,15,18,20,22,25)
# csv_pp <- "_ScanOnly"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(5,8,9,13,19,24)
# csv_pp <- "Post_Dest_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'tates_hell_a'
# # numbers = [2,4,6,7,13,14,15,19,25] # CP_
# # numbers = [3,10,11,16] # SO_
# site_crs = 26916 # utm zone 16
# laspath = "D:/5x5/tates_hell_a_laz"
# outpath = "D:/5x5/tates_hell_a"
# numbers = c(2,4,6,7,13,14,15,19,25)
# csv_pp <- "THA_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(3,10,11,16)
# csv_pp <- "_ScanOnly"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'tates_hell_b'
# # numbers = [1,4,6,8,9,16,18,19] # CP_   note: no 5
# # numbers = [3,13,15] # SO_
# site_crs = 26916 # utm zone 16
# laspath = "D:/5x5/tates_hell_b_laz"
# outpath = "D:/5x5/tates_hell_b"
# numbers = c(1,4,6,8,9,16,18,19)
# csv_pp <- "THB_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(3,13,15)
# csv_pp <- "_ScanOnly"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'tenalquot'
# # numbers = [2,5,6,13,16,17,18,19,20] # CP_
# # numbers = [1,3,9,10] # SO_
# site_crs = 26910 # utm zone 10
# laspath = "D:/5x5/tenalquot_laz"
# outpath = "D:/5x5/tenalquot"
# numbers = c(2,5,6,13,16,17,18,19,20)
# csv_pp <- "CP_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
#
# numbers = c(1,3,9,10)
# csv_pp <- "SO_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)

# site = 'ttrs_hannah_hammock'
# #numbers = [1,3,6,8,12,15,17,18,21] # hh CP 
# #numbers = [5,7,9,10,11,16,23,24,25] # hh SO 
# site_crs = 26916 # utm zone 16
# laspath = "D:/5x5/ttrs_hannah_hammock_laz"
# outpath = "D:/5x5/ttrs_hannah_hammock"
# numbers = c(1,3,6,8,12,15,17,18,21)
# csv_pp <- "CP_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)
# 
# numbers = c(5,7,9,10,11,16,23,24,25)
# csv_pp <- "SO_"
# clip5x5with3mBuffer(laspath, outpath, site, numbers, csv_pp, site_crs)



