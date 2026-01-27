# Title: 04_plotting.virtual.forest.R
#
# Objective: 
# script for plotting virtual forest 
# adapted from Carlos Silva's crown metrics script
#
# Author(s): Nemens/Satterfield
# Date: 10/9/2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# NOTE: run following scripts first: 
# 00_classify_normalize_als.r
# 01_canopy_metrics_grid.R 
# 02_canopy_gaps.R
# 03_crownMetrics.R
#
# Required libraries: 
library(plyr)
library(lidR)
library(rLiDAR)
library(alphashape3d)
library(splancs)
library(rgdal)
# library(maptools)
library(sp)
library(mixtools)
library(leafR)
library(viridis)
library(terra)
library(sf)
library(rgl)
library(tidyr)

# select the field site you want plot
fieldsite <- "hitchiti_b"

#set to your working directory
base_folder = "C:/Users/dnemens/Documents/temp_3DF_ALS/"

# Input folder path: 
laz_norm_folder<-paste0(base_folder, fieldsite, "//04_crown_metrics//")

# Output folder path: 
output_folder<-paste0(base_folder, fieldsite, "//05_segmentation_plots//")
dir.create(output_folder, recursive = T)

# Additional information:
# install.packages("rLiDAR", repos="http://R-Forge.R-project.org")
# install.packages("rlang")


# set desired view orientation and save
# uM <- par3d()$userMatrix
# zoom<-par3d()$zoom
# then view to capture data values in uM_trees below

# set par3d view orientation
uM_trees <- matrix(
  
  # Taking sequence of elements
  c(0.8636110, -0.4695264, 0.1836330, 0,
    0.2244305, 0.6841909, 0.6939119, 0,
    -0.4514499, -0.5580571, 0.6962509, 0,
    0, 0, 0, 1),
  # No of rows
  nrow = 4,
  # No of columns
  ncol = 4,
  # By default matrices are in column-wise order
  # So this parameter decides how to arrange the matrix
  byrow = TRUE
)

# set par3d zoom value for the tree segmentation multicolor plots
zoom_ts <- 0.7462156
# set par3d zoom value for the green virtual forest plots
zoom_vf <- 0.7462156

# set par3d projMatrix
# set par3d view orientation
projM_trees <- matrix(
  
  # Taking sequence of elements
  c(4.520409, 0.000000, -0.05494506, 0,
    0.000000, 5.001304, -0.19047619, 0,
    0.000000, 0.000000, -3.86370373, -4250.252,
    0, 0, -1, 0),
  # No of rows
  nrow = 4,
  # No of columns
  ncol = 4,
  # By default matrices are in column-wise order
  # So this parameter decides how to arrange the matrix
  byrow = TRUE
)

# some site such as blackwater have more than one relevant file, so set variable for doing these individually
s <- 1


#set working directory
setwd(laz_norm_folder)
getwd()

# 04 - list of laz file
lazfiles_names<-list.files(laz_norm_folder,".laz")
lazfiles_names
# 
library(lidR)
laz <- lidR::readLAS(lazfiles_names[s])
# str(laz)

# pull the poly file for plotting virtual forest in next section
polyfile <- list.files(laz_norm_folder,pattern="poly.+.shp")
polyfile
str(polyfile)
product <- st_read(polyfile[s])
# str(product)

# plot the tree segmentation
plot(laz, bg = "white", size = 4, color = "treeID") # visualize trees

# position window and size as desired and use the four output numbers in windowRect in rgl::par3d() below
# par3d("projMatrix")
# par3d("windowRect")

# set window size
rgl::par3d(windowRect = c(293, 31, 1385, 1018), zoom=zoom_ts)

# set view orientation
par3d(userMatrix = uM_trees)

# save laz of tree segmentation with multiple colors
# plot will throw error if it is minimized - make sure plot is pulled up before using rgl.snapshot
 rgl.snapshot(paste0(output_folder, fieldsite,"_treeseg_multicolor.png"), fmt = 'png')

# -----------------------------------------------------

#### plotting canopy cover metrics histograms ####

# Site
# par(mfrow=c(1,2))
# hist(product$H99TH, main="H99th", xlab="height (m)")
# hist(product$CPA, main="Crown Projected Area", xlab="Area (m2)")

# -----------------------------------------------------

#### Plotting a virtual forest ####

# Set parameters of trees growing within the virtual stand
# CBH<-productslist$crown_metrics_poly_Blackwater_clip_norm@data$CBH # height at canopy base
# CL<-productslist$crown_metrics_poly_Blackwater_clip_norm@data$CL # tree crown length
# CW<-productslist$crown_metrics_poly_Blackwater_clip_norm@data$CR*2 # tree crown diameter
# Xcoord<-productslist$crown_metrics_poly_Blackwater_clip_norm@data$E - min(productslist$crown_metrics_poly_Blackwater_clip_norm@data$E) 
# Ycoord<-productslist$crown_metrics_poly_Blackwater_clip_norm@data$N - min(productslist$crown_metrics_poly_Blackwater_clip_norm@data$N) 

# Site
# remove NA rows from any sites that have them
# for example, sycan marsh has an NA in row 66 so remove these
product.clean <- product %>% drop_na(CBH, CL, CR, E, N, treeID)

product[which(is.na(product)),]
product.clean <- na.omit(product)

CBH <- product.clean$CBH
CL <- product.clean$CL
CW <- product.clean$CR*2
Xcoord <- product.clean$E - min(product.clean$E)
Ycoord <- product.clean$N - min(product.clean$N)
treeID <- product.clean$treeID

str(product.clean)

# Plot stand
# for( i in 1:nrow(productslist$crown_metrics_poly_Blackwater_clip_norm@data)){
#   LiDARForestStand(crownshape = "cone", CL = CL[i], CW = CW[i],
#                    HCB = CBH[i], X = Xcoord[i], Y = Ycoord[i], dbh = 0.4,
#                    crowncolor = "forestgreen", stemcolor = "chocolate4",
#                    resolution="high", mesh=TRUE)
# }
# # Add other plot parameters
# axes3d(c("x-", "x-", "y-", "z"), col="gray") # axes
# title3d(xlab = "X Coord", ylab = " Y Coord", zlab = "Height", col="red") # title
# planes3d(0, 0, -1, 0.001, col="gray", alpha=0.7) # set a terrain plane

# Plot stand
for( i in 1:nrow(product.clean)){
  LiDARForestStand(crownshape = "cone", CL = CL[i], CW = CW[i], HCB = CBH[i], X = Xcoord[i], Y = Ycoord[i], dbh = 1,
                   # crowncolor = treeID[i],
                   crowncolor = "forestgreen",
                   stemcolor = "white",
                   resolution="high", mesh=TRUE)
}

# Add other plot parameters
# axes3d(c("x-", "x-", "y-", "z"), col="lightgray") # axes
# title3d(xlab = "X Coord", ylab = " Y Coord", zlab = "Height", col="red") # title
# planes3d(0, 0, -1, 0.001, col=NA, alpha=0.7) # set a terrain plane

# set window size
rgl::par3d(windowRect = c(293, 31, 1385, 1018), zoom=zoom_vf)

# ?rgl::par3d

# set view orientation
par3d(userMatrix = uM_trees)
# 
# # see mouse settings
# par3d("mouseMode")
# par3d(mouseMode = "trackball")

rgl.snapshot(paste0(output_folder, fieldsite,'_treeseg_forestgreen.png'), fmt = 'png')
# ??rgl.snapshot
# ?rgl
