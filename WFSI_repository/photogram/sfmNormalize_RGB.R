# Title: sfmNormalize_RGB.R
#
# Objective: rotate (to square orientation) clipplots (.5 meter squares) and normalize
# using cloth simulation filter (CSF) 
# saves RGB color values also
# note: run other scripts in overviewDiagram.pptx first. 
# Use CloudCompare to segment out 1/2 meter square clipplots. 
# run this script against the clipplots. It is the last script in the photogrammetry process. 
# 
# Author(s): Brian Drye
# Date: 2/23/2021
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# Run other scripts to create a folder full of clipplot pointclouds, 
#
# Required libraries/modules: 
library(lidR)
library(sf)
library(stringr)
library(RCSF) 
library(rgl) 
library(recexcavAAR)

# Input folder: 
working_dir = paste0("D:/cloudCompare")  # contains pointclouds as .txt files

# Output folder: 
out_dir = paste0("D:/normalized")


options(digits=10)

#function for getting Minimum Bounding Rectangle
MBR <- function(p) {
  # Analyze the convex hull edges     
  a <- chull(p)                                   # Indexes of extremal points
  a <- c(a, a[1])                                 # Close the loop
  e <- p[a[-1],] - p[a[-length(a)], ]             # Edge directions
  norms <- sqrt(rowSums(e^2))                     # Edge lengths
  v <- e / norms                                  # Unit edge directions
  w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges
  
  # Find the MBR
  vertices <- p[a, ]                              # Convex hull vertices
  x <- apply(vertices %*% t(v), 2, range)         # Extremes along edges
  y <- apply(vertices %*% t(w), 2, range)         # Extremes normal to edges
  areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
  k <- which.min(areas)                           # Index of the best edge (smallest area)
  
  # Form a rectangle from the extremes of the best edge
  cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,])
}

setwd(working_dir)
getwd()

pointFiles = list.files(working_dir, pattern="\\.txt$", full.names = TRUE)
pointFiles

for(k in 1:length(pointFiles)) {
  singlePlotClip = read.csv(pointFiles[k], sep=" ")   # some are " " for sep
  if(length(singlePlotClip) == 12)
  names(singlePlotClip) <- c("X","Y","Z", "R","G","B","normals2","normals1","normals0","PointSourceId","ScanDirectionFlag","Intensity")
  # if(length(singlePlotClip) == 12)
  #   names(singlePlotClip) <- c("X","Y","Z", "normals2","normals1","normals0","PointSourceId","ScanDirectionFlag","Intensity", "R","G","B")
  if(length(singlePlotClip) == 11)
    names(singlePlotClip) <- c("X","Y","Z","R","G","B","normals2","normals1","normals0","PointSourceId","ScanDirectionFlag")
  # if(length(singlePlotClip) == 11)
    # names(singlePlotClip) <- c("X","Y","Z","normals2","normals1","normals0","PointSourceId","ScanDirectionFlag", "R","G","B")
  print(pointFiles[k])

  p = matrix(c(singlePlotClip$X, singlePlotClip$Y), ncol=2)
  mbr <- MBR(p)

  slope1 = (mbr[1,2] - mbr[2,2]) / (mbr[1,1] - mbr[2,1])
  slope2 = (mbr[2,2] - mbr[3,2]) / (mbr[2,1] - mbr[3,1])
  
  # which absolute slope is smaller? These slopes will be perpendicular. 
  smallestChange = slope1
  if(abs(slope2) < abs(slope1)) {
    smallestChange = slope2
  }
  
  angleToRotate = atan(smallestChange)*(180/pi)
  
  if(angleToRotate != 0)
  {
    tempRotation <- recexcavAAR::rotate(singlePlotClip$X, singlePlotClip$Y, singlePlotClip$Z, degrx = 0, degry = 0, degrz = ((-1)*mean(angleToRotate)))
    singlePlotClip$X = tempRotation$x
    singlePlotClip$Y = tempRotation$y
    singlePlotClip$Z = tempRotation$z
    
    # tls has been rotated...   
  }
  
   # singlePlotClip = LAS(data=singlePlotClip[, c("X", "Y", "Z")])
  singlePlotClip <- lidR::LAS(data=singlePlotClip[, c("X", "Y", "Z", "R", "G", "B")])
  singlePlotClip@data$Classification = 0L
  str(singlePlotClip)
  
  singlePlotClip@header@PHB[["X scale factor"]] <-
    0.000001 # this corrects for header errors
  singlePlotClip@header@PHB[["Y scale factor"]] <-
    0.000001 # this corrects for header errors
  singlePlotClip@header@PHB[["Z scale factor"]] <-
    0.000001 # this corrects for header errors
  
  tlsCSF <-
    CSF(
      singlePlotClip@data,
      sloop_smooth = FALSE,
      class_threshold = 0.1,
      cloth_resolution = .1,
      rigidness = 1L,
      iterations = 500L,
      time_step = 0.65
    )
  singlePlotClip@data$Classification[tlsCSF] <- 2
  
  # this is best (no ridges and good resolution) (note: Gina had keep_lowest = TRUE)
  tlsDTM <-
    grid_terrain(
      singlePlotClip,
      algorithm = knnidw(k = 6L, p = 2),
      res = .01,
      keep_lowest = TRUE,
      use_class = c(2L),
      full_raster = FALSE,
      is_concave = TRUE
    )
  # plot(tlsDTM)
  tlsNorm <- normalize_height(singlePlotClip, tlsDTM, add_lasattribute = TRUE)
  tlsNorm@data$Classification <- as.integer(tlsNorm@data$Classification)
  
  # if tlsNorm contains sub zero Z values, make zero
  tlsNorm@data$Z <-
    ifelse(tlsNorm@data$Z < 0, 0, tlsNorm@data$Z)
  
  
  fn = basename(pointFiles[k])
  fn = str_replace(fn, ".txt", ".laz")
  writeLAS(tlsNorm, paste0(out_dir, "/", fn))
  
  # plot(tlsNorm, color="RGB")
}
