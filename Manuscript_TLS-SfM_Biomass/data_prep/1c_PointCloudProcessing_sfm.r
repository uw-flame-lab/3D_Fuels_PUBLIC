# Title: 1_PointCloudProcessing_sfm.r
#
# Objective: 
# A script created by Gina Cova and Brian Drye to calculate
# structure metrics for each 0.5x0.5 voxel plot
# adapted by Deborah Nemens May 2023 
#
# Author(s): Brian Drye
# Date: 3/14/2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
#
# Required libraries: 
library(rgl)
library(moments)
library(stats)
library(VoxR)
library(moments)
library(misc3d)
library(Rvcg)
library(MASS)
library(stringr)
library(geometry)
library(dplyr)
library(lidR)

# Input folder path: 
input <-  "C:/Users/dnemens/Box/UW FLAME Lab/PROJECTS/3D FUELS/"

# Output folder path: 
output <- paste0(input, "_Common_files/global_inputs/photogram/")

## SFM inputs ###


#set up final dataframe
V.final <- data.frame(
  id = integer(0),
  file = character(0),
  Site = character(0),
  ClipPlot = character(0),
  Visit = character(0),
  destructiveYN = character(0),
  stringsAsFactors = FALSE
)

# file path to folder of 0.5 x 0.5 m clipped .laz files
list_of_sites <-
   c(
    'aucilla',
    'blackwater',
    'ft_stewart',
    "glacial",
    'hitchiti_a',
    'hitchiti_b',
    'lanl_forest',
    'lanl_grass',
    'lubrecht',
    'methow',
    "osceola",
    'sycan_2a',
    'sycan_forest',
    'sycan_grass',
    'tates_hell_a',
    'tates_hell_b',
    "tenalquot",
    'ttrs_hannah_hammock')

for (site in list_of_sites) {
  print(site)
  list_of_files <-
    list.files(
      path = paste0(
        input,
        site,
        "//data//photogram//cr_sfm//norm"
      ),
      recursive = F,
      full.names = TRUE,
      pattern = ".laz$"
    )

  #create data frame to store metrics in
  V.Store <- data.frame(
    id = integer(0),
    file = character(0),
    Site = character(0),
    ClipPlot = character(0),
    Visit = character(0),
    destructiveYN = character(0),
    points=numeric(0),
    minX=numeric(0),
    maxX=numeric(0),
    minY=numeric(0),
    maxY=numeric(0),
    minZ=numeric(0),
    maxZ=numeric(0),
    aveZ=integer(0),
    occupied_volume=numeric(0),
    PAD=numeric(0),
    PADV=numeric(0),
    surface_area=numeric(0),
    qhullvolume=numeric(0),
    stringsAsFactors = FALSE
  )
  

# loop through all files and output metrics for each one
for(j in 1:length(list_of_files)) {
  inDataTLS = readLAS(list_of_files[j])
  print(j)
  print(list_of_files[j])
  
  inDataTLS = inDataTLS@data
 
  # grab the X, Y, and Z columns - change this if it's not the 1, 2, and 3rd cols
  inData = as.data.frame(inDataTLS[, 1:3])
  print(dim(inData))
  
  colnames(inData)<- c("X","Y","Z")
  options(digits=9)
  
  # normalize point clouds to flat surface and north-south square orientation
  # (this relies on min point being close to corner)  
  inData$X = inData$X - min(inData$X)  
  inData$Y = inData$Y - min(inData$Y)  
  #inData$Z = inData$Z - min(inData$Z)
  
  # filter out any stray points that may have crept past our 0.5 m limit
  inData = inData[inData$X < 0.5 &
                    inData$Y < 0.5,]
  
  inData = inData[inData$X > 0 &
                    inData$Y > 0,]
  
  # if TLS contains sub zero Z values, make zero
  inData$Z <- ifelse(inData$Z < 0, 0, inData$Z)
  
  # if TLS contains values above 1 m z, discard
  inData <- inData[inData$Z < 1.0,]
  
  inData = round(inData, digits = 7)
  
  # create copy of inData for voxel purposes (for ov and height)
  # we need to shift all coordinates by 0.05 or we'll end up with an extra voxel
  inData.10cm = inData
  res = .1
  data=inData.10cm
  data=data.table::data.table(data[,1:3])
  data.table::setnames(data,c("x","y","z"))
  x=y=z=npts=.N=.=':='=NULL
  
  data[,':='(x = Rfast::Round( x / res ) * res,
             y = Rfast::Round( y / res ) * res,
             z = Rfast::Round( z / res ) * res)]
  data$x = data$x +0.05
  data$y = data$y +0.05
  data$z = data$z +0.05
  data = unique(data[,npts:=.N,by=.(x,y,z)])
  test=vox(inData.10cm, res=0.01) 
  data[which(data$npts < (mean(test$npts)-sd(test$npts))*100),]$npts = 0
  data = data[which(data$npts > 5),]
  data = data[which(data$x < 0.55),]
  data = data[which(data$y < 0.55),]
  voxels_10cm = data
  
  inData.10cm$X = inData.10cm$X - 0.05
  inData.10cm$Y = inData.10cm$Y - 0.05
  inData.10cm$Z = inData.10cm$Z - 0.05
  
  # create another version at 1 cm resolution
  inData.1cm = inData
  
  inData.1cm$X = inData.1cm$X - 0.005
  inData.1cm$Y = inData.1cm$Y - 0.005
  inData.1cm$Z = inData.1cm$Z - 0.005
  
  voxels_10cm <- vox(inData.10cm, res = .1)  # see how many 10cm voxels we have
  
  V.Store[j,'id'] = j
  V.Store[j,'file'] = list_of_files[j]
  V.Store[j, 'Site'] = sub(".*Outputs//", "", V.Store$file[j])
  V.Store[j, 'Site'] = sub("//video.*", "", V.Store$Site[j])
  V.Store[j, 'ClipPlot'] = sub(".*normalized/", "", V.Store$file[j])
   V.Store[j, 'ClipPlot'] = gsub(".laz", "", V.Store$ClipPlot[j])
  V.Store[j,'points'] = length(inData$X)
  V.Store[j,'minX'] = min(inData$X)
  V.Store[j,'maxX'] = max(inData$X)
  V.Store[j,'minY'] = min(inData$Y)
  V.Store[j,'maxY'] = max(inData$Y)
  V.Store[j,'minZ'] = min(inData$Z)
  V.Store[j,'maxZ'] = max(inData$Z)
  
  planeXY.10cm = inData.10cm
  planeXY.10cm$Z = 0   # flatten so we have XY plane of points
  planeXY.10cm = planeXY.10cm[planeXY.10cm$X < .5 & # disregard peripheral points
                      planeXY.10cm$Y < .5,]
  xyVoxels.10cm <- vox(planeXY.10cm, res=.1)
  colnames(xyVoxels.10cm)<- c("X","Y","Z","NBPTS")
  
  planeXY.1cm = inData.1cm
  planeXY.1cm$Z = 0

  xyVoxels.1cm <- vox(planeXY.1cm, res=.01)
  colnames(xyVoxels.1cm)<- c("X","Y","Z","NBPTS")

  numRows = length(xyVoxels.10cm$X)
  
  W.Store <- data.frame(X = numeric(numRows),
                        Y = numeric(numRows),
                        points=numeric(numRows),
                        meanZ=integer(numRows),
                        stringsAsFactors=FALSE)
  
  for (k in 1:length(xyVoxels.10cm$X)) {
    # get column for this square
    singleColumn = subset(inData, inData$X > xyVoxels.10cm$X[k] & 
                            inData$X <= xyVoxels.10cm$X[k] + 0.1 &
                            inData$Y > xyVoxels.10cm$Y[k] & 
                            inData$Y <= xyVoxels.10cm$Y[k] + 0.1)
    W.Store[k,1] = xyVoxels.10cm$X[k]
    W.Store[k,2] = xyVoxels.10cm$Y[k]
    W.Store[k,3] = xyVoxels.10cm$NBPTS[k]
    W.Store[k,4] = quantile(singleColumn$Z, .99, na.rm = TRUE)
  }
  
  V.Store[j,'aveZ'] = mean(W.Store$meanZ, na.rm = TRUE)
  
  V.Store[j,'occupied_volume'] = length(voxels_10cm$x)/(5*5*10)
  #number of voxels in whole area / number of possible voxels 
    
  
  V.Store[j,'PAD'] = length(xyVoxels.1cm$X)
  
  planeXZ = inData.1cm
  planeXZ$Y = 0   # flatten so we have XZ plane of points
  xzVoxels <- vox(planeXZ, res=.01)
  colnames(xzVoxels)<- c("X","Y","Z","NBPTS")
  V.Store[j,'PADV'] = length(xzVoxels$X)
  
  ## CONVEX HULL CALCULATION
  inDataAbove10cm = subset(inData, inData$Z >= .1)
  convexHull <- as.data.frame(matrix(nrow = 1, ncol = 2))
  convexHull[1,1] <- 0
  convexHull[1,2] <- 0
  colnames(convexHull) <- c("area", "vol")
  
  if(nrow(inDataAbove10cm) > 4) {
    convexHull = convhulln(inDataAbove10cm, "FA")}
  
  V.Store[j,'surface_area'] = convexHull$area
  V.Store[j,'qhullvolume'] = convexHull$vol
  
}
  V.final <- rbind(V.final, V.Store)
}

count(V.final, Site)
#Clean up site names to match field data
V.final$Site <- V.final$Site %>% str_replace_all(pattern=c("aucilla"= "Aucilla",
                                                     "lubrecht"= "Lubrecht",
                                                     "ttrs_hannah_hammock"= "Hannah Hammock",
                                                     "hitchiti_a"= "Hitchiti A",
                                                     "hitchiti_b" = "Hitchiti B",
                                                     "lanl_forest"= "LANL Forest",  
                                                     "lanl_grass"= "LANL Grassland",  
                                                     "blackwater"= "Blackwater",  
                                                     "ft_stewart" = "Fort Stewart",
                                                     "glacial"= "Glacial Heritage",  
                                                     "tenalquot"="Tenalquot",  
                                                     "methow"= "Methow",  
                                                     "osceola" = "Osceola",
                                                     "sycan_2a"= "Sycan Forest II",  
                                                     "sycan_forest"= "Sycan Forest I",  
                                                     "sycan_grass"="Sycan Grassland",  
                                                     "tates_hell_a"="Tates Hell A",  
                                                     "tates_hell_b"= "Tates Hell B"))

#make naming convention for pre/post plots uniform between lidar and field csvs
V.final <- V.final %>%
  mutate(Visit = case_when(grepl("_POST", ClipPlot)~"POST"))
V.final$Visit[is.na(V.final$Visit)] <- "PRE"
V.final$ClipPlot = gsub("_POST", "", V.final$ClipPlot)
count(V.final, ClipPlot)

V.final$ClipPlot <- V.final$ClipPlot %>% str_replace_all(pattern = c("ONF" = "", 
                                                                     "_TLS - Cloud" = "",
                                                                      "FSA_" = "",
                                                           "FortStewart_Site_A_Plot_" = "",
                                                           " - Cloud" = "",
                                                           "_TLS" = "",
                                                           "03" = "3",
                                                           "08"="8",
                                                           "07"="7",
                                                           "06"="6"))

#make naming of destructive and non-des plots uniform for lidar/field csvs
#all POST plots are destructive, so they get Y's
V.final[V.final$Visit == "POST", "destructiveYN"] = "Y"
postRows <- V.final[V.final$Visit == "POST", ]
#all other PRE plots are destructive
#V.final$destructiveYN <- V.final$destructiveYN %>% replace_na("Y")
V.final$destructiveYN <- "Y"

#remove unnec columns
V.final <- V.final[,3:19]

# write to file
write.csv(V.final, paste0(output, "PointCloudStructuralMetrics_SFM.csv"), row.names = F)

