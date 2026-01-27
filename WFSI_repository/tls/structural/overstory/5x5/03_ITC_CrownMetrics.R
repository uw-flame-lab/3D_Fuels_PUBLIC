# Title: 03_ITC_CrownMetrics.R
#
# Objective: 
# Calculate crown metrics (for TLS 5x5 scans)
# note: this script is almost identical to \_Common_files\scripts\als\structural\03_ITC_CrownMetrics.r
#
# Author(s): Carlos Alberto Silva, Brian Drye
# Date: Oct 2020
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# NOTE: run following scripts first:
# 01_canopy_metrics_grid.R 
# 02_canopy_gaps.R
#
# Run this script for each site (one at a time)
# this script will read files from  D:\temp\<site>\outputs\tls\5x5_metrics\canopy_metrics\04_crown_metrics\01_basics\
# script will create a folder in the site subfolder
# D:\temp\<site>\outputs\tls\5x5_metrics\canopy_metrics\04_crown_metrics
#
# Required libraries/modules: 
require(pacman)

p_load(plyr,
       lidR,
#       rLiDAR,
       alphashape3d,
       splancs,
       rgdal,
#       maptools,
       sp,
       mixtools,
       leafR,
       viridis,
       sf,  # added sf to use sf::st_write() -BrianDrye 
	   terra) # Added 'terra' package to use 'terra::intersect()' -BCB


# Current site: 
site = 'glacial'

# set max_cr_factor by site location... 
#max_cr_factor = 0.6 # Maximum value of a crown diameter given as a proportion of the tree height. Default is 0.6, meaning 60% of the tree height 
#max_cr_factor = 0.4 # SE sites (Aucilla, blackwater, HH, THA, THB, FS, HitA, HitB, osceola, tenalquot, ttrs_hh)
max_cr_factor = 0.35 # Western sites (glacial, Lubrecht, lanlF, lanlG, sycan_2a, sycan_forest, methow)


# Input folder path: 
base_folder = "D:/temp/"
input_folder = paste0(base_folder, site, "/data/tls/plots/5x5/biomass/norm")
#input_folder = paste0(base_folder, site, "/norm/")


# Output folder path: 
output_folder <- paste0(base_folder, site, "/outputs/tls/5x5_metrics/canopy_metrics/04_crown_metrics/")


# Additional information: 

###--------------------------------------------------------------------------------------------------------###
# modified by Brian Drye to do limited metric calculations
# quicker. This version is configured to run on the UW lab machine
#----------------------------------------------------------------------------------------------------------###
# SILVA, C. A. (2020)                                                                                      ###
# carlos_engflorestal@outlook.com.br                                                                       ###
# Skype: flowds                                                                                            ###
#----------------------------------------------------------------------------------------------------------###

#----------------------------------------------------------------------------------------------------------###
# Functions
#----------------------------------------------------------------------------------------------------------###
#rm(list=ls())

# function for getting crown metrics
custom_crown_metrics = function(z) {
  std_dev = sd(z)
  
  metrics = list(
    ave_h  = mean(z),
    sd_h = std_dev,
    h_99 = quantile(z, .99), 
    cbh = mean(z[z>2]) - std_dev  # ave height above 2m minus 1 std_dev
  )
  
  return(metrics)
}


# 02 - Individual tree crown delineation and metrics computation function
ITC_CrownMetrics<-function(las_norm=plot01, 
                           chm_res=1, #change?
                           tws=5,
                           sws=3,
                           max_cr_factor =0.6,
                           exclusion = 0.2, 
                           outname="plot01", 
                           outdir=getdir()){
  
  cha<-function(x,y){
    chull(x,y)->i
    return(areapl(cbind(x[i],y[i])))
  }
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  HStats<-function(xyzid) {
    cat (".");utils::flush.console()
    #print(xyzid[1,4])
    #plot(xyzid[,1:2], main=unique(xyzid[,5]))
    "skewness" <- function(x, na.rm = FALSE) {
      if (is.matrix(x)) 
        apply(x, 2, skewness, na.rm = na.rm)
      else if (is.vector(x)) {
        if (na.rm) 
          x <- x[!is.na(x)]
        n <- length(x)
        (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
      }
      else if (is.data.frame(x)) 
        sapply(x, skewness, na.rm = na.rm)
      else skewness(as.vector(x), na.rm = na.rm)
    }
    "kurtosis" <- function(x, na.rm = FALSE) {
      if (is.matrix(x)) 
        apply(x, 2, kurtosis, na.rm = na.rm)
      else if (is.vector(x)) {
        if (na.rm) 
          x <- x[!is.na(x)]
        n <- length(x)
        n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
      }
      else if (is.data.frame(x)) 
        sapply(x, kurtosis, na.rm = na.rm)
      else kurtosis(as.vector(x), na.rm = na.rm)
    }
   
    quiet(CBH<-tryCatch({
      model<- normalmixEM(x=xyzid[,3], k=2)
      index.higher <- which.max(model$mu)
      CHB<-model$mu[index.higher]-model$sigma[index.higher]*1.5
    },error = function(err) { return(CBH=quantile(xyzid[,3],0.25))}))
    n<-nrow(xyzid)
    xcenter<-mean(xyzid[,1])
    xmin<-min(xyzid[,1])
    xmax<-max(xyzid[,1])
    ycenter<-mean(xyzid[,2])
    ymin<-min(xyzid[,2])
    ymax<-max(xyzid[,2])
    hmax<-max(xyzid[,3])
    hmin<-min(xyzid[,3])
    hmean<-mean(xyzid[,3])
    hskew<-skewness(xyzid[,3],na.rm = T)
    hkur<-kurtosis(xyzid[,3],na.rm = T)
    
    if (CBH < 2) {CBH<-hmax/2
    #CBH<-quantile(xyzid[,3],0.25)
    }
    
    if ( nrow(xyzid)==1) { hsd<-0; hvar<-0} else {
      hsd<-sd(as.numeric(xyzid[,3]), na.rm=T)
      hvar<-var(as.numeric(xyzid[,3]), na.rm=T) }
    
    quiet(CA<-cha(xyzid[,1],xyzid[,2]))
    CL<-hmax-CBH
    CRatio<-CBH/hmax
    CRad<-sqrt(CA/pi)
    hq<-quantile(xyzid[,3], c(seq(0.05,1,0.05)[-20],0.99))
    CDens<-(nrow(subset(xyzid,xyzid[,3] >= CBH))/ nrow(xyzid))*100
    
    imax<-max(xyzid[,4])
    imin<-min(xyzid[,4])
    imean<-mean(xyzid[,4])
    if ( nrow(xyzid)==1) { isd<-0; ivar<-0} else {
      isd<-sd(as.numeric(xyzid[,4]), na.rm=T)
      ivar<-var(as.numeric(xyzid[,4]), na.rm=T) }
    iq<-quantile(xyzid[,4], c(seq(0.05,1,0.05)[-20],0.99))
    
    suppressMessages(if (n <= 3) { CV=c(0,0)} else {
      CV<-tryCatch({paste0(chullLiDAR3D(xyzid[,c(1:3,5)],plotit = FALSE)[,2:3])}, 
                   error = function(err) { return(CV=c(0,0))})})
    
    xyzid2<-xyzid[,1:3]
    xyzid2[,1]<-min(xyzid[,1])- xyzid[,1]
    xyzid2[,2]<-min(xyzid[,2])- xyzid[,2]
    
    if (n <= 3) { AlphaShape1=0; AlphaShape0.5=0; AlphaShape0.25=0} else {
      suppressMessages(AlphaShape1<-tryCatch({paste0(volume_ashape3d(ashape3d(as.matrix(xyzid2),alpha=1, pert=TRUE)))}, 
                            error = function(err) { return(AlphaShape1=0)}))
      
      suppressMessages(AlphaShape0.5<-tryCatch({paste0(volume_ashape3d(ashape3d(as.matrix(xyzid2),alpha=0.5, pert=TRUE)))}, 
                              error = function(err) { return(AlphaShape0.5=0)}))
      
      suppressMessages(AlphaShape0.25<-tryCatch({paste0(volume_ashape3d(ashape3d(as.matrix(xyzid2),alpha=0.25, pert=TRUE)))}, 
                               error = function(err) { return(AlphaShape0.25=0)}))}
    
    
    suppressMessages(crown_lai<-tryCatch({
      crown_lai=lai(
        lad.profile(
          lad.voxels(
            quiet(lidR::LAS(xyzid[,1:3])), 
            grain.size=1)
        ), min=CBH
      )
    },error = function(err) { return(crown_lai=NA)}))
    
    
    CRT<-CL/CBH
    CFI<-CL/(CRad*2)
    CTI<-(CRad*2)/CL
    CSR<-(CRad*2)/CBH
    
    stats<-c(n,xcenter,xmin,xmax,ycenter,ymin,ymax,hmax,hmin,hmean,hsd,hvar,hkur,hskew,hq,imax,imin,imean,isd,ivar,iq,CDens,CL,CBH,CRatio,CRad,CA,CV,ASV0.25=AlphaShape0.25,
             ASV0.5=AlphaShape0.5,ASV1=AlphaShape1,crown_lai, CRT,CFI,CTI,CSR)
    return(stats)
  }
  
  
  TreeMetrics<-function(xyzid) {
    Stats<-ddply(data.frame(xyzid), "treeID",HStats)
    colnames(Stats)<-c("TreeID","Nreturns","E","Emin","Emax","N","Nmin","Nmax","HMAX","HMIN","HMEAN",
                       "HSD","HVAR","HKUR","HSKEW","H5TH","H10TH","H15TH","H20TH","H25TH","H30TH","H35TH","H40TH",
                       "H45TH","H50TH","H55TH","H60TH","H65TH","H70TH","H75TH","H80TH","H85TH","H90TH","H95TH","H99TH",
                       "IMAX","IMIN","IMEAN","ISD","IVAR","IH5TH","I10TH","I15TH","I20TH","I25TH","I30TH","I35TH","I40TH",
                       "I45TH","I50TH","I55TH","I60TH","I65TH","I70TH","I75TH","I80TH","I85TH","I90TH","I95TH","I99TH",
                       "CDens","CL","CBH","CRatio","CRad","CPA","CSA","CV","ASV0.25","ASV0.5","ASV1","CrownLAI","CRT","CFI","CTI","CSR")
    return(Stats)
  }
  
  
  ConvexHull_AlphaShape3D<-function(xyzid) {
    if (n <= 5) { CV=c(0,0)} else {
      suppressMessages(CV<-tryCatch({paste0(chullLiDAR3D(xyzid,plotit = FALSE)[,2:3])},
                   error = function(err) { return(CV=c(0,0))}))}
    xyzid2<-xyzid[,1:3]
    xyzid2[,1]<-min(xyzid[,1])- xyzid[,1]
    xyzid2[,2]<-min(xyzid[,2])- xyzid[,2]
    if (n <= 3) { AlphaShape1=0; AlphaShape0.5=0; AlphaShape0.25=0} else {
      suppressMessages(AlphaShape1<-tryCatch({paste0(volume_ashape3d(ashape3d(as.matrix(xyzid2),alpha=1, pert=TRUE)))},
                            error = function(err) { return(AlphaShape1=0)}))
      suppressMessages(AlphaShape0.5<-tryCatch({paste0(volume_ashape3d(ashape3d(as.matrix(xyzid2),alpha=0.5, pert=TRUE)))},
                              error = function(err) { return(AlphaShape0.5=0)}))
      suppressMessages(AlphaShape0.25<-tryCatch({paste0(volume_ashape3d(ashape3d(as.matrix(xyzid2),alpha=0.25, pert=TRUE)))},
                               error = function(err) { return(AlphaShape0.25=0)}))}
    return(cbind(CV,AlphaShape1,AlphaShape0.5,AlphaShape0.25))
  }
  
  lad.voxels = function(.las, grain.size = 1, k = 1){
    
    #empty list object that will be fueling with binneds data.frames
    LAD_VOXELS = list()
    Z = NA
    
    #load normalized las cloud
    
    .las@data$Z[.las@data$Z < 0] = 0
    
    maxZ = floor(max(.las@data$Z))
    
    func = formula(paste0("~pointsByZSlice(Z, ", maxZ, ")"))
    t.binneds    = lidR::grid_metrics(.las, func, res = grain.size,
                                      start = c(min(.las@data$X), max(.las@data$Y)))
    t.binneds    = data.frame(sp::coordinates(t.binneds), raster::values(t.binneds))
    names(t.binneds)[1:2] = c("X", "Y")
    
    
    #getting the coordinates X and Y
    #t.binneds$X = coordinates(t.binneds)[,1]
    #t.binneds$Y = coordinates(t.binneds)[,2]
    #t.binneds = as.data.frame(t.binneds) #transforming in a data.frame
    
    #clip product by las files limits
    #t.binneds = t.binneds[t.binneds$X < xmax(.las) &
    #                        t.binneds$X > xmin(.las) &
    #                        t.binneds$Y > ymin(.las) &
    #                        t.binneds$Y < ymax(.las),]
    
    
    #select ground returns
    ground.returns = t.binneds[, grep("ground", names(t.binneds))]
    
    #select columns vegetation above 1m:
    if(nrow(t.binneds) != 1){ #this if is necessary when grain size is the whole plot
      pulses.profile.dz1 = t.binneds[, c(grep("pulses", names(t.binneds)))]
    }else{
      pulses.profile.dz1 = data.frame(matrix(as.numeric(as.character(t.binneds[, c(grep("pulses", names(t.binneds)))])), ncol = length(grep("pulses", names(t.binneds)))))
      names(pulses.profile.dz1) = names(t.binneds)[c(grep("pulses", names(t.binneds)))]
    }
    
    #invert data.frames for the sky be first
    pulses.profile.dz1 = pulses.profile.dz1[,length(pulses.profile.dz1):1] #invert columns
    
    #add grounds returns (0-1m)
    pulses.profile.dz1 = cbind(pulses.profile.dz1, ground.returns)
    rm(ground.returns)
    
    ### total matriz and cumsum.matrix:
    total.pulses.matrix.dz1 = matrix(apply(pulses.profile.dz1, 1, sum), ncol = length(pulses.profile.dz1), nrow = nrow(pulses.profile.dz1))
    cumsum.matrix.dz1 = matrix(apply(pulses.profile.dz1, 1, cumsum), ncol = length(pulses.profile.dz1), nrow = nrow(pulses.profile.dz1), byrow = T)
    
    rm(pulses.profile.dz1)
    
    #Pulses out for each voxel
    pulse.out.dz1 = total.pulses.matrix.dz1 - cumsum.matrix.dz1
    
    #The pulses.out of voxel 1 is the pulses.in of voxel 2 and so on...
    #Therefore, pulse.in is pulse.out without the last line and adding in the
    #first line the total pulses:
    if(nrow(t.binneds) != 1){ #if used when grain size of the whole plot
      pulse.in.dz1 <- cbind(total.pulses.matrix.dz1[,1], pulse.out.dz1[,-c(ncol(pulse.out.dz1))])
    }else{
      pulse.in.dz1 <- c(total.pulses.matrix.dz1[,1], pulse.out.dz1[,-c(ncol(pulse.out.dz1))])
    } #enf if
    
    rm(total.pulses.matrix.dz1, cumsum.matrix.dz1)
    
    # MacArthur-Horn eqquation
    # LAD = ln(S_bottom/S_top)*(1/(dz*K))
    #k value for LAD equation
    dz = 1
    
    LAD.dz1 = log(pulse.in.dz1/pulse.out.dz1) * 1/k * 1/dz
    
    rm(pulse.in.dz1, pulse.out.dz1)
    
    # Remove infinite and NaN values
    #Inf ocorre qndo pulses.out eh zero
    #NaN ocorre qndo pulses.in eh zero
    LAD.dz1[is.infinite(LAD.dz1)] <- NA; LAD.dz1[is.nan(LAD.dz1)] <- NA;
    
    #remove the first 1 meter close to the ground (and the ground too)
    LAD.dz1 = LAD.dz1[, -c(ncol(LAD.dz1))]
    
    #fuel list object
    LAD_VOXELS[["LAD"]] = LAD.dz1
    LAD_VOXELS[["coordenates"]] = t.binneds[,c("X", "Y")]
    
    rm(LAD.dz1, t.binneds)
    
    return(LAD_VOXELS)
  }#End function
  
  #shp<-ITD_shp
  CrownMetricsLiDAR<-function(las){
    TreesMetrics<-TreeMetrics(las@data[,c("X","Y","Z","Intensity","treeID")])
    indx <- sapply(TreesMetrics, is.list)
    TreesMetrics[indx] <- lapply(TreesMetrics[indx], function(x) as.numeric(as.character(x)))
    indx <- sapply(TreesMetrics, is.character)
    TreesMetrics[indx] <- lapply(TreesMetrics[indx], function(x) as.numeric(as.character(x)))
    return(TreesMetrics)
  }
  
  chm <- grid_canopy(las_norm, res = chm_res, pitfree(c(0,2,5,10,15), c(0, 1.5)))
  ker <- matrix(1, sws, sws)
  
  schm <- raster::focal(chm, w = ker, fun = mean, na.rm = TRUE)
      
  ttops <- locate_trees(schm, lmf(tws, 1.3, shape = "circular"))
  
  
  crowns=lidR::silva2016(chm, ttops,max_cr_factor = max_cr_factor,
                             exclusion = exclusion,
                             ID = "treeID")
      
   contour_sf <- sf::st_as_sf(stars::st_as_stars(crowns()),as_points = FALSE, merge = TRUE)
   contour<-sf::as_Spatial(contour_sf)
        
   colnames(contour@data)[1]<-"treeID"
   contour@data$CA<-raster::area(contour)
   contour@data$CR<-sqrt(contour@data$CA/pi)
    
   #windows()
#   plot(chm)
#   plot(ttops, add=T)
#   plot(contour, border="red", add=T)
#   plot(ttops, add=T, col="black", pch=3)
   
   contour<-terra::intersect(contour,ttops) ### Originally causing: Error in as.vector(x) : no method for coercing this S4 class to a vector, changed to 'terra::intersect()' -BCB
   ttops<-terra::intersect(sf::as_Spatial(ttops),contour) ### Originally causing:Error in as.vector(x) : no method for coercing this S4 class to a vector, changed to 'terra::intersect()' -BCB
  
   quiet(las_class <- segment_trees(las_norm, crowns))
   quiet(trees <- filter_poi(las_class, !is.na(treeID)))
   npoints<-tapply(trees@data$Z,trees@data$treeID, length)
   trees@data<-subset(trees@data,trees@data$Classification!=2 & trees@data$Z > 0.2 & trees@data$treeID %in% names(npoints[npoints>10]))
   trees@data<-trees@data[trees@data$treeID %in% names(npoints[npoints>10]),]
   trees@data<-trees@data[!duplicated(trees@data[,c("X","Y","Z")]),]
        
   #CrownMetrics<-CrownMetricsLiDAR(trees) # ########### This is what takes awhile to process (didn't change anything, just noting) -BCB
   
   # use a custom function to get metrics we care about, instead of the CrownMetrics<-CrownMetricsLiDAR(trees) line
   ccm = ~custom_crown_metrics(z = Z)
   CrownMetrics <- crown_metrics(trees, func = ccm, geom = "convex")
   
   # edit the custom_crown_metrics function near the top of this file 
   # to add additional metrics.

   plot(CrownMetrics["ave_h"], pal = hcl.colors)
#   plot(CrownMetrics["sd_h"], pal = hcl.colors)
#   plot(CrownMetrics["h_99"], pal = hcl.colors)
#   plot(CrownMetrics["cbh"], pal = hcl.colors)

   ttops<-ttops[ttops@data$treeID %in% names(npoints[npoints>10]),]
   contour<-contour[contour@data$treeID %in% names(npoints[npoints>10]),]
        
   ttops_metrics<-merge(ttops,CrownMetrics, by.x="treeID",by.y="TreeID")
   crown_metrics<-merge(contour,CrownMetrics, by.x="treeID",by.y="TreeID")
  
   writeLAS(trees, file.path(outdir,paste0(outname,"_tws_",tws,"_sws_",sws,".laz"))) 
   sf::st_write(as(ttops, "sf"), paste0(outdir, paste0("treetop_",outname,"_tws_",tws,"_sws_",sws)), driver="ESRI Shapefile", append=FALSE) 
   sf::st_write(as(ttops_metrics, "sf"), paste0(outdir, paste0("crown_metrics_point_",outname,"_tws_",tws,"_sws_",sws)), driver="ESRI Shapefile", append=FALSE) 
   sf::st_write(as(crown_metrics, "sf"), paste0(outdir, paste0("crown_metrics_poly_",outname,"_tws_",tws,"_sws_",sws)), driver="ESRI Shapefile", append=FALSE) 
   
 return(list(laz_trees=trees,
             chm=chm,
             tree_top=ttops,
             crown_metrics_point=ttops_metrics,
             crown_metrics_poly=crown_metrics)
        )      
}

###############################################
dir.create(output_folder, recursive = TRUE)

# 04 - list of laz file
lazfiles_names<-list.files(input_folder,".laz")

# brian: option to just do 1 tile...
#lazfiles_names = lazfiles_names[1]

# 05 - Individual tree detection and crown delineation parameters

# TLS synoptic params (decided 11/2 with Eric via zoom)
chm_res=.25 # CHM grid cell size
tws=5 # tree top window search size
sws=3 # smoothing window size

exclusion = 0.2 # For each tree, pixels with an elevation lower than exclusion multiplied by the tree height will be removed. Thus, this number belongs between 0 and 1.

# 05 - empty list to store the products
productslist<-NULL

nruns<-length(lazfiles_names)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nruns, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")


# 06 - ITC and crown metrics computation
# Rocha, K.D.; Silva, C.A.; Cosenza, D.N.; Mohan, M.; Klauberg, C.; Schlickmann, M.B.; Xia, J.; Leite, R.V.; Almeida, D.R.A.d.; Atkins, J.W.; Cardil, A.; Rowell, E.; Parsons, R.; Sánchez-López, N.; Prichard, S.J.; Hudak, A.T. Crown-Level Structure and Fuel Load Characterization from Airborne and Terrestrial Laser Scanning in a Longleaf Pine (Pinus palustris Mill.) Forest Ecosystem. Remote Sens. 2023, 15, 1002. https://doi.org/10.3390/rs15041002

for ( i in 1:length(lazfiles_names)){
  print ( paste( i, "out of",length(lazfiles_names) ))
  print(lazfiles_names[i])

  if  ( i ==1) { start_total <- Sys.time()}
  start_one_file <- Sys.time()
  print(paste('file size:', file.size(paste0(input_folder, "\\",lazfiles_names[i]))))
  
#  setTxtProgressBar(pb, i)
  
    # 4.1 Reading raw lidar data
  laz_norm_i <- lidR::readLAS(paste0(input_folder, "\\",lazfiles_names[i]))

  # voxelize
  #laz_voxel = voxelize_points(laz_norm_i, .5)
  #plot(laz_voxel, voxel = TRUE)
  
  
  plot_i<-ITC_CrownMetrics(las_norm=laz_norm_i,
                 chm_res=chm_res,
                 tws=tws,
                 sws=sws,
                 max_cr_factor = max_cr_factor,
                 exclusion = exclusion,
                 outname =gsub(".laz","",lazfiles_names[i]),
                 outdir=output_folder)
  
  # las_norm = laz_norm_i
  # outname =gsub(".laz","",lazfiles_names[i])
  # outdir = output_folder
  # 
  # chm <- grid_canopy(las_norm, res = chm_res, pitfree(c(0,2,5,10,15), c(0, 1.5)))
  # ker <- matrix(1, sws, sws)
  # plot(chm)
  # 
  # schm <- raster::focal(chm, w = ker, fun = mean, na.rm = TRUE)
  # 
  # plot(schm)
  # ttops <- locate_trees(schm, lmf(tws, 1.3, shape = "circular"))
  # 
  # plot(ttops, add=T)
  # crowns=lidR::silva2016(chm, ttops,max_cr_factor = max_cr_factor,
  #                        exclusion = exclusion,
  #                        ID = "treeID")
  # 
  # contour_sf <- sf::st_as_sf(stars::st_as_stars(crowns()),as_points = FALSE, merge = TRUE)
  # plot(contour_sf, add=T)
  # 
  # contour<-sf::as_Spatial(contour_sf)
  # 
  # colnames(contour@data)[1]<-"treeID"
  # contour@data$CA<-raster::area(contour)
  # contour@data$CR<-sqrt(contour@data$CA/pi)
  
  
  
  
  
  ##########

  names(plot_i)<-paste0(names(plot_i),"_",gsub(".laz","",lazfiles_names[i]))
  productslist<-append(productslist,plot_i)
  print(Sys.time() - start_one_file)

  if (i == nruns){
    close(pb)
    print((Sys.time() - start_total))
    print(paste("Processed", nruns, "laz files"))
  }
}  

