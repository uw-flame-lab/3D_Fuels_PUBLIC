# Title: server.R (Shiny Clipper Tool)
#
# Objective: 
# server.R and ui.R can be loaded in RStudio and run as a Shiny Application.
# The application provides an interface for segmenting clipplots from a pointcloud.
# This is a convenient alternative to segmenting in Cloud Compare
#
# Author(s): Brian Drye
# Date: 3/14/2022
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# in ui.R the default paths can be set
# in the application the paths can be entered manually
# load the .laz file
# use mouse clicks to do initial cropping
# use drag select to erase unwanted stray points
# double click to add points (sometimes used to make pole locations easier to detect/cluster)
# cluster the point clouds
# select each clip plot to orient and save
#
# Required libraries/modules: 
library(shiny)
library(reactlog)
library(terra)
library(purrr)
library(ggplot2)
require(Rvcg)
require(VoxR)
require(onion)
require(misc3d)
require(rgl)
require(moments)
require(lidR)
library(RCSF)
library(rgl)
library(stringr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(sf)
library(rgdal)
library(DT)

# Input folder: set in ui.R 
# Output folder: set in ui.R


# tell shiny to log all reactivity
reactlog_enable()
set.seed(123)

options(digits=10)

MBR <- function(p) {
  # Analyze the convex hull edges
  a <- chull(p)                                   # Indexes of external points
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



shinyServer(function(input, output, session) {
  
    rv <- reactiveValues(
      consoleInfo = "",
      totalAngleChange = 0,
      df = data.frame(X=rnorm(20), Y=rnorm(20), Z=rnorm(20)),
      lazFileIndex = 1,
      files = NULL,
      m = 1,
      u = 1,
      lazFileInfoText = "",
      PlotInfoText = "", 
      inData = NULL,
      inDataSub = NULL,
      inDataSubPreErase = NULL,
      V.Store = NULL,
      V.Store.NoPoles = NULL,
      plot_name = "",
      newcenter = NULL,
      temp = NULL, 
      clipBox = NULL,
      dfNoRotation = NULL, 
      mbrCenterX = 0,
      mbrCenterY = 0, 
      totalPoints = "", 
      plotToShow = "",
      k2 = NULL, 
      Temp_Poles = NULL, 
      Polecenter = NULL,
      K_Poles = NULL, 
      namelist = NULL,
      densityAngle = 0, 
      overviewRotation = 0,
      croplines = NULL,
      cropLeft = 0,
      cropRight = 0,
      cropTop = 0, 
      cropBottom = 0, 
      point1 = NULL,
      point2 = NULL,
      point3 = NULL,
      point4 = NULL, 
      overviewDf = NULL
    )
        
    inputFolder <- reactive({
      if(is.null(input$inputFolder))
      {
        return(NULL)
      }
      message(input$inputFolder)
      input$inputFolder
    })
    
    observeEvent(input$loadFiles, {
      #load a file from input folder
      rv$files = list.files(inputFolder(), full.names = T, pattern = "\\.laz$")
      fileChoices = rv$files
      names(fileChoices) <- list.files(inputFolder(), full.names = F, pattern = "\\.laz$")
      updateSelectInput(session, "filenames", label = "Choose file", fileChoices)
    })
    
    observeEvent(input$startOver, {
      rv$inDataSub = rv$inDataSubPreErase
    })
    
    output$table1 <- renderDT({
      datatable(rv$V.Store, editable = TRUE)
    })
    
    observeEvent(input$reflectance, {
      rv$inDataSub = subset(rv$inData, rv$inData$Reflectance >= input$reflectance)
      rv$inDataSub = subset(rv$inDataSub, rv$inDataSub$Z >= input$zrange[1] & rv$inDataSub$Z <= input$zrange[2])
      # forget deviation
    })
    observeEvent(input$zrange, {
      rv$inDataSub = subset(rv$inData, rv$inData$Reflectance >= input$reflectance)
      rv$inDataSub = subset(rv$inDataSub, rv$inDataSub$Z >= input$zrange[1] & rv$inDataSub$Z <= input$zrange[2])
    })
    
    observeEvent(input$table1_cell_edit, {
      row  <- input$table1_cell_edit$row
      clmn <- input$table1_cell_edit$col
      rv$V.Store[row, clmn] <- input$table1_cell_edit$value
    })
    
    output$plot_clickinfo <- renderPrint({
      # cat("Click:\n")
      # paste(str(input$plot_click$x), str(input$plot_click$y))
      print(paste("X:", userClicks$x))
      print(paste("Y:", userClicks$y))
    })
    
    # output$plot_brushinfo <- renderPrint({
    #   if(input$mode == "eraser")
    #   {
    #     cat("Brush (debounced):\n")
    #     str(input$plot_brush)
    #   }
    # })
    
    output$plot_brushedpoints <- renderTable({
      if(!is.null(rv$overviewDf) & input$mode == "eraser")
      {
        bpoints <- brushedPoints(rv$overviewDf, input$plot_brush, "X", "Y")
        if (nrow(bpoints) == 0)
        {
          return()
        }
        # else
        # {
        #   # set reflectance of brushed points to 0
        #   rowsToUpdate = match(c(rv$inData$X, rv$inData$Y), c(bpoints$X, bpoints$Y))
        #   rv$inData@data[rowsToUpdate, "Reflectance"] = 0
        # }
        bpoints
      }
    })
    
    observeEvent(input$plot_doubleclick, {
        poleToAdd = data.frame(X=rep(input$plot_doubleclick$x, 100), Y=rep(input$plot_doubleclick$y, 100), Z=1)
        rv$inDataSub@data = rbind(rv$inDataSub@data, poleToAdd, fill=TRUE)
    })
    
    observeEvent(input$plot_brush, {
      bpoints <- brushedPoints(rv$overviewDf, input$plot_brush, "X", "Y")
      if (nrow(bpoints) == 0)
      {
        return()
      }
      else
      {
        xmin = input$plot_brush$xmin
        xmax = input$plot_brush$xmax
        ymin = input$plot_brush$ymin
        ymax = input$plot_brush$ymax
        
        # set reflectance of points in brush area to something below -0.5... 
        # rv$inDataSub@data[rv$inDataSub@data$X > xmin & rv$inDataSub@data$X < xmax & rv$inDataSub@data$Y > ymin & rv$inDataSub@data$Y < ymax, "Reflectance"] = -1
        # remove points in window... 
        
        rv$inDataSub = subset(rv$inDataSub, !(rv$inDataSub@data$X > xmin & rv$inDataSub@data$X < xmax & rv$inDataSub@data$Y > ymin & rv$inDataSub@data$Y < ymax))
      }
    })
    
    userClicks <- reactiveValues(x=NULL, y=NULL)
    
    observeEvent(plot_click_slow(), {
      if(input$mode == "crop5x5")
      {
        clickX = round(plot_click_slow()$x, 2)
        clickY = round(plot_click_slow()$y, 2)
        if(length(userClicks$x) < 4)
        {
          userClicks$x <- c(userClicks$x, clickX)
          userClicks$y <- c(userClicks$y, clickY)
        }
        else
        {
          userClicks$x = NULL
          userClicks$y = NULL
        }
      }
    })
    
    ## Don't fire off the plot click too often
    plot_click_slow <- debounce(reactive(input$plot_click), 300)
    
    observeEvent(input$save5x5, {
      #inData.sub = subset(rv$inData@data, rv$inData@data$Reflectance >= -0.5)
      #plot(inData.sub$X, inData.sub$Y, col = "blue")
      #mbr5x5 = MBR(matrix(c(inData.sub$X, inData.sub$Y), ncol=2))
      # rotate point cloud by rv$overviewRotation (could be slow?)
      # tempRotation <-
      #   recexcavAAR::rotate(
      #     rv$inData@data$X,
      #     rv$inData@data$Y,
      #     rv$inData@data$Z,
      #     degrx = 0,
      #     degry = 0,
      #     degrz = ((-1) * mean(rv$overviewRotation)),
      #   )
      
      # create folder if it doesn't already exist
      dir.create(file.path(input$outputFolder, "5x5"))
      area5x5Clip <- clip_polygon(rv$inData, rv$croplines[, 1], rv$croplines[, 2])
      writeLAS(area5x5Clip, paste0(input$outputFolder, "/5x5/", rv$plot_name, "_5x5.laz"))
      
      # save a copy so we can restore if accidentally erase too much
      rv$inDataSubPreErase = rv$inDataSub

      # set rv$inData to roi
      rv$inData = area5x5Clip  #bdtest
      rv$inDataSub = NULL
      
      # rv$inDataSub = subset(rv$inData, rv$inData$Reflectance >= rv$reflectance)
      # rv$inDataSub = subset(rv$inDataSub, rv$inDataSub$Deviation < input$deviation)
      
      updateRadioButtons(session, "mode", selected="eraser")
    })

    runPoleCluster <-function()
    {
      # inData  <- rv$inData
      # inData.sub = subset(inData@data, inData@data$Reflectance >= -0.5)
      # plot(inData.sub$X, inData.sub$Y, col = "red")
      # 
      # inData.sub = subset(inData.sub, inData.sub$Deviation < input$deviation)
      df = rv$inDataSub@data[, 1:3]
      df_poles = subset(df, df$Z > 0.5)
      
      #todo: fix this... 
      K_Poles <- kmeans(df_poles, centers = input$plotcount + 4, nstart = 100)
      Temp_Poles = data.frame(df_poles, K_Poles$cluster)
      Polecenter = data.frame(K_Poles$centers[, 1], K_Poles$centers[, 2])
      colnames(Polecenter) <- c("X", "Y")
      #  ggplot(Temp_Poles,aes(X,Y)) + geom_point(aes(colour = factor(K_Poles$cluster), size = 4))+ geom_point(data=Polecenter, shape=19, size=4) + ggtitle(plot_name)
      
      rv$Temp_Poles = Temp_Poles
      rv$Polecenter = Polecenter
      rv$K_Poles = K_Poles
      
      rv$plotToShow = "poleCluster"
    }
    
    observeEvent(input$loadFile, {
      rv$debugInfo = ""
      rv$croplines = NULL
      userClicks$x = NULL
      userClicks$y = NULL

      if(is.null(input$filenames) | input$filenames == '')
      {
        rv$debugInfo = "Suggestion: Load Files first"
        return(NULL)
      }
      
      inData  <- readLAS(input$filenames)
      plot_name <- basename(input$filenames)
      
      #depending on location this replacement will be different...
      # tates_hell_a, tates_hell_b, sycan_forest, sycan_grass
      plot_name <-
        str_replace(plot_name, "_ClipPlot_norm_1cm.laz", "")
      # glacial, hitchiti, lubrecht,
      plot_name <- str_replace(plot_name, "_norm_1cm.laz", "")
      
      rv$lazFileInfoText = plot_name
      rv$plot_name = plot_name
      rv$inData = inData
      rv$inDataSub = NULL
      
      rv$plotToShow = "overview"
      updateRadioButtons(session, "mode", selected="crop5x5")
    })
    
    observeEvent(input$poleCluster, {
      runPoleCluster()
    })

    doKmeans <- reactive({
      rv$debugInfo = ""
      if(is.null(input$filenames) | input$filenames == '')
      {
        rv$debugInfo = "Suggestion: Load Files first"
        return(NULL)
      }
      V.Store <- data.frame(
        id = "",
        X = numeric(9),
        Y = numeric(9),
        Cluster = numeric(9),
        number = numeric(9),
        Angle = numeric(9),
        PlotName = character(9),
        PlotType = character(9),
        Dist = numeric(9)
      )
      
      l = 1

      withProgress(message = 'Processing... ', min=0, max=9,value = 0, {
        Temp_Poles = rv$Temp_Poles
        Polecenter = rv$Polecenter
        

        meanx = mean(Temp_Poles$X)
        meany = mean(Temp_Poles$Y)
        center = data.frame(meanx, meany)
        colnames(center) <- c("X", "Y")
        
        for (t in 1:(input$plotcount+4)) {
          Xpair <- merge(center$X, Polecenter[t,]$X)
          Ypair <- merge(center$Y, Polecenter[t,]$Y)
          names(Xpair) <- c("x1", "x2")
          names(Ypair) <- c("y1", "y2")
          dx <- c(Xpair$x2 - Xpair$x1)
          dy <- c(Ypair$y2 - Ypair$y1)
          
          V.Store$id[l] = rv$plot_name
          V.Store$X[l] = Polecenter$X[t]
          V.Store$Y[l] = Polecenter$Y[t]
          V.Store$Cluster[l] = unique(Temp_Poles[which(Temp_Poles$K_Poles.cluster == t),]$K_Poles.cluster)
          V.Store$number[l] = length(which(Temp_Poles$K_Poles.cluster == t))
          V.Store$Angle[l] = atan2(dx, dy) * 180 / pi
          if (V.Store$Angle[l] < 0) {
            V.Store$Angle[l] = V.Store$Angle[l] + 360
          }
          if (V.Store$Angle[l] > 0) {
            if (V.Store$Angle[l] < 90) {
              V.Store$PlotName[l] = "NE"
            }
          }
          if (V.Store$Angle[l] > 90) {
            if (V.Store$Angle[l] < 180) {
              V.Store$PlotName[l] = "SE"
            }
          }
          if (V.Store$Angle[l] > 180) {
            if (V.Store$Angle[l] < 270) {
              V.Store$PlotName[l] = "SW"
            }
          }
          if (V.Store$Angle[l] > 270) {
            if (V.Store$Angle[l] < 360) {
              V.Store$PlotName[l] = "NW"
            }
          }
          V.Store$Dist[l] = sqrt(dx ^ 2 + dy ^ 2)
          if (V.Store$Dist[l] < 0.9) {
            V.Store$PlotName[l] = "CTR"
          }
          if (V.Store$Dist[l] < 2.5) {
            V.Store$PlotType[l] = "Clip"
          }
          if (V.Store$Dist[l] > 2.5) {
            V.Store$PlotType[l] = "Pole"
          }
          l = l + 1
          setProgress(value=t)
        }

        if(input$plotcount == 0)
        {
          # this only breaks out of WithProgress(){...}
          return(NULL)
        }
        
        df = rv$inDataSub@data[, 1:3]
        plot(df$X, df$Y, col = "blue")
        
        
        df_Plots = subset(df, df$Z < 0.25)
        
        k2 <- kmeans(df_Plots, centers = input$plotcount, nstart = 25)
        temp = data.frame(df_Plots, k2$cluster)
        temp1 = k2$centers
        newcenter = data.frame(temp1[, 1], temp1[, 2])
        colnames(newcenter) <- c("X", "Y")
        ggplot(temp,aes(X,Y)) + geom_point(aes(colour = factor(k2$cluster), size = 4)) + geom_point(data=newcenter, shape=19, size=8) + ggtitle(rv$plot_name)
        # ignore poles...
        #V.Store.NoPoles = V.Store[V.Store$PlotType == "Clip", ]
        
      })
      
      rv$V.Store = V.Store
      rv$newcenter = newcenter
      rv$temp = temp
      rv$k2 = k2
      
      V.Store.NoPoles = V.Store[V.Store$PlotType == "Clip", ]
      if(length(V.Store.NoPoles$Cluster) != max(k2$cluster))
      {
        poles = paste0(V.Store[V.Store$PlotType == "Pole", "PlotName"], collapse = ",")
        clips = paste0(V.Store[V.Store$PlotType == "Clip", "PlotName"], collapse = ",")
        rv$debugInfo = paste("ERROR: clip count doesn't match cluster count. Edit table. Poles: ", poles, " Clips: ", clips)
      }
    })    

    observeEvent(input$process5x5, {
      doKmeans()
    })
    
    observeEvent(input$populatePlots, {
      V.Store = rv$V.Store
      newcenter = rv$newcenter
      temp = rv$temp
      k2 = rv$k2
      
      # ignore poles...
      V.Store.NoPoles = V.Store[V.Store$PlotType == "Clip", ]
      if(input$plotcount == 0)
      {
        return(NULL)
      }
      
      rv$V.Store.NoPoles = V.Store.NoPoles
      
      namelist = list()
      for(ii in 1:max(k2$cluster))
      {
        TVOX = temp[which(temp$k2.cluster == ii),]
        plotvox = vox(TVOX[,1:3], res=.01)
        for(jj in 0:10)
        {
          margin = .1 + jj*(.01) 
          ClipID = subset(V.Store.NoPoles, V.Store.NoPoles$X < max(plotvox$x)+margin & V.Store.NoPoles$X > min(plotvox$x)-margin & V.Store.NoPoles$Y < max(plotvox$y)+margin & V.Store.NoPoles$Y > min(plotvox$y)-margin)
          if(length(ClipID$Cluster) >= 1)
          {
            break
          }
          print(paste("pole not found near plot", ii, "with margin ", margin))
        }
        if(length(ClipID$Cluster) == 0 )
        {
          print(paste("ERROR: plot ", ii,  "pole not near enough to plot"))
        }
        else
        {
          if(length(ClipID$Cluster) > 1)  # take first ???
          {
            print(paste("warning: plot", ii, "multiple poles found nearby, taking first in list"))
            namelist[[ClipID$PlotName[1]]] = ii
          }
          else
          {
            namelist[[ClipID$PlotName]] = ii
          }
        }
      }
      
      rv$namelist = namelist
      rv$plotToShow = "5x5"
      updateSelectInput(session, "plotnumber", choices=namelist, selected="")
      
      updateRadioButtons(session, "mode", selected="singlePlot")
      # save .csv of V.Store
      write.csv(rv$V.Store, paste0(input$spatialFolder, "/poles/", rv$plot_name, "_5x5.csv"))
    })
    
    observeEvent(input$plotnumber, {
      if(input$plotnumber == ""){
        rv$debugInfo = "Suggestion: Load Files, crop, erase, save 5x5"
        return(NULL)
      }
      
      updateSliderInput(session, "rotatePoints", value=0)
      updateSliderInput(session, "shiftHorizontal", value=0)
      updateSliderInput(session, "shiftVertical", value=0)
      
        newcenter = rv$newcenter
        temp = rv$temp
        V.Store.NoPoles = rv$V.Store.NoPoles
        plot_name = rv$plot_name
        

        u = strtoi(input$plotnumber)
        m=1
        TVOX = temp[which(temp$k2.cluster == u),]
        plotvox = vox(TVOX[,1:3], res=.01)
        plot(plotvox$x, plotvox$y)
        
        # get name from plotnumber
        whichPlot = names(rv$namelist)[u]

        rv$plotInfoText = paste(plot_name, whichPlot)

        rv$dfNoRotation = data.frame(X=TVOX$X, Y=TVOX$Y)
        
        # use bounding box to find middle of points
        p = matrix(c(TVOX$X, TVOX$Y), ncol = 2)
        mbr <- MBR(p)
        
        # todo: test this... mbr method might work better???
        # roughCenterX = mean(mbr[1:4,1])
        # roughCenterY = mean(mbr[1:4,2])
        
        roughCenterX = newcenter$X[strtoi(input$plotnumber)]
        roughCenterY = newcenter$Y[strtoi(input$plotnumber)]
        
        # get rid of outliers
        temp = TVOX
        temp = temp[temp$X > roughCenterX - .4 & temp$X < roughCenterX + .4 &
                      temp$Y > roughCenterY - .4 & temp$Y < roughCenterY + .4, ]
        
        plot(TVOX$X, TVOX$Y)
        points(temp$X, temp$Y, col="red")

        # redo mbr to get more accurate center point (or do average????)
        # do average... assume sides have similar point counts
        # if not can shift manually
        # or adjust based on dominant x/y of most populated sides
        mbr <- MBR(matrix(c(temp$X, temp$Y), ncol=2))
        rv$mbrCenterX = roughCenterX = mean(mbr[1:4,1])
        rv$mbrCenterY = roughCenterY = mean(mbr[1:4,2])
        
        lines(mbr)
        
        # try average x/y... 
        # rv$mbrCenterX = mean(temp$X)
        # rv$mbrCenterY = mean(temp$Y)
        
        points(rv$mbrCenterX, rv$mbrCenterY, col="blue")
         
        rv$clipBox = data.frame(x=c(rv$mbrCenterX-.25, rv$mbrCenterX-.25, rv$mbrCenterX+.25, rv$mbrCenterX+.25, rv$mbrCenterX-.25), 
                                 y=c(rv$mbrCenterY-.25, rv$mbrCenterY+.25, rv$mbrCenterY+.25,rv$mbrCenterY-.25, rv$mbrCenterY-.25))
        
        lines(rv$clipBox, col="green")
        
        # loop through angles to find bins with most points (to find likely edges)
        upperEdgeY = 0
        lowerEdgeY = 0
        leftEdgeX = 0
        rightEdgeX = 0
        angleUpperEdge = 0
        angleLowerEdge = 0
        angleRightEdge = 0
        angleLeftEdge = 0 
        angleUpperEdgeD = 0
        angleLowerEdgeD = 0
        angleRightEdgeD = 0
        angleLeftEdgeD = 0 
        upperEdgePointCount = 0
        lowerEdgePointCount = 0
        leftEdgePointCount = 0
        rightEdgePointCount = 0
        topDensitySave = 0
        bottomDensitySave = 0
        rightDensitySave = 0
        leftDensitySave = 0
        totalPoints = 0
        
        for(angleIncrement in -45:45)
        {
            tempRotation <-
              recexcavAAR::rotate(
                TVOX$X,
                TVOX$Y,
                TVOX$Z,
                degrx = 0,
                degry = 0,
                degrz = ((-1) * mean(angleIncrement)),
                pivotx = rv$mbrCenterX,
                pivoty = rv$mbrCenterY,
                pivotz = 0
              )
            
            if(length(tempRotation[tempRotation$x > roughCenterX + .2, "x"]) > 20)
            {
              rightDensity = density(tempRotation[tempRotation$x > roughCenterX + .2, "x"])
              if(max(rightDensity$y) > rightDensitySave)
              {
                angleRightEdgeD = angleIncrement
                rightDensitySave = max(rightDensity$y)
              }
            }
            if(length(tempRotation[tempRotation$x < roughCenterX - .2, "x"]) > 20)
            {
              leftDensity = density(tempRotation[tempRotation$x < roughCenterX - .2, "x"])
              if(max(leftDensity$y) > leftDensitySave)
              {
                angleLeftEdgeD = angleIncrement
                leftDensitySave = max(leftDensity$y)
              }
            }
            if(length(tempRotation[tempRotation$y < roughCenterY - .2, "y"]) > 20)
            {
              bottomDensity = density(tempRotation[tempRotation$y < roughCenterY - .2, "y"])
              if(max(bottomDensity$y) > bottomDensitySave)
              {
                angleLowerEdgeD = angleIncrement
                bottomDensitySave = max(bottomDensity$y)
              }
            }
            if(length(tempRotation[tempRotation$y > roughCenterY + .2, "y"]) > 20)
            {
              topDensity = density(tempRotation[tempRotation$y > roughCenterY + .2, "y"])
              if(max(topDensity$y) > topDensitySave)
              {
                angleUpperEdgeD = angleIncrement
                topDensitySave = max(topDensity$y)
              }
            }

            
            # simplify this... 
            # get point count within rectangle
            toppoints = length(tempRotation[tempRotation$y > roughCenterY + .22 & tempRotation$y < roughCenterY + .28 &
                      tempRotation$x > roughCenterX-.22 & tempRotation$x < roughCenterX+.22, "x"])

            bottompoints = length(tempRotation[tempRotation$y > roughCenterY - .28 & tempRotation$y < roughCenterY - .22 &
                                           tempRotation$x > roughCenterX-.22 & tempRotation$x < roughCenterX+.22, "x"])

            rightpoints = length(tempRotation[tempRotation$y > roughCenterY - .22 & tempRotation$y < roughCenterY + .22 &
                                           tempRotation$x > roughCenterX+.22 & tempRotation$x < roughCenterX+.28, "x"])

            leftpoints = length(tempRotation[tempRotation$y > roughCenterY - .22 & tempRotation$y < roughCenterY + .22 &
                                           tempRotation$x > roughCenterX-.28 & tempRotation$x < roughCenterX-.22, "x"])
            if(toppoints + bottompoints + rightpoints + leftpoints > totalPoints)
            {
              angleToRotate = angleIncrement
              totalPoints = toppoints + bottompoints + rightpoints + leftpoints
            }
            
                        
            # plot(density(tempRotation[tempRotation$y > roughCenterY + .2, "y"]), col=2, yaxt="n", xaxt="n",
            #      bty='n', xlab="", ylab="", main='')
            # axis(4, las=1)
        }
        print(paste("UpperEdge density:", angleUpperEdgeD, "count:", topDensitySave))
        print(paste("LowerEdge density:", angleLowerEdgeD, "count:", bottomDensitySave))
        print(paste("RightEdge density:", angleRightEdgeD, "count:", rightDensitySave))
        print(paste("LeftEdge density:", angleLeftEdgeD, "count:", leftDensitySave))
        

        # print(paste("UpperEdge Angle estimate:", angleUpperEdge, "count:", upperEdgePointCount))
        # print(paste("LowerEdge Angle estimate:", angleLowerEdge, "count:", lowerEdgePointCount))
        # print(paste("RightEdge Angle estimate:", angleRightEdge, "count:", rightEdgePointCount))
        # print(paste("LeftEdge Angle estimate:", angleLeftEdge, "count:", leftEdgePointCount))
        
        rv$totalPoints = totalPoints
        
        # use angle from side with most points... 
        angleDf = data.frame(angle=c(angleUpperEdgeD, angleLowerEdgeD, angleRightEdgeD, angleLeftEdgeD),
                             ptcount=c(topDensitySave, bottomDensitySave, rightDensitySave, leftDensitySave))
        rv$densityAngle = angleDf[angleDf$ptcount == max(angleDf$ptcount), "angle"]

        # center point of clipBox might be far off... center based on sides?? 
        # rv$mbrCenterX = (angleDf$value[3] + angleDf$value[4])/2
        # rv$mbrCenterY = (angleDf$value[1] + angleDf$value[2])/2
        # 
        # rv$clipBox = data.frame(x=c(rv$mbrCenterX-.25, rv$mbrCenterX-.25, rv$mbrCenterX+.25, rv$mbrCenterX+.25, rv$mbrCenterX-.25), 
        #                         y=c(rv$mbrCenterY-.25, rv$mbrCenterY+.25, rv$mbrCenterY+.25,rv$mbrCenterY-.25, rv$mbrCenterY-.25))
        
        # update slider
        
        rv$defaultAngle = angleToRotate
        updateSliderInput(session, "rotatePoints", value=angleToRotate)
        
        # plot(TVOX$X, TVOX$Y)
        # lines(rv$clipBox)
        # points(rv$mbrCenterX, rv$mbrCenterY, col="Red")
#        points(newcenter$X[strtoi(input$plotnumber)], newcenter$Y[strtoi(input$plotnumber)], col="red")
        
        rv$plotToShow = "singlePlot"
        rv$df = data.frame(X=TVOX$X, Y=TVOX$Y, Z=TVOX$Z)
    })

    
    
    observeEvent(input$clipPlot, {
      # rotate and shift .5 by .5 square
      # to correct position in inData
      # note: clipBox has 5 points so pivot default of average
      # doesn't work... center will be off. 
      withProgress(message = 'Processing... ', min=0, max=8, value = 0, {
      
      whichPlot = names(rv$namelist)[strtoi(input$plotnumber)]
        
      tempBox = rv$clipBox
      tempBox$x = tempBox$x - input$shiftHorizontal
      tempBox$y = tempBox$y - input$shiftVertical
      
      tempBox <-
        recexcavAAR::rotate(
          tempBox$x,
          tempBox$y,
          c(0, 0, 0, 0, 0, 0),
          degrx = 0,
          degry = 0,
          degrz =  mean(rv$totalAngleChange),
          pivotx = rv$mbrCenterX,
          pivoty = rv$mbrCenterY,
          pivotz = 0
        )
      # sanity check
      boxRotated = matrix(c(tempBox$x, tempBox$y), ncol = 2)
      plot(rv$dfNoRotation$X, rv$dfNoRotation$Y)
          lines(boxRotated)

      # get roi from inData
      singlePlotClip <-
        clip_polygon(rv$inData, boxRotated[, 1], boxRotated[, 2])
        #plot(singlePlotClip)
        
        # rotate roi (singlePlotClip)
        tempPlotRotation <-
          recexcavAAR::rotate(
            singlePlotClip$X,
            singlePlotClip$Y,
            singlePlotClip$Z,
            degrx = 0,
            degry = 0,
            degrz =  (-1)* mean(rv$totalAngleChange),
            pivotx = rv$mbrCenterX,
            pivoty = rv$mbrCenterY,
            pivotz = 0
          )
        singlePlotClip$X = tempPlotRotation$x
        singlePlotClip$Y = tempPlotRotation$y
        singlePlotClip$Z = tempPlotRotation$z
        
        setProgress(value = 2)
        # pole removal code... 
        nonveg <-
          filter_poi(singlePlotClip,
                     singlePlotClip$Intensity > 60000,
                     singlePlotClip$Z > .4)
        veg <-
          filter_poi(singlePlotClip, singlePlotClip$Intensity < 60000)
        
        # if there are over 35 points detected, do cylinder clipping
        if (!is.empty(nonveg) && length(nonveg$X) > 35)
        {
          # 3d plot (show detected high intensity points)
          # x <- plot(veg, color = "Intensity", axis=TRUE)
          # plot(nonveg, color = "Intensity", pal="blue", size=3, add = x)
          
          # filter out points near x/y
          q <- quantile(nonveg$X, probs = c(.1,  .9))
          circleX = (q[1] + q[2]) / 2
          q <- quantile(nonveg$Y, probs = c(.1,  .9))
          circleY =  (q[1] + q[2]) / 2
          
          nopole <-
            filter_poi(veg, ((veg$X - circleX) ^ 2 + (veg$Y - circleY) ^ 2 > .004) |
                         (veg$Z < .3))
          
          extractedPoints <-
            filter_poi(veg, ((veg$X - circleX) ^ 2 + (veg$Y - circleY) ^ 2 <= .004) &
                         (veg$Z >= .3))
          
          x <- plot(nopole, color = "Intensity", axis = TRUE)
          plot(extractedPoints,
               color = "Intensity",
               pal = "blue",
               add = x)
          
          singlePlotClip <- nopole
        }

        setProgress(value = 4)
        
        # normalize
        # singlePlotClip@header@PHB[["X scale factor"]] <-
        #   0.000001 # this corrects for header errors
        # singlePlotClip@header@PHB[["Y scale factor"]] <-
        #   0.000001 # this corrects for header errors
        # singlePlotClip@header@PHB[["Z scale factor"]] <-
        #   0.000001 # this corrects for header errors
        # tlsCSF <-
        #   CSF(
        #     singlePlotClip@data,
        #     sloop_smooth = FALSE,
        #     class_threshold = 0.1,
        #     cloth_resolution = .1,
        #     rigidness = 1L,
        #     iterations = 500L,
        #     time_step = 0.65
        #   )
        # singlePlotClip@data$Classification[tlsCSF] <- 2
        # 
        # # this is best (no ridges and good resolution) (note: Gina had keep_lowest = TRUE)
        # tlsDTM <-
        #   grid_terrain(
        #     singlePlotClip,
        #     algorithm = knnidw(k = 6L, p = 2),
        #     res = .01,
        #     keep_lowest = TRUE,
        #     use_class = c(2L),
        #     full_raster = FALSE,
        #     is_concave = TRUE
        #   )
        # # plot(tlsDTM)
        # tlsNorm <-
        #   normalize_height(singlePlotClip, tlsDTM, add_lasattribute = TRUE)
        # tlsNorm@data$Classification <-
        #   as.integer(tlsNorm@data$Classification)
        # 
        # setProgress(value = 6)
        #         
        # # if tlsNorm contains sub zero Z values, make zero
        # tlsNorm@data$Z <-
        #   ifelse(tlsNorm@data$Z < 0, 0, tlsNorm@data$Z)
        
        print(paste0(input$outputFolder, "/laz2/", rv$plot_name, "_", whichPlot, ": wrote .laz"))
        
        dir.create(file.path(input$outputFolder, "laz2"))
        
        # save shapefile (of rotated/shifted square which is used to crop original data)
        # v <- vect(tempBox, c("x","y"))  # plain 4 points

        z <- rbind(cbind(object=1, part=1, tempBox[, c("x", "y")], hole=0))
        d <- data.frame(rv$V.Store[rv$V.Store$PlotName == whichPlot & rv$V.Store$PlotType == "Clip", ])

        # crs ... "+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs"
        v <- vect(as.matrix(z), type="polygons", atts=d, crs=input$crs)
        # plot(v)
        
        # save to "spatial" just in site folder... 
        dir.create(input$spatialFolder)
        writeVector(v, paste0(input$spatialFolder, "/clipplot/", rv$plot_name, "_", whichPlot, ".shp"), overwrite=TRUE)
        
        writeLAS(singlePlotClip, paste0(input$outputFolder, rv$plot_name, "_", whichPlot, ".laz"))
        
        setProgress(value = 8)
      })        
    })
    
    output$visualize1 <- renderPlot({
      if(rv$plotToShow == "overview")
      {
        if(is.null(rv$inDataSub))
        {
          rv$inDataSub = subset(rv$inData, rv$inData$Reflectance >= input$reflectance)
          rv$inDataSub = subset(rv$inDataSub, rv$inDataSub$Z >= input$zrange[1] & rv$inDataSub$Z <= input$zrange[2])
        }
        df = rv$inDataSub@data[, 1:3]
        
        rv$overviewDf = df

        plot(df$X, df$Y, col = "blue")
        if(length(userClicks$x) >= 2)
        {
          # close polygon (4 points back to first point)
          rv$croplines = matrix(c(
                                  c(userClicks$x, userClicks$x[1]),
                                  c(userClicks$y, userClicks$y[1])), ncol=2)

          lines(rv$croplines, col="red")
        }
        else
        {
          points(userClicks$x, userClicks$y, col="red", pch=19)
        }
      }
      if(rv$plotToShow == "5x5")
      {
        plot5x5 = ggplot(rv$temp, aes(X,Y)) + geom_point(aes(colour = factor(rv$k2$cluster), size = 2)) + 
          geom_point(data=rv$newcenter, shape=3, size=4) + 
          geom_point(data=rv$V.Store.NoPoles, shape=19, col="blue", size=4)
          ggtitle(rv$plot_name)
        return(plot5x5)
      }
      if(rv$plotToShow == "poleCluster")
      {
        plotPoles = ggplot(rv$Temp_Poles,aes(X,Y)) + geom_point(aes(colour = factor(rv$K_Poles$cluster), size = 5))+ geom_point(data=rv$Polecenter, shape=19, size=3) + ggtitle(rv$plot_name)
        return(plotPoles)        
      }
      if(rv$plotToShow == "singlePlot" & !is.null(rv$clipBox) & !is.null(rv$df))
      {
        tempdf = rv$df
        rv$totalAngleChange = input$rotatePoints 
        angleToRotate = input$rotatePoints
        tempRotation <-
          recexcavAAR::rotate(
            tempdf$X,
            tempdf$Y,
            tempdf$Z,
            degrx = 0,
            degry = 0,
            degrz = ((-1) * mean(angleToRotate)),
            pivotx = rv$mbrCenterX,
            pivoty = rv$mbrCenterY,
            pivotz = 0
          )
        tempdf$X = tempRotation$x
        tempdf$Y = tempRotation$y
        tempdf$Z = tempRotation$z
        
        rv$tempdf = tempdf # for histograms... 
        
        tempdf$X = tempdf$X + input$shiftHorizontal
        tempdf$Y = tempdf$Y + input$shiftVertical
        
        xlimitleft = mean(rv$df$X)-.4
        xlimitright = mean(rv$df$X)+.4
        
        ylimittop = mean(rv$df$Y)+.4
        ylimitbottom = mean(rv$df$Y)-.4
        
        if(xlimitleft > min(rv$clipBox$x))
        {
          xlimitleft = min(rv$clipBox$x)
        }
        if(xlimitright < max(rv$clipBox$x))
        {
          xlimitright = max(rv$clipBox$x)
        }
        if(ylimitbottom > min(rv$clipBox$y))
        {
          ylimitbottom = min(rv$clipBox$y)
        }
        if(ylimittop < max(rv$clipBox$y))
        {
          ylimittop = max(rv$clipBox$y)
        }
        
        # top (get center + .22 to center + .28 )
        toppoints = length(tempdf[tempdf$Y > rv$mbrCenterY + .22 & tempdf$Y < rv$mbrCenterY + .28 &
                                          tempdf$X > rv$mbrCenterX - .22 & tempdf$X < rv$mbrCenterX + .22, "X"])
        
        bottompoints = length(tempdf[tempdf$Y > rv$mbrCenterY - .28 & tempdf$Y < rv$mbrCenterY - .22 &
                                             tempdf$X > rv$mbrCenterX-.22 & tempdf$X < rv$mbrCenterX+.22, "X"])
        
        rightpoints = length(tempdf[tempdf$Y > rv$mbrCenterY - .22 & tempdf$Y < rv$mbrCenterY + .22 &
                                            tempdf$X > rv$mbrCenterX+.22 & tempdf$X < rv$mbrCenterX+.28, "X"])
        
        leftpoints = length(tempdf[tempdf$Y > rv$mbrCenterY - .22 & tempdf$Y < rv$mbrCenterY + .22 &
                                           tempdf$X > rv$mbrCenterX-.28 & tempdf$X < rv$mbrCenterX-.22, "X"])
        rv$totalPoints = toppoints + bottompoints + rightpoints + leftpoints
        
        
        
        topArea = data.frame(x=c(rv$mbrCenterX-.22, rv$mbrCenterX-.22, rv$mbrCenterX+.22, rv$mbrCenterX+.22, rv$mbrCenterX-.22), 
                             y=c(rv$mbrCenterY+.22, rv$mbrCenterY+.28, rv$mbrCenterY+.28,rv$mbrCenterY+.22, rv$mbrCenterY+.22))
        

        bottomArea = data.frame(x=c(rv$mbrCenterX-.22, rv$mbrCenterX-.22, rv$mbrCenterX+.22, rv$mbrCenterX+.22, rv$mbrCenterX-.22), 
                             y=c(rv$mbrCenterY-.28, rv$mbrCenterY-.22, rv$mbrCenterY-.22,rv$mbrCenterY-.28, rv$mbrCenterY-.28))

        rightArea = data.frame(x=c(rv$mbrCenterX+.22, rv$mbrCenterX+.22, rv$mbrCenterX+.28, rv$mbrCenterX+.28, rv$mbrCenterX+.22), 
                             y=c(rv$mbrCenterY-.22, rv$mbrCenterY+.22, rv$mbrCenterY+.22,rv$mbrCenterY-.22, rv$mbrCenterY-.22))

        leftArea = data.frame(x=c(rv$mbrCenterX-.28, rv$mbrCenterX-.28, rv$mbrCenterX-.22, rv$mbrCenterX-.22, rv$mbrCenterX-.28), 
                             y=c(rv$mbrCenterY-.22, rv$mbrCenterY+.22, rv$mbrCenterY+.22,rv$mbrCenterY-.22, rv$mbrCenterY-.22))
        
        
        
        
        ggp <- ggplot(tempdf, aes(X, Y)) +    # Basic ggplot2 plot
          geom_point(color="grey46") +
          geom_path(data=rv$clipBox, aes(x,y), color="blue", size=2) + 
          geom_path(data=topArea, aes(x,y), color="blue") +
          geom_path(data=rightArea, aes(x,y), color="blue") +
          geom_path(data=bottomArea, aes(x,y), color="blue") +
          geom_path(data=leftArea, aes(x,y), color="blue") +
          geom_point(data=data.frame(X=rv$mbrCenterX + input$shiftHorizontal, Y=rv$mbrCenterY+ input$shiftVertical), size=4,color="firebrick3") +
          xlim(xlimitleft, xlimitright) +
          ylim(ylimitbottom, ylimittop) 
        ggp
      }
    }, height=400, width=440)
    
    output$consoleInfo <- renderText({
      print(paste("5X5:", rv$lazFileInfoText, "Plot:", rv$plotInfoText, "Rotation:", rv$totalAngleChange, rv$debugInfo))
    })
    
    output$angleInfo <- renderText({
      print( paste("points:", rv$totalPoints, "\ndefault angle: ", rv$defaultAngle, "\ndensity angle:", rv$densityAngle))
    })
})

# once app has closed, display reactlog from shiny
#shiny::reactlogShow()
