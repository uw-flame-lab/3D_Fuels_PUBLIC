
# Title: ui.R (Shiny Clipper Tool)
#
# Objective: 
# server.R and ui.R can be loaded in RStudio and run as a Shiny Application.
# The application provides an interface for segmenting clipplots from a pointcloud.
# This is a convenient alternative to segmenting in Cloud Compare
#
# Author(s): Brian Drye
# Date: 3/14/2023
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
library(leaflet)
library(DT)

# Input folder path: 
# set inputFolder (search below) to location of .laz files

# Output folder path: 
# set outputFolder (search below) to location where clipplot files should be saved
# set spatialFolder (search below) to location where csv of pole locations should be saved

# Other default variables to set: 
# set crs (search below) according to the location of the site

# Additional information: 
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' button (in RStudio).
#


shinyUI(fluidPage(
    # Application title
    titlePanel("Clipper Tool"),

    sidebarLayout(
        mainPanel(
					   fluidRow(
					     wellPanel(
					       verbatimTextOutput("consoleInfo")
					     ),
					     column(12, plotOutput("visualize1", click="plot_click", brush="plot_brush", dblclick="plot_doubleclick")), 
					   ),
					   fluidRow(
					     wellPanel(
					       radioButtons("mode", "Mode:",
					                    c("Crop5x5" = "crop5x5",
					                      "Eraser" = "eraser",
					                      "Single Plot" = "singlePlot"), inline=TRUE),
					       conditionalPanel(condition = "input.mode == 'crop5x5'",
					                        verbatimTextOutput("plot_clickinfo"),
					       ),
					       conditionalPanel(condition = "input.mode == 'eraser'",
					                        actionButton("startOver", "Start Over"),
					                        verbatimTextOutput("plot_brushinfo"), 
					                        tableOutput("plot_brushedpoints")
					       ),
					       conditionalPanel(condition = "input.mode == 'singlePlot'",
					                        fluidRow(
					                          sliderInput("rotatePoints", "Rotate Points",
					                                      min = -45, max = 45,
					                                      value = 0.0, step = 1),
					                          column(width=6, sliderInput("shiftHorizontal", "Horizontal",
					                            min = -0.2, max = .2,
					                            value = 0.0, step = 0.01)),
					                          column(width=6, sliderInput("shiftVertical", "Vertical",
					                            min = -0.2, max = .2,
					                            value = 0.0, step = 0.01))
					                        )
					       ),
					       DTOutput("table1")
					     )
					   )
      ),
    	sidebarPanel(
    	  fluidRow(
            wellPanel(
			        textInput("inputFolder", "Input Folder", value="D:\\temp\\ft_stewart\\data\\tls\\plots\\5x5\\biomass\\norm"),
			        textInput("outputFolder", "Output Folder", value="D:\\temp\\ft_stewart\\tls\\plots\\clipplot"),
			        textInput("spatialFolder", "Spatial Folder", value="D:\\temp\\ft_stewart\\data\\spatial"),
			        
              textInput("crs", "CRS", value="+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"),
              actionButton("loadFiles", "Load Files"),
              selectInput("filenames", "", c()),
              actionButton("loadFile", "Load LAZ"),
              sliderInput("zrange", "Z range",
                          min = -.3, max = 2,
                          value = c(0, 1.3), step = .1),
              sliderInput("reflectance", "Reflectance",
                          min = -1, max = 5,
                          value = -.5, step = .1),
              actionButton("save5x5", "Save 5x5 LAZ"),
              numericInput("plotcount", "Plot Count", 5),
              actionButton("poleCluster", "Run Pole Cluster"),
              actionButton("process5x5", "Process 5x5"),
              actionButton("populatePlots", "Populate Plots"),
              selectInput("plotnumber", "Choose Plot", c()),
              actionButton("clipPlot", "Clip Plot")
            ),
            wellPanel(
              verbatimTextOutput("angleInfo")
              # plotOutput("xHistPlot", height=150),
              # plotOutput("yHistPlot", height=150)
            )
          )
  		)
  		
    )
))
