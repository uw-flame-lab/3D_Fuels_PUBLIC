clip5x5with3mBufferArcGisNotebook.ipynb = python notebook, run in context of ArcGIS Pro 
clip5x5with3mBufferArcGisNotebook.py = same code as above exported as plain python file. 
clip5x5with3mBufferArcGisNotebookOsceola.ipynb = python notebook. Osceola has different file naming. 

crop5x5with3mBufferFromTiles.R - R script for cropping 5 meter plots with 3 meter buffer from tile files. Uses pole csv files to create a polygon. This is an alternative to clipping from the 4 corner scans. 


# Title: clip5x5with3mBufferArcGisNotebook.py
#
# Objective: 
# This script is an export of the python notebook of the same name. 
# run the notebook in the context of ArcGIS Pro to create a .laz file of a plot with a 3 meter buffer. 
# normally, each plot contains 5 1/2 meter clipplots. 
#
# Author(s): Brian Drye
# Date: 3/14/2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# gather plot .laz files into a folder (lazpath)
# gather corresponding .csv files that have pole locations (csvpath)
# configure prefix strings if necessary (pp & csv_pp)
# run in the context of ArcGIS Pro as a python notebook. 
# run each step individually
# end result is .laz files that have 3m buffer
# also a .shp file is produced with the four corners.   
#
# Required libraries/modules: 
# ArcGIS Pro 
# LAStools
# import arcgisscripting
# gp = arcgisscripting.create()
# import lastools by using the geoprocessing script
# gp.AddToolbox(r"C:\LAStools\ArcGIS_toolbox\LAStools.tbx")