# Title: createModelAfterMarkersAgisoft.py
#
# Objective: create models after markers have been placed within Agisoft Metashape
# Refer to overviewDiagram.pptx for workflow.
#
# Author(s): Brian Drye
# Date: 2/23/2021
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# After running other scripts to create a folder full of .png files, 
# copy the folder (or folders) to D:\inputPhotosDevTesting\
# note: you can have multiple image folders (the script will run for each folder). 
#
# If you have anything open in Metashape that you want to save, save it now. 
# The script will wipe out your project
#
# Automated steps: 
# 1. Open Metashape
# 2. Choose Tools->Run Script
# 3. navigate to the location of this script. 
# 4. For arguments, enter the input and output folders
#    note: you can also change the default paths in the code below
# 5. click OK
#
# Expected outputs (SycanMarshGrassPlot24NWPreburn):
# SycanMarshGrassPlot24NWPreburn.files (folder containing all model related files)
# SycanMarshGrassPlot24NWPreburn.psx (Metashape project file, just refers to .files folder)
# SycanMarshGrassPlot24NWPreburn.3ds (Mesh)
# SycanMarshGrassPlot24NWPreburn.laz (Point cloud)
#
# Required libraries/modules: 

import sys
import time
import os
from os import walk
import Metashape


# Input folder path: 
inputProjectDir = 'D:\\inputPhotosDevTesting'

# Output folder path:
outputPath = 'D:\\outputFromStep1'

# Additional information: 
# requires licensed install of Agisoft Metashape 
# note: RealityCapture might be a free alternative to Agisoft Metashape

def absoluteFilePaths(directory):
    for dirpath,_,filenames in os.walk(directory):
        fileList = []
        for f in filenames:
            fileList.append(os.path.abspath(os.path.join(dirpath, f)))
    return fileList


# might do some time tracking... 
#ticks = time.time()
#print('==============================')
#print("Number of ticks since 12:00am, January 1, 1970:{}", ticks)
#print(time.time())


# check arguments for paths
if len(sys.argv) > 1:
    inputProjectDir = sys.argv[1]

print('scriptLog: ==============================')
print("scriptLog: Looking for photo folders in: ")
print('scriptLog: ' + sys.argv[1])

if len(sys.argv) > 2:
    outputPath = sys.argv[2]

print('scriptLog: ==============================')
print("scriptLog: Output folder: ")
print('scriptLog: ' + outputPath)
    
# list projects in path
psxProjects = []
for (dirpath, dirnames, filenames) in walk(inputProjectDir):
    psxProjects.extend(filenames)
    break

if len(psxProjects) == 0:
    print("scriptLog: Error: no psx project files found.")
    exit()

print('scriptLog: ==============================')
print("scriptLog: List of projects: ")
for project in psxProjects:
    print('scriptLog: ' + inputProjectDir + '\\' + project)



# Debug option: Edit to run against a specific psx 
# comment out to run against all .psx files in folder
# psxProjects = ["MWA_7_GX010262.psx"]

# for each project...
for project in psxProjects:
    doc = Metashape.app.document
    doc.open(inputProjectDir + '\\' + project)
    
    projectName = outputPath + '\\' + project
    print('scriptLog: ==============================')
    print('scriptLog: Saving... project saved to: ' + projectName)
    doc.save(path=projectName)

    chunk = doc.chunk
    
    # get a list of unaligned cameras
    na_list = list()
    for camera in chunk.cameras:
        if not camera.transform:
            na_list.append(camera)

    # should have less unaligned cameras now
    print('scriptLog: ==============================')
    print('scriptLog: number of unaligned cameras: ' + str(len(na_list)))    
  
    print('scriptLog: ==============================')
    print('scriptLog: buildDepthMaps...')
    chunk.buildDepthMaps(downscale=4, filter_mode=Metashape.MildFiltering)
    doc.save()
    
    
    print('scriptLog: ==============================')
#    print('scriptLog: buildDenseCloud... older version of Metashape (on Brian's home computer)')
#    chunk.buildDenseCloud(point_colors=True, )
    print('scriptLog: buildPointCloud...')       # buildDenseCloud is now buildPointCloud
    chunk.buildPointCloud(point_colors=True, )
    doc.save()

    print('scriptLog: ==============================')
    print('scriptLog: buildModel...')
    chunk.buildModel(surface_type=Metashape.Arbitrary, interpolation=Metashape.EnabledInterpolation, face_count=Metashape.HighFaceCount)
    doc.save()
    print('scriptLog: ==============================')
    print('scriptLog: buildUV...')
    chunk.buildUV(mapping_mode=Metashape.GenericMapping)
    doc.save()
    print('scriptLog: ==============================')
    print('scriptLog: buildTexture...')
    chunk.buildTexture(blending_mode=Metashape.MosaicBlending, texture_size=4096, fill_holes=True, ghosting_filter=True,)
    doc.save()
    print('scriptLog: ==============================')
    print('scriptLog: buildDEM...')
    chunk.buildDem()
    doc.save()
    print('scriptLog: ==============================')
    print('scriptLog: buildOrthomosaic...')
    chunk.buildOrthomosaic(fill_holes=True)
    doc.save()

    # exports... 

    print('scriptLog: ==============================')
    print('scriptLog: exportPoints... (LAZ)')
    pointPath = outputPath + '\\' + project + '.laz'
    #chunk.exportPoints(path=pointPath, format=Metashape.PointsFormat.PointsFormatLAZ)
    chunk.exportPointCloud(path=pointPath, format=Metashape.PointCloudFormat.PointCloudFormatLAZ)
    doc.save()    

    print('scriptLog: ==============================')
    print('scriptLog: exportModel... (3DS)')
    modelPath = outputPath + '\\' + project + '.3ds'
    chunk.exportModel(path=modelPath, format=Metashape.ModelFormat.ModelFormat3DS)
    doc.save()    
    
    # this won't work on sample images from "doll", but should work on plot models
    print('scriptLog: ==============================')
    print('scriptLog: exportRaster... (GeoTIFF)')
    geoTiffPath = outputPath + '\\' + project + '.tif'
    chunk.exportRaster(path=geoTiffPath, image_format=Metashape.ImageFormat.ImageFormatTIFF)
    doc.save()

    
#    break  # uncomment to just do one run

print("scriptLog: Done")

