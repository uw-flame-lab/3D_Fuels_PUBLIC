# Title: createModelWithoutMarkersAgisoft.py
#
# Objective: create model(s) based on images without markers within Agisoft Metashape
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
# steps: 
# 0. set input/output folder paths below
# 1. Open Metashape
# 2. Choose Tools->Run Script
# 3. navigate to the location of this script. 
# 4. click OK
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

# Input folder: 
inputPhotoDir = 'D:\\01pngFilesUnreviewed'

# Output folder: 
outputPath = 'D:\\03AgisoftModels'





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


print('scriptLog: ==============================')
print("scriptLog: Looking for photo folders in: ")
print('scriptLog: ' + inputPhotoDir)

print('scriptLog: ==============================')
print("scriptLog: Output folder: ")
print('scriptLog: ' + outputPath)
    
# list photo folders in path
photoFolders = []
for (dirpath, dirnames, filenames) in walk(inputPhotoDir):
    photoFolders.extend(dirnames)
    break

if len(photoFolders) == 0:
    print("scriptLog: Error: no photo folders found.")
    exit()

print('scriptLog: ==============================')
print("scriptLog: List of photo folders: ")
for folder in photoFolders:
    print('scriptLog: ' + folder)


# for each photoCollection, do metashape steps
for photoCollection in photoFolders:
    doc = Metashape.app.document
    doc.clear()
    
    project = photoCollection
 #   print(project)
 #   break
 #   exit
    
    projectName = os.path.splitext(outputPath + '\\' + photoCollection)[0]+'.psx'
    print('scriptLog: ==============================')
    print('scriptLog: Saving... project saved to: ' + projectName)
    doc.save(path=projectName)
    
    photos = (absoluteFilePaths(inputPhotoDir + '\\' + photoCollection))
    chunk = doc.addChunk()
    chunk.addPhotos(photos)
    #do image matching and alignment
    for frame in chunk.frames:
        frame.matchPhotos(downscale=1, keypoint_limit=40000, tiepoint_limit=4000)
        
    chunk.detectMarkers()
    chunk.alignCameras()
    
    # get a list of unaligned cameras
    na_list = list()
    for camera in chunk.cameras:
        if not camera.transform:
            na_list.append(camera)

    print('scriptLog: ==============================')
    print('scriptLog: number of cameras: ' + str(len(chunk.cameras)))

    print('scriptLog: number of unaligned cameras: ' + str(len(na_list)))
    print('scriptLog: Aligning unaligned cameras')
    chunk.alignCameras(na_list)        

    # get a list of unaligned cameras (again)
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
    print('scriptLog: buildDenseCloud...')
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

    
    #break  # uncomment to just do one run

print("scriptLog: Done")

