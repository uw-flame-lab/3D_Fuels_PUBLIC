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


# variables to configure for each site: 
pp = 'L4_'            # plot file name prefix 
csv_pp = 'L4_CP_'     # csv file name prefix

corners = ['NE', 'NW', 'SE', 'SW']

lazpath = 'D:/5x5/bw_laz' # local copy of files  
laspath = 'D:/5x5/bw_las' # .laz files will get converted to .las files and saved here

csvpath = 'D:/' + site + '/spatial/poles'  # location of csv files that contain pole coordinates




# Step 0: setup 
sites = ['blackwater']
#'aucilla','blackwater', 'ft_stewart','glacial', 'hitchiti_a','hitchiti_b','lanl_forest','lanl_grass','lubrecht','methow','osceola','sycan_2a','sycan_forest','sycan_grass','tates_hell_b','tates_hell_a','tenalquot','ttrs_hannah_hammock']
site = sites[0]

# todo: copy files, shp, norm, etc. (verify complete)
#numbers = [1,3,7,8,17,19,23,24,25] #hitichi B 

# (aucilla is done 4/18/2024)
#numbers = [4,9,12,13,16,17,19,23,24] # aucilla CP
#numbers = [2,3,5,6,10,14,15,20,21] # aucilla SO

# todo: redo after DN uploads new .laz files (with correct projection) (in progress)
#numbers = [2,3,4,5,8,11,13,15,17,19,21,25] # (skip?) fort stewart PostFire (no csv files, only one scan)
#numbers = [6,7,10,12,14,16,18,22,24] #fort stewart PostFire_Post_CP (has csvs)(12_SW missing - copied 12_SE as workaround)

# fort stewart (done, but some skips and didn't do SO 4/24/2024)
#numbers = [1,9,20,23] #fort stewart PostFire_SO (no csv files, skipped)
#numbers = [3,5,8,13,15,17,19,21,25] #fort stewart Pre_Dest_CP (no csv for 17,19,21,25)
#numbers = [3,5,8,13,15] #fort stewart Pre_Dest_CP (no csv for 17,19,21,25 -- skipped these)
#numbers = [6,7,10,12,14,16,18,22,24] #fort stewart Pre_NonDest_CP (no csv for 6)
#numbers = [7,10,12,14,16,18,22,24] #fort stewart Pre_NonDest_CP (no csv for 6)
#numbers = [1,9,20,23] #fort stewart SO (no csvs)

# (glacial is done 4/19/2024)
#numbers = [5,6,9,10,14,16,17,19,20] # glacial CP
#numbers = [4,13,18,21] # glacial SO

# ttrs_hannah_hammock (done 4/22/2024)
#numbers = [1,3,6,8,12,15,17,18,21] # hh CP 
#numbers = [5,7,9,10,11,16,23,24,25] # hh SO 

# lanl_forest (done)
#numbers = [2,6,9,10,14,15,18,20] # CP
#numbers = [3,12,16,17] # SO

# hitchiti A (done)
#numbers = [1,3,5,9,12,14,15,17,20] 
#numbers = [7,11,23] # SO

# hitchiti B (already done)
#numbers = [1,3,7,8,10,12,17,19,20,21,23,24,25] 
#numbers = [2,4,5,6,9,11,13,14,15,16,18,22] # SO

# lanl grass (done)
#numbers = [1,6,8,10,14,15,17,20,22] # CP
#numbers = [5,12,21,23] # SO

# methow (done, except for lasground_new.exe step... slow going)
#numbers = [4,6,7,8,13,14,15,18,19] # CP
#numbers = [12,17,22,24] # SO

# lubrecht (done)
#numbers = [1,2,3,4,5,6,7,8,9] # V
#numbers = [1,2,3,4,5,6,7,8,9] # S

# tates_hell_a (done)
# numbers = [2,4,6,7,13,14,15,19,25] # CP_
# numbers = [3,10,11,16] # SO_

# tates_hell_b (done) 
# numbers = [1,4,6,8,9,16,18,19] # CP_   note: no 5
# numbers = [3,13,15] # SO_

# tenalquot (done)
# numbers = [2,5,6,13,16,17,18,19,20] # CP_
# numbers = [1,3,9,10] # SO_

# sycan 2a (done)
# numbers = [2,9,20,22,24] # 
# numbers = [3,4,10] # 

# sycan grass (done)
# numbers = [5,8,9,11,13,17,19,21,24] # Pre 
# numbers = [3,6,7,14,15,18,20,22,25] # SO
# numbers = [1,2,3,4,6,7,10,12,16,17,18] # Post (didn't run, not sure what to do)
# numbers = [5,8,9,13,19,24] # Post Dest
# numbers = [14,15,20,25] # Post SO N/S (didn't run, not sure what to do)

# sycan forest (done)
# numbers = [1,8,11,15,16,18,19,22,23] # '' 
# numbers = [2,6,9,13,14,17,20,24,25] # SO
# numbers = [1,2,9,11,13,16,17,18,19,22,23,24] # Post_ / Post_Dest

# blackwater L1 (note: no scan_only for Blackwater)
# numbers = [6,14] # (done) 6/13/2024
# blackwater L3
# numbers = [2,13] # (done) 6/13/2024
# blackwater L4
numbers = [8,20] # 




# Step 1: see which .laz and/or .csv files might be missing
# copy .laz files from C:/Users/bdrye/Box/External 3D Fuels/data_NEW/' + site + '/tls/synoptic'
# to D:/hb_laz 
# set sites, numbers for each location. get numbers from tracking spreadsheet
# note: change prefix lines: 21, 27
from os import listdir

for site in sites:
    print(site)
    lazFiles = listdir(lazpath)
    #print(lazFiles)
    
    csvFiles = listdir(csvpath)
    #print(csvFiles)

    for n in numbers:
        # check for corner .laz files 
        # 1_NE - SINGLESCANS - 220225_174518.laz
        # CP_1_NE - SINGLESCANS - 220225_174518.laz
        for corner in corners:
            prefix = pp + str(n) + '_' + corner        
            #prefix = str(n) + '_' + corner        
            starts = [n for n, l in enumerate(lazFiles) if l.startswith(prefix)]
            print(starts)
            if len(starts) == 0:
                print('WARNING: LAZ not found: ' + prefix)
        
        csvFileName = csv_pp + str(n) + '_5x5.csv'
        #csvFileName = str(n) + '_5x5.csv'
        #csvFileName = str(n) + '_ScanOnly_5x5.csv'
        if not csvFileName in csvFiles:
            print('WARNING: csv not found: ' + csvFileName)

# if no warnings, the laz and Csv files exist for each numbered location

# Step 2: convert laz files to las files
# 
# hb_laz -> hb_las

# create geoprocessing script
import arcgisscripting
gp = arcgisscripting.create()

# import lastools by using the geoprocessing script
gp.AddToolbox(r"C:\LAStools\ArcGIS_toolbox\LAStools.tbx")

# set wd 
arcpy.env.workspace = lazpath 

for file in arcpy.ListFiles("*.laz"):
    outfile = arcpy.Describe(file).baseName
    outfile = outfile.split()[0] # get rid of spaces 1_NE - SINGLESCANS - 220225_174518.laz
    print(outfile)
    arcpy.gp.laszip(file, 
                    False, 
                    False, 
                    "las", 
                    laspath + '/' + outfile,
                    laspath, 
                    "", 
                    "", 
                    True)


# Step 3: get pole locations, make polygon txt file, 
# clip 4 las files and save as #_5x5.laz
import arcpy
import pandas as pd
import subprocess
from os import listdir

# this is what runs the LAStools command
def check_output(command,console):
    if console == True:
        process = subprocess.Popen(command)
    else:
        process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, universal_newlines=True)
    output,error = process.communicate()
    returncode = process.poll()
    return returncode,output 

for site in sites:
    print(site)
    #laspath = 'C:/Users/bdrye/Box/External 3D Fuels/data_NEW/' + site + '/tls/synoptic'
    lasFiles = listdir(laspath)
    #print(lasFiles)
    
    csvFiles = listdir(csvpath)

    for n in numbers:
#    for n in [20]:
        print("n: " + str(n))
        cornerFiles = []
        for corner in corners:
             prefix = pp + str(n) + '_' + corner  
             #prefix = str(n) + '_' + corner  
            
             index = [n for n, l in enumerate(lasFiles) if l.startswith(prefix)]
             #print(index[0])
             #print(lasFiles[index[0]])
             cornerFiles.append(laspath + '/' + lasFiles[index[0]])
        
        # get polygon coordinates from csv file
        # create a txt file with format below... 
        csvFileName = csv_pp + str(n) + '_5x5.csv'
        #csvFileName = str(n) + '_ScanOnly_5x5.csv'
        csvInfo = pd.read_csv(csvpath + '/' + csvFileName, sep=',')
        print(csvInfo)
        NE_X = csvInfo[(csvInfo.PlotName == 'NE') & (csvInfo.PlotType == 'Pole')].X
        NE_Y = csvInfo[(csvInfo.PlotName == 'NE') & (csvInfo.PlotType == 'Pole')].Y
        NW_X = csvInfo[(csvInfo.PlotName == 'NW') & (csvInfo.PlotType == 'Pole')].X
        NW_Y = csvInfo[(csvInfo.PlotName == 'NW') & (csvInfo.PlotType == 'Pole')].Y
        SW_X = csvInfo[(csvInfo.PlotName == 'SW') & (csvInfo.PlotType == 'Pole')].X
        SW_Y = csvInfo[(csvInfo.PlotName == 'SW') & (csvInfo.PlotType == 'Pole')].Y
        SE_X = csvInfo[(csvInfo.PlotName == 'SE') & (csvInfo.PlotType == 'Pole')].X
        SE_Y = csvInfo[(csvInfo.PlotName == 'SE') & (csvInfo.PlotType == 'Pole')].Y

        if len(NE_X) == 0 or len(NE_Y) == 0 or len(NW_X) == 0 or len(NW_Y) == 0 or len(SW_X) == 0 or len(SW_Y) == 0 or len(SE_X) == 0 or len(SE_Y) == 0:
            print('Error: CSV file is missing a pole. edit csv: change Clip to Pole')
            assert(False)
            
        with open(laspath + '/' + "corners.txt", "w") as file:
            file.write(str(NE_X.to_numpy()[0] + 3) + ' ' + str(NE_Y.to_numpy()[0] + 3) + "\n")
            file.write(str(NW_X.to_numpy()[0] - 3) + ' ' + str(NW_Y.to_numpy()[0] + 3) + "\n")
            file.write(str(SW_X.to_numpy()[0] - 3) + ' ' + str(SW_Y.to_numpy()[0] - 3) + "\n")
            file.write(str(SE_X.to_numpy()[0] + 3) + ' ' + str(SE_Y.to_numpy()[0] - 3) + "\n")
            file.write(str(NE_X.to_numpy()[0] + 3) + ' ' + str(NE_Y.to_numpy()[0] + 3) + "\n")
                        
        # save a .shp file
        from pathlib import Path
        Path(laspath + "/shp").mkdir(parents=True, exist_ok=True)

        coords = [(NE_X.to_numpy()[0]+3, NE_Y.to_numpy()[0]+3), 
                  (NW_X.to_numpy()[0]-3, NW_Y.to_numpy()[0]+3),
                  (SW_X.to_numpy()[0]-3, SW_Y.to_numpy()[0]-3),
                  (SE_X.to_numpy()[0]+3, SE_Y.to_numpy()[0]-3),
                  (NE_X.to_numpy()[0]+3, NE_Y.to_numpy()[0]+3)
                  ]
        ar = arcpy.Array()
        for x, y in coords:
            ar.add(arcpy.Point(x, y))
        polyline = arcpy.Polyline(ar)
        arcpy.management.CopyFeatures(polyline, laspath + "/shp/" + str(n) + '_5x5.shp')
                        
        # clip the 4 corner .las files for this number using a shp file from the csv
        fileString = cornerFiles[0] + " " + cornerFiles[1] + " " + cornerFiles[2] + " " + cornerFiles[3]
        #fileString = "D:/5x5/bw_las/L4_20_NE.las D:/5x5/bw_las/L4_20_NW.las D:/5x5/bw_las/L4_20_SE.las"
        #print(fileString)
        cornerString = laspath + '/corners.txt' 
        #print(cornerString)
        outputString = laspath + '/' + str(n) + '_5x5_3m.laz'   # something like 9_5x5.laz
        #print(outputString)
        callString = "C:/LAStools/bin/lasclip -i " + fileString + " -merged -poly " + cornerString + " -o " + outputString
        callString = callString.replace('/', '\\')
        print(callString)
        #subprocess.call(callString)
        check_output(callString, False)

# Step 4: copy files from D:\5x5\aucilla_las\*.laz to 

#outputPath = 'D:/' + site + '/tls/plots/5x5/biomass/scan'

# Step 5: normalize the *_5x5_3m.laz files using LAStools, copy to D:/<site>/plots/5x5/biomass/norm
# Step 6: repeat (from step 0) for Scan Only .laz files



