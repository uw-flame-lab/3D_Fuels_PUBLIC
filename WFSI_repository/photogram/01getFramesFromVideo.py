# Title: 01getFramesFromVideo.py
#
# Objective: 
# This script replaces the Manual Steps related to selecting 
# every nth frame of a video. This script is the first step in 
# the photogrammetry process of converting a video to a model. 
# Refer to overviewDiagram.pptx for workflow.
#
# Author(s): Brian Drye
# Date: 3/14/2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# put videos into folder.  Example: D:\01inputVideos
# create an output folder. Example: D:\02pngFilesUnreviewed
# open command prompt, enter "python getFramesFromVideo.py <input path> <output path>" 
# script will create a folder for each video with about 400 images.
#
# Required libraries/modules: 
import sys
import time
import os
from os import walk
#import Metashape
import cv2 
import numpy as np
import datetime
import math


# Input folder path (can also be passed as argument, sys.argv[1]): 
inputVideoDir = 'D:\\inputVideosDevTesting'

# Output folder path (can also be passed as argument, sys.argv[2]): 
outputPath = 'D:\\01pngFilesUnreviewed'

# Additional information: 

# to target certain colors, use the mask_type setting. 
# frames that do not include a threshold number of pixels will be excluded. 
# note: thresholds may need to be adjusted depending on video colors

# mask setting... set masktype to one of the following:
# r_w: red and white
# ry_w: red, yellow, white
# ryb: red, yellow, black (no white)
# ryb_w: red, yellow, black, and white
# yb_w: yellow, black, and white
# y_w: yellow and white
# blue_pole
# pink_pole

mask_type = "ry_w"
threshold_min = 6000
threshold_max = 300000  

numberOfFramesToSkip = 120  # exclude this number of frames from beginning of video

# some time tracking... 
print('scriptLog: time:' + str(datetime.datetime.now()))

# check arguments for paths
if len(sys.argv) > 1:
    inputVideoDir = sys.argv[1]

print('scriptLog: ==============================')
print("scriptLog: Looking for videos in: {}".format(inputVideoDir))

if len(sys.argv) > 2:
    outputPath = sys.argv[2]

print('scriptLog: ==============================')
print("scriptLog: Output folder: {}".format(outputPath))
    
# list videos  in path
videos = []
for (dirpath, dirnames, filenames) in walk(inputVideoDir):
    videos.extend(filenames)
    break

if len(videos) == 0:
    print("scriptLog: Error: no videos found.")
    exit()

print('scriptLog: ==============================')
print("scriptLog: List of videos: ")
for video in videos:
    print('scriptLog: ' + video)

cv2.namedWindow('input', cv2.WINDOW_NORMAL)
cv2.resizeWindow('input', (400, 300))

cv2.namedWindow('mask', cv2.WINDOW_NORMAL)
cv2.resizeWindow('mask', (400, 300))

cv2.namedWindow('erosion', cv2.WINDOW_NORMAL)
cv2.resizeWindow('erosion', (400, 300))

for video in videos:
    print('scriptLog: time:' + str(datetime.datetime.now()))
    print("getting frames for {}...".format(video))
    newpath = outputPath + '\\' + video.replace(" ", "")
    newpath = os.path.splitext(newpath)[0]
    if not os.path.exists(newpath):
        os.makedirs(newpath)
    vidcap = cv2.VideoCapture(inputVideoDir + '\\' + video )
    print('opened: {}'.format(vidcap.isOpened()))
    print('BackendName: {}'.format(vidcap.getBackendName()))
    print('Frames per second: {}'.format(vidcap.get(cv2.CAP_PROP_FPS)))
    print('FOURCC: {}'.format(vidcap.get(cv2.CAP_PROP_FOURCC)))
    totalFrames = vidcap.get(cv2.CAP_PROP_FRAME_COUNT)
    print('Frame Count: {}'.format(totalFrames))

    count = 0
    goodFrames = list()
    while count < totalFrames:
        success,image = vidcap.read()
        if success:
#            gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
#            gray = np.float32(gray)
#            corners = cv2.goodFeaturesToTrack(gray, 100, 0.01, 10)
#            corners = np.int0(corners)
#            for corner in corners:
#                x, y = corner.ravel()
#                cv2.circle(image, (x,y), 30, 255, -1)

            if count < numberOfFramesToSkip:
                count += 1
                continue

            lineimage = np.copy(image) * 0
            hsv = cv2.cvtColor(image, cv2.COLOR_BGR2HSV)

            maskwhite = cv2.inRange(hsv, (0, 0, 0), (0, 0, 0))  # blank initially

            if mask_type != "blue_pole" and mask_type != "pink_pole" and mask_type != "ryb":
                # white 1
                maskWhite1 = cv2.inRange(hsv, (0, 0, 153), (160, 11, 255))
                # white 2
                maskWhite2 = cv2.inRange(hsv, (90, 5, 166), (179, 255, 255))

                # white 3
                lower_white = np.array([0, 0, 215])
                upper_white = np.array([0, 0, 255])
                maskWhite3 = cv2.inRange(hsv, lower_white, upper_white)

                # white 4
                lower_white = np.array([90, 5, 166])
                upper_white = np.array([120, 160, 255])
                maskWhite4 = cv2.inRange(hsv, lower_white, upper_white)

                maskwhite = maskWhite1 + maskWhite2 + maskWhite3 + maskWhite4

            maskRed1 = cv2.inRange(hsv, (0, 0, 0), (0, 0, 0))  # blank initially
            maskRed2 = cv2.inRange(hsv, (0, 0, 0), (0, 0, 0))  # blank initially
            if mask_type == "r_w" or mask_type == "ryb_w" or mask_type == "ryb" or mask_type == "ry_w":
                # red 1
                lower_red = np.array([0, 120, 70])
                upper_red = np.array([10, 255, 255])
                maskRed1 = cv2.inRange(hsv, lower_red, upper_red)

                # red 2
                lower_red = np.array([170, 120, 70])
                upper_red = np.array([180, 255, 255])
                maskRed2 = cv2.inRange(hsv, lower_red, upper_red)

            maskYellow1 = cv2.inRange(hsv, (0, 0, 0), (0, 0, 0))  # blank initially
            maskYellow2 = cv2.inRange(hsv, (0, 0, 0), (0, 0, 0))  # blank initially
            if mask_type == "ryb_w" or mask_type == "y_w" or mask_type == "ryb" or mask_type == "ry_w":
                # yellow 1
                lower_yellow = np.array([20, 207, 205])
                upper_yellow = np.array([41, 255, 255])
                maskYellow1 = cv2.inRange(hsv, lower_yellow, upper_yellow)

                # yellow 2
                lower_yellow = np.array([25, 50, 216])
                upper_yellow = np.array([46, 198, 255])
                maskYellow2 = cv2.inRange(hsv, lower_yellow, upper_yellow)

            maskBlack1 = cv2.inRange(hsv, (0, 0, 0), (0, 0, 0))  # blank initially
            maskBlack2 = cv2.inRange(hsv, (0, 0, 0), (0, 0, 0))  # blank initially
            if mask_type == "ryb_w" or mask_type == "ryb":
                # mask for yellow/black tape (black part)
                lower_black = np.array([100, 82, 14])
                upper_black = np.array([120, 147, 95])
                maskBlack1 = cv2.inRange(hsv, lower_black, upper_black)

                lower_black = np.array([79, 23, 21])
                upper_black = np.array([161, 255, 251])
                maskBlack2 = cv2.inRange(hsv, lower_black, upper_black)

            maskPinkPole1 = cv2.inRange(hsv, (0, 0, 0), (0, 0, 0))  # blank initially
            maskPinkPole2 = cv2.inRange(hsv, (0, 0, 0), (0, 0, 0))  # blank initially
            if mask_type == "pink_pole":
                # aucilla (pink pool noodle)
                lower = np.array([0,63,95])
                upper = np.array([13,128,180])
                maskPinkPole1 = cv2.inRange(hsv, lower, upper)
                #
                lower = np.array([0,63,95])
                upper = np.array([13,128,180])
                maskPinkPole2 = cv2.inRange(hsv, lower, upper)

            maskBluePole1 = cv2.inRange(hsv, (0, 0, 0), (0, 0, 0))  # blank initially
            if mask_type == "blue_pole":
                # Fort Stewart (blue pool noodle)
                lower = np.array([90,104,204])
                upper = np.array([109,255,255])
                maskBluePole1 = cv2.inRange(hsv, lower, upper)

            if mask_type == "r_w":
                maskred = maskRed1 + maskRed2
            if mask_type == "ry_w":
                maskred = maskRed1 + maskRed2 + maskYellow1 + maskYellow2
            if mask_type == "ryb_w" or mask_type == "ryb":
                maskred = maskRed1 + maskRed2 + maskYellow1 + maskYellow2 + maskBlack1 + maskBlack2
            if mask_type == "yb_w":
                maskred = maskYellow1 + maskYellow2 + maskBlack1 + maskBlack2
            if mask_type == "y_w":
                maskred = maskYellow1 + maskYellow2
            if mask_type == "blue_pole":
                maskred = maskBluePole1
            if mask_type == "pink_pole":
                maskred = maskPinkPole1 + maskPinkPole2

            mask = maskred + maskwhite


            # get rid of random pixels in the mask, then use erosion as mask
            kernel = np.ones((2,2), np.uint8)
            erosion = cv2.erode(mask, kernel, iterations = 4)
            
            # display number of pixels in mask that are positive
            numberOfWhitePixelsInMask = cv2.countNonZero(erosion)
            if numberOfWhitePixelsInMask < threshold_min or numberOfWhitePixelsInMask > threshold_max:
                cv2.line(image, (0,0), (400,400), (0,0,255), 100)
                cv2.line(image, (0,400), (400,0), (0,0,255), 100)
                cv2.imshow('input', image)
            else:
                # add good frame to list of frames
                copy = image.copy()
                #cv2.line(copy, (0,0), (400,400), (0,255,0), 100)
                #cv2.line(copy, (0,400), (400,0), (0,255,0), 100)
                cv2.circle(copy, (200,200), 100, (0,255,0), 100)
                goodFrames.append(image)
                print(numberOfWhitePixelsInMask)
                cv2.imshow('input', copy)

            cv2.imshow('mask', mask)
#            cv2.imshow('result', result)
            cv2.imshow('erosion', erosion)
               
        count += 1    
        if cv2.waitKey(1) & 0xFF == ord('q'):
            break
            
    print('Good Frame Count: {}'.format(str(len(goodFrames))))

    if len(goodFrames) < 400:
        print("not many good frames found... adjust mask colors or pixel threshold")

    # select about 400 images from goodFrames
    everyNth = round(len(goodFrames) / 400)
    savedFrameCount = 0
    for i in range(0, len(goodFrames), everyNth):
        cv2.imwrite(newpath + '\\' + "frame%s.png" % str(savedFrameCount).zfill(4), goodFrames[i])
        savedFrameCount += 1

    vidcap.release()
    cv2.destroyAllWindows()
    print('scriptLog: time:' + str(datetime.datetime.now()))    
 
exit()
