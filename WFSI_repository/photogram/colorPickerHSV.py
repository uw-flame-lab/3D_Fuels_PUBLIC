# Title: colorPickerHSV.py
#
# Objective: 
# This is a utility script for selecting colors
# use this to get possible color ranges to use 
# in 01getFramesFromVideo.py
# Refer to overviewDiagram.pptx for workflow.
#
# Author(s): Brian Drye
# Date: 3/14/2023
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# take a screenshot or save a frame from the video. Example: D:\Aucilla_12_NW_GOPR0271.JPG
# run the script
# experiment with the mask ranges below to test different ranges
# press the 'q' key to exit the script
#
# Required libraries/modules: 

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.widgets import Slider, Button
import math
import cv2
import pandas as pd


# Input example path: 
inputImage = 'D:\\Aucilla_12_NW_GOPR0271.JPG'

# Output path: n/a


def doNothing(x):
    pass


cv2.namedWindow('Track Bars', cv2.WINDOW_NORMAL)

cv2.createTrackbar('H1min', 'Track Bars', 1, 179, doNothing)
cv2.createTrackbar('S1min', 'Track Bars', 0, 255, doNothing)
cv2.createTrackbar('V1min', 'Track Bars', 0, 255, doNothing)
cv2.createTrackbar('H1max', 'Track Bars', 1, 179, doNothing)
cv2.createTrackbar('S1max', 'Track Bars', 0, 255, doNothing)
cv2.createTrackbar('V1max', 'Track Bars', 0, 255, doNothing)

cv2.createTrackbar('H2min', 'Track Bars', 1, 179, doNothing)
cv2.createTrackbar('S2min', 'Track Bars', 0, 255, doNothing)
cv2.createTrackbar('V2min', 'Track Bars', 0, 255, doNothing)
cv2.createTrackbar('H2max', 'Track Bars', 1, 179, doNothing)
cv2.createTrackbar('S2max', 'Track Bars', 0, 255, doNothing)
cv2.createTrackbar('V2max', 'Track Bars', 0, 255, doNothing)

# reading the image
image = cv2.imread(inputImage)

cv2.namedWindow('source', cv2.WINDOW_NORMAL)
cv2.resizeWindow('source', (400, 300))

cv2.namedWindow('standard masks', cv2.WINDOW_NORMAL)
cv2.resizeWindow('standard masks', (400, 300))
cv2.namedWindow('custom1', cv2.WINDOW_NORMAL)
cv2.resizeWindow('custom1', (400, 300))
cv2.namedWindow('custom2', cv2.WINDOW_NORMAL)
cv2.resizeWindow('custom2', (400, 300))
cv2.namedWindow('custom (1 + 2)', cv2.WINDOW_NORMAL)
cv2.resizeWindow('custom (1 + 2)', (400, 300))

hsv = cv2.cvtColor(image, cv2.COLOR_BGR2HSV)

while True:
    h1min = cv2.getTrackbarPos('H1min', 'Track Bars')
    s1min = cv2.getTrackbarPos('S1min', 'Track Bars')
    v1min = cv2.getTrackbarPos('V1min', 'Track Bars')

    h1max = cv2.getTrackbarPos('H1max', 'Track Bars')
    s1max = cv2.getTrackbarPos('S1max', 'Track Bars')
    v1max = cv2.getTrackbarPos('V1max', 'Track Bars')

    h2min = cv2.getTrackbarPos('H2min', 'Track Bars')
    s2min = cv2.getTrackbarPos('S2min', 'Track Bars')
    v2min = cv2.getTrackbarPos('V2min', 'Track Bars')

    h2max = cv2.getTrackbarPos('H2max', 'Track Bars')
    s2max = cv2.getTrackbarPos('S2max', 'Track Bars')
    v2max = cv2.getTrackbarPos('V2max', 'Track Bars')

    # custom
    custom1 = cv2.inRange(hsv, (h1min,s1min,v1min), (h1max,s1max,v1max))
    # red 2
    custom2 = cv2.inRange(hsv, (h2min,s2min,v2min), (h2max,s2max,v2max))

    custom = custom1 + custom2

    ############### standard masks ##################
    # red 1
    lower_red = np.array([0, 120, 70])
    upper_red = np.array([10, 255, 255])
    mask = cv2.inRange(hsv, lower_red, upper_red)

    # red 2
    lower_red = np.array([170, 120, 70])
    upper_red = np.array([180, 255, 255])
    mask2 = cv2.inRange(hsv, lower_red, upper_red)

    # white 1
    lower_white = np.array([0, 0, 215])
    upper_white = np.array([0, 0, 255])
    mask3 = cv2.inRange(hsv, lower_white, upper_white)

    # white 2
    lower_white = np.array([90, 5, 166])
    upper_white = np.array([120, 160, 255])
    mask4 = cv2.inRange(hsv, lower_white, upper_white)

    # yellow 1
    lower_yellow = np.array([20, 207, 205])
    upper_yellow = np.array([41, 255, 255])
    mask5 = cv2.inRange(hsv, lower_yellow, upper_yellow)

    # yellow 2
    lower_yellow = np.array([25, 50, 216])
    upper_yellow = np.array([46, 198, 255])
    mask6 = cv2.inRange(hsv, lower_yellow, upper_yellow)

    # mask for yellow/black tape (black part) ( only use if frame has black tape)
    lower_black = np.array([100, 82, 14])
    upper_black = np.array([120,147,95])
    mask7 = cv2.inRange(hsv, lower_black, upper_black)

    lower_black = np.array([79,23,21])
    upper_black = np.array([161,255,251])
    mask8 = cv2.inRange(hsv, lower_black, upper_black)

    mask = mask + mask2 + mask3 + mask4 + mask5 + mask6 + mask7 + mask8
    ############### standard masks ##################

    # showing the mask image
    cv2.imshow('source', image)
    cv2.imshow('standard masks', mask)
    cv2.imshow('custom1', custom1)
    cv2.imshow('custom2', custom2)
    cv2.imshow('custom (1 + 2)', custom)

    # checking if q key is pressed to break out of loop
    key = cv2.waitKey(25)
    if key == ord('q'):
        break
