# Title: sfmShiftXYvalues.R
#
# Objective: 
# shift Structure From Motion (SFM) clipplot point clouds so they are in same x/y position as TLS clipplots...
# assumes square and units are same scale.
#
# Author(s): Brian Drye
# Date: 5/2/2024
#
# Project: SERDP-funded 3D fuels project (RC19-1064)
#
# How to use this script:
# 1. adjust the getCorrespondingTLSFile() function depending on the file name prefix 
# associated with each site
# 2. set the site
# 3. set the base_path 
# 4. run to update the x/y position of the SFM files (in place)
#
# Required libraries/modules: 
library(stringr)
library(lidR)


# Site: 
site = 'aucilla'

# Base path: 
base_path = "D:/temp/"

# Input folder paths: 
tls_dir = paste0(base_path, site, "/tls/plots/cliplot")
sfm_dir = paste0(base_path, site, "/photogram/cr_sfm/norm")

# Output folder path: n/a

# Additional information: 
# summary of steps: 
# open TLS point cloud
# get upper right corner
# open SFM point cloud
# get upper right corner
# get X/Y differences
# adjust SFM x/y values
# save SFM
# manual test: open TLS and SFM in CloudCompare (should occupy the same space, and hopefully look similar)





options(digits=15)

# notes for various sites. 
# site = 'aucilla' # done
# site = 'blackwater' # done, spot check of 8... 8_SW is rotated 180
# site = 'ft_stewart' # done. sfm = no prefix, tls = 3 possible prefixes: PostFire_Possst_CP_, Pre_Dest_, Pre_NonDest_ 
#                      sfm without corresponding tls: 17, 21, 25
# site = 'glacial'    #done    #ERROR: reading header.file_signature... cannot open lasreaderlas... 
# ERROR: reading header.file_signature
# ERROR: cannot open lasreaderlas with file name '/Users/briandrye/Library/CloudStorage/Box-Box/External 3D Fuels/data_NEW/glacial/photogram/cr_sfm/norm/20_CTR_glacial.laz'
# Error: LASlib internal error. See message above.
# Creation of a LAS object from data but without a header:
#   Scale factors were set to 0.001 and XYZ coordinates were quantized to fit the scale factors.
#site = 'hitchiti_a' # done
# site = 'hitchiti_b' # done
# site = 'lanl_forest' # done, no tls for plot 4
# site = 'lanl_grass' # done
# site = 'lubrecht' # done
# site = 'methow' # done
# site = 'osceola' # done. B1-1, B1-2, B1-3, B1-4, and B2-1, B2-3 = missing TLS files
# site = 'sycan_2a' # done
# site = 'sycan_forest'  # 5_NE & 5_SE missing TLS files
# site = 'sycan_grass' # done
# site = 'tates_hell_a' # done
# site = 'tates_hell_b' # done plot 5 missing from TLS
# site = 'tenalquot' # done
# site = 'ttrs_hannah_hammock' # done

print(tls_dir)
print(sfm_dir)

tlsFiles = list.files(tls_dir, pattern="\\.laz$", full.names = TRUE)
sfmFiles = list.files(sfm_dir, pattern="\\.laz$", full.names = TRUE)


getCorrespondingTLSFile <- function(tlsFiles, sfmFile)
{
  parts = str_split_1(sfmFile, '_')
  
  # osceola
  #parts[1] = str_replace(parts[1], 'ONF', '')
  #parts[1] = str_replace(parts[1], ' - Cloud.laz', '')
  
  prefix = paste0(parts[1], "_", parts[2])
  #prefix = paste0(parts[1])
  print(prefix)
  
  #ft_stewart 3 possible prefixes: PostFire_Post_CP_, Pre_Dest_, Pre_NonDest_  (ignore PostFire_Post_CP_)
#  f1 <- function(x) str_replace(x, 'PostFire_Post_CP_', '')
#  f2 <- function(x) str_replace(x, 'Pre_Dest_', '')
#  f3 <- function(x) str_replace(x, 'Pre_NonDest_', '')

  #glacial, hitchit_a, methow, tenalquot, ttrs_hannah_hammock
  f1 <- function(x) str_replace(x, 'CP_', '')

  # lubrecht
  #f1 <- function(x) str_replace(x, 'V', '')

  # THA/B
#  f1 <- function(x) str_replace(x, 'THB_', '')
  
  temp = tlsFiles
  temp = lapply(tlsFiles, f1)  
#  temp = lapply(temp, f2)  
#  temp = lapply(temp, f3)  
  
  #bases = substring(basename(tlsFiles), 10) # remove first n characters
  for(i in 1:length(temp))
  {
    base = basename(temp[[i]])
    if(startsWith(base, prefix))
    {
      return(tlsFiles[i])
    }
  }
  return('')
}

for(i in 1:length(sfmFiles))
{
  print(sfmFiles[i])
  tlsFile = getCorrespondingTLSFile(tlsFiles, basename(sfmFiles[i]))
  print(tlsFile)
  
  #next  # use to just show corresponding files
  
  if(tlsFile == '')
  {
    print('Error: did not find corresponding TLS file. ')
#    break
    next
  }
  
  tls = readLAS(tlsFile)
  
  tlsMaxX = max(tls$X)
  tlsMaxY = max(tls$Y)
  tlsMinX = min(tls$X)
  tlsMinY = min(tls$Y)
  
  tlsDifX = tlsMaxX - tlsMinX
  tlsDifY = tlsMaxY - tlsMinY
  print(paste("tlsXdif:", tlsDifX))
  print(paste("tlsYdif:", tlsDifY))
  
  sfm = readLAS(sfmFiles[i])
  
  sfmMaxX = max(sfm$X)
  sfmMaxY = max(sfm$Y)
  sfmMinX = min(sfm$X)
  sfmMinY = min(sfm$Y)

  sfmDifX = sfmMaxX - sfmMinX
  sfmDifY = sfmMaxY - sfmMinY
  print(paste("sfmXdif:", sfmDifX))
  print(paste("sfmYdif:", sfmDifY))
  
  xShift = tlsMaxX - sfmMaxX
  yShift = tlsMaxY - sfmMaxY
  
  sfm@header@PHB[["X offset"]] <- tls@header@PHB[["X offset"]]
  sfm@header@PHB[["Y offset"]] <- tls@header@PHB[["Y offset"]]
  sfm@header@PHB[["X scale factor"]] = 0.00025
  sfm@header@PHB[["Y scale factor"]] = 0.00025
  sfm@header@PHB[["Z scale factor"]] = 0.00025
    
  sfm$X = sfm$X + xShift
  sfm$Y = sfm$Y + yShift
    
  fn = tools::file_path_sans_ext(sfmFiles[i])
  
  writeLAS(sfm, paste0(fn, 'Shifted', '.laz'))
}  


