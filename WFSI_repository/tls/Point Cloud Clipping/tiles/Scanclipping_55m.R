library(lidR)
library(dplyr)

# --- Paths ---
setwd("E:/FLAME_LAB/Sycan_Grass/")
input_dir <- "E:/FLAME_LAB/Sycan_Grass/clipped/"
output_dir <- "E:/FLAME_LAB/Sycan_Grass/clipped_55m/"

if (!dir.exists(output_dir)) dir.create(output_dir)

# --- List LAZ/LAS files ---
terre_files <- list.files(path = input_dir, pattern = "\\.laz$", full.names = TRUE)

# --- Loop through each file ---
for (file in terre_files) {
  
  # Step 1: Load the LAS file
  las <- readLAS(file)
  if (is.empty(las)) {
    message("Skipping empty file: ", basename(file))
    next
  }
  
  # Step 2: Filter out extreme canopy/ground
  z_range <- quantile(las@data$Z, probs = c(0.05, 0.95), na.rm = TRUE)
  filtered <- filter_poi(las, Z >= z_range[1] & Z <= z_range[2])
  
  if (npoints(filtered) == 0) {
    message("No valid points after filtering: ", basename(file))
    next
  }
  
  # Step 3: Estimate scanner location
  xyz <- filtered@data[, c("X", "Y", "Z")]
  centroid <- colMeans(xyz)
  dists <- sqrt((xyz$X - centroid[1])^2 +
                  (xyz$Y - centroid[2])^2 +
                  (xyz$Z - centroid[3])^2)
  scanner_xyz <- xyz[which.min(dists), ]
  
  message("Scanner location for ", basename(file), ": ",
          paste(round(scanner_xyz, 3), collapse = ", "))
  
  # Step 4: Clip circle of 25m radius around scanner location
  clipped <- clip_circle(las,
                         xcenter = scanner_xyz$X,
                         ycenter = scanner_xyz$Y,
                         radius = 55)
  
  if (is.empty(clipped)) {
    message("No points found within 55m for ", basename(file))
    next
  }
  
  # Step 5: Save clipped file
  out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file)), "_clipped55m.laz"))
  writeLAS(clipped, out_file)
  
  message("Saved clipped file: ", out_file)
}

