library(lidR)
library(stringr)

# --- Define directories ---
input_dir  <- "" # where the segmented tiles are
output_dir <- "" # where the individual trees are to be saved


# Create output directory if needed
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- List all LAS/LAZ files ---
las_files <- list.files(input_dir, pattern = "\\.(las|laz)$", full.names = TRUE)

# --- Loop through all segmented files ---
for (file in las_files) {
  cat("Processing:", basename(file), "\n")
  
  # Read file
  las <- readLAS(file)
  if (is.empty(las)) {
    cat("Skipping empty file:", basename(file), "\n")
    next
  }
  
  # Make sure PredInstance column exists
  if (!"PredInstance" %in% names(las@data)) {
    cat("No PredInstance column in:", basename(file), "\n")
    next
  }
  
  # Get unique tree IDs (excluding 0 or NA if present)
  tree_ids <- unique(las$PredInstance)
  tree_ids <- tree_ids[!is.na(tree_ids) & tree_ids != 0]
  
  # Extract prefix from filename (e.g., tile_226350_3353350)
  prefix <- tools::file_path_sans_ext(basename(file))
  
  
  # Loop over each tree ID
  for (id in tree_ids) {
    tree_points <- filter_poi(las, PredInstance == id)
    if (is.empty(tree_points)) next
    
    # Build output filename: e.g. FARO_100_1_tree_267.laz
    out_name <- paste0(prefix, "_tree_", id, ".laz")
    out_path <- file.path(output_dir, out_name)
    
    # Write file
    writeLAS(tree_points, out_path)
  }
  
  cat("Finished:", basename(file), "→", length(tree_ids), "trees saved.\n\n")
}

cat("✅ All tree point clouds extracted successfully!\n")
