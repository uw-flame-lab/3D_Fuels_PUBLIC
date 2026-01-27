# tree_metrics_summary_tiles.R
# -------------------------------------------------------
# Computes per-tree metrics from individual tree point clouds
# named like: tile_226350_3353350_tree_267.laz
# Adds InBuffer = y/n based on a buffer edge (default 5 m)
# -------------------------------------------------------

# -------------------------------------------------------
# Libraries
# -------------------------------------------------------
library(data.table)
library(dplyr)
library(stringr)
library(lidR)
library(ITSMe)

# -------------------------------------------------------
# User paths / parameters
# -------------------------------------------------------
pc_folder   <- ""  # folder with individual tree point clouds (*.las/*.laz)
out_csv     <- ""  # e.g., "D:/ALK/FARO_Processing/tree_metrics_summary_tiles.csv"

tile_size_m <- 50   # <-- SET THIS to your actual tile width/height (meters)
buffer_m    <- 5    # overlap buffer width (meters)

# -------------------------------------------------------
# Helper: extract tile origin (xmin,ymin) and treeid from filename
# Expected patterns (robust):
#   tile_226350_3353350_tree_267.laz
#   tile_226350_3353350-tree-267.laz
# -------------------------------------------------------
extract_tile_ids <- function(fname) {
  bn <- basename(fname)
  
  # Try explicit pattern tile_<x>_<y>_tree_<id>
  m <- str_match(
    bn,
    "tile[_\\-](\\d+)[_\\-](\\d+).*?(?:tree|t)[_\\-]?(\\d+)"
  )
  
  if (all(is.na(m))) {
    # fallback: take first 3 numbers found in the name
    nums <- str_extract_all(bn, "\\d+")[[1]]
    if (length(nums) >= 3) {
      return(list(tile_x = as.numeric(nums[1]),
                  tile_y = as.numeric(nums[2]),
                  treeid = as.integer(nums[3])))
    } else {
      return(list(tile_x = NA_real_, tile_y = NA_real_, treeid = NA_integer_))
    }
  }
  
  list(tile_x = as.numeric(m[2]),
       tile_y = as.numeric(m[3]),
       treeid = as.integer(m[4]))
}

# -------------------------------------------------------
# Helper: determine if a point (x,y) is in the buffer band of a tile
# Assumes tile bounds:
#   [tile_x, tile_x + tile_size_m] x [tile_y, tile_y + tile_size_m]
# Core (non-buffer) bounds:
#   [tile_x + buffer_m, tile_x + tile_size_m - buffer_m] similarly for y
# InBuffer = y if position lies OUTSIDE the core but still in the tile
# -------------------------------------------------------
in_buffer_flag <- function(x, y, tile_x, tile_y, tile_size_m, buffer_m) {
  if (any(is.na(c(x, y, tile_x, tile_y, tile_size_m, buffer_m)))) return(NA_character_)
  
  xmin <- tile_x
  ymin <- tile_y
  xmax <- tile_x + tile_size_m
  ymax <- tile_y + tile_size_m
  
  # core bounds (inside these => NOT buffer)
  core_xmin <- xmin + buffer_m
  core_xmax <- xmax - buffer_m
  core_ymin <- ymin + buffer_m
  core_ymax <- ymax - buffer_m
  
  # If outside tile entirely, still mark NA (shouldn't happen if naming matches)
  if (x < xmin || x > xmax || y < ymin || y > ymax) return(NA_character_)
  
  in_core <- (x >= core_xmin && x <= core_xmax && y >= core_ymin && y <= core_ymax)
  if (in_core) "n" else "y"
}

# -------------------------------------------------------
# Find input files
# -------------------------------------------------------
files <- list.files(
  pc_folder,
  pattern = "\\.(laz|las)$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(files) == 0) {
  stop("No LAS/LAZ files found in: ", pc_folder)
}

# -------------------------------------------------------
# Output table (ONLY requested columns)
# -------------------------------------------------------
cols <- c(
  "file", "treeid", "total_points",
  "X_pos", "Y_pos", "InBuffer",
  "DBH_m", "DBH_funct_m", "DBH_residual",
  "height_m", "projected_area_m2", "alpha_volume_m3",
  "crown_points_n", "notes"
)

out <- data.table(matrix(nrow = 0, ncol = length(cols)))
setnames(out, cols)

# -------------------------------------------------------
# Loop and compute metrics
# -------------------------------------------------------
for (i in seq_along(files)) {
  f <- files[i]
  message(sprintf("[%d/%d] Processing: %s", i, length(files), basename(f)))
  
  ids <- extract_tile_ids(f)
  tile_x <- ids$tile_x
  tile_y <- ids$tile_y
  treeid <- ids$treeid
  
  # default row
  row <- as.list(rep(NA, length(cols)))
  names(row) <- cols
  row$file  <- basename(f)
  row$treeid <- treeid
  row$notes <- NA_character_
  
  tryCatch({
    las <- readLAS(f)
    if (is.null(las) || is.empty(las)) {
      row$notes <- "Empty or unreadable LAS/LAZ"
      out <- rbind(out, as.data.table(row), fill = TRUE)
      next
    }

    las_df <- as.data.frame(las@data)
    
    # ensure XYZ
    if (!all(c("X", "Y", "Z") %in% names(las_df))) {
      coords <- try(lidR::xyz(las), silent = TRUE)
      if (inherits(coords, "try-error") || is.null(coords)) {
        row$notes <- "Couldn't extract XYZ from LAS"
        out <- rbind(out, as.data.table(row), fill = TRUE)
        next
      } else {
        las_df <- data.frame(X = coords[, 1], Y = coords[, 2], Z = coords[, 3])
      }
    } else {
      las_df <- las_df %>% dplyr::select(X, Y, Z)
    }
    
    row$total_points <- nrow(las_df)
    pc <- las_df
    
    # ---------- tree position ----------
    pos <- tryCatch({
      p <- tree_position_pc(pc = pc)
      if (is.list(p)) {
        list(X = as.numeric(p$X), Y = as.numeric(p$Y))
      } else if (is.numeric(p) && length(p) >= 2) {
        list(X = as.numeric(p[1]), Y = as.numeric(p[2]))
      } else {
        list(X = NA_real_, Y = NA_real_)
      }
    }, error = function(e) {
      list(X = NA_real_, Y = NA_real_)
    })
    
    row$X_pos <- pos$X
    row$Y_pos <- pos$Y
    
    # ---------- InBuffer (y/n) ----------
    row$InBuffer <- in_buffer_flag(
      x = row$X_pos, y = row$Y_pos,
      tile_x = tile_x, tile_y = tile_y,
      tile_size_m = tile_size_m, buffer_m = buffer_m
    )
    
    # ---------- DBH ----------
    dbh_out <- tryCatch({
      dbh_pc(pc = pc, plot = FALSE, functional = TRUE)
    }, error = function(e) NULL)
    
    if (!is.null(dbh_out)) {
      row$DBH_m         <- if (!is.null(dbh_out$dbh))  dbh_out$dbh  else NA_real_
      row$DBH_funct_m   <- if (!is.null(dbh_out$fdbh)) dbh_out$fdbh else NA_real_
      row$DBH_residual  <- if (!is.null(dbh_out$R2))   dbh_out$R2   else NA_real_
    }
    
    # ---------- height ----------
    row$height_m <- tryCatch({
      th <- tree_height_pc(pc = pc, plot = FALSE)
      if (is.list(th) && !is.null(th$h)) th$h else as.numeric(th)
    }, error = function(e) NA_real_)
    
    # ---------- crown classification ----------
    crown_out <- tryCatch({
      classify_crown_pc(pc = pc, plot = FALSE)
    }, error = function(e) NULL)
    
    crown_pc <- NULL
    if (!is.null(crown_out) && !is.null(crown_out$crownpoints)) {
      crown_pc <- as.data.frame(crown_out$crownpoints)
      row$crown_points_n <- nrow(crown_pc)
    } else {
      row$crown_points_n <- NA_integer_
    }
    
    # ---------- projected area / alpha volume ----------
    proj_in <- if (!is.null(crown_pc) && nrow(crown_pc) > 0) crown_pc else pc
    
    row$projected_area_m2 <- tryCatch({
      pa <- projected_area_pc(pc = proj_in, plot = FALSE)
      if (is.list(pa) && !is.null(pa$pa)) pa$pa else as.numeric(pa)
    }, error = function(e) NA_real_)
    
    row$alpha_volume_m3 <- tryCatch({
      av <- alpha_volume_pc(pc = proj_in, plot = FALSE)
      if (is.list(av) && !is.null(av$av)) av$av else as.numeric(av)
    }, error = function(e) NA_real_)
    
    out <- rbind(out, as.data.table(row), fill = TRUE)
    
    rm(las)
    gc()
    
  }, error = function(e) {
    row$notes <- paste0("Fatal error: ", e$message)
    out <- rbind(out, as.data.table(row), fill = TRUE)
  })
}

# -------------------------------------------------------
# Save CSV
# -------------------------------------------------------
setcolorder(out, cols)
fwrite(out, out_csv)
message("Saved summary CSV to: ", out_csv)

