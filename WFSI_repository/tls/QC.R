library(lidR)
library(dplyr)

# ---- Directory with LAZ files ----
input_dir <- "D:/FlameLab/TLS/Lubrecht/ThinFinal/"

laz_files <- list.files(input_dir, pattern = "\\.laz$", full.names = TRUE)

results <- data.frame(
  file = basename(laz_files),
  readable = NA,
  n_points = NA,
  error_msg = NA,
  stringsAsFactors = FALSE
)

for (i in seq_along(laz_files)) {
  f <- laz_files[i]
  
  tryCatch({
    las <- readLAS(f)
    
    if (is.empty(las)) {
      results$readable[i] <- FALSE
      results$n_points[i] <- 0
      results$error_msg[i] <- "Empty LAS object"
    } else {
      results$readable[i] <- TRUE
      results$n_points[i] <- nrow(las@data)
      results$error_msg[i] <- NA
    }
    
  }, error = function(e) {
    results$readable[i] <- FALSE
    results$n_points[i] <- NA
    results$error_msg[i] <- conditionMessage(e)
  })
}

# ---- Summary ----
summary(results$readable)

# ---- View corrupted files ----
bad_tiles <- results %>% filter(!readable)
good_tiles <- results %>% filter(readable)

print(bad_tiles)
print(nrow(bad_tiles))

print(nrow(good_tiles))
