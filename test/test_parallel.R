
library(doParallel)
library(foreach)
library(terra)

# Setup path to R files
r_dir <- "../R"
if (!dir.exists(r_dir)) r_dir <- "R"

source(file.path(r_dir, "predpoint.R"))
source(file.path(r_dir, "learner_glm.R"))

# Mock Predday logic (simplified version of the loop in predday.R)
test_parallel_predday <- function(model_fun, ncpu = 2) {
  
  message("Setting up parallel cluster with ", ncpu, " cores...")
  registerDoParallel(cores=ncpu)
  
  # Mock Data
  message("Creating mock data...")
  # Grid: 4 points
  grid_df <- data.frame(
    x = c(0, 0, 1, 1),
    y = c(0, 1, 0, 1),
    var1 = runif(4)
  )
  
  # Stations: 3 points
  ref_df <- data.frame(
    lon = c(0.1, 0.9, 0.5),
    lat = c(0.1, 0.9, 0.5),
    var1 = runif(3),
    val = c(10, 20, 15)
  )
  
  covars <- "var1"
  coords <- c("lon", "lat")
  crs <- "EPSG:4326"
  thres <- NA
  neibs <- 2
  
  message("Starting parallel foreach loop...")
  
  # The exact foreach call we want to test
  j <- NULL
  tryCatch({
    rr <- foreach(j = 1:nrow(grid_df), .combine=cbind, .export = c("predpoint")) %dopar% {
      predpoint(can = grid_df[j, ], 
                ref = ref_df, 
                model_fun = model_fun,
                thres = thres, 
                neibs = neibs,
                covars = covars, 
                coords = coords, 
                crs = crs)
    }
    message("SUCCESS: Parallel execution completed.")
    print(rr)
  }, error = function(e) {
    message("FAILURE: ", e$message)
  })
  
  stopImplicitCluster()
}

test_parallel_predday(learner_glm)
