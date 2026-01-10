
library(foreach)
library(doParallel)
library(xgboost)

# Load the local function directly to be 100% sure
source("R/learner_xgboost.R")

# Mock data setup
n_stations <- 20
sts <- data.frame(
  ID = paste0("st", 1:n_stations),
  lon = rnorm(n_stations),
  lat = rnorm(n_stations),
  alt = rnorm(n_stations)
)
covars <- c("lon", "lat", "alt")

# Scenario A: All Rain (The potential breaker)
val_rain <- rep(5.0, n_stations) # numeric
ref_rain <- cbind(sts, val = val_rain)
can_rain <- ref_rain[1, ]
ref_rain <- ref_rain[-1, ]

# Scenario B: Mixed
val_mixed <- c(rep(0, 10), rep(5.0, 10))
ref_mixed <- cbind(sts, val = val_mixed)
can_mixed <- ref_mixed[1, ]
ref_mixed <- ref_mixed[-1, ]

print("--- Starting Parallel Test ---")
registerDoParallel(cores=2)

results <- foreach(i = 1:2, .combine=rbind, .packages=c('xgboost')) %dopar% {
  # We must source inside the worker if not exporting, but usually export works.
  # Let's try to verify if the function on worker is the updated one.
  
  # For debug: redefine the function LOCALLY on the worker if needed, 
  # or rely on export. Here we explicitly source it to be safe.
  source("R/learner_xgboost.R") 
  
  if (i == 1) {
     print("Worker 1: Testing All Rain (Single Class)")
     out <- learner_xgboost(ref_rain, can_rain, covars)
     return(c(i, out))
  } else {
     print("Worker 2: Testing Mixed Class")
     out <- learner_xgboost(ref_mixed, can_mixed, covars)
     return(c(i, out))
  }
}

print("--- Parallel Test Finished ---")
print(results)
