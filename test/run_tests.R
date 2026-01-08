# ------------------------------------------------------------------------------
# TEST WORKFLOW FOR reddPrec
# ------------------------------------------------------------------------------
#
# This script runs a series of tests on the functions available in the
# `reddPrec` R package.
#
# Workflow:
# 1. Install and load required packages.
# 2. Source all R functions from the R/ directory.
# 3. Generate synthetic precipitation data.
# 4. Run quality control checks with qcPrec().
# 5. Store the results in the 'test/results' directory.
# 6. (Future) Run gap filling and homogenization functions.
#
# ------------------------------------------------------------------------------

# --- 1. SETUP: Install and load packages ---

message("--- Installing and loading packages ---")

required_packages <- c("terra", "stats", "foreach", "doParallel", "reshape", 
                       "qmap", "BreakPoints", "trend", "car", "geosphere", 
                       "gridExtra", "lattice", "pracma", "xts", "zoo", 
                       "e1071", "neuralnet", "randomForest", "reshape2", "xgboost")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    message("Installing package: ", pkg)
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}


# --- 2. SOURCE FUNCTIONS ---

message("--- Sourcing R functions ---")

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  source(file)
}


# --- 3. DATA GENERATION ---

message("--- Generating synthetic data ---")

set.seed(123)
# Using a smaller dataset for quicker testing
n_days <- 100
n_stations <- 20

prec <- round(matrix(rnorm(n_days * n_stations, mean = 1.2, sd = 6), n_days, n_stations), 1)
prec[prec < 0] <- 0
colnames(prec) <- paste0('sts_', 1:n_stations)

sts <- data.frame(ID = paste0('sts_', 1:n_stations), 
                  lon = rnorm(n_stations, -3, 1), 
                  lat = rnorm(n_stations, 40, 1), 
                  dcoast = rnorm(n_stations, 200, 50))


# --- 4. RUNNING qcPrec TEST ---

message("--- Running qcPrec() function ---")

# Create results directory
if (!dir.exists("test/results")) {
  dir.create("test/results", recursive = TRUE)
}

qc_result <- NULL
qc_error <- NULL

tryCatch({
  # Use a smaller number of neighbors for the test
  qc_result <- qcPrec(
    prec = prec,
    sts = sts,
    crs = 'EPSG:4326',
    coords = c('lon', 'lat'),
    coords_as_preds = TRUE,
    neibs = 5,
    thres = NA,
    qc = 'all',
    ncpu = 1
  )
}, error = function(e) {
  qc_error <- e
  message("An error occurred while running qcPrec():")
  print(e)
})


# --- 5. SAVE RESULTS ---

message("--- Saving results ---")

if (!is.null(qc_result)) {
  save(qc_result, file = "test/results/qc_result.RData")
  
  # Capture the structure of the output
  output_summary <- capture.output(str(qc_result))
  writeLines(output_summary, "test/results/qc_result_summary.txt")
  
  message("qcPrec() executed successfully. Results saved in 'test/results'.")
  
} else {
  error_message <- paste("qcPrec() failed. Error: ", qc_error$message)
  writeLines(error_message, "test/results/qc_error.txt")
  
  message("qcPrec() failed. Check 'test/results/qc_error.txt' for details.")
}


# --- 6. PLACEHOLDER FOR OTHER TESTS ---

message("--- Workflow finished ---")
message("Add tests for gapFilling(), hmg_Ts() below.")

#
# TODO: Add tests for gapFilling()
#

#
# TODO: Add tests for hmg_Ts()
#