library(xgboost)
library(terra)

# Mock learner_xgboost function with debug prints
learner_xgboost_debug <- function(ref, can, covars) {
  print("--- Inside learner_xgboost_debug ---")
  
  rr <- as.data.frame(ref)
  rr$val[rr$val > 0] <- 1
  
  unique_vals <- unique(rr$val)
  print(paste("Unique values in rr$val:", paste(unique_vals, collapse = ", ")))
  print(paste("Class of rr$val:", class(rr$val)))
  
  if (length(unique_vals) < 2) {
    print("Single class detected. Skipping xgboost.")
    pb <- as.numeric(unique_vals[1])
  } else {
    print("Multiple classes detected. Running xgboost.")
    
    # model
    f <- as.formula(
      paste(paste(covars, collapse = " + "), "~ val", collapse = " ")
    )
    
    # model
    set.seed(123)
    
    label_data <- rr$val
    print(paste("Label data sample:", paste(head(label_data), collapse = ", ")))
    print(paste("Label data class:", class(label_data)))
    
    tryCatch({
        fmb <- xgboost::xgboost(data = as.matrix(rr[, covars]),
                                label = label_data, # Testing without explicit cast first
                                verbose = 0, nthread = 1,
                                objective = "binary:logistic",
                                nrounds = 5)
        print("Xgboost training successful.")
        
        # prediction
        pb <- predict(fmb,
                      newdata = as.matrix(as.data.frame(can[, covars])),
                      type = "prob")
        pb <- as.numeric(pb)
    }, error = function(e) {
        print(paste("Xgboost training FAILED:", e$message))
    })
  }
  
  return(c(pb, 0, 0)) # Return dummy amount/error
}

# --- Test Data Setup ---
# Create dummy station data
n_stations <- 20
sts <- data.frame(
  ID = paste0("st", 1:n_stations),
  lon = rnorm(n_stations),
  lat = rnorm(n_stations),
  alt = rnorm(n_stations)
)
covars <- c("lon", "lat", "alt")

# Case 1: Mixed 0s and 1s (Should work)
print("\n=== TEST CASE 1: Mixed 0s and 1s ===")
val_mixed <- sample(c(0, 10), n_stations, replace = TRUE) # 0 and positive (rain)
ref_mixed <- cbind(sts, val = val_mixed)
can_mixed <- ref_mixed[1, ]
ref_mixed <- ref_mixed[-1, ]
learner_xgboost_debug(ref_mixed, can_mixed, covars)

# Case 2: All 0s (Should be skipped by logic)
print("\n=== TEST CASE 2: All 0s ===")
val_zeros <- rep(0, n_stations)
ref_zeros <- cbind(sts, val = val_zeros)
can_zeros <- ref_zeros[1, ]
ref_zeros <- ref_zeros[-1, ]
learner_xgboost_debug(ref_zeros, can_zeros, covars)


# Case 3: All > 0 (Should be skipped by logic, treated as all 1s)
print("\n=== TEST CASE 3: All > 0 ===")
val_rain <- runif(n_stations, 1, 10)
ref_rain <- cbind(sts, val = val_rain)
can_rain <- ref_rain[1, ]
ref_rain <- ref_rain[-1, ]
learner_xgboost_debug(ref_rain, can_rain, covars)

# Case 4: The "Got numeric y" Trigger - potential edge case
# What if rr$val contains something that isn't purely 0 or 1 but xgboost complains?
# Or what if logic check fails?
print("\n=== TEST CASE 4: Reproducing 'Got numeric y' ===")
# Force a scenario where we pass numeric 1s to xgboost to see if it complains
# We'll simulate this by bypassing the check
val_force_fail <- rep(10, n_stations) # All rain
ref_force <- cbind(sts, val = val_force_fail)
rr_force <- as.data.frame(ref_force)
rr_force$val[rr_force$val > 0] <- 1

print("Forcing xgboost with all 1s (numeric)...")
tryCatch({
    xgboost::xgboost(data = as.matrix(rr_force[, covars]),
                    label = rr_force$val,
                    verbose = 0, nthread = 1,
                    objective = "binary:logistic", # This expects 0/1 integers generally?
                    nrounds = 5)
    print("Xgboost Passed with all 1s numeric")
}, error = function(e) {
    print(paste("Xgboost Failed with all 1s numeric:", e$message))
})


