
library(xgboost)

# Case 1: All 1s numeric
labels <- c(1, 1, 1, 1, 1)
data <- matrix(rnorm(10), 5, 2)

tryCatch({
  xgboost(data = data, label = labels, nrounds = 2, objective = "binary:logistic", verbose = 0)
  print("Case 1 (All 1s numeric) Passed")
}, error = function(e) {
  print(paste("Case 1 Failed:", e$message))
})

# Case 2: 0s and 1s numeric
labels2 <- c(0, 1, 0, 1, 0)
tryCatch({
  xgboost(data = data, label = labels2, nrounds = 2, objective = "binary:logistic", verbose = 0)
  print("Case 2 (Mixed 0/1 numeric) Passed")
}, error = function(e) {
  print(paste("Case 2 Failed:", e$message))
})

# Case 3: All 1s but not integer?
labels3 <- c(1.0, 1.0, 1.0)
data3 <- matrix(rnorm(6), 3, 2)
tryCatch({
  xgboost(data = data3, label = labels3, nrounds = 2, objective = "binary:logistic", verbose = 0)
  print("Case 3 (All 1.0 numeric) Passed")
}, error = function(e) {
  print(paste("Case 3 Failed:", e$message))
})
