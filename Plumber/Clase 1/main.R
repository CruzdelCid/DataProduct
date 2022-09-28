library(plumber)
r <- plumb("prediction_api.R")
r$run(port=8001,quiet = TRUE)
