library(plumber)
r <- plumb("lab.R")
r$run(port=8001,quiet = TRUE)
