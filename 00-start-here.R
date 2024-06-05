### Main script to run all model analysis.  This sequentially loads and run files in three directories:

# Note: FireShapefile data downloaded from: https://cwfis.cfs.nrcan.gc.ca/datamart - select National Fire Database fire polygon data  (too large to store shapefiles on github)


# data-process: prepares all data for analysis
# model-process: runs the model and parameter estimations
# results-process: runs script files to prepare model figures.

# At all stages these libraries need to be loaded:
library(tidyverse)
library(lubridate)
library(FireGrow)

# Prepare all the data
data_files <- list.files(path = 'data-process',full.names=TRUE)
for (i in 1: length(data_files)) {
  print(data_files[[i]])
  source(data_files[[i]])
}


## Use devtools to build, install, document:
# devtools::build()
# devtools::document()
# devtools::install()


# Process model
set.seed(20240601)  ## Set random seed for reproducibility
### Main script to run all model analysis.  This sequentially loads and run files in three directories:

# data-process: prepares all data for analysis
# model-process: runs the model and parameter estimations
# results-process: runs script files to prepare model figures.

# At all stages these libraries need to be loaded:
library(tidyverse)
library(lubridate)
library(FireGrow)

# Prepare all the data
data_files <- list.files(path = 'data-process',full.names=TRUE)
for (i in 1: length(data_files)) {
  print(data_files[[i]])
  source(data_files[[i]])
}
# 3 not working
source(yoop[[4]])

## Use devtools to build, install, document:
# devtools::build()
# devtools::document()
# devtools::install()


# Process model
library(tidyverse)
library(lubridate)
library(FireGrow)

# Prepare parameter estimates
set.seed(20240601)  ## Set random seed for reproducibility
model_files <- list.files(path = 'model-process',full.names=TRUE)
for (i in 1: length(model_files)) {
  print(model_files[[i]])
  source(model_files[[i]])
}


results_files <- list.files(path = 'results-process',full.names=TRUE)
for (i in 1: length(results_files)) {
  print(results_files[[i]])
  source(results_files[[i]])
}


