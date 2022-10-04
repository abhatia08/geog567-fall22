## Load Libraries
library(tidyverse)
library(rlang)



# ENSURE DIRECTORY ----
## Create directory if it doesn't exist
ensure_directory <- function(directory) {
  if (!dir.exists(directory)) {
    dir.create(directory)
    
  }
}



