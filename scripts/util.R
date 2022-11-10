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

# PLOTS -----

## Tuneplot
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(min(x$results$RMSE), quantile(x$results$RMSE, probs = probs))) +  theme_bw() + scale_y_reverse()
}



