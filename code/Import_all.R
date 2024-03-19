source("code/Hyper_parameter.R")
source("code/Requiredpkg.R")
source("code/Response.R")

source("code/Methods.R")

source("code/Data_and_SE.R")

source("code/Visualization.R")
# directory

fig.dir <- "figures/"
log.dir <- "log/"
data.dir <- "data/"
sanity.dir <- paste0(data.dir,"sanity check/")
date <- 20240117

## Default threshold
Theta=uniroot(function(x) PF(x)-alpha,c(-10,10))$root

