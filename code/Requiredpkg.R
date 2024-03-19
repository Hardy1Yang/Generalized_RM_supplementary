pkg_list <- c("plyr","lattice","Rmisc",
              "ggplot2","dplyr","hrbrthemes",
              "viridis","tidyverse","latex2exp",
              "gridExtra","magrittr","grid","msm")
  
new.packages <- pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(pkg_list, require, character.only = TRUE)
