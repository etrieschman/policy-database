# ______________________________________________________________ 
# --------------- Setup file for policy database work------------ 
# ______________________________________________________________
# ______________________________________________________________

dir <- "C:/Users/ErichTrieschman/dev"
setwd(dir)

#### ... packages ####
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("scales")
# install.packages("stringr")
# install.packages("googledrive")
# install.packages("ggpubr")
# install.packages("tidyselect")
# install.packages("sqldf")
# install.packages("here")
library(tidyselect)
library(ggplot2)
# here(tidyverse)
# library(tidyr)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)
library(stringr)
library(googledrive)
library(ggpubr)
library(sqldf)
library(here)

# color function
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

