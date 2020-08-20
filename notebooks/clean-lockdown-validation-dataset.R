# ______________________________________________________________ 
# --------------- explore WHO PHSM database ------------------- 
# ______________________________________________________________
# * Download dataset
# * subset to lockdown policies by stringency
# * quality control those policies
# ______________________________________________________________
# QUALITY CONTROL AND SENSITIVITIES
# 1. spot-check dates
# 2. spot-check sub-national policies

#### Setup ####
dir <- "C:/Users/ErichTrieschman/dev"
setwd(dir)

#### ... packages ####
# install.packages("ggplot2", dependencies = T)
# install.packages("tidyverse", dependencies = T)
# install.packages("tidyr", dependencies = T)
# install.packages("dplyr", dependencies = T)
# install.packages("reshape2", dependencies = T)
# install.packages("scales",dependencies= T)
require(ggplot2)
require(tidyverse)
require(tidyr)
require(dplyr)
require(plyr)
require(reshape2)
require(scales)

# color function
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


# Import data file from 2020.07.29
date <- "20200729"
val_raw <- read.csv(paste0("./policy-database/data/internal/nationwide_lockdown_policy_", date,".csv"),
                    skip=1, stringsAsFactors = F)
names(val_raw) <- c("country", "country_code3", "country_code2", "temp", 
                    "announce", "start", "end", "enforce")

val_raw <- val_raw %>% filter(!country_code3 == "") %>% 
  mutate(group = ifelse(announce == "", "control", "treatment"))

# split data files
# trt_raw <- val_raw %>% filter(announce != "") %>% select(-"temp")
# ctrl_raw <- val_raw %>% filter(announce == "" & country_code3 != "")

# save data files
write.csv(val_raw, file= paste0("./policy-database/data/interim/validation_dataset_", date,".csv"))






                 