# ______________________________________________________________ 
# --------------- clean masscpr constructed dataset------------- 
# ______________________________________________________________
# TO-DOS: NEED TO FIGURE OUT HOW TO APPLY ALL-BUSINESS BEFORE FIRST SUB-BUSINESS POLICY
# ______________________________________________________________

#### Setup ####
dir <- "/Users/ErichTrieschman/dev"
setwd(dir)

#### ... packages ####
library(tidyselect)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)
library(stringr)
library(googledrive)
library(ggpubr)
library(sqldf)
library(gtable)

# color function
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

today <- gsub( "-","", Sys.Date())
date_cutoff <- as.Date("2020-07-31")
date_origin <- "1970-01-01"


# Import data
# e_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_emergency_v1_unverifiedPuxi_20200916.csv", stringsAsFactors = FALSE)
# e_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_emergency_v1_verified_20200917.csv", stringsAsFactors = FALSE)
# e_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_emergency_v2_verifiedXinyi_20200924.csv", stringsAsFactors = FALSE)
e_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_emergency_v3_verified_20201001.csv", stringsAsFactors = FALSE)



# --------------------clean based on summaries  ------------
# ____________________________________________________________________________

# remove unvalidated responses
e_cln <- e_raw %>% filter(verification_action != "delete") %>%
  mutate(flag_sub_variation = response_level == "Level II - var",
         response_level_cln = ifelse(response_level == "Level II - var", "Level II", response_level))

# clean categories
e_cln <- e_cln %>%
  mutate(date_announce_cln = as.Date(date_announce, format = "%m/%d/%Y"),
         date_start_cln = as.Date(date_start, format = "%m/%d/%Y"))



# --------------------summarize raw data to understand database  ------------
# ____________________________________________________________________________

dim(e_cln)
min(e_cln$date_start_cln)
max(e_cln$date_start_cln)

# summarize policy distribution
e_summ <- e_cln %>% group_by(response_level_cln, flag_sub_variation) %>% 
  dplyr::summarize(n= n(),
                   min_start_date = min(date_start_cln),
                   max_start_date = max(date_start_cln))

ggplot(data= e_cln) + geom_boxplot(aes(y= response_level_cln, x= date_start_cln, 
                                       fill= response_level_cln, color= flag_sub_variation))


# --------------------create poi policy strings  ------------
# ____________________________________________________________________________

# create strings
e_string <- e_cln %>%
  arrange(province_region, date_start_cln) %>%
  mutate(use_cutoff_date = ifelse(is.na(lead(province_region)) | (!is.na(province_region) & province_region != lead(province_region)), TRUE, FALSE),
         date_end_cln = as.Date(ifelse(use_cutoff_date, date_cutoff, lead(date_start_cln) - 1), origin= date_origin)) %>%
  select(policy_id, province_region, response_level_cln, flag_sub_variation, date_announce_cln, date_start_cln, date_end_cln) %>%
  arrange(province_region, date_start_cln)



# visualize
# BEFORE OVERRIDES
p_poi <- ggplot(data= e_string) + 
  
  geom_linerange(aes(xmin = date_start_cln, xmax= date_end_cln, y= province_region, color= response_level_cln, linetype= flag_sub_variation), size= 1.5) + 
  geom_point(aes(x= date_announce_cln, y= province_region, color= response_level_cln), position = position_nudge(y = 0), size= 4, alpha= .5) + 
  labs(title= "Response levels by province", color= "Response level", linetype= "Atypical sub-variation") + 
  
  # facet_grid(province_region ~ ., space= "free_y", scales= "free_y", switch= "y") + 
  scale_x_date(date_labels= "%b-%d", date_breaks= "2 weeks", limits= c(as.Date("2020-01-15"), date_cutoff)) +
  theme(axis.title = element_blank())

plot(p_poi)






# --------------------output dataset  ------------
# ____________________________________________________________________________

write.csv(x= e_string, file= paste0("./policy-database/data/interim/masscpr_emergencyresponse_db_", today,".csv"))







# --------------------Appendix  ------------
# ____________________________________________________________________________


