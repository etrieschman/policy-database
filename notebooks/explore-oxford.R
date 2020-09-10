# ______________________________________________________________ 
# --------------- explore Oxford database ------------------- 
# ______________________________________________________________
# * Download dataset
# * subset to lockdown policies by stringency
# * quality control those policies
# ______________________________________________________________

#### Setup ####
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
# install.packages("gtable")
library(tidyselect)
library(ggplot2)
# library(tidyverse)
# library(tidyr)
library(plyr)
library(dplyr)
library(reshape2)
library(scales)
library(stringr)
library(googledrive)
library(ggpubr)
library(sqldf)
library(gtable)

# install package from https://covid19datahub.io/index.html
# install.packages("COVID19", dependencies = T)
# require(COVID19)

# Import data
# DECISION TO NOT USE covid19 FOR THIS DATASOURCE
# raw_oxford <- covid19(raw= F)
# unique(raw_oxford$administrative_area_level_1)

raw_oxford <- read.csv(file= "./policy-database/data/external/oxcgrt/OxCGRT_latest.csv", stringsAsFactors = F)

# --------------------Clean variables based on exploration below ------------
# ____________________________________________________________________________

# lowercase names(
cln_oxford <- raw_oxford
names(cln_oxford) <- tolower(names(cln_oxford))

# clean dates and cutoff at date cutoff
cln_oxford <- cln_oxford %>%
  mutate(date_cln = as.Date(as.character(date), format= "%Y%m%d"))
cln_oxford <- cln_oxford[cln_oxford$date_cln < date_cutoff,]



# create character stay at home values
cln_oxford <- cln_oxford %>%
  mutate(stay_home = ifelse(c6_stay.at.home.requirements == 0, "0_no_measures",
                                     ifelse(c6_stay.at.home.requirements == 1, "1_recommended",
                                            ifelse(c6_stay.at.home.requirements == 2, "2_required_exceptions",
                                                   ifelse(c6_stay.at.home.requirements == 3, "3_required",
                                                          "missing"))))) %>%
  mutate(stay_home_flag = ifelse(is.na(c6_flag), "missing", 
                                      ifelse(c6_flag == 0, "0_targeted",
                                      ifelse(c6_flag == 1, "1_general",
                                             "missing"))))


# create strings of like policies
col_pre_oxford <- cln_oxford %>% arrange(countryname, date_cln) %>%
  select(countryname, countrycode, stay_home, stay_home_flag, date_cln)

# index on rows and create counters
col_pre_oxford$date_start <- NA
col_pre_oxford$date_end <- NA
col_pre_oxford$date_start[1] <- col_pre_oxford$date_cln[1]
for(i in 2:nrow(col_pre_oxford)){
  
  col_pre_oxford$date_start[i] <- with(col_pre_oxford, 
                                       ifelse(countryname[i] == countryname[i-1] & 
                                                stay_home[i] == stay_home[i-1] &
                                                stay_home_flag[i] == stay_home_flag[i-1],
                                              NA, date_cln[i]))
  
  col_pre_oxford$date_end[i] <- with(col_pre_oxford, 
                                               ifelse(countryname[i] == countryname[i+1] & 
                                                        stay_home[i] == stay_home[i+1] &
                                                        stay_home_flag[i] == stay_home_flag[i+1],
                                                      NA, date_cln[i]))
  
  col_pre_oxford$date_start[i] <- ifelse(is.na(col_pre_oxford$date_start[i]),
                                                 col_pre_oxford$date_start[i-1],
                                                 col_pre_oxford$date_start[i])
  print(i)
}  
col_pre_oxford$date_end[nrow(col_pre_oxford)] <- col_pre_oxford$date_cln[nrow(col_pre_oxford)]

col_oxford <- col_pre_oxford %>% 
  group_by(countryname, countrycode, stay_home, stay_home_flag, date_start) %>%
  summarize(date_end = max(date_end, na.rm= T)) %>%
  arrange(countryname, date_start)

col_oxford$date_start <- as.Date(col_oxford$date_start, origin= "1970-01-01")
col_oxford$date_end <- as.Date(col_oxford$date_end, origin= "1970-01-01")


# --------------------compare lockdowns to validated dataset ------------------
# ____________________________________________________________________________
val_data <- read.csv("./policy-database/data/interim/validation_dataset_20200729.csv", stringsAsFactors = FALSE)
val_data$country_code3[val_data$country_code3 == "VMN"] <- "VNM"
val_data$start <- as.Date(val_data$start)
val_data$group_abb <- with(val_data, ifelse(group== "treatment", "_t",
                                            ifelse(group== "control", "_c", "_m")))



country_comparison <- merge(x= val_data, y= col_oxford, by.x= "country_code3", by.y= "countrycode", all= TRUE) %>% 
  select(country_code3, country, countryname) %>% distinct()
# with the exception of tonga and armenia, all countries in Yichun's list are in oxford


# merge lockdown data onto val data
val_merge <- full_join(x= col_oxford, y= val_data, by = c("countrycode" = "country_code3"), suffixes= c("_o", "_y")) %>%
  select(countryname, countrycode, date_start, date_end, stay_home, stay_home_flag, announce:group)
val_merge$country_label <- with(val_merge, paste0(ifelse(is.na(group), "na_", 
                                                                      ifelse(group== "treatment", "t_",
                                                                      ifelse(group== "control", "c_", "na_"))), countrycode))


# function to plot countries
compare_countries <- function(i){
  
  compare_sub <- val_merge %>% filter(country_label %in% i)
  
    p <- ggplot(data= compare_sub) + 
      geom_point(aes(y = country_label, x= start), size= 7, shape= 21) + 
      geom_segment(aes(y = country_label, yend= country_label, x= date_start, xend= date_end, color= stay_home, linetype= stay_home_flag), size= 1)
    plot(p)
}

# loop through countries
countries <- sort(unique(val_merge$country_label))
countries_left <- length(unique(val_merge$country_label))
n_countries <- 10
while(countries_left > 0){
  
  m <- length(unique(val_merge$country_label)) - countries_left + 1
  n <- m + n_countries
  
  i <- countries[m:n]
  
  compare_countries(i)
  
  countries_left <- length(unique(val_merge$country_label)) - n
}



# write.csv(x= val_merge, file= "./policy-database/data/interim/manual_review_dataset.csv")

# --------------------Implement validation flagging algorithm  ------------------
# ____________________________________________________________________________

# run first order check
val_final <- val_merge %>%
  group_by(countryname) %>%
  mutate(all_requ = sum(stay_home %in% c("2_required_exceptions", "3_required"))>0,
         gen_requ = sum(stay_home_flag == "1_general" & all_requ)>0,
         tar_requ = sum(stay_home_flag == "0_targeted" & all_requ)>0,
         all_reco = sum(stay_home == "1_recommended")>0,
         date_start_gen_req_min = as.Date(min(date_start[stay_home %in% c("2_required_exceptions", "3_required") & 
                                                           stay_home_flag == "1_general"], na.rm= T)),) %>%
  ungroup() %>%
  mutate(check1 = abs(start - date_start_gen_req_min) <= 2)

# Summarize preliminary check for the treatment group
val_final_summ1 <- val_final %>% select(countryname, countrycode, tar_requ, gen_requ, all_reco, check1) %>% distinct()
val_final_summ2 <- val_final_summ1 %>% group_by(gen_requ, check1, tar_requ, all_reco) %>% 
  dplyr::summarize(n = n(), countries = paste0(countrycode, collapse = ", ")) %>% 
  arrange(desc(gen_requ),)


# output for manual review
write.csv(x = val_final, file = here(paste0("policy-database/notebooks/outputs/oxford_manual_review-", today, ".csv")))

# --------------------import manually reviewed data  ------------------
# ____________________________________________________________________________

mr_data <- read.csv(file= "./policy-database/notebooks/outputs/oxford_manual_review-20200820 - with updates.csv", stringsAsFactors = F)

# subset to treatment and control (for now), create updated variables, and limit columns
mr_data_sub <- mr_data %>% 
  filter(group %in% c("treatment", "control")) %>%
  filter(!(stay_home == "0_no_measures" & date_start == "1/1/2020"))

mr_data_cln <- mr_data_sub %>%
  mutate(is_update_start_date = ifelse(update_date == "", FALSE, TRUE),
         is_update_policy = ifelse(update_policy == "", FALSE, TRUE), 
         is_update_scope = ifelse(update_scope == "", FALSE, TRUE),
         date_start_u = as.Date(ifelse(is_update_start_date, update_date, date_start), format= "%m/%d/%Y"),
         date_end = as.Date(date_end, format= "%m/%d/%Y"),
         stay_home_u = ifelse(is_update_policy, update_policy, stay_home),
         stay_home_flag_u = ifelse(is_update_scope, update_scope, stay_home_flag),
         subcountry_first_num = ifelse(is.na(subcountry_first), 0, 1 * subcountry_first),
         recommendation_first_num = ifelse(is.na(recommendation_first), 0, 1 * recommendation_first)) %>%
  
  group_by(countryname, countrycode) %>%
  mutate(subcountry_first_cln = max(subcountry_first_num),
         recommendation_first_cln = max(recommendation_first_num)) %>%
  arrange(countryname, countrycode, date_start_u)

# update end dates accordingly
mr_data_lg <- mr_data_cln %>%
  mutate(next_start = as.Date(lead(date_start_u))) %>%
  mutate(is_update_end_date = ifelse(next_start - date_end == 1 | is.na(next_start), FALSE, TRUE),
         date_end_u = as.Date(ifelse(is_update_end_date, next_start - 1, date_end), origin= "1970-01-01")) %>%
  select(countryname, countrycode, group, date_start_u, date_end_u, stay_home_u, stay_home_flag_u, subcountry_first_cln, recommendation_first_cln,
         is_update_start_date, is_update_end_date, is_update_policy, is_update_scope)

# create new flags
mr_data_viz <- mr_data_lg %>%
  group_by(countryname, countrycode) %>%
  mutate(date_subcountry_first = min(date_start_u[stay_home_flag_u == "0_targeted" & subcountry_first_cln == 1]),
         date_recommendation_first = min(date_start_u[stay_home_u == "1_recommended" & recommendation_first_cln == 1]),
         date_stay_home = min(date_start_u[stay_home_u %in% c("2_required_exceptions", "3_required") &
           stay_home_flag_u == "1_general"])) %>% ungroup()

recommendation_first <- mr_data_viz %>%
  select(countryname, date_start_u, date_recommendation_first) %>%
  filter(!is.na(date_recommendation_first)) %>%
  distinct()


ggplot() + 
  geom_segment(data= mr_data_viz, aes(y = countryname, yend= countryname, x= date_start_u, xend= date_end_u, 
                   color= stay_home_u, linetype= stay_home_flag_u), size= 1) + 
  geom_point(data= mr_data_viz, aes(x= date_recommendation_first, y= countryname))
  



# --------------------visualize ------------------
# ____________________________________________________________________________

# visualize what's going on
# write a function to plot a country
plot_country <- function(i){

  lock_visual_sub <- cln_oxford %>% filter(countryname %in% i)
  
  # visualize policies
  p <- ggplot(data= lock_visual_sub, aes(x= date_cln, y= countryname, color = stay_home)) + 
    geom_point(aes(shape= stay_home_flag), size= 2, position= position_dodge(width= 0.7))
    
    # geom_text(aes(label= date_cln_min), size= 3, angle= 90, position= position_dodge(width= 0.7))
  
    # geom_linerange(aes(xmin= date_start_cln, xmax = date_end_cln, y= policy_area), position= position_dodge(width= 0.7)) + 
    # scale_color_manual(values = enforce_col) + 
    # scale_shape_manual(values = measure_shape) +
    # ggtitle(loop_country)
    
  
  plot(p)
}

# test the function
plot_country("Germany")
plot_country(i)


# pdf all of the countries for viewing
pdf("./policy-database/notebooks/outputs/oxgrt-lockdown-overview.pdf", width = 11, height= 8.5)

countries_left <- length(unique(cln_oxford$countryname))
n_countries <- 10
while(countries_left > 0){
  
  m <- length(unique(cln_oxford$countryname)) - countries_left + 1
  n <- m + n_countries
  
  i <- countries[m:n]
  
  plot_country(i)
  
  countries_left <- length(unique(cln_oxford$countryname)) - n
}


dev.off() 


                 