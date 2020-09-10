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
library(gridExtra)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}



today <- gsub( "-","", Sys.Date())
date_cutoff <- as.Date("2020-06-01")

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


country_comparison <- merge(x= val_data, y= col_oxford, by.x= "country_code3", by.y= "countrycode", all= TRUE) %>% 
  select(country_code3, country, countryname) %>% distinct()
# with the exception of tonga and armenia, all countries in Yichun's list are in oxford


# merge lockdown data onto val data
val_merge <- full_join(x= col_oxford, y= val_data, by = c("countrycode" = "country_code3"), suffixes= c("_o", "_y")) %>%
  select(countryname, countrycode, date_start, date_end, stay_home, stay_home_flag, announce:group)
val_merge$country_label <- with(val_merge, paste0(ifelse(is.na(group), "na_", 
                                                                      ifelse(group== "treatment", "t_",
                                                                      ifelse(group== "control", "c_", "na_"))), countrycode))


# # function to plot countries
# compare_countries <- function(i){
#   
#   compare_sub <- val_merge %>% filter(country_label %in% i)
#   
#     p <- ggplot(data= compare_sub) + 
#       geom_point(aes(y = country_label, x= start), size= 7, shape= 21) + 
#       geom_segment(aes(y = country_label, yend= country_label, x= date_start, xend= date_end, color= stay_home, linetype= stay_home_flag), size= 1)
#     plot(p)
# }
# 
# # loop through countries
# countries <- sort(unique(val_merge$country_label))
# countries_left <- length(unique(val_merge$country_label))
# n_countries <- 10
# while(countries_left > 0){
#   
#   m <- length(unique(val_merge$country_label)) - countries_left + 1
#   n <- m + n_countries
#   
#   i <- countries[m:n]
#   
#   compare_countries(i)
#   
#   countries_left <- length(unique(val_merge$country_label)) - n
# }

write.csv(x = col_oxford, file = paste0("./policy-database/notebooks/outputs/oxford_manual_review-", today, ".csv"))

# --------------------import WHO to run checks  ------------------
# ____________________________________________________________________________

who_raw <- read.csv("./policy-database/data/external/who-phsm/WHO_PHSM_Cleaned_V1_20_07_15.csv", stringsAsFactors = F)
sah_search <- c("stay-at-home")
who_sah <- who_raw %>% filter(grepl(paste(sah_search, collapse= "|"),tolower(who_measure)))

# --------------------import manually reviewed data  ------------------
# ____________________________________________________________________________

mr_data <- read.csv(file= "./policy-database/notebooks/outputs/oxford_manual_review-20200901 - with updates.csv", stringsAsFactors = F)

# subset to treatment and control (for now), create updated variables, and limit columns
mr_data_sub <- mr_data 
# %>% 
#   filter(group %in% c("treatment", "control")) %>%
#   filter(!(stay_home == "0_no_measures" & date_start == "1/1/2020"))

mr_data_cln <- mr_data_sub %>%
  mutate(is_update_start_date = ifelse(update_date == "", FALSE, TRUE),
         is_update_policy = ifelse(update_policy == "", FALSE, TRUE), 
         is_update_scope = ifelse(update_scope == "", FALSE, TRUE)) %>%
  mutate(date_start_u = as.Date(ifelse(is_update_start_date, update_date, date_start), format= "%m/%d/%Y"),
         date_end = as.Date(date_end, format= "%m/%d/%Y"),
         stay_home_u = ifelse(is_update_policy, update_policy, stay_home),
         stay_home_flag_u = ifelse(is_update_scope, update_scope, stay_home_flag),
         subcountry_first_cln = ifelse(is.na(subcountry_first), FALSE, subcountry_first),
         recommendation_first_cln = ifelse(is.na(recommendation_first), FALSE, recommendation_first)) %>%
  arrange(countryname, countrycode, date_start_u)

# update end dates accordingly
mr_data_lg <- mr_data_cln %>%
  group_by(countryname, countrycode) %>%
   mutate(next_start = as.Date(lead(date_start_u))) %>%

  mutate(is_update_end_date = ifelse(next_start - date_end == 1 | is.na(next_start), FALSE, TRUE),
         date_end_u = as.Date(ifelse(is_update_end_date, next_start - 1, date_end), origin= "1970-01-01")) %>%
  select(countryname, countrycode, date_start, date_end, date_start_u, date_end_u, stay_home_u, stay_home_flag_u, 
         subcountry_first_cln, recommendation_first_cln,
         is_update_start_date, is_update_end_date, is_update_policy, is_update_scope)

# estimate key dates
mr_data_summ <- mr_data_lg %>%
  group_by(countryname, countrycode) %>%
  mutate(sah_flag = stay_home_u %in% c("2_required_exceptions", "3_required") & stay_home_flag_u == "1_general",
         sah_noexep_flag = stay_home_u == "3_required" & stay_home_flag_u == "1_general",
         sah_rec_flag = stay_home_u == "1_recommended", 
         sah_sub_flag = stay_home_flag_u == "0_targeted") %>%
  mutate(date_sah_min = min(date_start_u[sah_flag], na.rm= T),
         date_sah_max = max(date_end_u[sah_flag], na.rm= T),
         date_sah_noexep_min = min(date_start_u[sah_noexep_flag], na.rm= T),
         date_sah_noexep_max = max(date_end_u[sah_noexep_flag], na.rm= T),
         date_sah_rec_min = min(date_start_u[sah_rec_flag & recommendation_first_cln], na.rm= T),
        date_sah_sub_min = min(date_start_u[sah_sub_flag & subcountry_first_cln], na.rm= T))

# --------------------create final dataset  ---------------------------------------
# ____________________________________________________________________________

# create final dataset
mr_ox_out <- mr_data_summ %>% ungroup() %>% 
  mutate(flag_sah_all = is.finite(date_sah_min),
         flag_sah_noexep = is.finite(date_sah_noexep_min),
         flag_sah_no_rec_first = is.finite(date_sah_min) & !is.finite(date_sah_rec_min),
         flag_say_no_sub_first = is.finite(date_sah_min) & !is.finite(date_sah_sub_min),
         flag_sah_no_rec_no_sub_first = is.finite(date_sah_min) & !is.finite(date_sah_rec_min) & !is.finite(date_sah_sub_min)) %>%
  select(countryname, countrycode, 
         date_sah_min, date_sah_max, date_sah_noexep_min, date_sah_noexep_max, date_sah_sub_min, date_sah_rec_min,
         flag_sah_all, flag_sah_noexep, flag_sah_no_rec_first, flag_say_no_sub_first, flag_sah_no_rec_no_sub_first) %>% distinct()

names(mr_ox_out)

# create summary of changes
mr_ox_summ <- mr_data_cln %>%
  summarize(countries = n_distinct(countryname),
            not_in_WHO = n_distinct(countryname[is.na(national_required)]),
            any_update = n_distinct(countryname[national_required == "update"]),
            date_update = n_distinct(countryname[national_required == "update" & update_date != date_start]),
            policy_update = n_distinct(countryname[national_required == "update" & update_policy != stay_home]),
            scope_update = n_distinct(countryname[national_required == "update" & update_scope != stay_home_flag]),
            countries_pct = countries / countries,
            not_in_WHO_pct = not_in_WHO / countries,
            any_update_pct = any_update / countries,
            date_update_pct = date_update / countries,
            policy_update_pct = policy_update / countries,
            scope_update_pct = scope_update / countries)
mr_ox_summ <- t(mr_ox_summ)


# save file to share with Yichun
write.csv(x = mr_ox_out, file = paste0("./policy-database/data/processed/oxford_sah_clean - ", today, ".csv"))
# --------------------plot for review  ---------------------------------------
# ____________________________________________________________________________

size_width <- 180
rows <- 2
sah_col <- gg_color_hue(length(unique(mr_data_summ$stay_home_u)))
names(sah_col) <- rev(unique(mr_data_summ$stay_home_u))
tar_line <- (1:length(unique(mr_data_summ$stay_home_flag_u)))
names(tar_line) <- rev(unique(mr_data_summ$stay_home_flag_u))

pdf("./policy-database/notebooks/outputs/oxford-manual-data-cleaning 20200901.pdf", width = 18, height= 12)
for(i in 1:length(unique(mr_data_summ$countrycode))){
# for(i in 1:3){

  # subset the datasets i want to plot
  countries_sub <- unique(mr_data_summ$countrycode)[i]
  sub_cln <- mr_data_summ[mr_data_summ$countrycode %in% countries_sub,]
  sub_og <- col_oxford[col_oxford$countrycode %in% countries_sub,]
  
  sub_who <- who_sah %>% filter(iso %in% countries_sub) %>% select(country_territory_area, admin_level, comments, date_start) %>%
    mutate(comments = str_wrap(paste0(substr(comments, 1, rows* size_width), "..."),width= size_width)) %>% arrange(date_start) %>% distinct


  # if theres enough data, then plot
    if(nrow(sub_cln) > 0 & nrow(sub_who) > 0){
      print(countries_sub)
      
      tt <- ttheme_default(base_size= 8)
      sub_who <- tableGrob(sub_who, rows=NULL, theme= tt)
    
    p_ox <- ggplot() + 
      geom_linerange(data= sub_cln, aes(y = paste0("cln_", countryname), xmin= date_start_u, xmax= date_end_u, 
                       color= stay_home_u, linetype= stay_home_flag_u), position= position_dodge(width= 0.1), size= 1) + 
      geom_text(data= sub_cln, aes(y= paste0("cln_", countryname), x= date_sah_min, label= date_sah_min), color= "darkgreen", size= 3, angle= 90) +
      geom_text(data= sub_cln, aes(y= paste0("cln_", countryname), x= date_sah_rec_min, label= date_sah_rec_min), color= "darkorange", size= 3, angle= 90) +
      geom_text(data= sub_cln, aes(y= paste0("cln_", countryname), x= date_sah_sub_min, label= date_sah_sub_min), color= "darkred", size= 3, angle= 90) +
      
      geom_linerange(data= sub_og, aes(y= paste0("raw_", countryname), xmin= date_start, xmax= date_end, color= stay_home, linetype= stay_home_flag),
                     position= position_dodge(width= 0.2), size= 1) +
  
      
      # scale_x_date(date_labels= "%b-%d", date_breaks= "1 month", limits= c(as.Date("2020-01-20"), as.Date("2020-07-01"))) +
      
      scale_color_manual(values = sah_col) + 
      scale_linetype_manual(values = tar_line) +
    
      labs(linetype= "Policy scope",
           color= "Policy measure",
           x = element_blank(),
           y= element_blank()) +
      coord_fixed(ratio=10)
    
    
    
    grid.arrange(p_ox, sub_who, heights= c(1,4))
  } else {print(paste(countries_sub, nrow(sub_cln), nrow(sub_who), sep = " - "))}
    
}
dev.off()


                 