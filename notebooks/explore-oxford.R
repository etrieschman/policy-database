# ______________________________________________________________ 
# --------------- explore Oxford database ------------------- 
# ______________________________________________________________
# * Download dataset
# * subset to lockdown policies by stringency
# * quality control those policies
# ______________________________________________________________


# install package from https://covid19datahub.io/index.html
# install.packages("COVID19", dependencies = T)
# require(COVID19)

# Import data
# DECISION TO NOT USE covid19 FOR THIS DATASOURCE
# raw_oxford <- covid19(raw= F)
# unique(raw_oxford$administrative_area_level_1)

raw_oxford <- read.csv(file= here("policy-database/data/external/oxcgrt/OxCGRT_latest.csv"), stringsAsFactors = F)

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

# --------------------Manually check where required  ------------------
# ____________________________________________________________________________

# add flags to help with manual checking
temp <- merge(x= val_final, y= val_final_summ2, by= )

# --------------------visualize ------------------
# ____________________________________________________________________________

# visualize what's going on
# write a function to plot a country
plot_country <- function(i){

  lock_visual_sub <- cln_oxford %>% filter(countryname %in% i)
  
  # visualize policies
  p <- ggplot(data= lock_visual_sub, aes(x= date_cln, y= countryname, color = stay_home)) + 
    geom_point(aes(shape= stay_home_flag), size= 2, position= position_dodge(width= 0.7)) + 
    
    geom_text(aes(label= date_cln_min), size= 3, angle= 90, position= position_dodge(width= 0.7))
  
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


                 