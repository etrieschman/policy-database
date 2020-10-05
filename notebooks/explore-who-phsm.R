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
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(plyr)
library(reshape2)
library(scales)
library(stringr)
library(strex)

# color function
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# dates
date_today <- gsub("-", "", Sys.Date())


# Import data
db_raw <- read.csv("./policy-database/data/external/who-phsm/WHO_PHSM_Cleaned_V1_20_07_15.csv", stringsAsFactors = F)

# Types of policies
unique(db_raw$who_category)
unique(db_raw$who_subcategory)
unique(db_raw$who_measure)
temp_summary <- db_raw %>% group_by(who_category, who_subcategory, who_measure) %>% 
  summarize(n = n()) %>%
  arrange(who_category, desc(n))

# --------------------Clean variables based on exploration below ------------
# ____________________________________________________________________________

# clean dates
db_cln <- db_raw
db_cln <- db_raw %>% mutate(date_start_cln = as.Date(date_start, format= "%m/%d/%Y"),
                            date_end_cln = as.Date(date_end, format= "%m/%d/%Y"))

# Summary of countries
summ_countries <- db_cln %>% group_by(country_territory_area) %>%
  dplyr::summarize(n = n())
# they look clean! doesn't look like we need to clean them

# clean admin levels and enforcement
unique(db_cln$admin_level)
db_cln <- db_cln %>% 
  mutate(admin_level_cln = tolower(admin_level)) %>%
  mutate(enforcement_cln = tolower(enforcement))

# understand and clean compliance variables if necessary
unique(db_cln$non_compliance_penalty) # unique categories that don't need cleaning
db_cln <- db_cln %>% mutate(compliance_cln = tolower(non_compliance_penalty))
unique(db_cln$compliance_cln)

# clean finest detail of measures
db_cln <- db_cln %>% mutate(targeted_cln = tolower(targeted))
# clean lockdown measures
# capture and flag subpopulations
temp_targeted <- db_cln %>% filter(who_measure == "Stay-at-home order") %>%
  group_by(targeted_cln, comments) %>% dplyr::summarize(n = n())


# --------------------visualize countries ------------------------------------
# ____________________________________________________________________________

drop_cats <- c("Drug-based measures", "Other measures", "Environmental measures")

db_sub <- db_cln %>%
  select(country_territory_area, admin_level_cln, area_covered, dataset, who_category, who_subcategory, who_measure, 
         date_start_cln, 
         targeted_cln, enforcement_cln, non_compliance_penalty) %>%
  filter(!who_category %in% drop_cats)


plot_country <- function(x){
  
  db_sub_ctry <- db_sub %>% 
    filter(country_territory_area == x) %>%
    mutate(regions = nchar(gsub("[^,]+", "", area_covered)) + 1)
  
  db_sub_ctry_dup <- db_sub_ctry %>%
    uncount(regions, .remove= FALSE, .id= "id") %>%
    mutate(str_start = ifelse(id == 1, 1, str_locate_nth(area_covered, ",", id-1)[,1]),
    str_end = ifelse(id == regions, nchar(area_covered), lead(str_start) - 1),
    area_covered_cln = str_trim(gsub("-", " ", gsub(",", "", substr(area_covered, str_start, str_end)))))
  
  
  p <- ggplot(data= db_sub_ctry_dup) + 
    geom_point(aes(x= date_start_cln, y= paste0("(", admin_level_cln, ") ", area_covered_cln), color= who_subcategory)) + 
    facet_grid(who_category ~ ., space= "free_y", scales= "free_y", switch= "y") + 
    labs(title = paste(x, "COVID-19 policies"), subtitle= paste0("Dropped categories: \n", paste(drop_cats, collapse= ", ")), color= "Subcategory") +
    theme(axis.title = element_blank()) + 
    scale_x_date(date_labels= "%b-%d", date_breaks= "3 weeks")
  
  plot(p)
}
plot_country("China")

unique(db_sub$country_territory_area)
pdf(paste0("./policy-database/notebooks/outputs/who-phsm-all-policies-overview - ", date_today, ".pdf"), width = 11, height= 8.5)
countries_of_interest <- c("United States Of America", "Canada", "China", "Germany", "France", "Italy", "Spain", 
                           "United Kingdom Of Great Britain And Northern Ireland")
for(i in 1:length(countries_of_interest)){
  
  country <- countries_of_interest[i]
  plot_country(country)
  
}
dev.off() 

# --------------------quick pca analysis -------------------------
# ____________________________________________________________________________

country <- "China"

db_pca_pre_cntry <- db_sub %>%
  filter(country_territory_area == country & area_covered != "") %>%
  mutate(start_since_2020 = as.numeric(date_start_cln - readr::parse_date("2020-01-01")),
         who_subcat_full = paste(who_category, sep= " - ")) %>%
  pivot_wider(id_cols = c(country_territory_area, area_covered), names_from= who_subcat_full, values_from= start_since_2020, values_fn= min)

db_pca_pre_cntry[is.na(db_pca_pre_cntry)] <- 0

pca <- princomp(db_pca_pre_cntry[, 3:length(db_pca_pre_cntry)])
summary(pca)
pca$loadings
biplot(pca)

# --------------------subset to measures of interest -------------------------
# ____________________________________________________________________________
# subset to policies impacting behavior
db_behave <- db_cln %>% filter(!(who_category %in% c("Drug-based measures", "Environmental measures")))


# --------------------Summarize policy taxonomy (to inform cleaning)----------
# ____________________________________________________________________________


# subset to policies impacting behavior
db_behave <- db_cln %>% filter(!(who_category %in% c("Drug-based measures", "Environmental measures")))



# Summary of highest-level policies
summ_policy_1 <- db_cln %>% group_by(who_category) %>%
  dplyr::summarize(n = n(), 
                   countries = n_distinct(country_territory_area),
                   pct_subnational = sum(admin_level != "national") / n(),
                   min_date = min(date_start_cln, na.rm= TRUE), 
                   max_date = max(date_end_cln, na.rm= TRUE))
# there are 7 distinct high-level policies

# Summary of sub-policies
summ_policy_2 <- db_cln %>% group_by(who_category, who_subcategory) %>%
  dplyr::summarize(n = n(), 
                   countries = n_distinct(country_territory_area),
                   pct_subnational = sum(admin_level != "national") / n(),
                   min_date = min(date_start_cln, na.rm= TRUE), 
                   max_date = max(date_end_cln, na.rm= TRUE))
# there are 13 sub-policy levels. Some high-level policies don't have subpolicies

# Summary of sub-sub-policies (called measures)
summ_policy_3 <- db_cln %>% group_by(who_category, who_subcategory, who_measure) %>%
  dplyr::summarize(n = n(), 
                   countries = n_distinct(country_territory_area),
                   pct_subnational = sum(admin_level != "national") / n(),
                   min_date = min(date_start_cln, na.rm= TRUE), 
                   max_date = max(date_end_cln, na.rm= TRUE))
# there are 44 measures in tota. every policy has a measure. every category or sub-category has multiple measures

# Summary of measure targets
summ_policy_4 <- db_behave %>% group_by(who_category, who_subcategory, who_measure, targeted_cln) %>%
  dplyr::summarize(n = n(), 
                   countries = n_distinct(country_territory_area),
                   pct_subnational = sum(admin_level != "national") / n(),
                   min_date = min(date_start_cln, na.rm= TRUE), 
                   max_date = max(date_end_cln, na.rm= TRUE))
# Very specific and high level of detail. overall this is great, but will require string cleaning


# --------------------clean lockdown policies ----------------------------
# ____________________________________________________________________________

lockdown_search <- c("stay-at-home")
db_lockdown <- db_behave %>% filter(grepl(paste(lockdown_search, collapse= "|"),tolower(who_measure)))

# summarize again
summ_lockdown <- db_lockdown %>% 
  group_by(who_category, who_subcategory, who_measure, targeted_cln) %>%
  dplyr::summarize(n = n(), 
                   countries = n_distinct(country_territory_area),
                   pct_subnational = sum(admin_level != "national") / n(),
                   min_date = min(date_start_cln, na.rm= TRUE), 
                   max_date = max(date_end_cln, na.rm= TRUE))

# Manually flag full lockdowns (v1)
lockdown_indicators_v1 <- c("all day", "all residents", "all, except those", "complete lockdown", "^essential", "^full cur",
  "general p", "lockdown", "national pop", "non-essent", "stay-at-home", "stay at home", 
  "total curfew", "state of emergen")
db_lockdown <- db_lockdown %>% 
  mutate(lockdown_flg_v1 = grepl(paste(lockdown_indicators_v1, collapse = "|"), targeted_cln))

# --------------------compare lockdowns to validated dataset ------------------
# ____________________________________________________________________________
val_data <- read.csv("./policy-database/data/interim/validation_dataset_20200729.csv", stringsAsFactors = FALSE)
val_data$country_code3[val_data$country_code3 == "VMN"] <- "VNM"

country_comparison <- merge(x= val_data, y= db_lockdown, by.x= "country_code3", by.y= "iso", all= TRUE) %>% 
  select(country_code3, country, country_territory_area) %>% distinct()


# merge lockdown data onto val data
val_merge <- merge(x= val_data, y= db_lockdown, by.x= "country_code3", by.y= "iso", all= TRUE) %>%
  select(country_code3:group, country_territory_area, area_covered,
         who_measure, measure_stage, comments, enforcement, non_compliance_penalty, 
         date_start_cln:lockdown_flg_v1)

write.csv(x= val_merge, file= "./policy-database/data/interim/manual_review_dataset.csv")

# --------------------visualize ------------------
# ____________________________________________________________________________

# visualize what's going on
count_lines <- 5
size_width <- 20
db_viz <- db_lockdown  %>% 
  mutate(area_covered_abbrev = substr(area_covered, 1, 15)) %>%
  mutate(comment_nl = str_wrap(paste0(substr(comments, 1, count_lines * size_width), "..."),
                               width= size_width))

# loop through countries and plot results
enforce_col <- gg_color_hue(length(unique(db_viz$enforcement_cln)))
names(enforce_col) <- rev(unique(db_viz$enforcement_cln))
measure_shape <- factor(1:length(unique(db_viz$measure_stage)))
names(measure_shape) <- rev(unique(db_viz$measure_stage))

# write a function to plot a country
plot_country <- function(i){
  loop_country <- sort(unique(db_viz$country_territory_area))[i]
  print(loop_country)

  lock_visual_sub <- db_viz %>% 
    filter(country_territory_area == loop_country) %>%
    mutate(policy_area = ifelse(grepl("national", admin_level), paste0("country_", country_territory_area),
                                ifelse(admin_level == "state", paste0("state_", area_covered_abbrev), "other" )))

  target_fill <- gg_color_hue(length(unique(lock_visual_sub$comments)))
  names(target_fill) <- rev(unique(lock_visual_sub$comments))
  
  # visualize policies
  p <- ggplot(data= lock_visual_sub, aes(color = enforcement_cln, shape= measure_stage)) + 
    geom_point(aes(x= date_start_cln, y= policy_area), size= 5, position= position_dodge(width= 0.7)) + 
    geom_text(aes(x= date_start_cln, label= date_start_cln, y= policy_area), angle= 90,  size = 2.5, position= position_dodge(width= 0.7)) +
    
    geom_label(aes(x= date_start_cln + 8, label= comment_nl, y= policy_area), 
               size= 1.5, alpha= .5, label.padding = unit(.1, "lines"),
               position= position_dodge(width= 0.7))+ 
    # geom_linerange(aes(xmin= date_start_cln, xmax = date_end_cln, y= policy_area), position= position_dodge(width= 0.7)) + 
    scale_color_manual(values = enforce_col) + 
    scale_shape_manual(values = measure_shape) +
    
    ggtitle(loop_country)
    
  
  plot(p)
}

plot_country(64)

# pdf all of the countries for viewing
pdf("./policy-database/notebooks/outputs/who-phsm-lockdown-overview.pdf", width = 11, height= 8.5)
for(i in 1:length(unique(db_lockdown$country_territory_area))){
# for(i in 1:10){
  
  plot_country(i)
}
dev.off() 




# --------------------Recursively create policy strings-----------------------
# ____________________________________________________________________________
db_lockdown <- db_lockdown %>% mutate(following_measure_number_cln = ifelse(following_measure_number == "", NA,following_measure_number))

db_lockdown_new <- db_lockdown %>% filter(measure_stage == "new") %>% 
  select(who_id, prev_measure_number, following_measure_number_cln)

db_lockdown_update <- db_lockdown %>% filter(!(who_id %in% db_lockdown_new$who_id)) %>% 
  select(who_id, prev_measure_number, following_measure_number_cln)

names(db_lockdown_new)[names(db_lockdown_new) == "following_measure_number_cln"] <- "following_measure_number_cln_0"
db_lockdown_string <- db_lockdown_new[FALSE, ]
 i <- 5
 j <- 0
for (i in 1:length(db_lockdown_new$who_id)){
# for (i in 1:6){
  # initialize
  mod_id <- db_lockdown_new[i, ]$following_measure_number_cln_0
  db_lockdown_string_i <- db_lockdown_new[i,]
  

  # add to string while there are following measures
  print(paste("row: ", i, "; id:", db_lockdown_string_i$who_id[1]))
    j <- 0
    while(!is.na(mod_id)){

      db_lockdown_string_i <- merge(x = db_lockdown_string_i, y = db_lockdown_update, 
                              by.x = paste0("following_measure_number_cln_", j), 
                              by.y = "who_id",
                              all.x= T, suffixes = c("", paste0("_", j+1)))
      
      names(db_lockdown_string_i)[names(db_lockdown_string_i) == "following_measure_number_cln"] <- 
        paste0("following_measure_number_cln_", j+1)
      
      
      
      
      
      mod_id <- db_lockdown_string_i[,paste0("following_measure_number_cln_", j+1)]
      j <- j+1
      
      print(paste("row: ", i, "; id:", db_lockdown_string_i$who_id[1],"; merge:", j, "; idnext:", mod_id))
      
    }
    
    db_lockdown_string <- bind_rows(db_lockdown_string, db_lockdown_string_i)
  

}


                 