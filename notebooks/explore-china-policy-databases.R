# ______________________________________________________________ 
# --------------- explore China policies ------------------- 
# ______________________________________________________________
# ______________________________________________________________

# -----------
# SETUP
# -----------
dir <- "/Users/ErichTrieschman/dev"
idir <- "./policy-database/data/external"
setwd(dir)

# PACKAGES
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(plyr)
library(reshape2)
library(scales)
library(stringr)
library(strex)
library(sqldf)

# color function
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# dates
date_today <- gsub("-", "", Sys.Date())
date_cutoff <- as.Date('2020-07-31')


# Import external policy data
who_raw <- read_csv(paste0(idir, "/who-phsm/WHO_PHSM_Cleaned_V1_20_09_23.csv"))
sol_raw <- read_csv(paste0(idir, "/solomon-hsiang/CHN_processed.csv"))
# cnet_raw <- read_csv(paste0(idir, "/coronanet/coronanet_release.csv"))
# acaps_raw <- read_csv(paste0(idir, "/acaps/acaps_covid19_government_measures_dataset.csv"))
# hit_raw <- read_csv(paste0(idir, "/hit/hit-covid-longdata.csv"))

# import emergency level data
sul_erl <- read_csv("./policy-database/data/interim/masscpr_emergencyresponse_db_20201007.csv")

# import beijing policy data
sul_pa <- read_csv("./policy-database/data/interim/masscpr_policydb_20201008.csv")

# -------------------------
# SUBSET AND SUMMARIZE WHO
# -------------------------

names(who_raw)
unique(who_raw$country_territory_area)
drop_cats <- c("Drug-based measures", "Other measures", "Environmental measures")
# drop_cats <- NULL
who_sub <- who_raw %>% filter(country_territory_area == "China"& !who_category %in% drop_cats)

# CLEAN UP REGIONS
unique(who_sub$area_covered)
who_sub_int <- who_sub %>%
  mutate(regions = ifelse(is.na(area_covered), 1, nchar(gsub("[^,]+", "", area_covered)) + 1),
         regions2 = ifelse(grepl(" and", area_covered), regions + 1, regions),
         test = regions == regions2)
who_sub_exp <- who_sub_int %>%
  uncount(regions2, .remove= FALSE, .id= "id") %>%
  mutate(str_start = ifelse(id == 1, 1, str_locate_nth(area_covered, ",|and", id-1)[,1]),
         str_end = ifelse(id == regions2, nchar(area_covered), lead(str_start) - 1),
         area_covered_cln = str_trim(gsub("-|\\.", " ", gsub(",|and", "", substr(area_covered, str_start, str_end)))),
         area_covered_cln2 = str_trim(gsub("province$|Province$|municipality$", "", 
                                           gsub("Uygur Autonomous Region", "",
                                                gsub("Inner Mongolia|Nei Mongol", "Neimenggu", 
                                                     gsub("Zhejiang", "Zhengjiang", 
                                                          gsub("Shangdong", "Shandong", 
                                                               gsub("SAR|Hong Kong SAR", "Hong Kong",
                                                                    ifelse(is.na(area_covered_cln), "China", area_covered_cln)))))))))
sort(unique(who_sub_exp$area_covered_cln2))

# CLEAN UP MEASURES
unique(who_sub_exp$who_category)
category_summ_pre <- who_sub_exp %>% group_by(who_category, who_subcategory, who_measure) %>% dplyr::summarize(n = n())
targeted_summ_pre <- who_sub_exp %>% group_by(who_category, who_subcategory, who_measure, targeted) %>% dplyr::summarize(n = n())
who_sub_cln <- who_sub_exp %>%
  mutate(who_measure_cln = ifelse(grepl("suspending or restricting international", tolower(who_measure)), 
                                  "Suspending or restricting international travel", 
                                  ifelse(grepl("public gatherings|private gatherings outside", tolower(who_measure)), 
                                         "Canceling, restricting or adapting public gatherings", who_measure)),
         who_category_group = ifelse(grepl("domestic|gatherings|office|school|special pop", tolower(who_subcategory)), 
                                     who_subcategory, who_category),
         who_category_group_cln = str_trim(gsub("measures", "", 
                                                gsub("Surveillance and response", "Surveillance", 
                                                     gsub("Gatherings, businesses and services", "Gatherings", 
                                                          gsub("Offices, businesses, institutions and operations", "Offi, busi, inst", 
                                                               who_category_group))))),
         measure_stage_cln = ifelse(grepl("phase|finish", tolower(measure_stage)), "finish", 
                                    ifelse(grepl("modification|extension", tolower(measure_stage)), "update", measure_stage)),
         targeted_cln = str_trim(tolower(targeted)))
category_summ_post <- who_sub_cln %>% group_by(who_category_group_cln, who_measure_cln) %>% dplyr::summarize(n = n())
unique(who_sub_cln$measure_stage_cln)
targeted_summ_post <- who_sub_cln %>% group_by(who_category_group_cln, who_measure_cln, targeted_cln) %>% dplyr::summarize(n = n())

# -----------------------------
# VALIDATE WHO DATA AGAINST SUL
# -----------------------------
who_sub_cln %>% 
  select(who_category_group_cln, who_measure_cln) %>% 
  filter(!(who_category_group_cln %in% c("International travel", "Surveillance", "Special populations"))) %>% 
  arrange(who_category_group_cln) %>%  distinct()
           
# categorize SUL data into WHO format
names(sul_pa)
sul_pa %>% select(policy_type_cln, policy_sub_type_cln, policy_measure_cln) %>% arrange(policy_type_cln) %>% distinct()
sul_pa_cln <- sul_pa %>% 
  mutate(sul_category_group = ifelse(grepl("busi|offi|cinema|inst", policy_type_cln), "offi, busi, inst", 
                                     ifelse(policy_type_cln == "schools", "school", policy_type_cln)))
sul_pa_cln %>% select(sul_category_group, policy_measure_cln) %>% arrange(sul_category_group) %>% distinct()

# visualize differences between SUL dataset and WHO dataset
g <- ggplot() + 
        geom_rect(data= sul_erl[sul_erl$province_region == "Beijing",], 
                  aes(xmin = date_start_cln, xmax= date_end_cln, ymin= -Inf, ymax= Inf, fill= response_level_cln), alpha = .2) +
        geom_point(data= who_sub_cln[who_sub_cln$area_covered_cln2 %in% c("Beijing", "Wuhan", "China"),], 
                   aes(x= date_start, y= tolower(who_category_group_cln), color = area_covered_cln2),
                   position= position_nudge(y= .1), shape = 22, size = 4, alpha = .9) + 
        geom_point(data= sul_pa_cln, aes(x= date_start_cln, y= sul_category_group),
                   position= position_nudge(y= -.1), color = "darkgrey", shape = 21, size = 4, alpha = .9) + 
        labs(title= "Beijing policy anouncements", subtitle = "Comparing WHO and internal SUL datasets\nSquare = WHO; Circle = SUL",
             color = "WHO regions") + 
        theme(axis.title = element_blank()) + 
        scale_x_date(date_labels= "%b", date_breaks= "1 month", limits = c(as.Date('2020-01-01'), date_cutoff))
plot(g)

# summarize difference
who_sub_cln %>% filter(area_covered_cln2 %in% c("Beijing", "Wuhan", "China")) %>% 
  group_by(who_category_group_cln, area_covered_cln2) %>% 
  dplyr::summarize(n = n()) %>% ungroup() %>%
  pivot_wider(id_cols = who_category_group_cln, names_from = area_covered_cln2, names_prefix = "n_" , values_from= n) %>%
  rowwise() %>% mutate(n_total = sum(across(!who_category_group_cln), na.rm= T),
                       who_category_group_cln = tolower(who_category_group_cln))
sul_pa_cln %>% group_by(sul_category_group) %>%
  dplyr::summarize(n = n())

data_check <- who_sub_cln %>% filter(area_covered_cln2 == "Beijing" & who_category_group_cln == "School")

# ----------------------------------
# VALIDATE WHO DATA AGAINST SOLOMON
# ----------------------------------

names(sol_raw)
sol_cln <- sol_raw %>% select(adm1_name, adm2_name, date, travel_ban_local, emergency_declaration, home_isolation) %>%
  arrange(adm1_name, adm2_name, date) %>%
  group_by(adm1_name, adm2_name) %>%
  mutate(date_start_tb = as.Date(ifelse(date == first(date), date, ifelse(travel_ban_local == lag(travel_ban_local), NA, date)), origin= '1970-01-01'),
         date_start_ed = as.Date(ifelse(date == first(date), date, ifelse(emergency_declaration == lag(emergency_declaration), NA, date)), origin= '1970-01-01'),
         date_start_hi = as.Date(ifelse(date == first(date), date, ifelse(home_isolation == lag(home_isolation), NA, date)), origin= '1970-01-01'))

sol_cln_tb <- sol_cln %>% rename(date_start_tb = date_start_tb) %>% select(adm1_name, adm2_name, date_start) 

sol_cln_st <- rbind(sol_cln %>% select(adm1_name, adm2_name, date_start_tb))

sol_cln[1, "date_start_tb"] <- sol_cln[1, "date"]


# ----------------------
# COMBINE WITH ERLS
# ----------------------

# flag primary level II (the one that follows level I)
sul_erl <- sul_erl %>% group_by(province_region) %>%
  mutate(date_start_min = min(date_start_cln),
         date_start_l1 = max(date_start_cln[response_level_cln == "Level I"], na.rm= T),
         flag_l2_prim = ifelse(response_level_cln == "Level II" & date_start_min == date_start_l1, TRUE, 
                               ifelse(response_level_cln == "Level II" & date_start_cln != date_start_min, TRUE, FALSE)),
         response_level_cln2 = ifelse(response_level_cln == "Level II", ifelse(flag_l2_prim, "Level II", "Level II pre-I"), response_level_cln))

# compare who regions to emergency levels
test <- merge(x= cbind(sul_erl, sul= 1), y= cbind(who_sub_cln, who= 1), by.x= "province_region", 
              by.y= "area_covered_cln2", all= T) %>% select(province_region, sul, who) %>% distinct()

# pull over emergency response levels within the defined date buffer
# create a merge column in the WHO data that maps non-province regions to specific province
default_province <- "Beijing"
who_sub_cln <- who_sub_cln %>%
  mutate(key_area = ifelse(area_covered_cln2 %in% unique(sul_erl$province_region), area_covered_cln2, default_province))

date_buffer <- 7
who_sub_erl <- sqldf(sprintf("SELECT DISTINCT l.*, 
                     r.area_covered_cln2, r.who_category_group_cln, r.who_measure_cln, r.measure_stage_cln, r.targeted, r.enforcement,
                     r.date_start
                     FROM sul_erl as l
                     LEFT JOIN who_sub_cln as r
                     ON (l.province_region == r.key_area)
                     AND (((l.date_start_cln <= r.date_start) AND
                           (l.date_end_cln >= r.date_start)) OR
                          (abs(l.date_start_cln - r.date_start) <= %i) OR
                          (abs(l.date_end_cln - r.date_start) <= %i))", 
                             date_buffer, date_buffer)) %>% as_tibble()

# update province flag
who_sub_erl <- who_sub_erl %>% 
  mutate(area_covered_cln3 = ifelse(is.na(area_covered_cln2), province_region, area_covered_cln2),
         province_match = province_region == area_covered_cln3)

# SUMMARY: how many policies get double counted with our policy overlap?
nrow(who_sub_erl) - nrow(who_sub_cln %>% filter(date_start <= date_cutoff & date_start >= min(sul_erl$date_start_cln)))


# SUMMARY: what cities get dropped in the merge?
# regions and announcements included
nrow(who_sub_cln %>% filter(area_covered_cln2 %in% sul_erl$province_region)) 
nrow(who_sub_cln %>% filter(area_covered_cln2 %in% sul_erl$province_region) %>% select(area_covered_cln2) %>% distinct())

# regions and announcements excluded
(who_dropped_cities <- who_sub_cln %>% filter(!(area_covered_cln2 %in% sul_erl$province_region)) %>% 
    group_by(country_territory_area, admin_level, area_covered_cln2) %>% dplyr::summarize(n= n()))
who_dropped_cities %>% summarize(n = sum(n) - who_dropped_cities[who_dropped_cities$admin_level == "national",]$n)
who_dropped_cities %>% summarize(n = sum(n[area_covered_cln2 %in% c("Hong Kong", "Taiwan", "Wuhan")]))

# ------------------------------------------
# SUMMARIZE ERLS X ANNOUNCEMENT RELATIONSHIP
# ------------------------------------------

# create new flags
who_sub_erl_calc <- who_sub_erl %>%
  mutate(policies = ifelse(!is.na(who_category_group_cln), 1, 0),
         policies_required = ifelse(enforcement == "Required", 1, 0),
         abs_days_from_start = as.numeric(abs(date_start - date_start_cln)),
         abs_days_from_end = as.numeric(abs(date_start - date_end_cln)))

# summarize for broadest category
who_erl_t_cat <- who_sub_erl_calc %>% pivot_wider(id_cols = c(province_region, area_covered_cln3, province_match, response_level_cln2, date_start_cln, date_end_cln), 
                                             names_from = c(who_category_group_cln), names_sort = TRUE,
                                             values_from = c(policies, policies_required, abs_days_from_start, abs_days_from_end), 
                                             values_fn = c(policies = function(x){sum(x, na.rm= T)}, 
                                                           policies_required = function(x){sum(x, na.rm= T)}, 
                                                           abs_days_from_start = function(x){mean(x, na.rm= T)}, 
                                                           abs_days_from_end = function(x){mean(x, na.rm= T)}), values_fill = 0)

test <- who_sub_erl_calc %>% filter(province_region == "Anhui" & who_category_group_cln == "Domestic travel") %>% 
  select(response_level_cln2, province_region, area_covered_cln3, who_category_group_cln, who_measure_cln, date_start_cln, date_end_cln, date_start)

# --------------------
# VISUALIZE POLICIES
# --------------------

# factorize plot variables for consistency in plotting
erl_fact <- gg_color_hue(length(unique(sul_erl$response_level_cln)))
names(erl_fact) <- unique(sort(sul_erl$response_level_cln))
erl_fact
stage_fact <- c(24, 22, 25)
names(stage_fact) <- c("new", "update", "finish")
stage_fact

# function to plot countries
plot_prov <- function(prov){

  who_cntry <- who_sub_cln %>% filter(area_covered_cln2 %in% prov)
  
  if(prov %in% unique(sul_erl$province_region)){
    erl_cntry <- sul_erl %>%  filter(province_region %in% prov)
    subtitle_text <- NULL
  }else {
    erl_cntry <- sul_erl %>% filter(province_region == "Beijing")
    subtitle_text <- " (with Beijing Emergency Levels)"
  }

  p <- ggplot(data= who_cntry) + 
    geom_rect(data= erl_cntry, inherit.aes = FALSE, 
              aes(xmin= date_start_cln, xmax= date_end_cln, ymin= -Inf, ymax= +Inf, 
                  fill= response_level_cln), alpha= .25, stat= "identity") + 
    geom_point(aes(x= date_start, y= who_measure_cln, shape= measure_stage_cln)) + 
    facet_grid(who_category_group_cln ~ ., scales = "free_y") + 
    labs(title= "Policy announcements", subtitle= paste0(prov, subtitle_text), 
         fill= "Emergency Response Level", shape= "Policy announcement type") + 
    theme(axis.title = element_blank()) + 
    
    scale_fill_manual(values = erl_fact) + 
    scale_shape_manual(values = stage_fact) +
    scale_x_date(date_labels= "%b", date_breaks= "1 month", limits = c(as.Date('2020-01-01'), date_cutoff))
  plot(p)
}

# plot countries
sort(unique(who_sub_cln$area_covered_cln2))
plot_prov("Shanghai")
plot_prov("Zhengjiang")
plot_prov("Beijing")
plot_prov("China")

# pdf all of the countries for viewing
pdf(paste0("./policy-database/notebooks/outputs/who-policies-by-chinese-province-", date_today, ".pdf"), width = 11, height= 8.5)
for(i in 1:length(unique(who_sub_cln$area_covered_cln2))){
  
  country <- sort(unique(who_sub_cln$area_covered_cln2))[i]
  plot_prov(country)
}
dev.off() 

# ---------------
# WRITE OUTPUTS
# ---------------

write_csv(x= who_erl_t_cat, file = paste0("./policy-database/notebooks/outputs/who-policy-summary-", date_today, ".csv"))

# ---------------
# APPENDIX
# ---------------


# SOLOMON HSIANG
names(sol_raw)
unique(sol_raw$adm1_name)
sol_raw %>% summarize(min_date= min(date), max_date= max(date))
summary(sol_raw)

# geographic overlap
test <- merge(x= cbind(sul_erl, sul=1), y= cbind(sol_raw, sol= 1), by.x= "province_region", 
              by.y= "adm1_name", all= T) %>% select(province_region, sul, sol) %>% distinct()

# CORONANET
names(cnet_raw)
unique(cnet_raw$country)
cnet_sub <- cnet_raw %>% filter(country == "China")
summary(cnet_sub)
cnet_summ <- cnet_sub %>% group_by(init_country_level, province, city) %>% dplyr::summarize(n= n())
          
                 