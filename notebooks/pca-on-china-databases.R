# ______________________________________________________________ 
# --------------- explore China policies ------------------- 
# ______________________________________________________________
# ______________________________________________________________

# -----------
# SETUP
# -----------
dir <- "/Users/ErichTrieschman/Dropbox (MIT)"
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
sol_src_raw <- read_csv(paste0(idir, "/solomon-hsiang/sources_from_SH_paper.csv"))

# import emergency level data
sul_erl <- read_csv("./policy-database/data/interim/masscpr_emergencyresponse_db_20201007.csv")

# -------------------------
# SUBSET AND SUMMARIZE WHO
# -------------------------

who_taxonomy <- who_raw %>% 
  filter(country_territory_area == "China") %>% 
  select(who_category, who_subcategory) %>% 
  distinct() %>% arrange(who_category, who_subcategory)

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

# ----------------------------------
# RUN PRINCIPAL COMPONENT ANALYSIS
# ----------------------------------

# subset to only new policies without subpopulations
pca_who <- who_sub_cln %>% 
  filter(measure_stage_cln != "finish" & !(area_covered_cln2 %in% c("Hong Kong", "Taiwan", "Macao"))) %>%
  select(area_covered_cln2, who_category_group_cln, who_measure_cln, targeted_cln, date_start) %>%
  mutate(date_start_i = as.numeric(date_start - as.Date("2020-01-01")))
class(pca_who$date_start_i)

# understand records that will get dropped
pca_who_checkdrops <- pca_who %>% group_by(area_covered_cln2, who_category_group_cln, who_measure_cln) %>% dplyr::summarise(n = n())

pca_who_t <- pca_who %>% 
  pivot_wider(id_cols= area_covered_cln2, names_from= who_category_group_cln, values_from= date_start_i, values_fn= min)

pca_who_t_imp <- pca_who_t %>%
  rowwise() %>%
  dplyr::mutate(drop_median = max(across(-area_covered_cln2), na.rm= T)) %>%
  dplyr::mutate(across(where(is.numeric), function(x){ifelse(is.na(x), drop_median, x)})) %>%
  select(-drop_median, -area_covered_cln2)

test <- princomp(pca_who_t_imp, cor= TRUE)
head(test$x)
plot(test)
test$loadings
biplot(test)

# ----------------------------------
# VALIDATE WHO DATA AGAINST SOLOMON
# ----------------------------------

date_cutoff_sh <- max(sol_raw$date)

names(sol_raw)
sol_cln <- sol_raw %>% select(adm1_name, adm2_name, date, travel_ban_local, emergency_declaration, home_isolation) %>%
  arrange(adm1_name, adm2_name, date) %>%
  group_by(adm1_name, adm2_name) %>%
  mutate(date_start_tb = as.Date(ifelse(date == first(date), date, ifelse(travel_ban_local == lag(travel_ban_local), NA, date)), origin= '1970-01-01'),
         date_start_ed = as.Date(ifelse(date == first(date), date, ifelse(emergency_declaration == lag(emergency_declaration), NA, date)), origin= '1970-01-01'),
         date_start_hi = as.Date(ifelse(date == first(date), date, ifelse(home_isolation == lag(home_isolation), NA, date)), origin= '1970-01-01'))

# travel ban
sol_cln_tb <- sol_cln %>% dplyr::rename(date_start = date_start_tb) %>% filter(!is.na(date_start)) %>%
  mutate(date_end = as.Date(ifelse(adm2_name == lead(adm2_name), lead(date_start) - 1, date_cutoff_sh), origin= '1970-01-01'),
         policy = "travel_ban") %>%
  filter(travel_ban_local == 1) %>%
  select(adm1_name, adm2_name, policy, date_start, date_end) 

# emergency declaration
sol_cln_ed <- sol_cln %>% dplyr::rename(date_start = date_start_ed) %>% filter(!is.na(date_start)) %>%
  mutate(date_end = as.Date(ifelse(adm2_name == lead(adm2_name), lead(date_start) - 1, date_cutoff_sh), origin= '1970-01-01'),
         policy = "emergency_declaration") %>%
  filter(emergency_declaration == 1) %>%
  select(adm1_name, adm2_name, policy, date_start, date_end) 

# home isolation
sol_cln_hi <- sol_cln %>% dplyr::rename(date_start = date_start_hi) %>% filter(!is.na(date_start)) %>%
  mutate(date_end = as.Date(ifelse(adm2_name == lead(adm2_name), lead(date_start) - 1, date_cutoff_sh), origin= '1970-01-01'),
         policy = "home_isolation") %>%
  filter(home_isolation == 1) %>%
  select(adm1_name, adm2_name, policy, date_start, date_end) 

# stack
sol_cln_st <- rbind(sol_cln_tb, sol_cln_ed, sol_cln_hi)

# is there intra-province variation?
sol_cln_summ <- sol_cln_st %>% group_by(adm1_name, policy) %>%
                  dplyr::summarize(n = n(), cities = n_distinct(adm2_name),
                   distinct_start_dates = n_distinct(date_start),
                   distinct_end_dates = n_distinct(date_end))
# yes there is some
h <- ggplot() + 
  geom_point(data= sol_cln_st, aes(x= date_start, y= adm1_name, fill = policy), 
             position= position_jitter(w= 0, h= 0), color= "lightgrey", alpha= .2, shape= 21, size= 4) + 
  
  geom_point(data= sol_cln_st[sol_cln_st$date_end != date_cutoff_sh,], 
             aes(x= date_end, y= adm1_name, fill = policy), 
             position= position_jitter(w= 0, h= 0), color= "lightgrey", alpha= .2, shape= 22, size= 4) + 
  
  geom_point(data= who_sub_cln[who_sub_cln$area_covered_cln2 %in% unique(sol_cln_st$adm1_name),], 
             aes(x= date_start, y= area_covered_cln2, color= who_category_group_cln), position= position_jitter(w=0, h= 0.3), size= 2, shape= 18, alpha= 1) + 
  
  labs(title = "Comparing Solomon Hsiang policy data to WHO policy data", 
       subtitle = "Circle = SH policy start; Square = SH policy end; Diamond = WHO policy announcement", 
       color= "WHO policies", fill= "Solomon Hsiang policies") +
  scale_x_date(date_labels= "%b", date_breaks= "1 month", limits = c(as.Date('2020-01-01'), date_cutoff_sh))
h


# ----------------------------------
# SOLOMON LOCKDOWN DATA
# ----------------------------------

# understand solomon sources
temp <- sol_sources %>% group_by(policy, source) %>% dplyr::summarize(n = n()) %>% arrange(-n)

# clean manual data created from solomon sources
sol_src_cln <- sol_src_raw %>% select(-sah_original, -source, -notes, -place, -et_manual) %>% 
  distinct() %>% arrange(province, place_cln, date_start, sah_standard) %>%
  mutate(flag_any_policy_overlap = date_start == lead(date_start) & place_cln == lead(place_cln) & 
           province == lead(province) & sah_standard != lead(sah_standard),
         flag_shutdown_policy_overlap = flag_any_policy_overlap & sah_standard != "closed management",
         flag_date_overlap = place_cln == lead(place_cln) & province == lead(province) & date_end > lead(date_start))

sol_scr_summ <- sol_src_cln %>% filter(sah_standard == "partial shutdown") %>%
  group_by(city_level) %>%
  dplyr::summarize(n = n_distinct(ifelse(is.na(place_cln), province, place_cln)),
                   date_start_earliest = min(date_start),
                   date_start_latest = max(date_start))

sol_scr_partial <- sol_src_cln %>% filter(sah_standard == "partial shutdown") %>% select(province:sah_standard)
  
# plot to visualize the data
g <- ggplot(data= sol_src_cln) + 
  geom_point(aes(x= date_start, y= place_cln, color= sah_standard), alpha= .7, size= 2) + 
  geom_linerange(aes(xmin= date_start, xmax= date_end, y= place_cln, color= sah_standard), size= .75) + 
  facet_grid(province ~ ., scales= "free_y", space= "free_y") + 
  labs(title= "Stay-at-home (SAH) measures from SH paper sources", color= "SAH measures") + 
  theme(axis.text.y = element_text(size= 6, angle= 45),
        strip.text.y = element_text(size= 8, angle= 0),
        axis.title = element_blank())
g

# ----------------------
# WHO BUSINESS CLOSURES
# ----------------------

# categorize pois
keep_cats <- c("Offi, busi, inst", "Gatherings", "School")
drop_meas <- c("closing internal land", "restricting entry")
restaurant <- c("restaurant", "catering", "bar", "pub", "eater")
entertainment <- c("entertainment", "party", "karaoke", "hair", "beauty", "salon", "club", "cinema", 
                   "parlor", "bathhouse", "performance", "casino", "recreation")
hotels <- c("hotel")
sports <- c("sport", "fitness", "gym")
markets <- c("market", "food", "convenience", "salmon", "outlet")
office_govt <- c("civil", "immigration", "facilit", "govt employees", "government employees")
office_comm <- c("office", "workplace", "non-essential busi", "factori")
scenic <- c("disney", "religi", "church", "baseball", "event", "cultural", "leisure", 
            "beach", "cultural", "museum", "playground", "tour", "temple")
govt_service <- c("government service", "driving test", "librar", "social welf")
all_emp_busi <- c("all employee", "all busine", "all worker")

# categorize policy stage
stage_finish <- c("reopen", "lifted", "resume", "back to", "opened", "resumption", "coming to an end", 
            "can take off their facemask", "open", "return to", "lift lock", "to return", "permitted to")
stage_finish_negate <- "not to resume|not resume|deferred resumption|renewed closure|reopen date pending|will be postponed"


# create pois
who_poi <- who_sub_cln %>% 
  filter(who_category_group_cln %in% keep_cats & 
           !grepl(paste0(drop_meas, collapse= "|"), tolower(who_measure_cln))) %>%
  mutate(poi_restaurant = grepl(paste0(restaurant, collapse= "|"), targeted_cln),
         poi_entertainment = grepl(paste0(entertainment, collapse= "|"), targeted_cln),
         poi_hotel = grepl(paste0(hotels, collapse= "|"), targeted_cln),
         poi_sports = grepl(paste0(sports, collapse= "|"), targeted_cln),
         poi_markets = grepl(paste0(markets, collapse= "|"), targeted_cln),
         poi_off_government = grepl(paste0(office_govt, collapse= "|"), targeted_cln),
         poi_off_commercial = grepl(paste0(office_comm, collapse= "|"), targeted_cln),
         poi_scenic_spot = grepl(paste0(scenic, collapse= "|"), targeted_cln),
         poi_govt_service = grepl(paste0(govt_service, collapse= "|"), targeted_cln),
         poi_all_busi_all_emp = grepl(paste0(all_emp_busi, collapse= "|"), targeted_cln),
         poi_any = poi_restaurant | poi_entertainment | poi_hotel | poi_sports | poi_markets | 
           poi_off_government  | poi_off_commercial | poi_scenic_spot | poi_govt_service | poi_all_busi_all_emp,
         school = who_category_group_cln == "School",
         all_gather = who_category_group_cln == "Gatherings",
         keep_check = poi_any | school | all_gather) %>%
  mutate(stage_reopen = grepl(paste0(stage_finish, collapse= "|"), tolower(comments)) & 
           !grepl(stage_finish_negate, tolower(comments)))

temp <- who_poi %>% select(who_category_group_cln, who_measure_cln, targeted_cln, starts_with("poi_"), school, all_gather, keep_check) %>% 
  distinct() %>% arrange(who_category_group_cln, who_measure_cln)

temp <- who_poi %>% group_by(who_category_group_cln, measure_stage_cln) %>% dplyr::summarize(n= n())

who_poi_t <- who_poi %>% pivot_longer(cols= c(starts_with("poi_"), school, all_gather), 
                                      names_to= "targeted_cln2", values_to= "keep") %>%
  select(area_covered_cln2, key_area, who_category_group_cln, who_measure_cln, measure_stage_cln , targeted_cln2, keep, enforcement, date_start) %>% 
  filter(keep == TRUE & targeted_cln2 != "poi_any") %>% distinct()

# visualize
g <- ggplot(data= who_poi_t) + 
  geom_point(aes(x= date_start, y= who_measure_cln, color= targeted_cln2, shape= measure_stage_cln), 
             size= 2, alpha= .5, position= position_jitter(w= 0, h= .15)) + 
  facet_grid(who_category_group_cln ~ ., space= "free_y", scales= "free_y") + 
  labs(title= "Categorized WHO measures", color= "POI categories", shape= "Announcement type") + 
  theme(axis.title = element_blank())

g


# ---------------
# WRITE OUTPUTS
# ---------------

                 