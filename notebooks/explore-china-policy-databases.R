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
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
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


# Import external policy data
who_raw <- read_csv(paste0(idir, "/who-phsm/WHO_PHSM_Cleaned_V1_20_09_23.csv"))
sol_raw <- read_csv(paste0(idir, "/solomon-hsiang/CHN_processed.csv"))
cnet_raw <- read_csv(paste0(idir, "/coronanet/coronanet_release.csv"))
acaps_raw <- read_csv(paste0(idir, "/acaps/acaps_covid19_government_measures_dataset.csv"))
hit_raw <- read_csv(paste0(idir, "/hit/hit-covid-longdata.csv"))

# import emergency level data
sul_erl <- read_csv("./policy-database/data/interim/masscpr_emergencyresponse_db_20201001.csv")

# -------------------
# SUBSET AND SUMMARIZE WHO
# -------------------

names(who_raw)
unique(who_raw$country_territory_area)
drop_cats <- c("Drug-based measures", "Other measures", "Environmental measures")
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
         area_covered_cln2 = str_trim(gsub("province$|Province$", "", 
                                           gsub("Uygur Autonomous Region", "",
                                                gsub("Inner Mongolia|Nei Mongol", "Neimenggu", 
                                                     gsub("Zhejiang", "Zhengjiang", 
                                                          gsub("Shangdong", "Shandong", area_covered_cln)))))))
sort(unique(who_sub_exp$area_covered_cln2))

# compare who regions to emergency levels
test <- merge(x= cbind(sul_erl, sul= 1), y= cbind(who_sub_exp, who= 1), by.x= "province_region", 
              by.y= "area_covered_cln2", all= T) %>% select(province_region, sul, who) %>% distinct()

# pull over emergency response levels within the defined date buffer
date_buffer <- 0
who_sub_erl <- sqldf("SELECT DISTINCT l.*, 
                     r.who_category, r.who_subcategory, r.who_measure, r.measure_stage, r.targeted, r.enforcement,
                     r.date_start
                     FROM sul_erl as l
                     LEFT JOIN who_sub_exp as r
                     ON l.province_region == r.area_covered_cln2
                     AND ((l.date_start_cln <= r.date_start + '$date_buffer') OR 
                         (l.date_start_cln <= r.date_start - '$date_buffer'))
                     AND ((l.date_end_cln >= r.date_start + '$date_buffer') OR 
                         (l.date_start_cln >= r.date_start - '$date_buffer'))") %>% as_tibble()


# what cities get dropped in the merge?
(who_dropped_cities <- who_sub_exp %>% 
  filter(!(area_covered_cln2 %in% sul_erl$province_region)) %>% 
  select(country_territory_area, admin_level, area_covered_cln2) %>% 
  distinct())

# --------------------
# VISUALIZE POLICIES
# --------------------

prov <- "Beijing"

p <- ggplot(data= who_sub_exp[who_sub_exp$area_covered_cln2 == prov,]) + 
  geom_rect(data= sul_erl[sul_erl$province_region == prov, ], inherit.aes = FALSE, 
            aes(xmin= date_start_cln, xmax= date_end_cln, ymin= -Inf, ymax= +Inf, fill= response_level_cln), alpha= .25, stat= "identity") + 
  geom_point(aes(x= date_start, y= who_measure, color = who_subcategory, shape= measure_stage)) + 
  facet_grid(who_category ~ ., scales = "free_y", space = "free_y") + 
  guides(fill= "none", color= "none")

plot(p)

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
          
                 