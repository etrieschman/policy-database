# ______________________________________________________________ 
# --------------- explore China policies ------------------- 
# ______________________________________________________________
# ______________________________________________________________

# -----------
# SETUP
# -----------
dir <- "/Users/etriesch/dev"
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
library(FactoMineR)
library(gridExtra)
library(ggfortify)

# color function
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# dates
date_today <- gsub("-", "", Sys.Date())
date_cutoff <- as.Date('2020-07-31')
date_index <- as.Date("2020-01-01")


# Import external policy data
who_raw <- read_csv(paste0(idir, "/who-phsm/WHO_PHSM_Cleaned_V1_20_09_23.csv"))
sol_raw <- read_csv(paste0(idir, "/solomon-hsiang/CHN_processed.csv"))
sol_src_raw <- read_csv(paste0(idir, "/solomon-hsiang/sources_from_SH_paper.csv"))
sul_beijing <- read_csv("./policy-database/data/internal/MassCPR_level1_v4_verified_20200910.csv") %>% 
  filter(!(verification_status %in% c("update", "delete")))

# import emergency level data
sul_erl <- read_csv("./policy-database/data/interim/masscpr_emergencyresponse_db_20201007.csv")

# -------------------------
# SUBSET AND SUMMARIZE WHO
# -------------------------

who_taxonomy <- who_raw %>% 
  filter(country_territory_area == "China") %>% 
  select(who_category, who_subcategory, who_measure) %>% 
  distinct() %>% arrange(who_category, who_subcategory, who_measure)

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
  mutate(str_start = ifelse(id == 1, 1, strex::str_locate_nth(area_covered, ",|and", id-1)[,1]),
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

# drop "finish" policies
# drop hong kong, taiwan, and macau
who_sub_cln_sub <- who_sub_cln %>% 
  filter(measure_stage_cln != "finish") %>% 
  filter(!grepl("hong kong|macao|taiwan", tolower(area_covered_cln2)))



# get a summary of the transposed data to show how thin it is
summ_who_sub_t <- who_sub_cln_sub %>%
  pivot_wider(id_cols= area_covered_cln2, names_from= who_category_group_cln, values_from= who_id, values_fn= n_distinct) %>%
  rowwise() %>%
  dplyr::mutate(n_policies = sum(across(-area_covered_cln2), na.rm= T)) %>% arrange(-n_policies)

# count remaining rows
summ_who_sub_t[-(1:10),] %>% summarize(n_tenplus = sum(n_policies, na.rm= T))

# ----------------------------------
# RUN PRINCIPAL COMPONENT ANALYSIS
# ----------------------------------

# subset to only new policies without subpopulations
pca_who <- who_sub_cln_sub %>% 
  filter(measure_stage_cln != "finish" & !(area_covered_cln2 %in% c("Hong Kong", "Taiwan", "Macao"))) %>%
  select(area_covered_cln2, who_category_group_cln, who_measure_cln, targeted_cln, date_start) %>%
  mutate(date_start_i = as.numeric(date_start - date_index))
class(pca_who$date_start_i)

# understand records that will get dropped
pca_who_checkdrops <- pca_who %>% group_by(area_covered_cln2, who_category_group_cln, who_measure_cln) %>% dplyr::summarise(n = n()) %>%
  mutate(one = 1,
         n_mone = n - one) %>%
  ungroup() %>%
  dplyr::summarize(across(c(n, one, n_mone), sum, na.rm= T))

# transpose the data in categories
pca_who_t <- pca_who %>% 
  pivot_wider(id_cols= area_covered_cln2, names_from= who_category_group_cln, values_from= date_start_i, values_fn= min)

# how many missing per row?
summ_miss_policy <- pca_who_t %>%
  rowwise() %>%
  dplyr::mutate(n_miss = sum(across(where(is.numeric), ~if_else(is.na(.), 1, 0))),
                pct_miss = n_miss / 7)

ggplot(data= summ_miss_policy) + geom_histogram(aes(x= pct_miss), bins= 10) + 
  labs(title= "Count of regions with missing policies") + 
  xlab("% of policies that are missing (7 total)") + 
  theme(axis.title.y = element_blank())

# impute missing data
pca_who_t_imp <- pca_who_t %>%
  dplyr::mutate(across(where(is.numeric), ~if_else(is.na(.), median(., na.rm= T), as.numeric(.)))) %>%
  column_to_rownames("area_covered_cln2")

pca_policies <- princomp(pca_who_t_imp, cor= TRUE)
summary(pca_policies)
pca_policies$loadings
plot(pca_policies)
pca_policies$loadings
biplot(pca_policies)

# ----------------------------------
# PCA ROBUSTNESS CHECKS
# ----------------------------------

# indata sensitivity options
s_full <- pca_who_t
s_chi <- pca_who_t %>% filter(area_covered_cln2 != "China")
s_cit <- pca_who_t %>% filter(!area_covered_cln2 %in% c("China", "Beijing's Chaoyang district", "Hongshan District of Wuhan",
                                                         "Jia county", "Some regions", "Some provinces"))

# impute function options
s_f_med <- function(x,y){if_else(is.na(x), median(x, na.rm=T), as.numeric(x))}
s_f_rand <- function(x, y){if_else(is.na(x), floor(runif(nrow(y), min=rand_min, max= rand_max+1)), as.numeric(x))}

d <- s_chi
f <- s_f_rand

# write a function to plot biplots
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
run_pca <- function(d, f){
  
  rand_min <- min(dplyr::summarize(d, across(where(is.numeric), min, na.rm= T)))
  rand_max <- max(dplyr::summarize(d, across(where(is.numeric), max, na.rm= T)))
  n_policies <- sum(dplyr::summarize(d, across(where(is.numeric), ~if_else(!is.na(.), 1, 0))))
  dat <- d
  
  pca_in <- d %>% dplyr::mutate(across(where(is.numeric), f, dat)) %>%
    column_to_rownames("area_covered_cln2")
  pca_policies_s <- princomp(pca_in, cor= TRUE)
  # biplot(pca_policies_s)
  b <- autoplot(pca_policies_s, label= T, size= .5, label.size= 3, alpha= .2, label.alpha= .5, loadings= TRUE, loadings.label= TRUE, scale= 0) +
    labs(title= paste0("Regions: ", nrow(d), "; Policies: ", n_policies)) 
    # scale_x_continuous(limits= c(-.5, 1))+
    # scale_y_continuous(limits=c(-.5, 1))
  b
}

# build plots
p_full_med <- run_pca(s_full, s_f_med)
p_full_rand <- run_pca(s_full, s_f_rand)
p_chi_med <- run_pca(s_chi, s_f_med)
p_chi_rand <- run_pca(s_chi, s_f_rand)
p_cit_med <- run_pca(s_cit, s_f_med)
p_cit_rand <- run_pca(s_cit, s_f_rand)

# plot all
# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
grid.arrange(arrangeGrob(top= "Median imputation", left= "Full sample", p_full_med), 
             arrangeGrob(top= "Random imputation", p_full_rand), 
             arrangeGrob(left= "Drop China", p_chi_med), p_chi_rand, 
             arrangeGrob(left= "Drop China and districts", p_cit_med), p_cit_rand, ncol=2)

# -------------------------------------
# RUN MULTIPLE CORRESPONDENCE ANALYSIS
# -------------------------------------

mca_who <- pca_who %>% 
  mutate(flag = 1) %>%
  pivot_wider(id_cols= c(area_covered_cln2, date_start_i), names_from= who_category_group_cln, values_from= flag, values_fn= max) %>%
  dplyr::mutate(across(c(-area_covered_cln2, -date_start_i), ~if_else(is.na(.), FALSE, TRUE)))

test <- MCA(mca_who[,3:9], graph= FALSE)
head(test$var$coord)

fviz_screeplot(test, addlabels = TRUE)

# ------------------------------------
# VALIDATE SOLOMON POLICY DATA IN PCA
# ------------------------------------

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
  
  geom_point(data= who_sub_cln_sub[who_sub_cln_sub$area_covered_cln2 %in% unique(sol_cln_st$adm1_name),], 
             aes(x= date_start, y= area_covered_cln2, color= who_category_group_cln), position= position_jitter(w=0, h= 0.3), size= 2, shape= 18, alpha= 1) + 
  
  labs(title = "Comparing Solomon Hsiang policy data to WHO policy data", 
       subtitle = "Circle = SH policy start; Square = SH policy end; Diamond = WHO policy announcement", 
       color= "WHO policies", fill= "Solomon Hsiang policies") +
  scale_x_date(date_labels= "%b", date_breaks= "1 month", limits = c(as.Date('2020-01-01'), date_cutoff_sh))
h

# merge solomon data with who data
sol_cln_t <- sol_cln_st %>% 
  mutate(date_start_i = as.numeric(date_start - date_index)) %>%
  pivot_wider(id_cols= adm1_name, names_from= policy, values_from= date_start_i, values_fn= min)
class(sol_cln_t$home_isolation)

# sol_cln_t_imp <- sol_cln_t %>%
#   ungroup() %>%
#   dplyr::mutate(across(where(is.numeric), ~if_else(is.na(.), median(., na.rm= T), as.numeric(.))))

# merge and only keep overlapping rows, but first drop any dates that happen later than the last solomon date
pca_who_t_date_cln <- pca_who_t %>% mutate(across(where(is.numeric)), ~if_else(x > date_cutoff_sh, NA, x))
who_sol <- pca_who_t_date_cln %>% inner_join(sol_cln_t, by= c("area_covered_cln2" = "adm1_name"))

# impute
rand_min <- min(dplyr::summarize(who_sol, across(where(is.numeric), min, na.rm= T)))
rand_max <- max(dplyr::summarize(who_sol, across(where(is.numeric), max, na.rm= T)))
n_policies <- sum(dplyr::summarize(who_sol, across(where(is.numeric), ~if_else(!is.na(.), 1, 0))))

who_sol_in <- who_sol %>% dplyr::mutate(across(where(is.numeric), s_f_rand, who_sol)) %>%
  column_to_rownames("area_covered_cln2")
pca_policies_s <- princomp(who_sol_in, cor= TRUE)
b <- autoplot(pca_policies_s, label= T, size= .5, label.size= 3, alpha= .2, label.alpha= .5, loadings= TRUE, loadings.label= TRUE, scale= 0) +
  labs(title= paste0("Regions: ", nrow(who_sol), "; Policies: ", n_policies)) 
b

                
                
pca_who_sol <- princomp(who_sol, cor= TRUE)
summary(pca_who_sol)
pca_who_sol$loadings
plot(pca_who_sol)
biplot(pca_who_sol)
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

# ---------------
# WRITE OUTPUTS
# ---------------

                 