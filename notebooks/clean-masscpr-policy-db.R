# ______________________________________________________________ 
# --------------- clean masscpr constructed dataset------------- 
# ______________________________________________________________
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
require(tidyselect)
require(ggplot2)
# require(tidyverse)
# require(tidyr)
require(plyr)
require(dplyr)
require(reshape2)
require(scales)
require(stringr)
require(googledrive)
require(ggpubr)
require(sqldf)

# color function
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

today <- gsub( "-","", Sys.Date())
date_cutoff <- as.Date("2020-07-31")


# Import data
# policy_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_level1_v3_unverified_20200818.csv", stringsAsFactors = FALSE)
policy_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_level1_v3_pverified_20200819.csv", stringsAsFactors = FALSE)

business_scope <- c("all_business", "hotel", "restaurant", "entertainment", "market", "sport", "theater")
institution_scope <- c("all_insti", "scenic", "public service", "traffic", "social welfare", "healthcare")

# --------------------clean based on summaries  ------------
# ____________________________________________________________________________

# remove et_validated responses
policy_clean <- policy_raw %>% filter(!(verification_status %in% c("update", "delete")))

# clean categories
policy_clean <- policy_clean %>%
  mutate(policy_type_cln = tolower(ifelse(grepl(paste0(business_scope, collapse= "|"), tolower(policy_scope)), "businesses", 
                                          ifelse(grepl(paste0(institution_scope, collapse= "|"), tolower(policy_scope)), "institutions", policy_type)))) %>%
  
  mutate(policy_sub_type_cln = tolower(ifelse(grepl(paste0(business_scope, collapse= "|"), tolower(policy_scope)), "business adaptations", 
                                              ifelse(grepl(paste0(institution_scope, collapse= "|"), tolower(policy_scope)), "institution adaptations", 
                                                     policy_sub_type)))) %>%
  mutate(policy_measure_cln = tolower(policy_measure)) %>%
  mutate(policy_scope_cln = tolower(policy_scope)) %>%
  mutate(date_announce_cln = as.Date(date_announce, format= "%m/%d/%Y"),
         date_start_cln = as.Date(date_start, format= "%m/%d/%Y"),
         date_end_cln = as.Date(date_end, format= "%m/%d/%Y"))

# update some spelling
policy_clean <- policy_clean %>%
  mutate_at(c("policy_type_cln", "policy_sub_type_cln", "policy_measure_cln", "policy_scope_cln", "enforcer"), function(x){gsub("hygene", "hygiene", x)}) %>%
  mutate_at(c("compliance"), function(x){gsub("recommanded", "recommended", x)})

rbind(lapply(policy_clean, class))
unique(policy_clean$policy_scope)
# --------------------summarize raw data to understand database  ------------
# ____________________________________________________________________________

dim(policy_clean)
min(policy_clean$date_start_cln)
max(policy_clean$date_start_cln)

# summarize policy distribution
policy_summ <- policy_clean %>% group_by(policy_type_cln, policy_sub_type_cln, policy_measure_cln) %>% 
  dplyr::summarize(n= n())

policy_summ_scope <- policy_clean %>% group_by(policy_type_cln, policy_sub_type_cln, policy_measure_cln, policy_scope_cln) %>% 
  dplyr::summarize(n= n(),
                   min_start_date = min(date_start_cln),
                   max_start_date = max(date_start_cln))

temp <- policy_clean %>% filter(policy_scope_cln == "entertainment")


# --------------------create poi policy strings  ------------
# ____________________________________________________________________________

# subset to poi policies
policy_poi_sort <- policy_clean %>% 
  filter(grepl("busi|inst|office|school", policy_type_cln)) %>% 
  mutate(poi_hierarchy = as.numeric(paste0(gsub("\\D", "", policy_measure_cln), 
                                ifelse(grepl("all_", policy_scope_cln), 2, 1)))) %>%
  arrange(policy_type_cln, policy_scope_cln, date_start_cln) %>%
  select(policy_type_cln, policy_sub_type_cln, policy_measure_cln, policy_scope_cln, date_start_cln, date_end_cln, poi_hierarchy, compliance, enforcer)

# create strings of dates
policy_poi_sort$date_string_start <- NA
policy_poi_sort$date_string_end <- NA
policy_poi_sort$date_string_start[1] <- policy_poi_sort$date_start[1]
# define end date
policy_poi_sort$date_string_end[1] <- with(policy_poi_sort, 
                                           ifelse(policy_scope_cln[1] == policy_scope_cln[2] & 
                                                    policy_measure_cln[1] != policy_measure_cln[2],
                                                  max(date_end_cln[1], date_start_cln[2], na.rm= T), date_end_cln[1]))
for(i in 2:nrow(policy_poi_sort)){

  # carry string if same grouping
  policy_poi_sort$date_string_start[i] <- with(policy_poi_sort, 
                                       ifelse(policy_scope_cln[i] == policy_scope_cln[i-1] & 
                                                policy_measure_cln[i] == policy_measure_cln[i-1],
                                              NA, date_start_cln[i]))
  
  # group on start date
  policy_poi_sort$date_string_start[i] <- ifelse(is.na(policy_poi_sort$date_string_start[i]),
                                                 policy_poi_sort$date_string_start[i-1],
                                                 policy_poi_sort$date_string_start[i])
  

  # define end date
  policy_poi_sort$date_string_end[i] <- with(policy_poi_sort, 
                                         ifelse(policy_scope_cln[i] == policy_scope_cln[i+1] & 
                                                  policy_measure_cln[i] != policy_measure_cln[i+1],
                                                max(date_end_cln[i], date_start_cln[i+1], na.rm= T), date_end_cln[i]))


  print(i)
}

# collapse on these strings and clean up the dataset a bit
policy_poi_sort$date_string_end[nrow(policy_poi_sort)] <- policy_poi_sort$date_end_cln[nrow(policy_poi_sort)]

policy_poi_col <- policy_poi_sort %>% 
  group_by(policy_type_cln, policy_sub_type_cln, policy_measure_cln, poi_hierarchy, policy_scope_cln, date_string_start) %>%
  dplyr::summarize(date_string_end = max(date_string_end, na.rm= T)) %>%
  arrange(policy_type_cln, policy_sub_type_cln, date_string_start)

policy_poi_col <- policy_poi_col %>% rowwise() %>% mutate(date_string_end = pmin(abs(date_string_end), date_cutoff))

policy_poi_col$date_string_start <- as.Date(policy_poi_col$date_string_start, origin= "1970-01-01")
policy_poi_col$date_string_end <- as.Date(policy_poi_col$date_string_end, origin= "1970-01-01")

# Override higher-level policies
# policy_poi_override <- sqldf::sqldf("SELECT l.*, r.policy_scope_cln as all_scope, 
#                                     r.policy_measure_cln as all_measure,
#                                     r.date_string_start as all_start, r.date_string_end as all_end
#                                     FROM  policy_poi_col l
#                                     LEFT JOIN  policy_poi_col r
#                                     ON l.policy_type_cln = r.policy_type_cln 
#                                     AND l.poi_hierarchy < r.poi_hierarchy
#                                     AND l.date_string_start >= r.date_string_start
#                                     AND r.date_string_start >= l.date_string_end") %>% as_tibble()



# visualize
ggplot() + 
  geom_linerange(data= policy_poi_col, aes(xmin= date_string_start, xmax= date_string_end, y= policy_scope_cln, color= policy_measure_cln),
               position = position_dodge(width = .7), size= 1.3, alpha= .8) + 
  geom_point(data= policy_poi_sort, aes(x= date_start_cln, y= policy_scope_cln, color= policy_measure_cln, shape= compliance),
             position = position_dodge(width = .7), size= 4, alpha= .5) + 
  
  facet_grid(policy_type_cln ~ ., space= "free_y", scales= "free_y") + 
  scale_x_date(labels= date_format("%b-%Y"), breaks= "1 month")



# --------------------create individual policy strings  ------------
# ____________________________________________________________________________



# --------------------visualize overall policies -----------------------------
# ____________________________________________________________________________

# add new columns for data formatting
policy_viz <- policy_clean %>%
  mutate(policy_sub_viz = ifelse(grepl("busi|inst|office|school", policy_type_cln), policy_measure_cln, policy_sub_type_cln)) %>%
  mutate(policy_viz = factor(policy_type_cln, 
                             levels= c("domestic travel", "gatherings", "businesses", "institutions", "offices", "schools", "individual")))%>%
  mutate_at(c("policy_type_cln", "policy_sub_type_cln", "policy_measure_cln", "policy_scope_cln", "enforcer"), function(x){gsub("_", " ",x)})

# plot policy announcements by policy type and sub type
p_overview <- ggplot(data= policy_viz) + 
  geom_point(aes(x= date_start_cln, y= reorder(policy_sub_viz, desc(policy_sub_viz)), color= record_type)) + 
  facet_grid(policy_viz ~ ., scales= "free_y", space= "free_y", switch= "y") + 
  
  scale_color_discrete(guide= guide_legend(reverse= TRUE)) + 
  scale_x_date(date_labels= "%b-%y") +
  
  labs(title= "COVID-19 policy announcements",  
       subtitle= "Beijing policies by type and sub type",
       color= "Announcement type") + 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top")
  
plot(p_overview)  
  

# --------------------visualize POI  policies -----------------------------
# ____________________________________________________________________________
policy_poi_viz <- policy_viz %>%
  filter(grepl("busi|inst|school|office", policy_type_cln)) 

p_poi <- ggplot(data= policy_poi_viz) + 
  geom_point(aes(x= date_start_cln, y= reorder(policy_scope_cln, desc(policy_scope_cln)),color= policy_measure_cln), 
             size= 2, alpha= .8, position= position_dodge(width= 0.4)) + 
  facet_grid(policy_viz ~ ., scales= "free_y", space= "free_y", switch= "y") + 
  
  labs(title= "COVID-19 place-of-interest (POI) policy announcements",  
       subtitle= "Beijing policy announcements by POI type and measure",
       color= "Policy measure") + 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text= element_text(size= 15))

plot(p_poi)


# --------------------visualize protective behavior  policies -----------------
# ____________________________________________________________________________

# protective policies
policy_m_cln <- policy_viz %>%
  filter(grepl("indiv", policy_type_cln))

p_m <- ggplot(data= policy_m_cln, aes(x= date_start_cln, y= policy_sub_type_cln, color= compliance)) + 
  geom_point(aes(shape= enforcer), 
             size= 3, alpha= .8, position= position_dodge(width= 0.4)) + 
  
  labs(title= "COVID-19 protective behavior (POI) policy announcements",  
       subtitle= "Beijing policy announcements by compliance type and enforcement mechanism",  
       color= "Compliance",
       shape= "Enforcer") + 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        legend.position= "right")

plot(p_m)


# -------------visualize travel, gathering, and protective policies -------------
# ____________________________________________________________________________
policy_trvl_cln <- policy_viz %>%
  filter(grepl("domestic", policy_type_cln)) %>%
  mutate(policy_measure_viz = gsub("l\\d - ", "", policy_measure_cln))

p_trvl <- ggplot(data= policy_trvl_cln) + 
  geom_point(aes(x= date_start_cln, y= target_region, color= policy_measure_viz, shape= policy_scope_cln), 
             size= 3, alpha= .8, position = position_dodge(width = 0.4)) + 
  facet_grid(policy_sub_type ~ ., scales= "free_y", switch= "y") + 

  labs(title= "Domestic travel ",  
       color= "Policy measure",
       shape= "Policy scope") + 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot(p_trvl)

# gathering policies
policy_gath_cln <- policy_viz %>%
  filter(grepl("gather", policy_type_cln)) %>%
  mutate_at(c("policy_measure_cln"), function(x){gsub("l\\d - ", "", x)})

p_gath <- ggplot(data= policy_gath_cln) + 
  geom_point(aes(x= date_start_cln, y= policy_sub_type_cln, color= policy_measure_cln, shape= compliance), 
             size= 3, alpha= .8, position = position_dodge(width = 0.4)) + 

  labs(title= "Gatherings",  
       color= "Policy measure",
       shape= "Policy scope") + 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot(p_gath)




# arrange all plots
test <- ggarrange(p_gath, p_trvl, ncol= 2, nrow= 1)

plot(test)

# --------------------create a dataset for output  ------------
# ____________________________________________________________________________





# --------------------output dataset  ------------
# ____________________________________________________________________________

write.csv(x= policy_out, file= paste0("./masscpr-policy-database/data/interim/masscpr_policydb_", today,".csv"))



















