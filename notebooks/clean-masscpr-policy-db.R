# ______________________________________________________________ 
# --------------- clean masscpr constructed dataset------------- 
# ______________________________________________________________
# ______________________________________________________________

#### Setup ####
dir <- "C:/Users/ErichTrieschman/dev"
setwd(dir)

#### ... packages ####
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

# color function
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

today <- gsub( "-","", Sys.Date())
date_cutoff <- as.Date("2020-07-31")
date_origin <- "1970-01-01"


# Import data
# policy_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_level1_v3_unverified_20200818.csv", stringsAsFactors = FALSE)
# policy_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_level1_v3_pverified_20200819.csv", stringsAsFactors = FALSE)
# policy_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_level1_v3_verified_20200827.csv", stringsAsFactors = FALSE)
policy_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_level1_v4_verified_20200910.csv", stringsAsFactors = FALSE)

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
  mutate(policy_sub_type_cln = ifelse(policy_sub_type_cln %in% c("l2 - private gatherings outside home", "l3 - public gatherings"), 
                                      "l2 - public gatherings",
                                      ifelse(policy_sub_type_cln == "l4 - mass gatherings", "l3 - mass gatherings", policy_sub_type_cln))) %>%
  mutate(policy_measure_cln = tolower(policy_measure)) %>%
  mutate(policy_scope_cln = tolower(policy_scope)) %>%
  mutate(date_announce_cln = as.Date(date_announce, format= "%m/%d/%Y"),
         date_start_cln = as.Date(date_start, format= "%m/%d/%Y"),
         date_end_cln = as.Date(date_end, format= "%m/%d/%Y"))

# update some spelling
policy_clean <- policy_clean %>%
  mutate_at(c("policy_type_cln", "policy_sub_type_cln", "policy_measure_cln", "policy_scope_cln", "enforcer"), function(x){gsub("hygene", "hygiene", x)}) %>%
  mutate_at(c("compliance"), function(x){gsub("recommanded", "recommended", x)})

# make plot-friendly text
policy_clean <- policy_clean %>%
  mutate_at(c("policy_type_cln", "policy_sub_type_cln", "policy_measure_cln", "policy_scope_cln", "enforcer"), function(x){gsub("_", " ",x)}) %>%
  mutate(policy_scope_cln = ifelse(policy_scope_cln == "" & policy_type_cln == "schools", "all schools", policy_scope_cln))

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
  filter(grepl("busi|inst|office|school|cinema", policy_type_cln)) %>% 
  mutate(poi_hierarchy = as.numeric(paste0(gsub("\\D", "", policy_measure_cln), 
                                ifelse(grepl("all_", policy_scope_cln), 2, 1)))) %>%
  arrange(policy_type_cln, policy_scope_cln, compliance, date_start_cln) %>%
  select(policy_type_cln, policy_sub_type_cln, policy_measure_cln, policy_scope_cln, date_start_cln, compliance)

# create strings
policy_poi_string <- policy_poi_sort %>%
  mutate(use_cutoff_date = ifelse(!is.na(lead(compliance)) & (policy_scope_cln == lead(policy_scope_cln) & 
                               compliance == lead(compliance)), FALSE, TRUE),
    date_end_cln = as.Date(ifelse(use_cutoff_date, date_cutoff, lead(date_start_cln)), origin= date_origin)) %>%
  group_by(policy_type_cln, policy_sub_type_cln, policy_scope_cln, compliance) %>%
  mutate(date_start_min = min(date_start_cln, na.rm= T))



# visualize
# BEFORE OVERRIDES
p_poi <- ggplot(data= policy_poi_string) + 
  
  geom_point(aes(x= date_start_cln, y= policy_scope_cln, color= policy_measure_cln, shape= compliance),
             position = position_nudge(y = .2), size= 4, alpha= .5) + 
  
  labs(color= "Policy measure", shape= "Compliance") + 
  
  facet_grid(policy_type_cln ~ ., space= "free_y", scales= "free_y", switch= "y") + 
  scale_x_date(date_labels= "%b-%d", date_breaks= "2 week", limits= c(as.Date("2020-01-20"), date_cutoff)) +
  theme(axis.title = element_blank())

plot(p_poi)

# --------------------override policy strings  ------------
# ____________________________________________________________________________

# create a daily policy dataset
date_range_df <- data.frame(day= seq(min(policy_poi_string$date_start_cln), date_cutoff, by= "1 day"))
date_range_comp_df <- data.frame(compliance= c(rep("mandatory", nrow(date_range_df)), rep("recommended", nrow(date_range_df))), day= date_range_df)

date_scope_df <- sqldf("SELECT DISTINCT l.policy_type_cln, l.policy_sub_type_cln, l.policy_scope_cln, r.compliance, l.date_start_min,
                       r.day
                       FROM policy_poi_string as l
                       LEFT JOIN date_range_comp_df as r")

policy_poi_day <- sqldf("SELECT l.*, 
                        r.policy_measure_cln, r.date_start_cln, r.date_end_cln
                        FROM date_scope_df as l
                        LEFT JOIN policy_poi_string as r
                        ON r.date_start_cln <= l.day
                        AND r.date_end_cln >= l.day
                        AND l.policy_type_cln == r.policy_type_cln
                        AND l.policy_scope_cln == r.policy_scope_cln
                        AND l.compliance == r.compliance") %>% as_tibble()


# override individual policies with "all_" policies
policy_poi_day_all <- policy_poi_day %>% filter(grepl("all ", policy_scope_cln))

# merge on to do the comparison
policy_poi_override <- sqldf("SELECT l.*, r.policy_scope_cln as all_scope,
                              r.policy_measure_cln as all_measure,
                              r.compliance as all_compliance,
                              r.date_start_cln as all_start, r.date_end_cln as all_end
                                   
                              FROM  policy_poi_day l
                              LEFT JOIN  policy_poi_day_all r
                                    
                              ON (l.policy_type_cln = r.policy_type_cln
                              AND l.compliance = r.compliance 
                              AND l.day == r.day
                              AND l.policy_scope_cln != r.policy_scope_cln)
                              AND ((r.date_start_cln <= l.date_start_min)
                                   OR (l.date_start_cln <= r.date_start_cln
                                   AND r.date_start_cln <= l.date_end_cln))
                             
                             ORDER BY l.policy_scope_cln, l.compliance, l.day") %>% as_tibble()

policy_poi_override$all_start <- as.Date(policy_poi_override$all_start, origin= "1970-01-01")
policy_poi_override$all_end <- as.Date(policy_poi_override$all_end, origin= "1970-01-01")

temp <- policy_poi_override %>% filter(policy_scope_cln == "hotels")


# update individual policies if needed
date_gap <- 3
policy_poi_or_cln <- policy_poi_override %>%
  mutate(update_day = ifelse((all_start - date_start_cln) > date_gap, TRUE, FALSE), 
         update_day_minstart = ifelse(!is.na(all_start) & is.na(date_start_cln), TRUE, FALSE),
         policy_measure_cln_u = ifelse(update_day & !is.na(update_day), all_measure, 
                                       ifelse(update_day_minstart & !is.na(update_day_minstart), all_measure, policy_measure_cln)),
         compliance_u = ifelse(update_day_minstart & !is.na(update_day_minstart), all_compliance, compliance))

temp <- policy_poi_or_cln %>% filter(policy_scope_cln == "hotels")

# collapse
policy_poi_or_col <- policy_poi_or_cln %>%
  group_by(policy_type_cln, policy_sub_type_cln, policy_measure_cln, compliance_u, policy_scope_cln, date_start_cln, date_end_cln,
           all_start, all_end, update_day, policy_measure_cln_u) %>%
  summarize(date_start_u = min(day),
            date_end_u = max(day)) %>%
  arrange(policy_type_cln, policy_sub_type_cln, policy_scope_cln, compliance_u, date_start_u) %>%
  ungroup() %>%
  mutate(policy_measure_cln_u = ifelse(is.na(policy_measure_cln_u), "l0 - no policy", policy_measure_cln_u),
         compliance_u = ifelse(is.na(compliance_u), "no policy", compliance_u))

# split into recommendations and requirements
policy_poi_or_col_m <- policy_poi_or_col %>% filter(compliance_u == "mandatory")
policy_poi_or_col_r <- policy_poi_or_col %>% filter(compliance_u == "recommended")


# visualize
# after OVERRIDES
p_poi_o <- ggplot() + 
  
  # mandatory
  geom_linerange(data= policy_poi_or_col_m, aes(xmin= date_start_u, xmax= date_end_u, y= policy_scope_cln,
                     color= policy_measure_cln_u, linetype= compliance_u),
                 position = position_nudge(y = .2), size= 1.3, alpha= .8) +
  geom_point(data= policy_poi_or_col_m, aes(x= date_start_cln, y= policy_scope_cln, color= policy_measure_cln_u, shape= compliance_u),
             position = position_nudge(y = .2), size= 4, alpha= .5) +
  
  # recommended
  geom_linerange(data= policy_poi_or_col_r, aes(xmin= date_start_u, xmax= date_end_u, y= policy_scope_cln,
                                                color= policy_measure_cln_u, linetype= compliance_u),
                 position = position_nudge(y = -.2), size= 1.3, alpha= .8) +
  geom_point(data= policy_poi_or_col_r, aes(x= date_start_cln, y= policy_scope_cln, color= policy_measure_cln_u, shape= compliance_u),
             position = position_nudge(y = -.2), size= 4, alpha= .5) +
  
  labs(color= "Policy measure", shape= "Compliance announcement", linetype = "Compliance imputed") + 
  
  facet_grid(policy_type_cln ~ ., space= "free_y", scales= "free_y", switch= "y") + 
  scale_x_date(date_labels= "%b-%d", date_breaks= "2 week", limits= c(as.Date("2020-01-20"), date_cutoff)) +
  theme(axis.title = element_blank())

plot(p_poi_o)
# Note -- I get a warning here because the missing policy measures aren't being plotted (as we want)


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
  geom_point(aes(x= date_start_cln, y= reorder(policy_sub_viz, desc(policy_sub_viz)), color= policy_type_cln)) + 
  facet_grid(policy_viz ~ ., scales= "free_y", space= "free_y", switch= "y") + 
  
  scale_color_discrete(guide= guide_legend(reverse= TRUE)) + 
  scale_x_date(date_labels= "%b-%d", date_breaks= "1 week") +
  
  
labs(color= "Policy category") + 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.x= element_blank(),
        strip.text.y= element_blank(),
        legend.position= "top")
  
plot(p_overview)  

p_count <- ggplot(data= policy_viz, aes(x= date_start_cln)) + 
  geom_histogram(aes(fill= policy_type_cln), binwidth = 1) + 
  scale_x_date(date_labels= "%b-%d", date_breaks= "1 week") + 
  
  facet_grid(source.type ~ ., switch= "y") + 
  
  labs(y= "Count of policy announcements",
       fill= "Policy category") + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y= element_blank(),
        axis.ticks.y= element_blank(),
        strip.text.y= element_blank(),
        legend.position= "none")

plot(p_count)

grid.newpage()
grid.draw(rbind(ggplotGrob(p_overview), ggplotGrob(p_count), size = "first"))

gtable_show_layout(ggplotGrob(p_overview))
gtable_show_layout(ggplotGrob(p_count))
  

# --------------------visualize protective behavior  policies -----------------
# ____________________________________________________________________________

# protective policies
policy_m_cln <- policy_viz %>%
  filter(grepl("indiv", policy_type_cln))

p_m <- ggplot(data= policy_m_cln, aes(x= date_start_cln, y= policy_sub_type_cln, color= compliance)) + 
  geom_point(aes(shape= enforcer), 
             size= 3, alpha= .8, position= position_dodge(width= 0.4)) + 
  
  scale_x_date(date_labels= "%b-%d", date_breaks= "1 week", limits= c(as.Date("2020-01-20"), as.Date("2020-05-01"))) +
  
  labs(title= "Protective behavior", 
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
  facet_wrap(policy_sub_type ~ ., strip.position= "top", ncol= 1, scales= "free_y") + 
  
  scale_x_date(date_labels= "%b-%d", date_breaks= "1 week", limits= c(as.Date("2020-01-20"), as.Date("2020-05-01"))) +

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
  
  scale_x_date(date_labels= "%b-%d", date_breaks= "1 week", limits= c(as.Date("2020-01-20"), as.Date("2020-05-01"))) +

  labs(title= "Gatherings",  
       color= "Policy measure",
       shape= "Compliance") + 
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

plot(p_gath)




# arrange all plots
grid.newpage()
grid.draw(rbind(ggplotGrob(p_m), ggplotGrob(p_gath), ggplotGrob(p_trvl), size = "max"))



# --------------------create a dataset for output  ------------
# ____________________________________________________________________________





# --------------------output dataset  ------------
# ____________________________________________________________________________

write.csv(x= policy_out, file= paste0("./masscpr-policy-database/data/interim/masscpr_policydb_", today,".csv"))







# --------------------Appendix  ------------
# ____________________________________________________________________________

# INDEXING WAY OF CREATING POLICY STRINGS

# create strings of dates
policy_poi_sort$date_string_start <- NA
policy_poi_sort$date_string_end <- NA
policy_poi_sort$date_string_start[1] <- policy_poi_sort$date_start[1]
# define end date
policy_poi_sort$date_string_end[1] <- with(policy_poi_sort, 
                                           ifelse(policy_scope_cln[1] == policy_scope_cln[2] & 
                                                    policy_measure_cln[1] != policy_measure_cln[2],
                                                  date_start_cln[i+1], NA))
for(i in 2:nrow(policy_poi_sort)){
  
  # carry string if same grouping
  policy_poi_sort$date_string_start[i] <- with(policy_poi_sort, 
                                               ifelse(policy_scope_cln[i] == policy_scope_cln[i-1] & 
                                                        policy_measure_cln[i] == policy_measure_cln[i-1] & 
                                                        compliance[i] == compliance[i-1],
                                                      NA, date_start_cln[i]))
  
  # group on start date
  policy_poi_sort$date_string_start[i] <- ifelse(is.na(policy_poi_sort$date_string_start[i]),
                                                 policy_poi_sort$date_string_start[i-1],
                                                 policy_poi_sort$date_string_start[i])
  
  
  # define end date
  policy_poi_sort$date_string_end[i] <- with(policy_poi_sort, 
                                             ifelse(policy_scope_cln[i] == policy_scope_cln[i+1] & 
                                                      compliance[i] == compliance[i+1] & 
                                                      policy_measure_cln[i] != policy_measure_cln[i+1],
                                                    date_start_cln[i+1], NA))
  
  
  print(i)
}

# collapse on these strings and clean up the dataset a bit
policy_poi_col <- policy_poi_sort %>% 
  group_by(policy_type_cln, policy_sub_type_cln, policy_measure_cln, compliance, policy_scope_cln, date_string_start) %>%
  dplyr::summarize(date_string_end = max(date_string_end, na.rm= T)) %>%
  arrange(policy_type_cln, policy_sub_type_cln, date_string_start)

# set end dates to last date of analysis (if unavailable)
policy_poi_col <- policy_poi_col %>% rowwise() %>% mutate(date_string_end = pmin(abs(date_string_end), date_cutoff))

# format dates
policy_poi_col$date_string_start <- as.Date(policy_poi_col$date_string_start, origin= "1970-01-01")
policy_poi_col$date_string_end <- as.Date(policy_poi_col$date_string_end, origin= "1970-01-01")










