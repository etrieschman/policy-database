# ______________________________________________________________ 
# --------------- explore coronanet database ------------------- 
# ______________________________________________________________
# * Download coronanet
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
# install.packages("ggplot2", dependencies = T)
# install.packages("tidyverse", dependencies = T)
# install.packages("tidyr", dependencies = T)
# install.packages("dplyr", dependencies = T)
# install.packages("reshape2", dependencies = T)
require(ggplot2)
require(tidyverse)
require(tidyr)
require(dplyr)
require(reshape2)

# Import data
cnet_raw <- read.csv("./policy-database/data/external/coronanet/coronanet_release.csv", stringsAsFactors = F)

# Types of policies
unique(cnet_raw$type)

#### subset to lockdown policies####
lockdown_search <- c("lock", "shut", "restrictions of mass", "curfew", "closure and regulation of sch", "restriction and regulation of busi", "restriction of non-essential")
cnet_lockdown <- cnet_raw[grepl(paste(lockdown_search, collapse= "|"),tolower(cnet_raw$type)),]

# what are subpolicies?
names(cnet_lockdown)
lockdown_summary <- cnet_lockdown %>% group_by(type, type_sub_cat, compliance) %>% summarize(N = n())

# --------------------Visualize policy strings ----------------------------
# FIRST ANALYSIS: country level only, lockdown and quarantine events
# __________________________________________________________________


unique(cnet_lockdown$init_country_level)
# Subset to country level that targets all people or all residents
cnet_lockdown_c <- cnet_lockdown[grepl("national", tolower(cnet_lockdown$init_country_level)),] %>% arrange(country, type, type_sub_cat, date_announced)

# compliance column allows for multiple options, select the most sever option for each column
cnet_lockdown_c$max_compliance <- with(cnet_lockdown_c, 
                                       ifelse(grepl("Mandatory with Legal Penalties", compliance), "mandatory_jail",
                                              ifelse(grepl("Mandatory with Fines", compliance), "mandatory_fines",
                                                     ifelse(grepl("Mandatory \\(Unspecified/Implied\\)", compliance), "mandatory_implied",
                                                            ifelse(grepl("Mandatory with Exceptions", compliance), "mandatory_exceptions",
                                                                   ifelse(grepl("Voluntary/Recommended but No Penalties", compliance), "voluntary", NA))))))

                                                                                                                                     
                                                                                                                                                
# easiest approach -- max/min over a policy category
lockdown_c_temp <- cnet_lockdown_c %>% 
  group_by(country, type, type_sub_cat, max_compliance) %>%
  mutate(p_date_announced = min(date_announced), p_date_start = min(date_start), p_date_end = max(date_end), n_updates = n_distinct(policy_id, entry_type)) %>%
  select(country, init_country_level, province, city, type, p_date_announced, p_date_start, p_date_end, n_updates, type_sub_cat, 
         event_description, max_compliance, enforcer, date_announced, date_start, date_end) %>%
  distinct()

# visualize what's going on

# loop through countries and plot results
# for(i in 1:length(unique(cnet_lockdown$country))){
for(i in 1:3){
  loop_country <- sort(unique(cnet_lockdown$country))[i]
  print(loop_country)
  
  lock_visual_sub <- cnet_lockdown %>% 
    filter(country == loop_country) %>%
    mutate(policy_area = ifelse(grepl("national", tolower(init_country_level)), paste0("country_", country),
                               ifelse(tolower(init_country_level) == "provincial", paste0("province_", province),
                                      ifelse(tolower(init_country_level) == "municipal", "city-level", "other" )))) %>%
                                      # ifelse(tolower(init_country_level) == "municipal", paste0("city_", city), "other" )))) %>%
    
    mutate(date_end_cln = as.Date(ifelse(is.na(date_end), Sys.Date(), as.Date(date_end)), origin= "1970-01-01")) %>%
    mutate(date_start = as.Date(date_start)) %>%
    arrange(init_country_level, province, city)
  
  # visualize policies
  p <- ggplot(data= lock_visual_sub, aes(color = type)) + 
    geom_point(aes(x= as.Date(date_announced), y= policy_area), shape= 3) + 
    geom_linerange(aes(xmin= as.Date(date_start), xmax = date_end_cln, y= policy_area), position= position_dodge(width= 0.7)) + 
    ggtitle(loop_country) 
  
  plot(p)
  
}


# QUESTION: is there a new entry for each policy? 
# ANSWER: NO, THERE IS NOT, some policies start with "updates"
temp <- cnet_raw %>% select(policy_id, entry_type) %>% group_by(policy_id, entry_type) %>% summarize(n = n()) %>% 
  mutate(e_newentry = ifelse(entry_type == "new_entry", n,0)) %>% group_by(policy_id) %>% summarize(new_entry = max(e_newentry))


# QUESTION: is there one policy_id for each type_sub_cat at the country-level?
# ANSWER: no, there are multiple. On average, there are 2, for a max, there are 24
temp <- cnet_lockdown_c %>% group_by(type, type_sub_cat, country) %>% summarize(policies = n_distinct(policy_id))
summary(temp$policies)




                 