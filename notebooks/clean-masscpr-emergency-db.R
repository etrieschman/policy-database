# ______________________________________________________________ 
# --------------- clean masscpr constructed dataset------------- 
# ______________________________________________________________
# TO-DOS: NEED TO FIGURE OUT HOW TO APPLY ALL-BUSINESS BEFORE FIRST SUB-BUSINESS POLICY
# ______________________________________________________________

#### Setup ####
dir <- "C:/Users/ErichTrieschman/dev"
setwd(dir)

#### ... packages ####
library(tidyselect)
library(ggplot2)
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
# e_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_emergency_v1_unverifiedPuxi_20200916.csv", stringsAsFactors = FALSE)
e_raw <- read.csv(file= "./policy-database/data/internal/MassCPR_emergency_v1_verified_20200917.csv", stringsAsFactors = FALSE)



# --------------------clean based on summaries  ------------
# ____________________________________________________________________________

# remove et_validated responses
e_cln <- e_raw

# clean categories
e_cln <- e_cln %>%
  mutate(date_announce_cln = as.Date(date_announce, format = "%m/%d/%Y"),
         date_start_cln = as.Date(date_start, format = "%m/%d/%Y"))



# --------------------summarize raw data to understand database  ------------
# ____________________________________________________________________________

dim(e_cln)
min(e_cln$date_start_cln)
max(e_cln$date_start_cln)

# summarize policy distribution
e_summ <- e_cln %>% group_by(response_level) %>% 
  dplyr::summarize(n= n(),
                   min_start_date = min(date_start_cln),
                   max_start_date = max(date_start_cln))


# --------------------create poi policy strings  ------------
# ____________________________________________________________________________

# create strings
e_string <- e_cln %>%
  arrange(province_region, date_start_cln) %>%
  mutate(use_cutoff_date = ifelse(is.na(lead(province_region)) | (!is.na(province_region) & province_region != lead(province_region)), TRUE, FALSE),
         date_end_cln = as.Date(ifelse(use_cutoff_date, date_cutoff, lead(date_start_cln)), origin= date_origin)) %>%
  select(policy_id, province_region, response_level, date_announce_cln, date_start_cln, date_end_cln)



# visualize
# BEFORE OVERRIDES
p_poi <- ggplot(data= e_string) + 
  
  geom_point(aes(x= date_announce_cln, y= response_level, color= response_level), position = position_nudge(y = .2), size= 4, alpha= .5) + 
  geom_linerange(aes(xmin = date_start_cln, xmax= date_end_cln, y= response_level, color= response_level), size= 2) + 
  
  labs(title= "Response levels by city", color= "Response level") + 
  
  facet_grid(province_region ~ ., space= "free_y", scales= "free_y", switch= "y") + 
  scale_x_date(date_labels= "%b-%d", date_breaks= "2 week", limits= c(as.Date("2020-01-01"), date_cutoff)) +
  theme(axis.title = element_blank())

plot(p_poi)




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



# --------------------output dataset  ------------
# ____________________________________________________________________________

write.csv(x= policy_out, file= paste0("./masscpr-policy-database/data/interim/masscpr_policydb_", today,".csv"))







# --------------------Appendix  ------------
# ____________________________________________________________________________


