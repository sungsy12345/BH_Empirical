## -------------------- --------------------
##
## Script name: 4_Firm_Cleaning.R
##
## Email: sysung@berkeley.edu
##
## -------------------- --------------------



##### ##### ##### Merge in Clean Firm Name ##### ##### #####

## Convert LinkedIn Firm Size Categories to Best Match Crunchbase Firm Size Categories
crunchbase_dt[, `:=` (
  firm_size_linkedin = fifelse(firm_size_linkedin == "2-10", "1-10",
                               fifelse(firm_size_linkedin == "51-200", "101-250",
                                       fifelse(firm_size_linkedin == "201-500", "251-500", firm_size_linkedin)))
)]

## Merge LinkedIn Data and Crunchbase Data
crunchbase_dt[, `:=` (
  firm_name_clean = fifelse(!is.na(firm_name_cb), firm_name_cb, 
                            fifelse(!is.na(firm_name_linkedin), firm_name_linkedin, 
                                    fifelse(!is.na(s2_firm_name), s2_firm_name, NA))),
  firm_location_clean = fifelse(!is.na(firm_location_cb), firm_location_cb,
                                fifelse(!is.na(firm_location_linkedin), firm_location_linkedin, NA)),
  firm_size_clean = fifelse(!is.na(firm_size_cb), firm_size_cb, 
                            fifelse(!is.na(firm_size_linkedin), firm_size_linkedin, NA))
)]

firm_dt <- merge(firm_dt,
                 crunchbase_dt[, .(responseid, 
                                   firm_name_clean, firm_location_clean, firm_size_clean, 
                                   firm_name_cb, firm_name_linkedin,
                                   firm_url_cb, firm_url_linkedin,
                                   firm_location_cb, firm_location_linkedin,
                                   firm_size_cb, firm_size_linkedin)],
                 by = "responseid", 
                 all.x = TRUE)

##### ##### ##### Assign Firm Identifier ##### ##### #####

## Assign Unique Firm Identifier Based on Cleaned Firm Names
firm_dt[, firm_name_clean := trimws(firm_name_clean)]
firm_dt[, firm_id := .GRP, by = firm_name_clean]
