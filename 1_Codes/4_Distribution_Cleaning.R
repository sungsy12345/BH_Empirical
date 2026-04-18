## -------------------- --------------------
##
## Script name: 5_Distribution_Cleaning.R
##
## Email: sysung@berkeley.edu
##
## -------------------- --------------------




##### ##### ##### Match Firm & Distribution Data ##### ##### #####

#### Mark in Firm data (emailed, bounced, responded)
emailed_vec <- unique(tolower(na.omit(qualtrics_distribution_dt$`Email Address`)))
bounced_vec <- unique(tolower(na.omit(qualtrics_distribution_dt[Status == "Email Hard Bounce" | Status == "Email Soft Bounce"]$`Email Address`)))
attempted_vec <- unique(tolower(na.omit(qualtrics_distribution_dt[Status == "Survey Finished"]$`Email Address`)))
finished_vec <- unique(tolower(na.omit(c(firm_dt[gc==1]$RecipientEmail, firm_dt[gc==1]$intro_email))))

apollo_hr_dt[, emailed := fifelse(!is.na(Email) & tolower(Email) %chin% emailed_vec, 1L, 0L)]
apollo_eng_dt[, emailed := fifelse(!is.na(Email) & tolower(Email) %chin% emailed_vec, 1L, 0L)]

apollo_hr_dt[, bounced := fifelse(!is.na(Email) & tolower(Email) %chin% bounced_vec, 1L, 0L)]
apollo_eng_dt[, bounced := fifelse(!is.na(Email) & tolower(Email) %chin% bounced_vec, 1L, 0L)]

apollo_hr_dt[, emailed_rev := fifelse(emailed == 1 & bounced == 0, 1L, 0L)]
apollo_eng_dt[, emailed_rev := fifelse(emailed == 1 & bounced == 0, 1L, 0L)]

apollo_hr_dt[, attempted := fifelse(!is.na(Email) & tolower(Email) %chin% attempted_vec, 1L, 0L)]
apollo_eng_dt[, attempted := fifelse(!is.na(Email) & tolower(Email) %chin% attempted_vec, 1L, 0L)]

apollo_hr_dt[, finished := fifelse(!is.na(Email) & tolower(Email) %chin% finished_vec, 1L, 0L)]
apollo_eng_dt[, finished := fifelse(!is.na(Email) & tolower(Email) %chin% finished_vec, 1L, 0L)]

#### Remove Intermediate Files
rm(emailed_vec, bounced_vec, attempted_vec, finished_vec)

## Combine HR and Engineer Distribution Database
apollo_combined_dt <- rbind(apollo_hr_dt[, role := "HR"], apollo_eng_dt[, role := "Eng"], fill = TRUE)
apollo_combined_dt[, role := factor(role, levels = c("HR", "Eng"))]
rm(apollo_hr_dt, apollo_eng_dt)
