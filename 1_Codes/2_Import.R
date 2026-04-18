## -------------------- --------------------
##
## Script name: 2_Import.R
##
## Email: sysung@berkeley.edu
##
## -------------------- --------------------


##### ##### ##### Import Main Survey Data ##### ##### ##### 

## Import Survey Data
firm_dt <- as.data.table(
  read_dta(paste0(data_root, "3output_data/main_empsurvey_cleaned_", date, ".dta")))

  #### Drop Incomplete
  firm_dt <- firm_dt[D_mainsample == 1]
  
  #### Drop Low Quality
  # Correlation between Coding and Test Cases < 0.25
  # HR Speeder (less than 10 seconds on 6 or more resumes)
  # Eng Speeder (less than 12 seconds on 6 or more resumes)
  
  #### Delete Random Assignments for Blinded Group
  vars_blind <- c(
    "resume_ver",
    paste0("race_r", 1:18),
    paste0("gender_r", 1:18),
    paste0("chinese_r", 1:18),
    paste0("indian_r", 1:18)
  )
  
  firm_dt[treat == "blind", (vars_blind) := "blind"]

  rm(vars_blind)
  
  #### Fix Missing Zipcode
  firm_dt[responseid == "R_3dE9sLywFdPIje3", s6_address_5 := "85288"]
  firm_dt[responseid == "R_742eNPPtSA5q1hK", s6_address_5 := "98014"]
  firm_dt[responseid == "R_67O7CShbir99teB", s6_address_5 := "75036"]

  
##### ##### ##### Import Student Data ##### ##### ##### 
  
## Import Student Data
student_dt <- as.data.table(
  read_csv(paste0(data_root, "3output_data/main_empsurvey_student.csv")))

  
##### ##### ##### Import Distribution Data ##### ##### ##### 
  
## Import Qualtrics Distribution Data
qualtrics_distribution_dt <- as.data.table(
  read_csv(paste0(data_root, "3output_data/main_empsurvey_distribution_final.csv")))

## Import Apollo Data

  #### HR's Firm Data
  path <- paste0(data_root, "15firm_recruitment/B_Contact_Sources")
  hr_files <- list.files(path, pattern = "(?i)HR.*\\.csv$", full.names = TRUE)
  apollo_hr_dt <- rbindlist(lapply(hr_files, fread), fill = TRUE)
  setDT(apollo_hr_dt)
  rm(path, hr_files)
  
  #### Eng's Firm Data
  path <- paste0(data_root, "15firm_recruitment/B_Contact_Sources")
  eng_files <- list.files(path, pattern = "(?i)ENG.*\\.csv$", full.names = TRUE)
  apollo_eng_dt <- rbindlist(lapply(eng_files, fread), fill = TRUE)
  setDT(apollo_eng_dt)
  rm(path, eng_files)
  
  
  
##### ##### ##### Import Firm Name (Crunchbase) Data ##### ##### ##### 
  
## Import Student Data
crunchbase_dt <- as.data.table(
  read_csv(paste0(data_root, "3output_data/main_empsurvey_clean_firm_names.csv")))
  

# ##### ##### ##### Import Dynamic Weights Data ##### ##### ##### 
#   
# ## Import Dynamic Weights
# weights_dt <- as.data.table(
#   read_csv(paste0("../../../3output_data/dynamic_weights.csv")))

  
  
  