## -------------------- --------------------
##
## Script name: 6_Hiring_Simulation.R
##
## Email: sysung@berkeley.edu
##
## -------------------- --------------------



##### ##### ##### ##### ##### Create HR - Engineer Pair Crosswalks ##### ##### ##### ##### #####

##### ##### Full Matching

  #### Identify Response IDs for Blind and Nonblind by Role
  hr_blind <- unique(
    firm_dt[role == "HR" & treat == "blind",
            .(responseid_hr  = responseid,
              treat_hr       = treat,
              resume_ver_hr  = "blind")]   # force version label "blind"
  )
  
  hr_nonblind <- unique(
    firm_dt[role == "HR" & treat == "nonblind",
            .(responseid_hr  = responseid,
              treat_hr       = treat,
              resume_ver_hr  = resume_ver)]
  )
  
  eng_blind <- unique(
    firm_dt[role == "Eng" & treat == "blind",
            .(responseid_eng = responseid,
              treat_eng      = treat,
              resume_ver_eng = "blind")]
  )
  
  eng_nonblind <- unique(
    firm_dt[role == "Eng" & treat == "nonblind",
            .(responseid_eng = responseid,
              treat_eng      = treat,
              resume_ver_eng = resume_ver)]
  )
  
  
  #### Complete BH
  complete_bh_pairs <- CJ(
    responseid_hr  = hr_blind$responseid_hr,
    responseid_eng = eng_blind$responseid_eng
  )[
    hr_blind, 
    on = "responseid_hr"
  ][
    eng_blind,
    on = "responseid_eng"
  ][,
    arm := "Complete_BH"
  ]
  
  #### Realistic BH
  realistic_bh_pairs <- CJ(
    responseid_hr  = hr_blind$responseid_hr,
    responseid_eng = eng_nonblind$responseid_eng
  )[
    hr_blind,
    on = "responseid_hr"
  ][
    eng_nonblind,
    on = "responseid_eng"
  ][,
    arm := "Realistic_BH"
  ]
  
  #### Control (Non-Blind)
  nonblind_pairs <- hr_nonblind[
    eng_nonblind,
    on = .(resume_ver_hr = resume_ver_eng),
    allow.cartesian = TRUE
  ][
    eng_nonblind, 
    on = c("responseid_eng", "treat_eng")
  ][,
    arm := "Control"
  ]
  
  ## Append
  matching_crosswalk <- rbindlist(
    list(
      nonblind_pairs,
      realistic_bh_pairs,
      complete_bh_pairs
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  #### Reorder Variables
  setcolorder(matching_crosswalk,
              c("arm",
                "responseid_hr", "treat_hr", "resume_ver_hr",
                "responseid_eng", "treat_eng", "resume_ver_eng"))
  
  #### Factorize Arm
  matching_crosswalk[, arm := factor(
    arm,
    levels = c("Complete_BH", "Realistic_BH", "Control")
  )]
  
  rm(hr_blind, hr_nonblind,
     eng_blind, eng_nonblind,
     nonblind_pairs,
     realistic_bh_pairs,
     complete_bh_pairs)
  
  
##### ##### Matching on Firm Characteristics
  
  
  
##### ##### Matching on Job Characteristics
  
  #### Create A Copy
  matching_crosswalk_jobs <- copy(matching_crosswalk)
  
  #### Merge in Job Information
  jobs_hr <- firm_dt[role == "HR", c(.(responseid_hr = responseid), setNames(.SD, paste0("job_", 1:17, "_hr"))), .SDcols = paste0("job_", 1:17)]
  jobs_eng <- firm_dt[role == "Eng", c(.(responseid_eng = responseid), setNames(.SD, paste0("job_", 1:17, "_eng"))), .SDcols = paste0("job_", 1:17)]
  
  matching_crosswalk_jobs <- merge(matching_crosswalk_jobs, jobs_hr, by = "responseid_hr")
  matching_crosswalk_jobs <- merge(matching_crosswalk_jobs, jobs_eng, by = "responseid_eng")
  
  #### Keep Matches Based on Job Categories
  matching_crosswalk_jobs[, `:=` (
    
    # Cluster 1: Core/Backend (Jobs 13, 2, 7)
    match_core  = as.integer((job_13_hr | job_2_hr | job_7_hr) & 
                               (job_13_eng | job_2_eng | job_7_eng)),
    
    # Cluster 2: Frontend/UI (Jobs 6, 7, 15, 16, 10)
    match_front = as.integer((job_6_hr | job_7_hr | job_15_hr | job_16_hr | job_10_hr) & 
                               (job_6_eng | job_7_eng | job_15_eng | job_16_eng | job_10_eng)),
    
    # Cluster 3: Data & AI (Jobs 1, 9)
    match_data  = as.integer((job_1_hr | job_9_hr) & 
                               (job_1_eng | job_9_eng)),
    
    # Cluster 4: Infrastructure/Systems (Jobs 5, 14, 11, 8, 4)
    match_infra = as.integer((job_5_hr | job_14_hr | job_11_hr | job_8_hr | job_4_hr) & 
                               (job_5_eng | job_14_eng | job_11_eng | job_8_eng | job_4_eng))
  )]
  
  matching_crosswalk_jobs <- matching_crosswalk_jobs[match_core == 1 | match_front == 1 | match_data == 1 | match_infra == 1]
  
  vars_to_drop <- c(paste0("job_", 1:17, "_hr"), paste0("job_", 1:17, "_eng"))
  matching_crosswalk_jobs[, (vars_to_drop) := NULL]
  
  rm(vars_to_drop)

  
##### ##### ##### ##### ##### Base HR - Eng Data Creation ##### ##### ##### ##### #####

  
##### ##### Full Paired Data (No Simulations)

  #### Prep HR Long Data 
  hr_long_dt <- firm_long_dt[role == "HR",
                           !c("role", "treat", "open_resume", "open_code1", "open_code2")]

    ###### Subscript HR Data
    vars_to_not_subscript <- c("resume_index",
                               "true_race_gender", "demo_group", "demo_group_asian",
                               "test_case", "readability", "time_efficiency", "space_efficiency", "runtime",
                               "gpa", "num_proj", "num_exp", "num_lead", "num_awards",
                               "code_opt_out", "q_version", "overall_score", "overall_score_z", "readability", "readability_score_z",
                               "work_1000", "work_1000_twice", "work_newstartup", "work_research", "work_teaching")
    
    vars_to_subscript <- setdiff(names(hr_long_dt), vars_to_not_subscript)
    
    setnames(hr_long_dt, old = vars_to_subscript, new = paste0(vars_to_subscript, "_hr"))
    
    rm(vars_to_not_subscript, vars_to_subscript)
  
  #### Prep Eng Long Data
  eng_long_dt <- firm_long_dt[role == "Eng",
                              !c("role", "treat", "sc_advance", "overall_score", "overall_score_z", "readability", "readability_score_z",
                                 "true_race_gender", "demo_group", "demo_group_asian",
                                 "test_case", "readability", "time_efficiency", "space_efficiency", "runtime",
                                 "gpa", "num_proj", "num_exp", "num_lead", "num_awards",
                                 "code_opt_out", "q_version", "work_1000", "work_1000_twice", "work_newstartup", "work_research", "work_teaching")]

    ###### Subscript Eng Data
    vars_to_not_subscript <- c("resume_index", "eng_rank")
    
    vars_to_subscript <- setdiff(names(eng_long_dt), vars_to_not_subscript)
    
    setnames(eng_long_dt, old = vars_to_subscript, new = paste0(vars_to_subscript, "_eng"))
    
    rm(vars_to_not_subscript, vars_to_subscript)

  #### Merge in HR and Eng Long Datasets
  firm_twostage_long_dt <- merge(matching_crosswalk, hr_long_dt, by = c("responseid_hr", "resume_ver_hr"), allow.cartesian = TRUE)
  firm_twostage_long_dt <- merge(firm_twostage_long_dt, eng_long_dt, by = c("responseid_eng", "resume_ver_eng", "resume_index"), allow.cartesian = TRUE)
  
  rm(hr_long_dt, eng_long_dt)
  
  #### Matching Sanity Check
  stopifnot(matching_crosswalk[, .N, by = .(arm)][arm == "Control", "N"] * 18 == firm_twostage_long_dt[, .N, by = .(arm)][arm == "Control", "N"])  

  #### Reorder Variable Names
  new_order_names <- c('arm', 'responseid_hr','treat_hr', 'resume_ver_hr',
                       'responseid_eng', 'treat_eng', 'resume_ver_eng',
                       'true_race_gender', 'demo_group', 'demo_group_asian',
                       'test_case', 'overall_score', 'overall_score_z', 'readability', 'readability_score_z', 'runtime',
                       'num_exp', 'code_opt_out', 'work_1000',
                       'work_1000_twice', 'work_newstartup', 'work_research', 'work_teaching')
  setcolorder(firm_twostage_long_dt, new_order_names)
  rm(new_order_names)  
  
  #### Variable Cleaning
  firm_twostage_long_dt[, pair_id := paste0(responseid_hr, "_", responseid_eng)]
  
  firm_twostage_long_dt[, arm := factor(arm,
                                        levels = c("Control",
                                                   "Realistic_BH",
                                                   "Complete_BH"))]  
  
## Version Exclude Version 7 (Make Copy in Below Section)


## Matched on Firm Characteristics (No Simulations)
# firm_twostage_firm_char_long_dt <- copy(firm_twostage_long_dt)
  
  #### ADD FILTER HERE  

## Matched on Job Characteristics (No Simulations)
# firm_twostage_job_char_long_dt <- copy(firm_twostage_long_dt)

  #### ADD FILTER HERE


##### ##### ##### ##### ##### Two Stage Hiring Funnel - Stage Specific Rankings ##### ##### ##### ##### #####

  
## Simulate Rankings on Full Pairs

  #### Simulate HR Stage
  hr_long_dt <- firm_long_dt[role == "HR", .(responseid, resume_ver, resume_index, sc_overall_z, sc_coding_z, sc_advance)]
  hr_long_dt[, `:=` (
    responseid_hr = responseid,
    resume_ver_hr = resume_ver
  )]

    ###### ADD FILTER HERE
    

    ###### Exogenous Advance Decision
    set.seed(42382)
    hr_long_dt <- hr_long_dt[, hr_rank := frank(list(-sc_overall_z, -sc_coding_z), ties.method = "random"),
                             by = .(responseid)]
    
    max_candidates <- 18
    
    for (k in 1:max_candidates) {
      hr_col_name <- paste0("hr_pass", k)
      
      hr_long_dt[, (hr_col_name) := fifelse(hr_rank <= k, 1, 0)]
    }
    
    hr_long_dt[, `:=` 
               (hr_rank = NULL)]
    rm(k, hr_col_name, max_candidates)
  
    ###### Counts Passed by HR Stage
    hr_long_dt[, sc_advance_count_hr := sum(sc_advance), by = .(responseid)]
    
    ###### Remove Unwanted Variables
    hr_long_dt[, `:=` (
      sc_overall_z = NULL,
      sc_coding_z = NULL,
      sc_advance = NULL,
      responseid = NULL,
      resume_ver = NULL
    )]
  
  #### Simulate Eng Stage Ranking
  eng_long_dt <- firm_long_dt[role == "Eng", .(responseid, resume_ver, resume_index, sc_overall_z, sc_coding_z)]
  eng_long_dt[, `:=` (
    responseid_eng = responseid,
    resume_ver_eng = resume_ver
  )]
  
    ###### ADD FILTER HERE
    
    ###### Engineer Score Ranking
    set.seed(29483)
    eng_long_dt <- eng_long_dt[, eng_rank := frank(list(-sc_overall_z, -sc_coding_z), ties.method = "random"),
                               by = .(responseid)]
    
    max_candidates <- 18

    for (k in 1:max_candidates) {
      eng_col_name <- paste0("eng_top", k)
      
      eng_long_dt[, (eng_col_name) := fifelse(eng_rank <= k, 1, 0)]
    }    
    
    rm(k, max_candidates)
    
    ###### Remove Unwanted Variables
    eng_long_dt[, `:=` (
      sc_overall_z = NULL,
      sc_coding_z = NULL,
      responseid = NULL,
      resume_ver = NULL
    )]
    
  #### Merge in with Paired Long Data
  firm_twostage_long_dt <- merge(x = firm_twostage_long_dt, y = hr_long_dt, by = c("responseid_hr", "resume_ver_hr", "resume_index"), allow.cartesian = TRUE)
  firm_twostage_long_dt <- merge(x = firm_twostage_long_dt, y = eng_long_dt, by = c("responseid_eng", "resume_ver_eng", "resume_index"), allow.cartesian = TRUE)    

  #### Merge in with Long Data (Single Stage)
  firm_long_dt <- merge(x = firm_long_dt, 
                        y = hr_long_dt, 
                        by.x = c("responseid", "resume_ver", "resume_index"), 
                        by.y = c("responseid_hr", "resume_ver_hr", "resume_index"), 
                        all.x = TRUE)
  firm_long_dt <- merge(x = firm_long_dt, 
                        y = eng_long_dt, 
                        by.x = c("responseid", "resume_ver", "resume_index"), 
                        by.y = c("responseid_eng", "resume_ver_eng", "resume_index"), 
                        all.x = TRUE)    
  rm(hr_long_dt, eng_long_dt)
  
## Simulate Rankings on Subset Pairs without V7 (Make Copy in Below Section)

  
  
## Simulate Rankings on Subset Pairs on Firm Characteristics



## Simulate Rankings on Subset Pairs on Job Characteristics
    
    
    
##### ##### ##### ##### ##### Simulate Final Selection Process ##### ##### ##### ##### #####
  
## Final Selection - Full Pairs
  
  ###### HR and Engineers Select Based on Overall Score and Coding Score
  for (h in 1:18) {
    hr_passed_name <- paste0("hr_pass", h)
    eng_ranking_name <- paste0("eng_rankingfrom", h)
    firm_twostage_long_dt[get(hr_passed_name) == 1, (eng_ranking_name) := frank(list(eng_rank)), by = .(responseid_hr, responseid_eng)]
    
    for (e in 1:h) {
      eng_col_name <- paste0("eng_hire", e, "from", h)
      firm_twostage_long_dt[, (eng_col_name) := fifelse(get(eng_ranking_name) <= e, 1, 0, na = 0)]
    }
    firm_twostage_long_dt[, (eng_ranking_name) := NULL]
  }
  rm(hr_passed_name, eng_ranking_name, eng_col_name, h, e)
  
  ###### HR Advance; Engineers Select Based on Overall Score and Coding Score
  firm_twostage_long_dt[sc_advance_hr == 1, eng_rankingfromadvance := frank(list(eng_rank)), by = .(responseid_hr, responseid_eng)]
  
  for (e in 1:18) {
    eng_col_name <- paste0("eng_hire", e, "fromadvanced")
    firm_twostage_long_dt[, (eng_col_name) := fifelse(eng_rankingfromadvance <= e, 1, 0, na = 0)]
    
    if (e == 1) {
      firm_twostage_long_dt[get(eng_col_name) == 1, weights_select1fromadvance := 1]
    }
    
    if (e > 1) {
      weights_col_name <- paste0("weights_select", e, "fromadvance")
      firm_twostage_long_dt[get(eng_col_name) == 1 & sc_advance_count_hr < e, (weights_col_name) := e / sc_advance_count_hr]
      firm_twostage_long_dt[get(eng_col_name) == 1 & sc_advance_count_hr >= e, (weights_col_name) := 1]
    }
  }
  rm(eng_col_name, weights_col_name, e)  

  
  
## Final Selection - on Subset Pairs without V7
firm_twostage_dropv7_long_dt <- copy(firm_twostage_long_dt)

  # Filter out V7
  firm_twostage_dropv7_long_dt <- firm_twostage_dropv7_long_dt[resume_ver_eng != "v7"]
  
## Final Selection - Subset Pairs on Firm Characteristics
    

## Final Selection - Subset Pairs on Job Characteristics
    
    
##### ##### ##### ##### ##### Create Other Data sets ##### ##### ##### ##### #####
  
  ##### ##### Matching on Job Characteristics
  firm_twostage_jobs_long_dt <- copy(firm_twostage_long_dt)
  
  firm_twostage_jobs_long_dt <- merge(
    x = matching_crosswalk_jobs, 
    y = firm_twostage_jobs_long_dt, 
    by = c("responseid_hr", "responseid_eng", "arm"), 
    all.x = TRUE  # Keeps all of X, drops unmatched Y
  )  
  
  
##### ##### ##### ##### ##### Merge in Weights ##### ##### ##### ##### #####


  
# weights_dt[, w_ind_hr := sum(w), by = .(responseid_hr)]
# weights_dt[, w_ind_eng := sum(w), by = .(responseid_eng)]
# temp_hr_dt <- unique(weights_dt[, .(responseid_hr, treat_HR, w_ind_hr)])[order(w_ind_hr)]
# temp_eng_dt <- unique(weights_dt[, .(responseid_eng, treat_eng, w_ind_eng)])[order(w_ind_eng)]
# 
# ggplot(temp_hr_dt, aes(x = w_ind_hr, fill = treat_HR)) +
#   geom_histogram(bins = 30, color = "white", alpha = 0.7, position = "identity") +
#   theme_minimal() +
#   labs(x = "Sum of Weights for HR", y = "Count", title = "HR: Distribution of Summed Weights Up To responseid Level", fill = "Treatment")
# 
# ggplot(temp_eng_dt, aes(x = w_ind_eng, fill = treat_eng)) +
#   geom_histogram(bins = 30, color = "white", alpha = 0.7, position = "identity") +
#   theme_minimal() +
#   labs(x = "Sum of Weights for Eng", y = "Count", title = "Eng: Distribution of Summed Weights Up Io responseid Level", fill = "Treatment")
#   
# 
# 
# BEng
# - Matched to all BHR
#   
# NBEng
# - Matched to NBHR with Same Versions
# - Matched to all BHR
#   
# R_3h0K7LlycnSiHHx NB V1
# R_7B6uVsPgbuscDU5 NB V6


  
  
  


