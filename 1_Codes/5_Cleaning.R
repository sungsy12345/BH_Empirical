## -------------------- --------------------
##
## Script name: 5_Cleaning.R
##
## Email: sysung@berkeley.edu
##
## -------------------- --------------------



##### ##### ##### Clean Survey Data ##### ##### ##### 

## Order by Date
setorder(firm_dt, startdate)

## Order Response Ids by Date
ordered_responseids <- firm_dt[order(recordeddate), responseid]

## Convert all possible column to numeric; keep original for non-convertible ones
should_convert_num <- function(x) {
  if (!(is.character(x) || is.factor(x))) return(FALSE)
  v <- trimws(as.character(x))
  v <- v[!(is.na(v) | v == "")]
  if (!length(v)) return(FALSE)
  if (any(grepl("[,;|]", v))) return(FALSE)            # skip multi-selects like "1,2"
  all(grepl("^[-+]?[0-9]*\\.?[0-9]+$", v))             # all entries look numeric
}
cols_to_num <- names(firm_dt)[vapply(firm_dt, should_convert_num, logical(1))]
firm_dt[, (cols_to_num) := lapply(.SD, function(x) as.numeric(trimws(as.character(x)))), .SDcols = cols_to_num]
rm(should_convert_num, cols_to_num)

## Resume Display Order
firm_dt[!is.na(fl_193_do), fl_193_do := gsub("3_2_Resume", "", fl_193_do, ignore.case = TRUE, fixed = FALSE)]
firm_dt[!is.na(fl_193_do), c(paste0("r", 1:18, "_DO")) := lapply(tstrsplit(fl_193_do, "\\|"), function(x) as.integer(trimws(x)))]


##### ##### ##### Indicator Variables / Factorize Variables ##### ##### ##### 


## Section 1: Survey Variables

  #### Response Ids
  firm_dt[responseid %in% ordered_responseids, responseid_ordered := factor(responseid, levels = ordered_responseids)]
  rm(ordered_responseids)
  
  #### Blind indicator (assumes treat is "blind"/"nonblind")
  firm_dt[, treat_blind := as.integer(treat == "blind")]
  
  #### Role: HR and Engineers
  firm_dt[, role := factor(role, levels = c("HR", "Eng"))]

    
## Section 2: Respondent's Background Data
  
  #### Respondent Age (s1_yob)
  survey_year <- 2025
  firm_dt[, resp_age := survey_year - s1_yob]
  firm_dt[, resp_age_centered := resp_age - median(resp_age)]
  
  #### Respondent Gender (s2_gender)
  firm_dt[, s2_gender := factor(s2_gender, 
                                levels = c(1, 2, 3, 96),
                                labels = c("Female", "Male", "Non-binary", "Other"))]
  
  firm_dt[, `:=`(
    resp_gender = s2_gender,
    resp_female = fifelse(s2_gender == "Female", 1L, 0L),
    resp_male = fifelse(s2_gender == "Male", 1L, 0L),
    resp_nonbinaryother = fifelse(s2_gender != "Female" & s2_gender != "Male", 1L, 0L))
  ]
  
  #### Respondent Race (s2_race)
  firm_dt[, `:=`(
    resp_race = fifelse(s2_race == 1, "White", 
                          fifelse(s2_race == 2, "Black", 
                                  fifelse(s2_race == 3, "Hispanic", 
                                          fifelse(s2_race == 5, "Asian", "Other")))),
    resp_white = fifelse(s2_race == 1, 1L, 0L),
    resp_black = fifelse(s2_race == 2, 1L, 0L),
    resp_hispanic = fifelse(s2_race == 3, 1L, 0L),    
    resp_asian = fifelse(s2_race == 5, 1L, 0L),
    resp_chinese = fifelse(s2_race == 5 & s2_asian_eth == 1, 1L, 0L),
    resp_indian = fifelse(s2_race == 5 & s2_asian_eth == 3, 1L, 0L),
    resp_japanese = fifelse(s2_race == 5 & s2_asian_eth == 4, 1L, 0L),
    resp_korean = fifelse(s2_race == 5 & s2_asian_eth == 5, 1L, 0L),
    resp_pakistani = fifelse(s2_race == 5 & s2_asian_eth == 6, 1L, 0L),
    resp_raceother = fifelse(s2_race != 1 & s2_race != 2 & s2_race != 3 & s2_race != 5 , 1L, 0L))
  ]  
  
  #### Respondent Race & Gender (2 Versions: 1 with Asian and 1 with Smaller Asian Groups)
  firm_dt[, `:=` (
      resp_race_gender = fifelse(resp_male == 1 & resp_white == 1, "White_Male", 
                                 fifelse(resp_female == 1 & resp_white == 1, "White_Female", 
                                         fifelse(resp_male == 1 & resp_asian == 1, "Asian_Male", 
                                                 fifelse(resp_female == 1 & resp_asian == 1, "Asian_Female",
                                                         fifelse(resp_male == 1 & resp_hispanic == 1, "Hispanic_Male",
                                                                 fifelse(resp_female == 1 & resp_hispanic == 1, "Hispanic_Female",
                                                                         fifelse(resp_male == 1 & resp_black == 1, "Black_Male",
                                                                                 fifelse(resp_female == 1 & resp_black == 1, "Black_Female", "Other", na = "Error"))))))))
    )]
    
  firm_dt[, resp_race_gender := factor(resp_race_gender, 
                                    levels = c("White_Male", "White_Female",
                                               "Asian_Male", "Asian_Female",
                                               "Hispanic_Male", "Hispanic_Female", 
                                               "Black_Male", "Black_Female", 
                                               "Other", 
                                               "Error"))]
  
  firm_dt[, `:=` (
    resp_race_gender_detailed = fifelse(resp_male == 1 & resp_white == 1, "White_Male", 
                                        fifelse(resp_female == 1 & resp_white == 1, "White_Female", 
                                                fifelse(resp_male == 1 & resp_chinese == 1, "Chinese_Male", 
                                                        fifelse(resp_female == 1 & resp_chinese == 1, "Chinese_Female",
                                                                fifelse(resp_male == 1 & resp_indian == 1, "Indian_Male", 
                                                                        fifelse(resp_female == 1 & resp_indian == 1, "Indian_Female",
                                                                                fifelse(resp_male == 1 & resp_japanese == 1, "Japanese_Male", 
                                                                                        fifelse(resp_female == 1 & resp_japanese == 1, "Japanese_Female",
                                                                                                fifelse(resp_male == 1 & resp_korean == 1, "Korean_Male", 
                                                                                                        fifelse(resp_female == 1 & resp_korean == 1, "Korean_Female",
                                                                                                                fifelse(resp_male == 1 & resp_pakistani == 1, "Pakistani_Male", 
                                                                                                                        fifelse(resp_female == 1 & resp_pakistani == 1, "Pakistani_Female",
                                                                                                                                fifelse(resp_male == 1 & resp_hispanic == 1, "Hispanic_Male",
                                                                                                                                        fifelse(resp_female == 1 & resp_hispanic == 1, "Hispanic_Female",
                                                                                                                                                fifelse(resp_male == 1 & resp_black == 1, "Black_Male",
                                                                                                                                                        fifelse(resp_female == 1 & resp_black == 1, "Black_Female", "Other", na = "Error"))))))))))))))))
  )]
  
  firm_dt[, resp_race_gender_detailed := factor(resp_race_gender_detailed, 
                                                levels = c("White_Male", "White_Female",
                                                           "Chinese_Male", "Chinese_Female",
                                                           "Indian_Male", "Indian_Female",
                                                           "Japanese_Male", "Japanese_Female",
                                                           "Korean_Male", "Korean_Female",
                                                           "Pakistani_Male", "Pakistani_Female",
                                                           "Hispanic_Male", "Hispanic_Female", 
                                                           "Black_Male", "Black_Female", 
                                                           "Other", 
                                                           "Error"))]  
  
  
  #### Respondent Education (s2_educ)
  firm_dt[, `:=`(
    resp_educ_hs = fifelse(s2_educ == 1 | s2_educ == 2 | s2_educ == 3 | s2_educ == 4 | s2_educ == 5, 1L, 0L),
    resp_educ_uniabove = fifelse(s2_educ == 7 | s2_educ == 8 | s2_educ == 9, 1L, 0L))
  ]    
  
  #### Respondent Income (s2_income)
  firm_dt[, `:=`(
    resp_income_50 = fifelse(s2_income== 1 | s2_income == 2, 1L, 0L),
    resp_income_100 = fifelse(s2_income == 3, 1L, 0L),
    resp_income_150 = fifelse(s2_income == 4, 1L, 0L),
    resp_income_200 = fifelse(s2_income == 5, 1L, 0L),
    resp_income_500ormore = fifelse(s2_income == 6 | s2_income == 7 | s2_income == 8, 1L, 0L),
    resp_income_100ormore = fifelse(s2_income ==  4 | s2_income ==  5 | s2_income ==  6 | s2_income == 7 | s2_income == 8, 1L, 0L)
  )
  ]
  
  #### Respondent How long at firm (s2_firm_howlong_1, s2_firm_howlong_2)
  firm_dt[, s2_firm_howlong_in_months := s2_firm_howlong_1 * 12 + s2_firm_howlong_2]
  firm_dt[, s2_firm_howlong_in_years := s2_firm_howlong_in_months / 12]

  #### Respondent Firm-size (s2_firm_size)
  firm_dt[, `:=`(
    firm_size_small = fifelse(s2_firm_size == 1 | s2_firm_size == 2, 1L, 0L, na = 0L),
    firm_size_mid = fifelse(s2_firm_size == 3 | s2_firm_size == 4, 1L, 0L, na = 0L),
    firm_size_large = fifelse(s2_firm_size == 5 | s2_firm_size == 6, 1L, 0L, na = 0L),
    firm_size = fcase(
      s2_firm_size %in% c(1,2), "Small",
      s2_firm_size %in% c(3,4), "Medium",
      s2_firm_size %in% c(5,6), "Large"
    ))
  ]
  
  firm_dt[, `:=` (
    firm_size = factor(firm_size, levels = c("Small", "Medium", "Large"))
  )]


## Section 4: Respondent's Hiring Experience
  
  #### Respondent Experience with Recruiting
  firm_dt[, s4_recruiting_in_months := s4_recruit_exp_1 * 12 + s4_recruit_exp_2]
  firm_dt[, s4_recruiting_in_years := s4_recruiting_in_months / 12]
  firm_dt[, s4_recruiting_in_years_centered := s4_recruiting_in_years - median(s4_recruiting_in_years)]
  
  #### Interaction with UCB Graduate
  firm_dt[, `:=`(
    ucb_yes = fifelse(s4_ever_ucbgrad == 1 | s4_ever_ucbgrad == 2 | s4_ever_ucbgrad == 3, 1L, 0L, na = 0L))
  ]
  
  #### Positions Currently Hiring For
  for (j in 1:17) {
    col_name <- paste0("job_", j)
    firm_dt[, (col_name) := as.integer(sapply(strsplit(s4_jobposition, ","), function(x) as.character(j) %in% trimws(x)))]
  }

  rm(j, col_name)

  #### Priority of "Diversity and Representation" in the s4_goals rank
  ## s4_goals_5 is the rank (1-7, stored as character) assigned to
  ## "Diversity and Representation"; 1 = top priority, 7 = lowest.
  firm_dt[, s4_goals_5_rank  := suppressWarnings(as.integer(s4_goals_5))]
  firm_dt[, div_priority_inv := 8 - s4_goals_5_rank]                          # 7 = top priority
  firm_dt[, div_priority_top3:= as.integer(s4_goals_5_rank <= 3)]
  firm_dt[, div_priority_z   := (div_priority_inv - mean(div_priority_inv, na.rm = TRUE)) /
                                   sd(div_priority_inv, na.rm = TRUE),
          by = .(role, treat)]

## Section 5-1: Hiring Policies
  
  #### Firm: Usage of ATS
  firm_dt[, `:=`(
    ats_yes = fifelse(s5_ats == 1, 1L, 0L, na = 0L))
  ]  
  
  #### Firm: Usage of BH
  firm_dt[, `:=`(
    bh_yes = fifelse(s5_bh == 1, 1L, 0L, na = 0L),
    bh_race = fifelse(grepl("3", s5_bh_which), 1L, 0L, na = 0L),
    bh_gender = fifelse(grepl("2", s5_bh_which), 1L, 0L, na = 0L))
  ]

  #### Firm: Count of DEI Policies (s5_policy_dei; dei_policy_1..6 already in .dta)
  firm_dt[, dei_policy_count   := dei_policy_1 + dei_policy_2 + dei_policy_3 + dei_policy_4 + dei_policy_5 + dei_policy_6]
  firm_dt[, dei_policy_3plus   := as.integer(dei_policy_count >= 3)]
  firm_dt[, dei_policy_count_z := (dei_policy_count - mean(dei_policy_count, na.rm = TRUE)) /
                                     sd(dei_policy_count, na.rm = TRUE),
          by = .(role, treat)]

  #### Firm: Target Groups for Advancing Representation (s5_policy_diversity, multi-select)
  ## Options: 1=White, 2=Black, 3=Hispanic, 4=Asian, 5=Male, 6=Female, 7=Other, 96=DK, 98=NA
  firm_dt[, firm_targets_white    := fifelse(!is.na(s5_policy_diversity) & grepl("(^|,)\\s*1\\s*(,|$)", s5_policy_diversity), 1L, 0L, na = NA_integer_)]
  firm_dt[, firm_targets_black    := fifelse(!is.na(s5_policy_diversity) & grepl("(^|,)\\s*2\\s*(,|$)", s5_policy_diversity), 1L, 0L, na = NA_integer_)]
  firm_dt[, firm_targets_hispanic := fifelse(!is.na(s5_policy_diversity) & grepl("(^|,)\\s*3\\s*(,|$)", s5_policy_diversity), 1L, 0L, na = NA_integer_)]
  firm_dt[, firm_targets_asian    := fifelse(!is.na(s5_policy_diversity) & grepl("(^|,)\\s*4\\s*(,|$)", s5_policy_diversity), 1L, 0L, na = NA_integer_)]
  firm_dt[, firm_targets_male     := fifelse(!is.na(s5_policy_diversity) & grepl("(^|,)\\s*5\\s*(,|$)", s5_policy_diversity), 1L, 0L, na = NA_integer_)]
  firm_dt[, firm_targets_female   := fifelse(!is.na(s5_policy_diversity) & grepl("(^|,)\\s*6\\s*(,|$)", s5_policy_diversity), 1L, 0L, na = NA_integer_)]
  firm_dt[, firm_targets_race_urm := as.integer(firm_targets_black == 1L | firm_targets_hispanic == 1L)]
  firm_dt[, firm_targets_any_urm  := as.integer(firm_targets_black == 1L | firm_targets_hispanic == 1L | firm_targets_female == 1L)]

## Section 5-2: DEI Views

  #### Pro DEI (Z) Index
  firm_dt[, ':=' (
    pro_dei_race_z = (s5_view_dei_race - mean(s5_view_dei_race, na.rm = TRUE)) / sd(s5_view_dei_race, na.rm = TRUE),
    pro_dei_gender_z = (s5_view_dei_gender - mean(s5_view_dei_gender, na.rm = TRUE)) / sd(s5_view_dei_gender, na.rm = TRUE),
    pro_diverse_z = (s5_view_diverse - mean(s5_view_diverse, na.rm = TRUE)) / sd(s5_view_diverse, na.rm = TRUE),
    pro_hiring_dei_z = (s5_view_hiring_dei - mean(s5_view_hiring_dei, na.rm = TRUE)) / sd(s5_view_hiring_dei, na.rm = TRUE)
    ),
    by = .(role, treat)]
  firm_dt[, pro_dei_index_z := rowMeans(.SD, na.rm = TRUE), .SDcols = c("pro_dei_race_z", "pro_dei_gender_z", "pro_diverse_z", "pro_hiring_dei_z")]
  firm_dt[, pro_dei_index_z := (pro_dei_index_z - mean(pro_dei_index_z, na.rm = TRUE)) / 
            sd(pro_dei_index_z, na.rm = TRUE),
          by = .(role, treat)]  
  
  #### Binary Tier - Pro DEI (Z) Index
  firm_dt[, pro_dei_basedon_index := fifelse(pro_dei_index_z > 0, 1, 0)]
          
  #### Three Tier - Pro DEI (Z) Index
  firm_dt[, pro_dei_index_3tiers := cut(pro_dei_index_z,
                                      breaks = quantile(pro_dei_index_z, 
                                                        probs = c(0, 1/3, 2/3, 1), 
                                                        na.rm = TRUE),
                                      labels = c("Anti-DEI", "Mid-DEI", "Pro-DEI"),
                                      include.lowest = TRUE),
          by = .(role)]
  firm_dt[, pro_dei_index_3tiers := relevel(pro_dei_index_3tiers, ref = "Anti-DEI")]

  #### Firm DEI Attention (perception of firm's current effort; 1=Very low ... 5=Very high)
  firm_dt[, firm_attention_race_z   := (s5_view_dei_race_f   - mean(s5_view_dei_race_f,   na.rm = TRUE)) /
                                          sd(s5_view_dei_race_f,   na.rm = TRUE),
          by = .(role, treat)]
  firm_dt[, firm_attention_gender_z := (s5_view_dei_gender_f - mean(s5_view_dei_gender_f, na.rm = TRUE)) /
                                          sd(s5_view_dei_gender_f, na.rm = TRUE),
          by = .(role, treat)]

  #### IRR: (Firm) Currently Hiring
  firm_dt[, `:=`(
    currentlyhiring_yes = fifelse(s1_irr_firm == 1, 1L, 0L, na = 0L))
  ] 
  
  #### IRR: Introduce me to UCB CS Student
  firm_dt[, `:=`(
    IRR_1 = fifelse(s1_send_resume == 1, 1L, 0L, na = 0L),
    IRR_2 = fifelse(s6_send_resume_again == 1, 1L, 0L, na = 0L),
    IRR_ever = fifelse(s1_send_resume == 1 | s6_send_resume_again == 1, 1L, 0L, na = 0L))
  ]   
  
  #### Display Order (Lumped by Three)
  cols_in  <- paste0("r", 1:18, "_DO")
  cols_out <- paste0(cols_in, "_bythree")
  firm_dt[, (cols_out) := lapply(.SD, function(x) ceiling(x/3)), .SDcols = cols_in]
  rm(cols_in, cols_out)
    
  #### Employer's Coding Languages
  firm_dt[, `:=` (
    resp_python = fifelse(s2_code_python != "", 1, 0, na = 0L),
    resp_java = fifelse(s2_code_java != "", 1, 0, na = 0L),
    resp_javascript = fifelse(s2_code_javascript != "", 1, 0, na = 0L),
    resp_cplusplus = fifelse(s2_code_cplusplus != "", 1, 0, na = 0L),
    resp_csharp = fifelse(s2_code_csharp != "", 1, 0, na = 0L)
    )]
  
  
##### ##### ##### Manipulation: Make Long Dataset ##### ##### ##### 

  
## Melt into Long

  #### Variable Selection
  vars_id <- c("responseid", "role", "treat", "resume_ver")
  vars_resp_demo <- c("resp_age", "resp_age_centered", "s2_educ", "s2_income",
                      "resp_python", "resp_java", "resp_javascript", "resp_cplusplus", "resp_csharp",
                      "resp_gender", "resp_female", "resp_male", "resp_nonbinaryother",
                      "resp_race", "resp_white", "resp_asian", "resp_hispanic", "resp_black", "resp_raceother",
                      "resp_race_gender", "resp_race_gender_detailed",
                      "pro_dei_race_z", "pro_dei_gender_z", "pro_diverse_z", "pro_hiring_dei_z", "pro_dei_index_z", "pro_dei_basedon_index", "pro_dei_index_3tiers",
                      "div_priority_z", "div_priority_top3",
                      "dei_policy_count", "dei_policy_count_z", "dei_policy_3plus",
                      "bh_yes", "firm_targets_white", "firm_targets_black", "firm_targets_hispanic",
                      "firm_targets_asian", "firm_targets_male", "firm_targets_female",
                      "firm_targets_race_urm", "firm_targets_any_urm",
                      "firm_attention_race_z", "firm_attention_gender_z",
                      "firm_id", "s2_firm_size", "firm_size", "firm_size_small", "firm_size_mid", "firm_size_large",
                      "IRR_1", "IRR_2", "IRR_ever", "s3_outside_res",
                      "s4_recruiting_in_years", "s4_recruiting_in_years_centered")
  vars_res_demo <- c("r", "race_r", "gender_r", "chinese_r", "indian_r") 
  vars_res_demo <- as.vector(outer(vars_res_demo, 1:18, paste, sep = ""))
  vars_res_score <- c("sc_coding", "sc_fit", "sc_stay", "sc_overall", "sc_advance")
  vars_res_score <- as.vector(outer(vars_res_score, 1:18, paste, sep = "_"))
  vars_opened_resume <- paste0("res", 1:18, "_resume_opened")
  vars_opened_code1  <- paste0("res", 1:18, "_code1_opened")
  vars_opened_code2  <- paste0("res", 1:18, "_code2_opened")
  vars_res_do <- as.vector(paste0("r", 1:18, "_DO"))
  vars_res_dobythree <- as.vector(paste0("r", 1:18, "_DO_bythree"))
  
  #### Subset Relevant Variables Only
  vars_keep <- c(vars_id, vars_resp_demo, vars_res_demo, vars_res_score, vars_opened_resume, vars_opened_code1, vars_opened_code2, vars_res_do, vars_res_dobythree)
  firm_long_dt <- firm_dt[, ..vars_keep]
  rm(vars_id, vars_resp_demo, vars_res_demo, vars_res_score, vars_opened_resume, vars_opened_code1, vars_opened_code2, vars_res_do, vars_res_dobythree, vars_keep)
  
  #### Wide -> Long Data (Original Blind Group Version / Demo Assigned)
  firm_long_dt <- melt(
    firm_long_dt,
    id.vars = c("responseid", "role", "treat", "resume_ver",
                "resp_python", "resp_java", "resp_javascript", "resp_cplusplus", "resp_csharp",
                "resp_age", "resp_age_centered", "s2_educ", "s2_income",
                "resp_gender", "resp_female", "resp_male", "resp_nonbinaryother",
                "resp_race", "resp_white", "resp_asian", "resp_hispanic", "resp_black", "resp_raceother",
                "resp_race_gender", "resp_race_gender_detailed",
                "pro_dei_race_z", "pro_dei_gender_z", "pro_diverse_z", "pro_hiring_dei_z", "pro_dei_index_z", "pro_dei_basedon_index", "pro_dei_index_3tiers",
                "div_priority_z", "div_priority_top3",
                "dei_policy_count", "dei_policy_count_z", "dei_policy_3plus",
                "bh_yes", "firm_targets_white", "firm_targets_black", "firm_targets_hispanic",
                "firm_targets_asian", "firm_targets_male", "firm_targets_female",
                "firm_targets_race_urm", "firm_targets_any_urm",
                "firm_attention_race_z", "firm_attention_gender_z",
                "firm_id", "s2_firm_size", "firm_size", "firm_size_small", "firm_size_mid", "firm_size_large",
                "IRR_1", "IRR_2", "IRR_ever", "s3_outside_res",
                "s4_recruiting_in_years", "s4_recruiting_in_years_centered"),
    measure.vars = patterns(
      name         = "^r[0-9]+$",
      race         = "^race_r[0-9]+$",      # race_r1, race_r2, ...
      gender       = "^gender_r[0-9]+$",    # gender_r1, gender_r2, ...
      chinese      = "^chinese_r[0-9]+$",   # chinese_r1, chinese_r2, ...
      indian       = "^indian_r[0-9]+$",    # indian_r1, indian_r2, ...
      sc_coding    = "^sc_coding_[0-9]+$",  # sc_coding_1, sc_coding_2, ...
      sc_fit       = "^sc_fit_[0-9]+$",
      sc_stay      = "^sc_stay_[0-9]+$",
      sc_overall   = "^sc_overall_[0-9]+$",
      sc_advance   = "^sc_advance_[0-9]+$",
      open_resume  = "^res[0-9]+_resume_opened$",
      open_code1   = "^res[0-9]+_code1_opened$",
      open_code2   = "^res[0-9]+_code2_opened$",
      r_do         = "^r[0-9]+_DO$",
      r_do_bythree = "^r[0-9]+_DO_bythree$"
    ),
    variable.name = "resume_index",  # automatically becomes 1–18
    value.name = c("resume_name", "race", "gender", "chinese", "indian",
                   "sc_coding", "sc_fit", "sc_stay", "sc_overall", "sc_advance",
                   "res_opened", "code1_opened", "code2_opened",
                   "display_order", "display_order_bythree")
  )


##### ##### ##### Major Manipulation: Manipulate Long Dataset ##### ##### ##### 

## Data Cleaning

  #### Create Treat Variables  
  firm_long_dt[, treat := factor(treat, levels = c("nonblind", "blind"))]
  
  #### Create Demographic Variable
  
  firm_long_dt[
    , race_gender := paste0(race, "_", gender)
  ]
  
  firm_long_dt[
    race_gender == "blind_blind",
    race_gender := "Blind"
  ]
  
  firm_long_dt[, race_gender := factor(race_gender, 
                                       levels = c("Blind", 
                                                  "White_Male", "White_Female",
                                                  "Asian_Male", "Asian_Female",
                                                  "Hispanic_Male", "Hispanic_Female"))]
  
  firm_long_dt[, race_gender_detailed := race_gender]
  firm_long_dt[gender == "Male" & chinese == 1, race_gender_detailed := "Chinese_Male"]
  firm_long_dt[gender == "Female" & chinese == 1, race_gender_detailed := "Chinese_Female"]
  firm_long_dt[gender == "Male" & indian == 1, race_gender_detailed := "Indian_Male"]
  firm_long_dt[gender == "Female" & indian == 1, race_gender_detailed := "Indian_Female"]
  firm_long_dt[, race_gender_detailed := factor(race_gender_detailed, 
                                                levels = c("Blind", 
                                                           "White_Male", "White_Female",
                                                           "Chinese_Male", "Chinese_Female",
                                                           "Indian_Male", "Indian_Female",
                                                           "Asian_Male", "Asian_Female",
                                                           "Hispanic_Male", "Hispanic_Female"))]
  
  #### Create Under-Represented Minority
  firm_long_dt[, urm_tech := fcase(
    race_gender %in% c("White_Male", "Asian_Male"), 0L,
    race_gender %in% c("White_Female", "Asian_Female", "Hispanic_Male", "Hispanic_Female"), 1L,
    race_gender %in% c("Blind"), NA_integer_)]
  
  firm_long_dt[, urm_tech_blind := fcase(
    race_gender %in% c("White_Male", "Asian_Male"), "Majority",
    race_gender %in% c("White_Female", "Asian_Female", "Hispanic_Male", "Hispanic_Female"), "Under-represented Minority",
    race_gender %in% c("Blind"), "Blind")]
  
  firm_long_dt[, urm_tech_blind := factor(urm_tech_blind, 
                                          levels = c("Blind", 
                                                     "Majority", "Under-represented Minority"))]
  
  firm_long_dt[, urm_gender := fcase(
    race_gender %in% c("White_Male", "Asian_Male", "Hispanic_Male"), 0L,
    race_gender %in% c("White_Female", "Asian_Female", "Hispanic_Female"), 1L,
    race_gender %in% c("Blind"), NA_integer_)]
  
  firm_long_dt[, urm_race := fcase(
    race_gender %in% c("White_Male", "White_Female", "Asian_Male", "Asian_Female"), 0L,
    race_gender %in% c("Hispanic_Male", "Hispanic_Female"), 1L,
    race_gender %in% c("Blind"), NA_integer_)]
  
  #### Create Demographic Concordance
  firm_long_dt[treat == "nonblind" & 
                 (resp_gender == "Male" | resp_gender == "Female"), 
               concordance_gender := fifelse(as.character(gender) == as.character(resp_gender), 1, 0)]
  firm_long_dt[treat == "nonblind" & 
                 resp_race == "White" | resp_race == "Asian" | resp_race == "Hispanic", 
               concordance_race := fifelse(as.character(race) == as.character(resp_race), 1, 0)]
  firm_long_dt[treat == "nonblind" & 
                 resp_race == "White" | resp_race == "Hispanic", 
               concordance_race_detailed := fifelse(as.character(race) == as.character(resp_race), 1, 0)]
  firm_long_dt[treat == "nonblind" & 
                 resp_race == "Asian" & resp_race_gender_detailed %in% c("Chinese_Male", "Chinese_Female"), 
               concordance_race_detailed := fifelse(chinese == 1, 1, 0)]  
  firm_long_dt[treat == "nonblind" & 
                 resp_race == "Asian" & resp_race_gender_detailed %in% c("Indian_Male", "Indian_Female"), 
               concordance_race_detailed := fifelse(indian == 1, 1, 0)]    
  firm_long_dt[treat == "nonblind" & 
                 (resp_race_gender == "White_Male" | resp_race_gender == "White_Female" | 
                    resp_race_gender == "Asian_Male" | resp_race_gender == "Asian_Female" | 
                    resp_race_gender == "Hispanic_Male" | resp_race_gender == "Hispanic_Female"), 
               concordance_race_gender := fifelse(as.character(race_gender) == as.character(resp_race_gender), 1, 0)]
  firm_long_dt[treat == "nonblind" & 
                 (resp_race_gender_detailed == "White_Male" | resp_race_gender_detailed == "White_Female" | 
                    resp_race_gender_detailed == "Chinese_Male" | resp_race_gender_detailed == "Chinese_Female" | 
                    resp_race_gender_detailed == "Indian_Male" | resp_race_gender_detailed == "Indian_Female" | 
                    resp_race_gender_detailed == "Hispanic_Male" | resp_race_gender_detailed == "Hispanic_Female"),
               concordance_race_gender_detailed := fifelse(as.character(race_gender_detailed) == as.character(resp_race_gender_detailed), 1, 0)]
  firm_long_dt[, concordance_race_gender_detailed_blind := fcase(
    concordance_race_gender_detailed == 1, "Concordant_Group",
    concordance_race_gender_detailed == 0, "Discordant_Group",
    (is.na(concordance_race_gender_detailed) & treat == "blind"), "Blind"
  )]
  firm_long_dt[, concordance_race_gender_detailed_blind := factor(concordance_race_gender_detailed_blind,
                                                                  levels = c("Blind", 
                                                                             "Concordant_Group", "Discordant_Group"))]
  
  #### Standardize Scores for Evaluations
  
    ###### Coding
    firm_long_dt[, sc_coding_z_1 := {
      mu_coding_1 <- mean(sc_coding, na.rm = TRUE)
      sg_coding_1 <- sd(sc_coding, na.rm = TRUE)
      (sc_coding - mu_coding_1) / sg_coding_1
    },
    by = responseid
    ]
    
    sc_coding_z_1_mean_hr  <- mean(firm_long_dt[treat == "blind" & role == "HR"]$sc_coding_z_1, na.rm = TRUE)
    sc_coding_z_1_sd_hr    <- sd(firm_long_dt[treat == "blind" & role == "HR"]$sc_coding_z_1, na.rm = TRUE)
    sc_coding_z_1_mean_eng <- mean(firm_long_dt[treat == "blind" & role == "Eng"]$sc_coding_z_1, na.rm = TRUE)
    sc_coding_z_1_sd_eng   <- sd(firm_long_dt[treat == "blind" & role == "Eng"]$sc_coding_z_1, na.rm = TRUE)
    
    firm_long_dt[role == "HR", sc_coding_z := {(sc_coding_z_1 - sc_coding_z_1_mean_hr) / sc_coding_z_1_sd_hr}]
    firm_long_dt[role == "Eng", sc_coding_z := {(sc_coding_z_1 - sc_coding_z_1_mean_eng) / sc_coding_z_1_sd_eng}]
    
    rm(sc_coding_z_1_mean_hr, sc_coding_z_1_sd_hr, sc_coding_z_1_mean_eng, sc_coding_z_1_sd_eng)
    
    ###### Fit
    firm_long_dt[, sc_fit_z_1 := {
      mu_fit_1 <- mean(sc_fit, na.rm = TRUE)
      sg_fit_1 <- sd(sc_fit, na.rm = TRUE)
      (sc_fit - mu_fit_1) / sg_fit_1
    },
    by = responseid
    ]  
    
    sc_fit_z_1_mean_hr  <- mean(firm_long_dt[treat == "blind" & role == "HR"]$sc_fit_z_1, na.rm = TRUE)
    sc_fit_z_1_sd_hr    <- sd(firm_long_dt[treat == "blind" & role == "HR"]$sc_fit_z_1, na.rm = TRUE)
    sc_fit_z_1_mean_eng <- mean(firm_long_dt[treat == "blind" & role == "Eng"]$sc_fit_z_1, na.rm = TRUE)
    sc_fit_z_1_sd_eng   <- sd(firm_long_dt[treat == "blind" & role == "Eng"]$sc_fit_z_1, na.rm = TRUE)  

    firm_long_dt[role == "HR", sc_fit_z := {(sc_fit_z_1 - sc_fit_z_1_mean_hr) / sc_fit_z_1_sd_hr}]
    firm_long_dt[role == "Eng", sc_fit_z := {(sc_fit_z_1 - sc_fit_z_1_mean_eng) / sc_fit_z_1_sd_eng}]
    
    rm(sc_fit_z_1_mean_hr, sc_fit_z_1_sd_hr, sc_fit_z_1_mean_eng, sc_fit_z_1_sd_eng)
    
    ###### Stay
    firm_long_dt[, sc_stay_z_1 := {
      mu_stay_1 <- mean(sc_stay, na.rm = TRUE)
      sg_stay_1 <- sd(sc_stay, na.rm = TRUE)
      (sc_stay - mu_stay_1) / sg_stay_1
    },
    by = responseid
    ]
    
    sc_stay_z_1_mean_hr  <- mean(firm_long_dt[treat == "blind" & role == "HR"]$sc_stay_z_1, na.rm = TRUE)
    sc_stay_z_1_sd_hr    <- sd(firm_long_dt[treat == "blind" & role == "HR"]$sc_stay_z_1, na.rm = TRUE)
    sc_stay_z_1_mean_eng <- mean(firm_long_dt[treat == "blind" & role == "Eng"]$sc_stay_z_1, na.rm = TRUE)
    sc_stay_z_1_sd_eng   <- sd(firm_long_dt[treat == "blind" & role == "Eng"]$sc_stay_z_1, na.rm = TRUE)
    
    firm_long_dt[role == "HR", sc_stay_z := {(sc_stay_z_1 - sc_stay_z_1_mean_hr) / sc_stay_z_1_sd_hr}]
    firm_long_dt[role == "Eng", sc_stay_z := {(sc_stay_z_1 - sc_stay_z_1_mean_eng) / sc_stay_z_1_sd_eng}]
    
    rm(sc_stay_z_1_mean_hr, sc_stay_z_1_sd_hr, sc_stay_z_1_mean_eng, sc_stay_z_1_sd_eng)

    ###### Overall
    firm_long_dt[, sc_overall_z_1 := {
      mu_overall_1 <- mean(sc_overall, na.rm = TRUE)
      sg_overall_1 <- sd(sc_overall, na.rm = TRUE)
      (sc_overall - mu_overall_1) / sg_overall_1
    },
    by = responseid
    ]
    
    sc_overall_z_1_mean_hr  <- mean(firm_long_dt[treat == "blind" & role == "HR"]$sc_overall_z_1, na.rm = TRUE)
    sc_overall_z_1_sd_hr    <- sd(firm_long_dt[treat == "blind" & role == "HR"]$sc_overall_z_1, na.rm = TRUE)
    sc_overall_z_1_mean_eng <- mean(firm_long_dt[treat == "blind" & role == "Eng"]$sc_overall_z_1, na.rm = TRUE)
    sc_overall_z_1_sd_eng   <- sd(firm_long_dt[treat == "blind" & role == "Eng"]$sc_overall_z_1, na.rm = TRUE)
    
    firm_long_dt[role == "HR", sc_overall_z := {(sc_overall_z_1 - sc_overall_z_1_mean_hr) / sc_overall_z_1_sd_hr}]
    firm_long_dt[role == "Eng", sc_overall_z := {(sc_overall_z_1 - sc_overall_z_1_mean_eng) / sc_overall_z_1_sd_eng}]
    
    rm(sc_overall_z_1_mean_hr, sc_overall_z_1_sd_hr, sc_overall_z_1_mean_eng, sc_overall_z_1_sd_eng)    

    
    
  #### Proportion of Resume / Codes Opened
  firm_long_dt[role == "Eng", `:=` (
    prop_open_resume = mean(open_resume),
    prop_open_code = mean(c(open_code1, open_code2))),
    by = (responseid)]   
    
    
##### ##### ##### Major Manipulation: True Demographics ##### ##### ##### 

## From Version 7
true_racegender <- unique(firm_long_dt[resume_ver == "v7", .(resume_index, race_gender)][, `:=` (true_race_gender = race_gender, race_gender = NULL)])

firm_long_dt <- firm_long_dt[true_racegender, on = .(resume_index)]


##### ##### ##### Major Manipulation: Task Master ##### ##### ##### 

## Identify all TaskMaster time variables (both On and Off)
time_on_cols <- paste0("page_", 13:31, "_timeonpage")
time_off_cols <- paste0("page_", 13:31, "_timeoffpage")
resume_do_cols <- paste0("r", 1:18, "_DO")

## Extract into a separate data.table()
taskmaster_dt <- firm_dt[, .SD, .SDcols = c("responseid", "role", time_on_cols, time_off_cols, resume_do_cols, "s3_outside_res")]

## Rename Taskmaster Variables
for (col in c(time_on_cols, time_off_cols)) {
  # extract the number after "page_"
  old_num <- as.integer(sub("^page_([0-9]+)_.*$", "\\1", col))
  # compute the new number
  new_num <- old_num - 12
  # build the new column name
  new_name <- sub("^page_[0-9]+_", paste0("shown_", new_num, "_"), col)
  # rename
  setnames(taskmaster_dt, old = col, new = new_name)
}
rm(resume_do_cols, time_on_cols, time_off_cols, old_num, new_num, new_name, col)

## Remove Time Spent On / Off Resume 4's Attention Page
for (suffix in c("timeonpage", "timeoffpage")) {
  # all Shown_1..19 for this suffix
  shown_cols <- paste0("shown_", 1:19, "_", suffix)
  # destination columns (we overwrite Shown_1..18)
  dest_cols  <- paste0("shown_", 1:18, "_", suffix)
  
  taskmaster_dt[,
                (dest_cols) := {
                  k <- r4_DO[1L]                        # resume 4 position (1–18) for this row
                  v <- unlist(.SD, use.names = FALSE)   # length 19: Shown_1..Shown_19
                  # keep everything except Shown_{k+1} (attention page)
                  keep_idx <- c(
                    seq_len(k),                         # Shown_1..Shown_k
                    if (k + 2 <= 19) (k + 2):19 else integer()  # Shown_{k+2}..Shown_19
                  )
                  as.list(v[keep_idx])                  # length 18 -> fills Shown_1..Shown_18
                },
                .SDcols = shown_cols,
                by = responseid
  ]
}
rm(shown_cols, dest_cols, k, v, keep_idx, suffix)

taskmaster_dt[, paste0("shown_19_", c("timeonpage", "timeoffpage")) := NULL]

## DO Look up List
do_long <- melt(taskmaster_dt, 
                id.vars = c("responseid", "role"), 
                measure.vars = patterns("^r[0-9]+_DO$"), 
                variable.name = "resume_var", 
                value.name = "position_shown")

do_long[, resume_index := as.integer(sub("r([0-9]+)_DO", "\\1", resume_var))]
do_long[, resume_var := NULL] # Clean up


## Time Map
time_long <- melt(taskmaster_dt, 
                  id.vars = c("responseid", "role"),
                  measure.vars = patterns("^shown_"), 
                  variable.name = "raw_variable", 
                  value.name = "time_value")

## Extract the position number
time_long[, position_shown := as.integer(sub("shown_([0-9]+)_.*", "\\1", raw_variable))]

## Extract the type (timeonpage vs timeoffpage)
time_long[, type := sub("shown_[0-9]+_(.*)", "\\1", raw_variable)]

## Pivot back to wide so you have separate columns for TimeOn and TimeOff
time_final <- dcast(time_long, 
                    responseid + role + position_shown ~ type, 
                    value.var = "time_value")

## Join the Resume Map with the Time Map using 'responseid' and the 'position_shown'
taskmaster_long_dt <- merge(do_long, time_final, 
                  by = c("responseid", "role", "position_shown"), 
                  all.x = TRUE)

## Flag Individual Pages < 15 Seconds

## TimeOn thresholds (hard-coded here)
threshold_upper <- 100   # clip at 100 sec
threshold_lower <- 15    # flag < 15 sec

  ## Clip Values and Flag Indicators
  taskmaster_long_dt[, `:=`(
    timeonpage_clipped  = pmin(timeonpage,  threshold_upper),
    timeoffpage_clipped  = pmin(timeoffpage, threshold_upper),
    out_On              = timeonpage  > threshold_upper,   # TimeOn > upper
    out_Off             = timeoffpage > threshold_upper,   # TimeOff > upper
    timeon_flag         = timeonpage  < threshold_lower    # TimeOn < lower
  )
  ]

## Clean up of Taskmaster Dataset
setnames(taskmaster_long_dt, old = "position_shown", new = "r_do")
setorder(taskmaster_long_dt, responseid, resume_index)
taskmaster_long_dt[, resume_index := as.factor(resume_index)]

rm(do_long, taskmaster_dt, time_final, time_long)

## Merge into Long Dataset
stopifnot(nrow(firm_long_dt) == nrow(taskmaster_long_dt))
firm_long_dt <- merge(firm_long_dt, taskmaster_long_dt,
                      by = c("responseid", "role", "resume_index", "r_do"), 
                      all.x = TRUE)

## Counting Off Page Occurrences
firm_long_dt[, `:=` (
  count_offpage = sum(timeoffpage > 10, na.rm = TRUE)),
  by = (responseid)]


##### ##### ##### Major Manipulation: Merge in Job Applicant / Resume Characteristics ##### ##### ##### 

#### Manipulate student_dt

  ###### Actual Coding Overall Score / Test Case Bin (Top Half)
  student_dt[, top_half_coder := fifelse(test_case >= median(test_case), 1, 0)]
  student_dt[, bottom_half_coder := fifelse(test_case < median(test_case), 1, 0)]

  ###### GPA Bin (Binary)
  student_dt[, gpa_tier2 := cut(gpa, breaks = c(-Inf, 3.80, Inf), labels = c("Non_Elite_GPA", "Elite_GPA"))]
  student_dt[, gpa_tier2 := relevel(gpa_tier2, ref = "Non_Elite_GPA")] 
  
  ###### GPA Bin (Terciles)
  student_dt[, gpa_tier3 := cut(gpa, breaks = c(-Inf, 3.5, 3.8, Inf), labels = c("Low_GPA", "Mid_GPA", "Top_GPA"))]
  student_dt[, gpa_tier3 := relevel(gpa_tier3, ref = "Low_GPA")]  
  
  ###### GPA Bin (Quartiles)
  student_dt[, gpa_tier4 := cut(gpa, breaks = c(-Inf, 3.30, 3.60, 3.85, Inf), labels = c("Low_GPA", "LowMid_GPA", "HighMid_GPA", "Top_GPA"))]
  student_dt[, gpa_tier4 := relevel(gpa_tier4, ref = "Low_GPA")] 
  
  ###### Standardize Overall Score
  student_dt[, overall_score_z := {
    mu_overall_score <- mean(overall_score, na.rm = TRUE)
    sg_overall_score <- sd(overall_score, na.rm = TRUE)
    (overall_score - mu_overall_score) / sg_overall_score
  }]

  ###### Standardize Readability Score
  student_dt[, readability_score_z := {
    mu_readability_score <- mean(readability, na.rm = TRUE)
    sg_readability_score <- sd(readability, na.rm = TRUE)
    (readability - mu_readability_score) / sg_readability_score
  }]
  
  ###### Standardize Test Case Score
  student_dt[, test_case_z := {
    mu_test_case <- mean(test_case, na.rm = TRUE)
    sg_test_case <- sd(test_case, na.rm = TRUE)
    (test_case - mu_test_case) / sg_test_case
  }]
  
  ###### Standardize Test Case Score
  student_dt[, gpa_z := {
    mu_gpa <- mean(gpa, na.rm = TRUE)
    sg_gpa <- sd(gpa, na.rm = TRUE)
    (gpa - mu_gpa) / sg_gpa
  }]
  

#### Merge In student_dt
student_dt[, resume_index := as.character(resume_index)]
firm_long_dt <- firm_long_dt[student_dt, on = .(resume_index)]
