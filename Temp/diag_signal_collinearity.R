## Diagnose why some signals get dropped in the §5 spec.
rm(list = ls())
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled, writexl,
       tidyverse, dplyr, ggplot2, ggbreak, ggthemes, ggrepel, ggtext, ggpattern, ggforce, patchwork, cowplot, corrplot,
       tikzDevice, lubridate, scales, RCT, Hmisc, fixest, sandwich, broom, marginaleffects, modelsummary, xtable, kableExtra,
       splitstackshape, spatstat, knitr, kableExtra, rmarkdown, tinytex, ezknitr, pdflscape, gridExtra, patchwork,
       zipcodeR, tigris, sf, scales, hexbin, patchwork, broom)
options(tigris_use_cache = TRUE); setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

cat("\n--- Per-resume values for the 17 signals ---\n")
print(unique(firm_long_dt[, .(resume_index, gpa_z, num_proj, num_exp, num_lead, num_awards,
                              work_1000, work_1000_twice, work_newstartup, work_research, work_teaching,
                              python, java, javascript, c_base, cplusplus, csharp, sql,
                              code_opt_out, q1_python, q1_java, q1_cplusplus,
                              q2_python, q2_java, q2_cplusplus)]))

cat("\n--- Variance of each signal across the 18 resume_index values ---\n")
sigs <- c("gpa_z", "num_proj", "num_exp", "num_lead", "num_awards",
          "work_1000", "work_1000_twice", "work_newstartup", "work_research", "work_teaching",
          "python", "java", "javascript", "c_base", "cplusplus", "csharp", "sql",
          "test_case_z", "code_opt_out", "q1_python", "q1_java", "q1_cplusplus",
          "q2_python", "q2_java", "q2_cplusplus")
for (s in sigs) {
  v <- unique(firm_long_dt[, .(resume_index, val = get(s))])
  cat(sprintf("%-22s  uniq_vals = %d  range = [%s, %s]  sum_of_1s_if_binary = %s\n",
              s, uniqueN(v$val),
              format(min(v$val, na.rm = TRUE), digits = 3),
              format(max(v$val, na.rm = TRUE), digits = 3),
              if (all(v$val %in% c(0, 1))) sum(v$val) else "n/a"))
}

cat("\n--- Try fitting the full Eng spec and see which terms get dropped ---\n")
hr_signals <- c("gpa_z", "num_proj", "num_exp", "num_lead", "num_awards",
                "work_1000", "work_1000_twice", "work_newstartup", "work_research", "work_teaching",
                "python", "java", "javascript", "c_base", "cplusplus", "csharp", "sql")
eng_only <- c("test_case_z", "code_opt_out", "q1_python", "q1_java", "q2_python", "q2_java")
eng_signals <- c(hr_signals, eng_only)
firm_long_dt[, resume_id := paste(resume_ver, resume_index, sep = "_")]

fml_eng <- as.formula(paste0("sc_overall_z ~ (", paste(eng_signals, collapse = " + "),
                              ") * i(treat, ref = 'nonblind') | resume_index + responseid + r_do"))
m_eng <- feols(fml_eng, data = firm_long_dt[role == "Eng"], vcov = ~ responseid + resume_id)
cat("\nFitted Eng model coefs:\n")
print(names(coef(m_eng)))

cat("\nFitted HR model coefs:\n")
fml_hr <- as.formula(paste0("sc_overall_z ~ (", paste(hr_signals, collapse = " + "),
                             ") * i(treat, ref = 'nonblind') | resume_index + responseid + r_do"))
m_hr <- feols(fml_hr, data = firm_long_dt[role == "HR"], vcov = ~ responseid + resume_id)
print(names(coef(m_hr)))
