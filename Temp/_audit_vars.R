## Audit: which evaluator-visible signals exist for ALL 69 vs only the experimental 18.
suppressMessages({library(pacman); p_load(haven, here, readr, data.table, stringr)})
options(warn = -1, readr.show_types = FALSE)
date <- "18mar2026"; data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
setwd(here::here())

sr <- fread(file.path(data_root, "3output_data/Main/student_data/student_matching/student_resume.csv"))
cat("================ student_resume.csv (all 69) ================\n")
cat("rows:", nrow(sr), "\n\ncandidate-signal columns present + %missing:\n")
sig <- c("test_case","gpa","gpa_major","num_proj","num_exp","num_lead","num_awards",
         "max_firm_signal_tier","q_version","python","java","javascript","c_base","cplusplus","csharp","sql",
         "overall_score","readability","time_efficiency","space_efficiency","runtime")
for (v in sig) if (v %in% names(sr))
  cat(sprintf("  %-22s %-8s  %.0f%% missing  | e.g. %s\n", v, class(sr[[v]])[1],
      100*mean(is.na(sr[[v]]) | sr[[v]]==""), paste(head(unique(sr[[v]]),3), collapse=", ")))
cat("\nfull column list:\n"); print(names(sr))

invisible(capture.output({for (f in c("2_Import","3_Firm_Cleaning","4_Distribution_Cleaning","5_Cleaning"))
  source(here::here("1_Codes", paste0(f, ".R")))}))
cat("\n================ candidate-signal vars USED in firm_long_dt (the 18) ================\n")
firm_sig <- c("top_exp","tier_top","tier_ordinary","repeated_top","qm1","qm2",
              "awards_z","test_case_z","gpa_z","misalign_z","max_firm_signal_tier")
cat("present in firm_long_dt:\n")
for (v in firm_sig) if (v %in% names(firm_long_dt))
  cat(sprintf("  %-16s -> equivalent in student_resume? %s\n", v,
      ifelse(v %in% names(sr), "YES", ifelse(v=="max_firm_signal_tier" & "max_firm_signal_tier"%in%names(sr),"YES(raw)","NEEDS BUILDING for 69"))))
cat("\nmax_firm_signal_tier values in student_resume (all 69):\n")
if ("max_firm_signal_tier" %in% names(sr)) print(table(sr$max_firm_signal_tier, useNA="always"))
cat("\nDONE.\n")
