## Diagnostic: §2.1 results under broad vs detailed-race concordance.
## Broad (current): all Asians match all Asians (Korean -> Chinese counts as concordant).
## Detailed: only Chinese-Chinese / Indian-Indian count as Asian concordant.
##           Korean/Japanese/etc. are kept as always-discordant (mirroring §2.4 spec).
library(pacman)
p_load(haven, here, base, readr, data.table, stringr, labelled, writexl,
       tidyverse, dplyr, fixest, knitr)
options(readr.show_types = FALSE)
setwd(here())

date      <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"

source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

## ---------- Build broad and detailed Race concordance factors ----------
firm_long_dt[, broad_race_blind := fcase(
  treat == "blind",                            "Blind",
  treat == "nonblind" & concordance_race == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_race == 0, "Nonblind_Nonconcordant",
  default = NA_character_
)]
firm_long_dt[, broad_race_blind := factor(broad_race_blind,
  levels = c("Blind", "Nonblind_Concordant", "Nonblind_Nonconcordant"))]

## Detailed: keep Korean/Japanese/etc. Asian as always-discordant in nonblind
firm_long_dt[, detailed_race_blind := fcase(
  treat == "blind",                                     "Blind",
  treat == "nonblind" & concordance_race_detailed == 1, "Nonblind_Concordant",
  treat == "nonblind" & concordance_race_detailed == 0, "Nonblind_Nonconcordant",
  is.na(concordance_race_detailed) & treat == "nonblind" &
    resp_race == "Asian", "Nonblind_Nonconcordant",
  default = NA_character_
)]
firm_long_dt[, detailed_race_blind := factor(detailed_race_blind,
  levels = c("Blind", "Nonblind_Concordant", "Nonblind_Nonconcordant"))]

## ---------- Cell counts ----------
cat("\n========== Cell counts (HR) ==========\n")
cat("\n-- Broad (concordance_race) --\n")
print(firm_long_dt[role == "HR" & resp_race %in% c("White","Asian","Hispanic"),
                   .N, by = broad_race_blind])
cat("\n-- Detailed --\n")
print(firm_long_dt[role == "HR" & resp_race %in% c("White","Asian","Hispanic"),
                   .N, by = detailed_race_blind])

cat("\n========== Cell counts (Engineer) ==========\n")
cat("\n-- Broad --\n")
print(firm_long_dt[role == "Eng" & resp_race %in% c("White","Asian","Hispanic"),
                   .N, by = broad_race_blind])
cat("\n-- Detailed --\n")
print(firm_long_dt[role == "Eng" & resp_race %in% c("White","Asian","Hispanic"),
                   .N, by = detailed_race_blind])

## ---------- Regressions: broad vs detailed ----------
fit_one <- function(d, lhs_name, conc_var) {
  feols(
    as.formula(sprintf(
      "%s ~ i(%s, ref = 'Blind') + resp_age + I(resp_age^2) + I(resp_age^3) | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
      lhs_name, conc_var)),
    data = d, vcov = ~ responseid)
}
flip <- function(m) { m$coefficients <- -1 * m$coefficients; m }
extract_coef <- function(m, conc_var) {
  cf <- coef(m); se <- sqrt(diag(vcov(m)))
  conc_name <- paste0(conc_var, "::Nonblind_Concordant")
  disc_name <- paste0(conc_var, "::Nonblind_Nonconcordant")
  data.frame(
    Concordant_est = cf[conc_name],
    Concordant_se  = se[conc_name],
    Discordant_est = cf[disc_name],
    Discordant_se  = se[disc_name],
    N = nobs(m)
  )
}

cat("\n========== HR: Overall (Z) coefficients, signs flipped ==========\n")
m_broad_hr <- fit_one(firm_long_dt[role == "HR" & resp_race %in% c("White","Asian","Hispanic")],
                     "sc_overall_z", "broad_race_blind") |> flip()
m_det_hr   <- fit_one(firm_long_dt[role == "HR" & resp_race %in% c("White","Asian","Hispanic")],
                     "sc_overall_z", "detailed_race_blind") |> flip()
res_hr <- rbind(
  cbind(spec = "Broad (Asian = single group)", extract_coef(m_broad_hr, "broad_race_blind")),
  cbind(spec = "Detailed (Korean/Japanese = always discordant)", extract_coef(m_det_hr, "detailed_race_blind"))
)
print(res_hr, row.names = FALSE)

cat("\n========== Engineer: Overall (Z) coefficients, signs flipped ==========\n")
m_broad_eng <- fit_one(firm_long_dt[role == "Eng" & resp_race %in% c("White","Asian","Hispanic")],
                      "sc_overall_z", "broad_race_blind") |> flip()
m_det_eng   <- fit_one(firm_long_dt[role == "Eng" & resp_race %in% c("White","Asian","Hispanic")],
                      "sc_overall_z", "detailed_race_blind") |> flip()
res_eng <- rbind(
  cbind(spec = "Broad (Asian = single group)", extract_coef(m_broad_eng, "broad_race_blind")),
  cbind(spec = "Detailed (Korean/Japanese = always discordant)", extract_coef(m_det_eng, "detailed_race_blind"))
)
print(res_eng, row.names = FALSE)
