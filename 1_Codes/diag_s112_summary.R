## Extract the §1.1.2 plot's headline numbers: per-cell blinding TE at
## Low / Mid / Top of GPA and test_case, by role and by demographic dim.
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
library(pacman)
p_load(haven, here, data.table, fixest, readr, dplyr, tidyverse, stringr,
       labelled, splitstackshape, lubridate)
setwd(here())
source(here("1_Codes", "2_Import.R")); source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R")); source(here("1_Codes", "5_Cleaning.R"))
source(here("1_Codes", "6_Hiring_Simulation.R"))

build_aux <- function(d) {
  d[, urm_tech_blind := fcase(
    treat == "blind", "Blind",
    treat == "nonblind" & urm_tech == 1, "Nonblind_URM",
    treat == "nonblind" & urm_tech == 0, "Nonblind_NonURM",
    default = NA_character_)]
  d[, urm_tech_blind := factor(urm_tech_blind, levels = c("Blind","Nonblind_NonURM","Nonblind_URM"))]
  d[, urm_axis_blind := fcase(
    treat == "blind", "Blind",
    treat == "nonblind" & race_gender %in% c("White_Male","Asian_Male"), "Nonblind_MaleNonHisp",
    treat == "nonblind" & race_gender %in% c("White_Female","Asian_Female"), "Nonblind_FemaleNonHisp",
    treat == "nonblind" & race_gender %in% c("Hispanic_Male","Hispanic_Female"), "Nonblind_Hispanic",
    default = NA_character_)]
  d[, urm_axis_blind := factor(urm_axis_blind, levels = c("Blind","Nonblind_MaleNonHisp","Nonblind_FemaleNonHisp","Nonblind_Hispanic"))]
  d[, race_gender := factor(race_gender, levels = c("Blind","White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female"))]
  d
}

fit_cont <- function(d, fcol, qcol) {
  extra_fe <- if (any(d$role == "Eng")) " + resp_python + resp_java" else ""
  fml <- as.formula(sprintf(
    "I(-sc_overall_z) ~ i(%s, ref='Blind') * %s + resp_age | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income%s",
    fcol, qcol, extra_fe))
  feols(fml, data = d, vcov = ~ responseid + resume_index)
}

extract_te <- function(m, fcol, demos, qpts = c(-1, 0, 1)) {
  cf <- coef(m); V <- vcov(m); np <- length(cf)
  out <- list()
  for (d in demos) {
    main_nm <- paste0(fcol, "::", d)
    int_nm  <- paste0(fcol, "::", d, ":", attr(m, "qcol", exact = TRUE))
    int_nm  <- grep(paste0(fcol, "::", d, ":"), names(cf), value = TRUE)[1]
    for (q in qpts) {
      v <- numeric(np)
      if (main_nm %in% names(cf)) v[match(main_nm, names(cf))] <- 1
      if (!is.na(int_nm) && int_nm %in% names(cf)) v[match(int_nm, names(cf))] <- q
      est <- sum(v * cf); se <- sqrt(c(v %*% V %*% v))
      p   <- 2*pnorm(-abs(est/se))
      out[[length(out)+1]] <- data.table(demo = d, q = q, est = est, se = se, p = p)
    }
  }
  rbindlist(out)
}

dt_hr  <- build_aux(firm_long_dt[role == "HR"])
dt_eng <- build_aux(firm_long_dt[role == "Eng"])

cat("\n=========== §1.1.2 SUMMARY: per-cell blinding TE at Low/Mid/Top quality ===========\n")
for (rl_lab in c("HR", "Eng")) {
  d <- if (rl_lab == "HR") dt_hr else dt_eng
  cat(sprintf("\n##### Role: %s #####\n", rl_lab))
  for (qcol in c("gpa_z", "test_case_z")) {
    cat(sprintf("\n--- Quality var: %s ---\n", qcol))
    ## race_gender (6 cells)
    m <- fit_cont(d, "race_gender", qcol)
    rg <- c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female")
    out <- extract_te(m, "race_gender", rg)
    cat("By race_gender:\n")
    out_w <- dcast(out, demo ~ q, value.var = c("est","p"))
    print(out_w)

    ## urm_axis_blind
    m <- fit_cont(d, "urm_axis_blind", qcol)
    ax <- c("Nonblind_MaleNonHisp","Nonblind_FemaleNonHisp","Nonblind_Hispanic")
    out <- extract_te(m, "urm_axis_blind", ax)
    cat("By urm_axis (Male NH / Female NH / Hispanic):\n")
    print(dcast(out, demo ~ q, value.var = c("est","p")))

    ## urm_tech_blind
    m <- fit_cont(d, "urm_tech_blind", qcol)
    ut <- c("Nonblind_NonURM","Nonblind_URM")
    out <- extract_te(m, "urm_tech_blind", ut)
    cat("By urm_tech (Majority / URM):\n")
    print(dcast(out, demo ~ q, value.var = c("est","p")))
  }
}
