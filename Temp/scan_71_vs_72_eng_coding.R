## Compare §7.1 (age polynomial, r_do) vs §7.2 (drop nonlinear age, r_do)
## for Engineers on the coding-ability split. Reports p-values for each
## (demographic, tier) cell to show which spec has more significance.

library(pacman)
p_load(haven, here, base, readr, data.table, stringr,
       tidyverse, dplyr, fixest)
setwd(here())
options(readr.show_types = FALSE)
date <- "18mar2026"
data_root <- "C:/Users/sungs/Dropbox/2_Research/A_By_Topics/A_Blind_hiring/"
source(here("1_Codes", "2_Import.R"))
source(here("1_Codes", "3_Firm_Cleaning.R"))
source(here("1_Codes", "4_Distribution_Cleaning.R"))
source(here("1_Codes", "5_Cleaning.R"))

build_aux <- function(d) {
  d <- copy(d)
  d[, urm_tech_blind := fcase(
    treat == "blind",                    "Blind",
    treat == "nonblind" & urm_tech == 1, "Nonblind_URM",
    treat == "nonblind" & urm_tech == 0, "Nonblind_NonURM",
    default = NA_character_)]
  d[, urm_tech_blind := factor(urm_tech_blind,
    levels = c("Blind", "Nonblind_NonURM", "Nonblind_URM"))]
  d[, urm_axis_blind := fcase(
    treat == "blind",                                                            "Blind",
    treat == "nonblind" & race_gender %in% c("White_Male", "Asian_Male"),         "Nonblind_MaleNonHisp",
    treat == "nonblind" & race_gender %in% c("White_Female", "Asian_Female"),     "Nonblind_FemaleNonHisp",
    treat == "nonblind" & race_gender %in% c("Hispanic_Male", "Hispanic_Female"), "Nonblind_Hispanic",
    default = NA_character_)]
  d[, urm_axis_blind := factor(urm_axis_blind,
    levels = c("Blind", "Nonblind_MaleNonHisp", "Nonblind_FemaleNonHisp", "Nonblind_Hispanic"))]
  d[, race_gender := factor(race_gender,
    levels = c("Blind", "White_Male", "White_Female",
               "Asian_Male", "Asian_Female",
               "Hispanic_Male", "Hispanic_Female"))]
  d[, top_half_coder := factor(top_half_coder, levels = c("0", "1"))]
  d
}
dt_eng <- build_aux(firm_long_dt[role == "Eng"])

fit_pooled_v <- function(d, factor_col, age_terms) {
  fml <- as.formula(sprintf(
    "I(-sc_overall_z) ~ %s * top_half_coder + %s | resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income",
    factor_col, age_terms))
  suppressMessages(suppressWarnings(
    feols(fml, data = d, vcov = ~ responseid + resume_index)
  ))
}

extract_te <- function(model, factor_col, demo_levels, demo_labels) {
  cf <- coef(model);  V <- vcov(model)
  np <- length(cf);   cf_names <- names(cf)
  rows <- list()
  for (i in seq_along(demo_levels)) {
    for (t in c("0", "1")) {
      d <- demo_levels[i]
      v <- numeric(np)
      idx_main <- match(paste0(factor_col, d), cf_names)
      if (!is.na(idx_main)) v[idx_main] <- 1
      if (t != "0") {
        idx_int <- match(paste0(factor_col, d, ":top_half_coder", t), cf_names)
        if (!is.na(idx_int)) v[idx_int] <- 1
      }
      est <- sum(v * cf); se <- sqrt(c(v %*% V %*% v))
      p <- 2 * pnorm(-abs(est / se))
      rows[[length(rows) + 1L]] <- data.frame(
        demo = demo_labels[i], tier = ifelse(t == "0", "Bot 50%", "Top 50%"),
        beta = est, se = se, p = p,
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

run_one <- function(label, age_terms) {
  cat("\n##############", label, "##############\n")
  m_rg <- fit_pooled_v(dt_eng, "race_gender",     age_terms)
  m_ax <- fit_pooled_v(dt_eng, "urm_axis_blind",  age_terms)
  m_ut <- fit_pooled_v(dt_eng, "urm_tech_blind",  age_terms)

  rg <- extract_te(m_rg, "race_gender",
    c("White_Male","White_Female","Asian_Male","Asian_Female","Hispanic_Male","Hispanic_Female"),
    c("White Male","White Female","Asian Male","Asian Female","Hispanic Male","Hispanic Female"))
  ax <- extract_te(m_ax, "urm_axis_blind",
    c("Nonblind_MaleNonHisp","Nonblind_FemaleNonHisp","Nonblind_Hispanic"),
    c("Male (NH)","Female (NH)","Hispanic"))
  ut <- extract_te(m_ut, "urm_tech_blind",
    c("Nonblind_NonURM","Nonblind_URM"),
    c("Majority","URM"))

  out <- rbind(
    cbind(group = "Per Race-Gender (6)", rg),
    cbind(group = "URM-Axis (3)",         ax),
    cbind(group = "URM-Tech (2)",         ut))
  out$beta <- -out$beta  # flip to "effect of blinding raises score" convention
  out$stars <- ifelse(out$p < 0.01, "***",
               ifelse(out$p < 0.05, "** ",
               ifelse(out$p < 0.10, "*  ", "")))
  print(out, row.names = FALSE, digits = 3)

  cat(sprintf("\n%s: cells with p<0.10 = %d  | p<0.05 = %d  | p<0.01 = %d  | total cells = %d\n",
              label,
              sum(out$p < 0.10), sum(out$p < 0.05), sum(out$p < 0.01), nrow(out)))
  invisible(out)
}

a <- run_one("S7.1 Baseline (age polynomial, r_do)",
             "resp_age + I(resp_age^2) + I(resp_age^3)")
b <- run_one("S7.2 Drop nonlinear age (r_do)",
             "resp_age")

cat("\n--- Done ---\n")
