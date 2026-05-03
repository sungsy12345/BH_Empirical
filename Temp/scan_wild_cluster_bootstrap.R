## Manual wild cluster bootstrap (one-way Rademacher on resume_index, the
## binding 18-cluster axis). Compares CR1 two-way analytic SEs with
## bootstrap p-values for the §7 baseline (race_gender x top_half_coder)
## HR + Eng regressions, at each tier level.

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

## ---- Manual wild cluster bootstrap (WCU = unrestricted) -------------------
## Uses fitted(model) + residuals(model) * Rademacher cluster weights to form
## bootstrap outcomes; refits feols and recomputes the t-stat against the
## ORIGINAL beta (not zero), giving symmetric two-sided p-values.
manual_wcb <- function(d, fml, params, cluster_var = "resume_index",
                        vcov_arg = ~ responseid + resume_index, B = 499, seed = 1) {
  set.seed(seed)
  m0 <- suppressMessages(suppressWarnings(feols(fml, data = d, vcov = vcov_arg)))
  cf0 <- coef(m0); V0 <- vcov(m0)
  params <- intersect(params, names(cf0))
  beta_obs <- cf0[params]
  se_obs   <- sqrt(diag(V0)[params])
  t_obs    <- beta_obs / se_obs

  d <- copy(d)
  d[, .yhat := fitted(m0)]
  d[, .res  := residuals(m0)]

  cl <- d[[cluster_var]]
  uc <- unique(cl)
  nc <- length(uc)

  ## Replace LHS of formula with .y_star while preserving the | FE block.
  ## update() corrupts fixest's `|` separator, so reconstruct as string.
  rhs_str <- as.character(fml)[3]
  fml_b   <- as.formula(paste(".y_star ~", rhs_str))

  t_boot <- matrix(NA_real_, nrow = B, ncol = length(params),
                   dimnames = list(NULL, params))
  for (b in seq_len(B)) {
    w  <- sample(c(-1, 1), nc, replace = TRUE)
    wo <- w[match(cl, uc)]
    d[, .y_star := .yhat + .res * wo]
    mb <- suppressMessages(suppressWarnings(feols(fml_b, data = d, vcov = vcov_arg)))
    cfb <- coef(mb); Vb <- vcov(mb)
    for (p in params) {
      if (p %in% names(cfb)) {
        t_boot[b, p] <- (cfb[p] - beta_obs[p]) / sqrt(Vb[p, p])
      }
    }
  }

  p_wild <- sapply(params, function(p) mean(abs(t_boot[, p]) >= abs(t_obs[p]), na.rm = TRUE))
  data.frame(param = params,
             beta_neg = beta_obs,
             se_cr1   = se_obs,
             p_cr1    = 2 * pnorm(-abs(t_obs)),
             p_wild   = p_wild,
             row.names = NULL)
}

run_compare <- function(role_lbl, ref_tier, B = 499) {
  cat("\n#####################################################\n")
  cat(sprintf("# %s --- ref_tier = %s (TE shown at this tier)\n", role_lbl, ref_tier))
  cat("#####################################################\n")
  d <- copy(firm_long_dt[role == role_lbl])
  d[, race_gender := factor(race_gender,
       levels = c("Blind", "White_Male", "White_Female",
                  "Asian_Male", "Asian_Female",
                  "Hispanic_Male", "Hispanic_Female"))]
  d[, top_half_coder := factor(top_half_coder, levels = c("0", "1"))]
  d[, top_half_coder := relevel(top_half_coder, ref = as.character(ref_tier))]

  fml <- I(-sc_overall_z) ~ race_gender * top_half_coder +
           resp_age + I(resp_age^2) + I(resp_age^3) |
           resume_index + r_do + resp_gender + resp_race + s2_educ + s2_income

  demos <- c("White_Male", "White_Female", "Asian_Male", "Asian_Female",
             "Hispanic_Male", "Hispanic_Female")
  params <- paste0("race_gender", demos)

  out <- manual_wcb(d, fml, params, cluster_var = "resume_index", B = B)
  ## Flip sign so positive = effect of blinding (the report convention)
  out$beta <- -out$beta_neg
  out$beta_neg <- NULL
  out <- out[, c("param", "beta", "se_cr1", "p_cr1", "p_wild")]
  print(out, row.names = FALSE, digits = 3)
  invisible(out)
}

t0 <- Sys.time()
run_compare("HR",  ref_tier = 0, B = 499)  # TE at bottom-half coders
run_compare("HR",  ref_tier = 1, B = 499)  # TE at top-half coders
run_compare("Eng", ref_tier = 0, B = 499)
run_compare("Eng", ref_tier = 1, B = 499)
cat(sprintf("\nTotal time: %.1f sec\n", as.numeric(Sys.time() - t0, units = "secs")))
cat("\n--- Done ---\n")
