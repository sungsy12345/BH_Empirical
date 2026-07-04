## Generate a fractional-weight version of the report by mechanically converting the
## §6 top-k filter-chunks to WEIGHTED regressions. §1.2 needs no change (it uses the
## selection column as the LHS, which simply becomes the fractional selection probability).
src <- readLines("1_Codes/8B_Stage_BH.Rmd", warn = FALSE)
x <- paste(src, collapse = "\n")
n0 <- list()

rep_fixed <- function(x, a, b) { n0[[a]] <<- lengths(regmatches(x, gregexpr(a, x, fixed = TRUE))); gsub(a, b, x, fixed = TRUE) }

## (1) subset: include any candidate with positive weight
x <- rep_fixed(x, "get(hr_col) == 1",  "get(hr_col) > 0")
x <- rep_fixed(x, "get(eng_col) == 1", "get(eng_col) > 0")

## (2) weighted feols (weights = the fractional selection column)
x <- rep_fixed(x, "data = hr_data, vcov = ~ responseid)",
                  "data = hr_data, weights = as.formula(paste0('~', hr_col)), vcov = ~ responseid)")
x <- rep_fixed(x, "data = eng_data, vcov = ~ responseid)",
                  "data = eng_data, weights = as.formula(paste0('~', eng_col)), vcov = ~ responseid)")

## (3) weighted standardization (nonblind mean/sd at each k), overall_score and get(cmp)
for (rl in c("hr","eng")) {
  col <- paste0(rl, "_col"); dat <- paste0(rl, "_data")
  x <- rep_fixed(x, sprintf("%s[treat == \"nonblind\", mean(overall_score, na.rm = TRUE)]", dat),
                    sprintf("%s[treat == \"nonblind\", .wmean(overall_score, get(%s))]", dat, col))
  x <- rep_fixed(x, sprintf("%s[treat == \"nonblind\", sd(overall_score,   na.rm = TRUE)]", dat),
                    sprintf("%s[treat == \"nonblind\", .wsd(overall_score, get(%s))]", dat, col))
  x <- rep_fixed(x, sprintf("%s[treat == \"nonblind\", mean(get(cmp), na.rm = TRUE)]", dat),
                    sprintf("%s[treat == \"nonblind\", .wmean(get(cmp), get(%s))]", dat, col))
  x <- rep_fixed(x, sprintf("%s[treat == \"nonblind\", sd(get(cmp),   na.rm = TRUE)]", dat),
                    sprintf("%s[treat == \"nonblind\", .wsd(get(cmp), get(%s))]", dat, col))
}

writeLines(x, "Temp/_8B_fracweight.Rmd")
cat("Replacement counts (each should be > 0 where expected):\n")
for (nm in names(n0)) cat(sprintf("  %3d  <- %s\n", n0[[nm]], nm))

## parse-check: purl to R and parse, to catch syntax breakage before a slow render
tmpR <- tempfile(fileext = ".R")
suppressWarnings(suppressMessages(knitr::purl("Temp/_8B_fracweight.Rmd", output = tmpR, quiet = TRUE)))
ok <- tryCatch({ parse(tmpR); TRUE }, error = function(e) { cat("PARSE ERROR:\n", conditionMessage(e), "\n"); FALSE })
cat(if (ok) "\nPARSE OK\n" else "\nPARSE FAILED\n")
