x <- readLines("Temp/_oos_report.Rmd")
x <- gsub("$=E[Y\\mid\\text{blind hires}]-E[Y\\mid\\text{non-blind hires}]$",
          "= E[Y | blind hires] - E[Y | non-blind hires]", x, fixed=TRUE)
reps <- list(
  c("$|{\\rm corr}|\\approx0.5$","|corr| approx 0.5"),
  c("$\\pm0.02$","+/- 0.02"),
  c("$\\sim$","~"), c("$\\times$"," x "), c("$\\to$"," -> "),
  c("$R^2$","R2"), c("$s(X)$","s(X)"), c("$SD(u)$","SD(u)"),
  c("$u$","u"), c("$Y$","Y"))
for (r in reps) x <- gsub(r[1], r[2], x, fixed=TRUE)
writeLines(x, "Temp/_oos_report.Rmd")
cat("remaining $ on any line:\n"); print(grep("\\$", x, value=TRUE))
