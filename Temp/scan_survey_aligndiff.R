## Find the s5_view_aligndiff{role} questions in the survey PDF.
library(pdftools)
pdf <- pdf_text("0_Design/Employer_Survey.pdf")
for (i in seq_along(pdf)) {
  txt <- pdf[i]
  if (grepl("aligndiff|hr_influence|engineer_care", txt, ignore.case = TRUE)) {
    cat("\n======== Page", i, "========\n")
    cat(txt)
  }
}
