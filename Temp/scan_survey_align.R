## Extract alignment-related text from the survey PDF.
library(pdftools)
pdf <- pdf_text("0_Design/Employer_Survey.pdf")
for (i in seq_along(pdf)) {
  txt <- pdf[i]
  if (grepl("aligned|misaligned|alignment", txt, ignore.case = TRUE)) {
    cat("\n======== Page", i, "========\n")
    cat(txt)
  }
}
