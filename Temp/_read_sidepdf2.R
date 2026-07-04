suppressMessages(library(pdftools))
f <- "2_Reports/SideReport_DropEng0jLRJv_20260630_1811.pdf"
txt <- pdf_text(f)
for (i in c(22,23,24,25,26)) { cat("\n=================== PAGE", i, "===================\n"); cat(txt[i]) }
