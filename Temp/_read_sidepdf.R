suppressMessages(library(pdftools))
f <- "2_Reports/SideReport_DropEng0jLRJv_20260630_1811.pdf"
txt <- pdf_text(f)
cat("total pages:", length(txt), "\n")
hits <- which(grepl("[Pp]roductiv|[Ee]fficien|[Hh]ired", txt))
cat("pages mentioning productivity/efficiency/hired:", paste(hits, collapse=","), "\n")
for (i in hits) { cat("\n##### PAGE", i, "#####\n"); cat(txt[i]) }
