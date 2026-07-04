suppressMessages(library(pdftools))
for (f in list.files("2_Reports", pattern="FracWeight.*pdf", full.names=TRUE))
  cat(basename(f), "->", length(pdf_text(f)), "pages\n")
