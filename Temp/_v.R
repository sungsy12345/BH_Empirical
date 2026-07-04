suppressMessages(library(pdftools)); f<-"2_Reports/Main_Results_20260702_1959.pdf"; t<-pdf_text(f)
s<-which(grepl("Signal Substitution",t))[1]; v<-tail(which(grepl("Signal Validity",t)),1)
cat("pages",length(t)," sub p",s," val p",v,"\n")
pdftools::pdf_convert(f,pages=c(s,v),filenames=c("Temp/_s7b.png","Temp/_t9b.png"),dpi=100,verbose=FALSE)
