suppressMessages(library(pdftools)); f<-"2_Reports/Main_Results_20260702_1535.pdf"; t<-pdf_text(f)
sub<-which(grepl("Signal Substitution",t)); val<-which(grepl("Signal Validity",t))
cat("total pages:",length(t),"| Substitution pages:",paste(sub,collapse=","),"| Validity pages:",paste(val,collapse=","),"\n")
p1<-sub[1]; p2<-val[length(val)]
pdftools::pdf_convert(f,pages=c(p1,p2),filenames=c("Temp/_s7.png","Temp/_t9.png"),dpi=100,verbose=FALSE)
cat("rendered pages",p1,p2,"\n")
