source("./functions.r")
#raw = import.google("https://docs.google.com/spreadsheet/pub?key=0Ar2MWSi_hKI6dE5nVjFlVTh4eVVZZ2U2UWhmZ05iYlE&single=true&gid=1&output=csv")
raw = import.google("https://docs.google.com/spreadsheet/pub?key=0Ar2MWSi_hKI6dE5nVjFlVTh4eVVZZ2U2UWhmZ05iYlE&single=true&gid=1&output=csv")
raw$study = paste(raw$author, ", ", raw$year, sep="")
raw = raw[!is.na(raw$coder)]
raw = raw[raw$coder == "Aaron",]
raw = raw[raw$include == 1,]
raw = raw[order(raw$year, decreasing=T),]
over = subset(raw, !is.na(bibtex))
over$other.x = ""
over$other.x[!is.na(over$other)] = "x"
saveRDS(over, "./data/over.RDS ")
saveRDS(over, "./md/search.RDS ")
# x = over
# View(x)
#x$es_num
#View(over)

out = data.frame(Study = x$study, 
               Citations = x$citations,
               Focus = x$focus, 
               Substance = x$substance,
               Role = x$role, 
               Gender = x$gender,
               Stat = x$stat,
               YearsIncluded = x$years,                
               PsycINFO = x$psycINFO, 
               MEDLINE = x$MEDLINE,
               CINAHL = x$CINAHL,
               #ERIC = x$ERIC,
               EMBASE = x$EMBASE,
               CJA = x$CJA,
               Scholar = x$Google,
               Other = x$other.x,
               Initial = x$flow.initial,
               Final = x$flow.final)


write.csv(out, "./out/output.csv",row.names=F, na="")
#write.csv(x, "./out/output.csv")
#View(over)
# View(over)
