library(irr)
source("./functions.r")
#es = import.google("https://docs.google.com/spreadsheet/pub?key=0Ar2MWSi_hKI6dHN6NWZvV09teWVSdUxVRElHWG5KSWc&single=true&gid=14&output=csv")
es = import.google("https://docs.google.com/spreadsheet/pub?key=0Ar2MWSi_hKI6dEtjZEhiVE11amZMRDZ2SzZIeG56Unc&single=true&gid=14&output=csv")
es = es[es$coder == "Aaron",]
es = es[es$include == 1, ]
es = es[!is.na(es$coder),]
es$weight = as.numeric(gsub(",", "", es$weight))
es = es[order(es$bibtex),]

View(es)
x = data.frame(me = as.double(es$d), her = as.double(es$me.recode))
for(i in 1:nrow(x)){
  if(is.na(x$her[i]))
    x$her[i] = x$me[i]
}
x = subset(x, !is.na(her))
x[24, 2] = .204
agree(x)
kappa2(ratings=x)
