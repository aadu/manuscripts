source("./functions.r")
#es = import.google("https://docs.google.com/spreadsheet/pub?key=0Ar2MWSi_hKI6dHN6NWZvV09teWVSdUxVRElHWG5KSWc&single=true&gid=14&output=csv")
es = import.google("https://docs.google.com/spreadsheet/pub?key=0Ar2MWSi_hKI6dEtjZEhiVE11amZMRDZ2SzZIeG56Unc&single=true&gid=14&output=csv")
es = es[es$coder == "Aaron",]
es = es[es$include == 5, ]
es = es[!is.na(es$coder),]
es$weight = as.numeric(gsub(",", "", es$weight))
es = es[order(es$bibtex),]

## WINSORIZE
x = es$weight
x[x > 1000] = 1000
x[x < 100] = 100
es$weight = x

## GENDER CODE
# es$bibtex = paste(es$bibtex, es$gender, sep=".")


## AGGREGATE
u = unique(es$bibtex)
x = rep(NA, length(u))
df = data.frame(bibtex = u, d = x, se = x, d.lb = x, d.ub = x, weight = x, n = x) 
for(i in 1:length(u)){
  x = es[es$bibtex == u[i],]  
  if(nrow(x) > 1){    
    y = rma(yi=as.numeric(x$d), sei = as.numeric(x$se), method="FE")
    df$d[i] = y$b
    df$se[i] = y$se
    df$n[i] = sum(x$k, na.rm=T)
    #df$d[i] = mean(as.numeric(x$d), na.rm=T)
    #df$d.lb[i] = mean(as.numeric(x$CI_lb), na.rm=T)
    #df$d.ub[i] = mean(as.numeric(x$CI_ub), na.rm=T)
    #df$se[i] = mean(as.numeric(x$se), na.rm=T)
    df$weight[i] = mean(as.numeric(x$weight), na.rm=T)
  } else if (nrow(x) == 1) {
    df$d[i] = as.numeric(x$d)
    #df$d.lb[i] = as.numeric(x$CI_lb)
    #df$d.ub[i] = as.numeric(x$CI_ub)
    df$se[i] = as.numeric(x$se)
    df$weight[i] = as.numeric(x$weight)
    df$n[i] = x$k
  } 
}

saveRDS(df, "./data/dfvict.RDS")

# #
# x = merge(df, over, by.x="bibtex")
# x = x[order(x$year),]
# x = x[order(x$d),]
# x$male = 0
# x$male[x$gender == "men"] = 1
# x$female = 0
# x$female[x$gender == "women"] = 1
# x$gen = factor(x$gender)
# over$bibtex
# df
# x$bibtex