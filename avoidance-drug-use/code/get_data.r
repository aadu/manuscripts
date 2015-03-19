## Load Data
library(foreign)
df = read.spss("./dose.sav", to.data.frame=T, trim.factor.names=T,
               use.value.labels=F)


# Clean data
rename = function(old, new, center=F){
  if (length(grep(old, names(df))) > 0){
    df[new] <<- df[old]
    df[old] <<- NULL
    if(center){
      df[paste(new, ".c", sep="")] <<- scale(df[new])
    }
  }
  else {
    cat("df$", old, " does not exist.\n", sep="")
  }
}

rename("p4", "id")
rename("agecalculated", "age", T)
rename("dincmon", "monthly_income")
rename("dincyr", "income", T)
rename("squareoverallphysicalpartner", "vict.physical")
rename("squareminorphysicalpartner", "vict.physical.minor")
rename("squareseverephysicalpartner", "vict.physical.severe")
rename("squareoverallinjurypartner", "vict.injury")
rename("squareoverallpsychpartner", "vict.psych")
rename("sqaureoverallsexpartner", "vict.sex")
rename("squareoverallphysicalself", "perp.physical")
rename("squareminorphysicalself", "perp.physical.minor")
rename("squareseverephysicalself", "perp.physical.severe")
rename("squareoverallinjuryself", "perp.injury")
rename("squareoverallpsychself", "perp.psych")
rename("squareoverallsexself", "perp.sex")
rename("csi_SS_total", "social", T)
rename("csi_PS_total", "problem", T)
rename("csi_A_total", "avoid", T)
rename("csisscope", "social", T)
rename("csipscope", "problem", T)
rename("csiavcope", "avoid", T)
rename("audittotal", "audit", T)
rename("cigarrettesum", "cig", T)
rename("dasttotal", "dast", T)
rename("cesd_total", "cesd", T)
rename("maqna", "neg.affect", T)
x = colnames(df)
x = gsub("CT", "", x)
x = gsub("YS", "", x)
x = gsub("YP", ".p", x)
x = gsub("A", "assault.", x)
x = gsub("I", "injury.", x)
x = gsub("P", "psych.", x)
x = gsub("N", "neg.", x)
x = gsub("S([A-Z])", "sex.\\1", x)
x = gsub("M.", "minor.", x)
x = gsub("M", "minor", x)
x = gsub("S.", "sev.", x)
x = gsub("S", "sev", x)
x = gsub(".T", "", x)
x = gsub(".E", ".emot", x)
x = gsub(".E.", ".emot.", x)
x = gsub(".C", ".cog", x)
x = gsub(".C.", ".cog.", x)
colnames(df) = x
saveRDS(df, "./dose.rds")
