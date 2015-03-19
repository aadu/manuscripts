#o = over
desc = function(x, digits = 1) {
  if(is.character(x) | is.factor(x)){
    x = factor(x)
    df = data.frame(i = names(summary(x)))
    df$n = as.vector(summary(x))
    df$p = round(df$n / length(x) * 100 , digits)
    return(df)
  } else {
    x[x > 0] = 1
    x = factor(x)
    return(desc(x))
  }
    
}

desc(o$ages)
desc(o$psycINFO)
desc(o$MEDLINE)
desc(o$CINAHL)
desc(o$ERIC)
desc(o$EMBASE)
desc(o$CJA)
desc(o$Google)
desc(o$ERIC)
desc(o$unpub)
desc(o$keywords)
desc(o$ref.search)
desc(o$contact)
desc(o$flow.initial)
desc(o$flow.final)
desc(o$journals)
desc(o$lang)


desc(o$agg.method)
desc(o$weighting)
desc(o$random + o$fixed)
desc(o$random)
desc(o$fixed)

desc(o$outlier)
desc(o$coders)
desc(o$role)
desc(o$substance)
desc(o$gender)
desc(o$vtype)
desc(o$dtype)
desc(o$statistic)

desc(o$unpub)
desc(o$onlypeer)



