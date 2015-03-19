
require(metafor, quietly=TRUE, warn.conflicts=FALSE)
require(MAc, quietly=TRUE, warn.conflicts=FALSE)
load(file="~/Dropbox/serotonin/data/alltogether.RData")

source("~/Dropbox/serotonin/analyses/alltogether/agg_cat_tab.r")


hetero_ <- function(m){  
  m <- m$Heterogeneity
  out <- paste("$Q_B$(", m$df.b, ") = ", r_(m$Qb, dig=2, add=T), ", \n", p_(m$p.b, 3), sep="")
  return(out)
}

homogeneo <- function(m){  
  #m <- mc1
  m <- m$Model
  lng <- length(m$I2)
  i2 <- gsub(pattern="%", replacement="\\\\%", x=m$I2[lng])
  i2 <- paste("$I^2$ =", i2)      
  qw <- r_(m$Q[lng], dig=2, add=F)
  ddf <- m$df[lng]
  ph <- p_(p=m$p.h[lng], dig=2)
  out <- paste("$Q_W$(", ddf, ") = ", qw, ", ", i2, ", ", ph, sep="")
  return(out)
}


gt_md_lvl <- function(m, l){
  #m <- m.src
  #l <- 2
  m <- m$Model
  m <- m[l,]
  r <- r_(m$estimate, dig=2, add=T)
  r <- paste("$r$ =", r)
  ci <- paste("95\\% $CI$[", r_(m$ci.l, dig=2, add=T), ", ", r_(m$ci.u, dig=2, add=T), "], ", p_(m$p, dig=2), sep="")
  return(list(r = r, ci = ci))
}

df <- fix_again(df=df, dig=3)
am1 <- macat(ztor=TRUE, var=z_v, es=z, mod=measure, data=df) 
am1.between <- hetero_(m=am1) ## BETWEEN AGGRESSION MEASURES
am1.within <- homogeneo(m=am1) ## BETWEEN AGGRESSION MEASURES32


df <- load_agg_data()
df <- subset(df, src == "self" | src == "other" | src == "observation")# | src == "laboratory")
df$src[df$src == "observation"] <- "other"
df$src[df$src == "laboratory"] <- "other"
df$src <- factor(df$src, levels=c("self", "other"), labels=c("aSelf", "cOther"))
(m.src <- macat(ztor=TRUE, var=z_v, es=z, mod=src, data=df))    
m.src$Model$mod <- c("Self", "Other", "Overall")  

msrc  <- gt_md_lvl(m.src, 2) ## Remember that 1 is r and 2 is CI
msrc2 <- gt_md_lvl(m.src, 1) ## Remember that 1 is r and 2 is CI21

DoAnalyses(name="aggression", subfolder="combined")