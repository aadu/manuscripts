#Create my data-set to show popularity
load("~/Dropbox/serotonin/data/hiaa_data.RData")
pop.hiaa <- data.frame(id=df$id, type="hiaa", year=df$year)
load("~/Dropbox/serotonin/data/atd_data.RData")
pop.atd <- data.frame(id=df$id, type="atd", year=df$year)
load("~/Dropbox/serotonin/data/endo_overall_data.RData")
pop.end <- data.frame(id=df$id, type="end", year=df$year)
load("~/Dropbox/serotonin/data/pharm_data.RData")
pop.pharm <- data.frame(id=df$id, type="pharm", year=df$year)
pop2 <- rbind(pop.hiaa, pop.atd, pop.end, pop.pharm)

.get_count <- function(pop){
  out <- data.frame(year = 1979:2011)
  out$count <- 0
  out$count < as.integer(out$count)
  for (i in 1:nrow(pop)){
    year <- pop[i,]$year
    out$count[which(out$year==year)] <- out$count[which(out$year==year)] + 1  
  }
  #out <- ts(out$count, start=1979, frequency=1)
  return(out)
}

hiaa <- .get_count(pop.hiaa)
atd <- .get_count(pop.atd)
end <- .get_count(pop.end)
pharm <- .get_count(pop.pharm)
pop <- .get_count(pop=pop2)

w <- 1.6
s <- .45

#plot(lowess(x=pop$year, y=pop$count, f=s), col="black", type='l', lwd=w, lty=2) # pharm = dot/dash
require(tikzDevice)
tikz(file='/home/duke/Dropbox/serotonin/paper/intro/figures/pop.tex', width=3.5, height=3.5)
plot(lowess(x=end$year, y=end$count, f=s), col="black", lty=3, lwd=w, type="l",
     xlab="Year", ylab="Studies per Year") # Endocrine challenge = dotted
lines(lowess(x=hiaa$year, y=hiaa$count, f=s), col="black", type='l', lwd=w, lty=1) # 5-hiaa = solid line
lines(lowess(x=atd$year, y=atd$count, f=s), col="black", type='l', lwd=w, lty=4) # Atd = Dashed 
lines(lowess(x=pharm$year, y=pharm$count, f=s), col="black", type='l', lwd=w, lty=5) # pharm = dot/dash
legend("topleft", inset=.02, legend=c("\\footnotesize{CSF 5-HIAA}", "\\footnotesize{ATD}",
                                      "\\footnotesize{EC}", "\\footnotesize{PC}"), lwd=1.3, lty=c(1, 4, 3, 5), seg.len=2,
       bty="n")
dev.off()






# xyplot(x=hiaa$year, y=hiaa$count, type="smooth", cex.main=.8, 
#      main="Number of Articles on 5-HT and Aggression",
#      xlab="Year", ylab="Number of Articles")
# 
# lines(x=hiaa$year, y=hiaa$count)
# lines(lowess(x=hiaa$year, y=hiaa$count, f=.45), col="black", lty="dashed")
# 
# 
# 
# 
# 
# hist(end$count)
# hist(pop.end$year)
# plot(density(pop.pharm$year))
# 
# plot.ts(x=log(hiaa), )
# plot.ts(x=hiaa, type="o", )
# ts.plot(hiaa)
# 
# 
# barplot(hiaa$count)
# 
# 
# plot.xy(xy.coords(x=end$year, y=end$count) , type="loess")
# 
# plot(x=pharm$year, y=pharm$count, type="smooth")
# 
# 
# xyplot(c(as.integer(pharm$year), as.double(pharm$count)))
# 
# x)
# 
# 
# #dev.off()


# 
# #df <- read.csv('/home/duke/Dropbox/serotonin/paper/intro/figures/pop.csv')
# #attach(df)
# # tikz(file='/home/duke/Dropbox/serotonin/paper/intro/figures/pop.tex', width=5, height=5)
# tikz(file='/home/duke/Dropbox/serotonin/paper/intro/figures/pop.tex', width=3, height=3)
# plot(x=year, y=count, type="h", cex.main=.8,#cex.axis=.7, cex.lab=.7,cex.main=.7,
#      main="Number of Articles on 5-HT and Aggression",
#      xlab="Year", ylab="Number of Articles")
# lines(x=year, y=count)
# lines(lowess(x=year, y=count, f=.45), col="black", lty="dashed")
# dev.off()
# detach(df)
# 
# 
# 
# 
# require('sm')
# pop <- subset(pop, year >= 1980 & year <=2010)
# pop.f <- factor(pop$type)
# sm.density.compare(pop$year, pop.f)
# 
# 
# #require(lessR)
# #color.density(x=pop$year)
# 
# 
# #jj <- ts(start=1979, frequency=1)
