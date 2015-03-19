#install.packages("MAd")
library(MAd)
source("./macatC.r")
#x = readRDS("./data/x.RDS")
x = readRDS("./data/xvict.RDS")
#x = readRDS("./data/xall.RDS")
# View(x)
x$var = x$se * x$se
x$mvw = 0
x$mvw[x$gen == "mixed"] = NA
x$mvw[x$gen == "men"] = 1
x
library(MAd)
omni(d, var=var, data=x)
rma(yi=d, vi=var, data=x)

#f1 = rma(yi=x$d, sei = x$se, weights=x$weight, method="REML", mods=cbind(x$male, x$female))
#x$year = scale(x$year, scale=F)
x$mixed = 0
x$mixed[x$gender == "mixed"] = 1
x$gen = factor(x$gen)
x$dtype[x$dtype == "longitudinal"] = "mixed"
macat(g=d, var=se*se, mod=gen, data=x)
macat(g=d, var=se*se, mod=primary, data=x)
macat(g=d, var=se*se, mod=vtype, data=x)
macat(g=d, var=se*se, mod=dtype, data=x)
macat(g=d, var=se*se, mod=ipv, data=x)
macat(g=d, var=se*se, mod=scz, data=x)
macat(g=d, var=se*se, mod=crime, data=x)
macat(g=d, var=se*se, mod=substance, data=x)W
x$viol
macat(g=d, var=se*se, mod=viol, data=x)
macat(g=d, var=se*se, mod=onlypeer, data=x)

## GENDER
macat(g=d, var=se*se, mod=gen, data=x)
x2 = subset(x, !is.na(mvw))
macat(g=d, var=se*se, mod=gen, data=x2)
out = (macatC(1, 2, g=d, var=var, mod=mvw, data=x2, type="planned" ))
x$gen = as.numeric(x$gen)
#x$gen = factor(x$gen, levels=c("1",))
(macatC(2, 3, g=d, var=var, mod=gen, data=x, type="planned" ))


## METARE
#summary(mareg(d ~ factor(gen), var=var, data=x))

summary(mareg(d ~ scale(year), var=var, data=x))
summary(mareg(d ~ k, var=var, data=x))
x$random[is.na(x$random)] = 0
summary(mareg(d ~ random, var=var, data=x))
x$inv = 0
x$inv[grep("inv", x$weighting)] = 1
summary(mareg(d ~ inv, var=var, data=x))
# OUTLIERS
x$outlier
x$out = 0
x$out[grep("yes", x$outlier)] = 1
x$out[grep("[1-9]", x$outlier)] = 1
summary(mareg(d ~ out, var=var, data=x))
# MULTIPLE CODERS
x$coders
x$cdrs = 0
x$cdrs[grep("[2-9]", x$coders)] = 1
summary(mareg(d ~ cdrs, var=var, data=x))
# UNPUBLISHED
x$unpub = as.integer(x$unpub)
x$unpub[is.na(x$unpub)] = 0
summary(mareg(d ~ unpub, var=var, data=x))
# PRIMARY
summary(mareg(d ~ primary, var=var, data=x))

# PRIMARY
x$role = factor(x$role)
summary(mareg(d ~ role, var=var, data=x))

# PRIMARY

(out <- summary(mareg(d ~ k + scale(year, scale=F) + random + inv + unpub + out + cdrs + primary, var=var, data=x)))

(out <- summary(mareg(d ~ k + scale(year) + random + inv + unpub + out + cdrs + primary, var=var, data=x)))

out = rma.mv(yi = d, mods = ~ k + scale(year, scale=F) + random + inv + unpub + out + cdrs + primary, V=var, data=x)
out = rma(yi = d, mods = ~ k + scale(year) + random + inv + unpub + out + cdrs + primary, data=x, sei = se, method="REML", knha=T, )
summary(out)

x$k[is.na(x$k)] = median(x$k, na.rm=T)
out = rma.mv(yi = d, mods =d ~ random + inv + unpub + out + cdrs + primary, random = ~ k + year, V=var, data=x)
out


rma.mv(yi = d, mods = ~ k + year + random + inv + unpub + out + cdrs + primary, V=var, data=x, method="REML")
rma.mv(yi = d, mods = ~ random + inv + unpub + out + cdrs + primary, V=var, data=x, method="REML")




out = out$coef
for(i in 1:9) {
  cat(round(out[i,1], 2), " [", round(out[i,4], 2), ", ", round(out[i, 5], 2), "]\n", sep="")  
}

nrow(x)

summary(mareg(d ~ male + female + ipv + exp + crime + year + primary + k, var=var, data=x))
summary(mareg(d ~ year, var=var, data=x))
summary(mareg(d ~ male + female, var=var, data=x))
summary(mareg(d ~ male + mixed, var=var, data=x))
summary(mareg(d ~ onlypeer + keywords + k + ref.search + contact + year + primary, var=var, data=x))
#tmp = plotcat(g=d, var=se, mod=gen, data=x)
#tmp
