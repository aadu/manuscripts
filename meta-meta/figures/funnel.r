df = readRDS("./data/df.RDS")
df = x

#--publication bias-
m1 <- rma.uni(yi=d, sei=se, method='REML', data=df)
ranktest.rma(m1) ## Rank test for funnel plot assymetry
regtest.rma(m1) ## Egger's regtest for funnel plot assymetry p < .05 = assymetric
fsn(yi=d, sei=se, data=df)[['fsnum']] ## Fail-safe N
df$pch = 16
df$pch[df$drug == "alcohol"] = 16
df$pch[df$drug == "drugs"] = 17
df$pch[df$drug == "combined"] = 15
df$pch[df$role == "victimization"] = 13

#png("Funnel.png", width=550, height=550, res=120)
#op <- par(mar=c(3,1,1,1), oma=c(0,0,0,0))
funnel.rma(m1, digits=c(1, 2), addtau2=T, yaxis="sei", pch=df$pch,
           xlab="Mean Standardized Difference", ylab="Standard Error",           
           cex=1.2, cex.axis=.7) # Funnel
title(main="Funnel plot with pseudo 95% confidence limits", cex.main=.7)
#legend("topright", inset=.05, legend=c("\\footnotesize{Prolactin}", "\\footnotesize{Cortisol}"),
       #pch=c(21, 17), cex=.7, bg="white", text.width=.35)
#par(op)#
dev.off()
