## Load up data
source('./functions.r')
x = readRDS("./data/x.RDS")

## Seperate out effects by gender category; calculate fit
fit = rma(yi=x$d, sei = x$se, weights=x$weight, method="REML")



men = x[x$gender == "men",];men = men[order(men$d),]
men.fit = rma(yi=men$d, sei=men$se, weights=men$weight, method="REML", knha=T)
women = x[x$gender == "women",];women = women[order(women$d),]
women.fit = rma(yi=women$d, sei=women$se, weights=women$weight, method="REML", knha=T)
mixed = x[x$gender == "mixed",];mixed = mixed[order(mixed$d),]
mixed.fit = rma(yi=mixed$d, sei=mixed$se, weights=mixed$weight, method="REML", knha=T)
x = rbind(women, men, mixed)


### FOREST PLOT ### 
## LABELS
author = x$study
l1 = prettyNum(x$n) ## Label 1 (sample size)
l2 = x$meas         ## Label 2 (measure)
lab = cbind(l1, l2) ## Labels

## GRAPHICAL PARAMETERS
sub.color  <- "grey80" ## Gender effects color
tot.color  <- "black"  ## Overall effect size color
x$psize = round(.40 * log(1/x$se), 2) ## Point Size
x$psize[x$psize < 1] = .9 ## Winsorize small values

size = .63 ## Overall CEX
llim = -4 ## Left Side
rlim = 2.5  ## Right side
l1.pos = llim + 1.6 ## Label 1 position
l2.pos = llim + 2.2 ## Label 2 position
lpos = c(l1.pos, l2.pos) 

## ROWS 
gap.above = 3.8
gap.between = 2.2
gap.below = 0
women.n = nrow(women)
men.n = nrow(men)
mixed.n = nrow(mixed)
top <- gap.above + 2*gap.between + gap.below + women.n + men.n + mixed.n
rows.1 = seq(top-gap.above, top - gap.above-women.n + 1, by=-1)
rows.2 = seq(top-gap.above - gap.between - women.n, top - gap.above-gap.between - women.n - men.n + 1, by=-1)
rows.3 = seq(top-gap.above - gap.between - women.n - gap.between - men.n, 
             top - gap.above - gap.between*2 - women.n - men.n - mixed.n + 1, by=-1)
(rows = c(rows.1, rows.2, rows.3)) ## Rows for each ES

hh <- top-1.2 ## Header height
row.men = min(rows.2)-1 
row.women = min(rows.1)-1
row.mixed = min(rows.3)-1
row.overall = min(rows)-2.4

## ACTUAL PLOT ##
# png("Forest.png", width=850, height=850, res=120)
png("Forest.png", width=1050, height=1050, res=150)
op <- par(mar=c(3,1,1,1), oma=c(0,0,0,0)) ## Change margin
forest(x$d, sei=x$se, slab=author, cex = size, at=seq(-.5,1.5,.5), xlab="", xlim=c(llim, rlim), ylim=c(-1, top), 
	ilab=lab, ilab.xpos=lpos, rows=rows, ilab.pos=rep(4, length(lpos)), psize=x$psize, efac=1.8, refline = 1)

addpoly(women.fit$b, sei=women.fit$se, col=sub.color, cex=size, row=row.women, mlab="", efac=1.5)
text(llim + .1, row.women, "Weighted Mean Estimate for Women", font=3, adj=0, cex=size)
addpoly(men.fit$b, sei=men.fit$se, col=sub.color, cex=size, row=row.men, mlab="", efac=1.5)
text(llim + .1, row.men, "Weighted Mean Estimate for Men", font=3, adj=0, cex=size)
addpoly(mixed.fit$b, sei=mixed.fit$se, col=sub.color, cex=size, row=row.mixed, mlab="", efac=1.5)
text(llim + .1, row.mixed, "Weighted Mean Estimate for Mixed Gender Samples", font=3, adj=0, cex=size)
addpoly(fit$b, sei=fit$se, col=tot.color, cex=size, row=row.overall, mlab="", efac=1.5)
text(llim + .1, row.overall, "Combined Mean Weighted Estimate", font=2, adj=0, cex=size)

## LABELS
text(rlim-.1, hh, "Weighted Standardized Mean Difference [95% CI]",cex=size, font=2, adj=1)
text(llim + .1, hh, "Author(s) and Year",cex=size, font=2, adj=0)
text(l1.pos + .1, hh, "k",cex=size, font=2, adj=0)
text(l2.pos + .15, hh, "Measure",cex=size, font=4, adj=0)
text(llim, max(rows.1) + 1, "Women", font=4, adj=0, cex=size)
text(llim, max(rows.2) + 1, "Men", font=4, adj=0, cex=size)
text(llim, max(rows.3) + 1, "Mixed-gender", font=4, adj=0, cex=size)
abline(h=row.overall+.7) ## Random line 
## Key 
key <- "Note. IPV = intimate partner violence.\n small effect: < 0.35, medium effect: 0.36-0.65, large effect: > 0.65"
mtext(text=key, side=1, cex=size, adj=.02, padj=1, font=3)
dev.off()


