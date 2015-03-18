source('./functions.r')
df = readRDS("./data/df2.RDS")
over = readRDS("./data/over.RDS")
over = over[!is.na(over$bibtex),]
#df$bibtex =   gsub("[.].*", "", df$bibtex )
df$bibtex = as.character(df$bibtex)
df$bibtex = gsub(" ", "", df$bibtex)
x = merge(df, over, by.x="bibtex")
x = x[order(x$year),]
x = x[order(x$d),]
x$male = 0
x$male[x$gender == "men"] = 1
x$female = 0
x$female[x$gender == "women"] = 1
x$gen = factor(x$gender)
saveRDS(x, "./data/x2.RDS")
df2 = readRDS("./data/df3.RDS")
df2$bibtex =   gsub("[.].*", "", df2$bibtex )
  #gsub("[.].*", "", df2$bibtex )
x2 = merge(df2, over, by.x="bibtex")
x2 = x2[order(x2$d),]
x2$male = 0
x2$male[x2$gender == "men"] = 1
x2$female = 0
x2$female[x2$gender == "women"] = 1
x2$gen = factor(x2$gender)
saveRDS(x2, "./data/x3.RDS")

#View(x)
sub.fit = rma(yi=x$d, sei = x$se, weights=x$weight, method="REML")
drug.fit = rma(yi=x2$d, sei = x2$se, weights=x2$weight, method="REML")
drug.fit
sub = x
drug = x2
x = rbind(sub, drug)
fit = rma(yi=x$d, sei = x$se, weights=x$weight, method="REML")
#View(x)


###
# LABELS
(author = x$study)
#l1 = x$gender
l1 = prettyNum(x$n)
l2 = x$meas
l3 = x$pop

# GRAPHICAL PARAMETERS
vote.color <- "white"
sub.color  <- "grey80"
#sub.color  <- "white"
tot.color  <- "black"
x$psize = .40 * log(1/x$se) ## Point Size
x$psize = round(x$psize, 2)
x$psize[x$psize < 1] = .9

size = .63
llim = -4 ## Left Side
rlim = 2.5  ## Right side
l1.pos = llim + 1.6 ## Label 1 position
l2.pos = llim + 2.2
l3.pos = llim + 3

lab = cbind(l1, l2)
lpos = c(l1.pos, l2.pos)



## ROWS 
gap.above = 3.8
gap.between = 2.2
gap.below = 0
sub.n = nrow(sub)
drug.n = nrow(drug)

top <- gap.above + gap.between + gap.below + sub.n + drug.n
rows.1 = seq(top-gap.above, top - gap.above-sub.n + 1, by=-1)
rows.2 = seq(top-gap.above - gap.between - sub.n, top - gap.above-gap.between-sub.n - drug.n + 1, by=-1)
(rows = c(rows.1, rows.2))

## ROWS 
# gap.above = 2.8
# gap.below = 0
# .n = nrow(x)
# 
# (top = gap.above + gap.below + .n)  # Upper limit
# (rows = seq(top-gap.above, top-gap.above-.n+1, by=-1)) # Sequence of rows

####
hh <- top-1.2 ## Header height
efac = 2.5

row.sub = min(rows.1)-1
row.drug = min(rows.2)-1
row.overall = min(rows)-2.4


png("Forest2.png", width=1050, height=850, res=150)
op <- par(mar=c(3,1,1,1), oma=c(0,0,0,0))
forest(x$d, sei=x$se, slab=author, cex = size, at=seq(-.5,1.5,.5), xlab="", 
       xlim=c(llim, rlim), ylim=c(-1, top), #pch=16,
       ilab=lab, ilab.xpos=lpos, rows=rows, ilab.pos=rep(4, length(lpos)), 
       psize=x$psize, efac=efac)

#addpoly(fit$b,ci.lb=fit$ci.lb, ci.ub=fit$ci.ub, col=vote.color, cex=size, row=row.overall, mlab="", efac=2, )
addpoly(sub.fit$b, sei=sub.fit$se, col=sub.color, cex=size, row=row.sub, mlab="", efac=efac)
text(llim + .1, row.sub, "Weighted Mean Estimate for Combined Alcohol and Illicit Drugs", font=3, adj=0, cex=size)
addpoly(drug.fit$b, sei=drug.fit$se, col=sub.color, cex=size, row=row.drug, mlab="", efac=efac-.2)
text(llim + .1, row.drug, "Weighted Mean Estimate for Illicit Drugs", font=3, adj=0, cex=size)
addpoly(fit$b, sei=fit$se, col=tot.color, cex=size, row=row.overall, mlab="", efac=efac)
text(llim + .1, row.overall, "Combined Mean Weighted Estimate", font=2, adj=0, cex=size)


#, mlab="Overall Random-effects Estimate"
text(rlim-.1, hh, "Weighted Standardized Mean Difference [95% CI]",cex=size, font=2, adj=1)
text(llim + .1, hh, "Author(s) and Year",cex=size, font=2, adj=0)
text(l1.pos + .1, hh, "k",cex=size, font=2, adj=0)
text(l2.pos + .15, hh, "Measure",cex=size, font=4, adj=0)
#text(l3.pos + .1, hh, "Measure",cex=size, font=2, adj=0)
#text(l4.pos + .15, hh, "Population",cex=size, font=2, adj=0)
#text(llim + .1, -1.7, "IPV = intimate partner violence",cex=size, font=1, adj=0)

text(llim, max(rows.1) + 1, "Combined Alcohol/Illicit Drugs", font=4, adj=0, cex=size)
text(llim, max(rows.2) + 1, "Illicit Drugs", font=4, adj=0, cex=size)
#abline(h=min(rows.3)-9)
abline(h=row.overall+.7)
key <- "Note. IPV = intimate partner violence.\n small effect: < 0.35, medium effect: 0.36-0.65, large effect: > 0.65"
mtext(text=key, side=1, cex=size, adj=.02, padj=1, font=3)
dev.off()
