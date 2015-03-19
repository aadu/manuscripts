options(scipen=15)
library(splines)
library(MASS)
library(xtable)
library(car)
library(ggplot2)


# Model
fit = lm(dast ~ avoid.c + neg.affect.c, data = df)
mod = update(fit, . ~ . + I(avoid.c^2))
mod2 = update(fit, . ~ . + I(avoid.c^2) + I(neg.affect.c^2))
anova(fit, mod)
anova(mod, mod2)
summary(mod)

# Table
myXtable <- xtable(summary(mod))
print(myXtable, type="html")
mod = lm(dast.c ~ neg.affect.c + poly(avoid.c, 2), data=df)
myXtable = xtable(summary(mod))
print(myXtable, type='html')
df$avoidcentered = as.vector(scale(df$avoid, center=T, scale=F))
df$neg.affect.cent = as.vector(scale(df$neg.affect, T, F))
mod = lm(dast ~ neg.affect.cent + poly(avoidcentered, 2), data=df)
summary(mod)
myXtable = xtable(summary(mod))
print(myXtable, type='html')
confint(mod)
mod = lm(dast ~ neg.affect.cent + avoidcentered + I(avoidcentered^2), data=df)
vif(mod)

# Plot data
df$avoid.c = as.vector(df$avoid.c)
df$size = as.vector(df$neg.affect.c + min(df$neg.affect.c) + 1)
df$size = 2
df$size[df$neg.affect.c >=1] = 3
df$size[df$neg.affect.c <=-1] = 1
df$size = factor(df$size, levels=c("1", "2", "3"))
df$size = as.integer(df$size)
model = lm(dast ~ neg.affect.c + poly(avoid.c, 2), data = df)
summary(model)

# Base plot
qplot(avoid.c,dast, data=df, geom="jitter",
      position=position_jitter(width=.3, height=.3),
      size=size, alpha=.3) + scale_size(range=c(1.3, 5.2)) +
  geom_smooth(method="lm", se=F, formula=y ~ poly(x, 2), size=1, fullrange=T,
              color="black") + ylab("Drug Use Problems") +
  xlab("Avoidance Coping (z-score)") +
  theme_bw() + theme(legend.position="none")

# Make a 6x6 inch image at 300dpi
ppi <- 300
png("plot.png", width=5.5 * ppi, height=6 * ppi, res=ppi)
print(qplot(avoid.c,dast, data=df, geom="jitter",
            position=position_jitter(width=.3, height=.3),
            size=size, alpha=.3) + scale_size(range=c(1.3, 5.2)) +
        stat_smooth(method="lm", se=F, formula=y ~ poly(x, 2),
                    size=0.8, fullrange=T, color="black", linetype="solid") +
        ylab("Drug Use Problems") + xlab("Avoidance Coping (z-score)") +
        theme_bw() + theme(legend.position="none"))
dev.off()

# Plot 2
rez = 300
tiff("plot2.tiff", width=5.5 * rez, height=6 * rez, res=rez)
print(qplot(avoid.c,dast, data=df, geom="jitter",
            position=position_jitter(width=.3, height=.3),
            size=size, alpha=.3) + scale_size(range=c(1.3, 5.2)) +
        stat_smooth(method="lm", se=F, formula=y ~ poly(x, 2),
                    size=0.8, fullrange=T, color="black", linetype="solid") +
        ylab("Drug Use Problems") + xlab("Avoidance Coping (z-score)") +
        theme_bw() + theme(legend.position="none") )
dev.off()
