require(metafor, quietly=TRUE, warn.conflicts=FALSE)
require(MAc, quietly=TRUE, warn.conflicts=FALSE)
load(file="~/Dropbox/serotonin/data/alltogether.RData")
# df = df.measure
df = df.ss
macat(ztor=TRUE, var=z_v, es=z, mod=src, data=df)


df.hostility = subset(df, construct=="Hostility")
macat(ztor=TRUE, var=z_v, es=z, mod=src, data=df.hostility)


df = df.ss
df$subscale = gsub("irritability/assaultiveness", "assault + irritability", df$subscale)
df$subscale = gsub("assaultiveness", "assault", df$subscale)
df$construct[df$measure == "LHA"] <- "Aggression & Hostility"
df$construct[df$measure == "BGA"] <- "Aggression & Hostility"
df$construct[df$subscale == "assault"] <- "Aggression"
df$construct[df$subscale == "indirect"] <- "Anger"
df$construct[df$subscale == "resentment"] <- "Hostility"
df$construct[df$subscale == "verbal"] <- "Aggression"
df$construct[df$subscale == "negativism"] <- "Hostility"
df$construct[df$subscale == "suspiciousness"] <- "Hostility"
df$construct[df$subscale == "attitudinal"] <- "Hostility"
df$construct[df$subscale == "hostility"] <- "Hostility"
df$construct[df$subscale == "belligerence"] <- "Hostility"
df$construct[df$subscale == "irritability"] <- "Anger"
df$construct[df$subscale == "angry"] <- "Anger"
df$construct[df$subscale == "angry hostility"] <- "Anger & Hostility"
df$construct[df$subscale == "anger-hostility"] <- "Anger & Hostility"
df$construct[df$subscale == "assault + irritability"] <- "Anger & Aggression"
df$construct[df$subscale == "aggression against people"] <- "Aggression"
df$construct[df$subscale == "aggressive behavior"] <- "Aggression"
df$construct[df$subscale == "aggressive responding"] <- "Aggression"
df$construct[df$subscale == "aggressive responses"] <- "Aggression"
df$construct[df$subscale == "aggression"] <- "Aggression"
df$construct[df$subscale == "20s"] <- "Aggression"
df$construct[df$subscale == "arguing/fighting"] <- "Aggression & Hostility"
df$construct[df$subscale == "amicable-antagonistic"] <- "Hostility"
df$construct[df$subscale == "aspd"] <- "Aggression & Hostility"
df$construct[df$subscale == "inhibition of aggression"] <- "Aggression"
df$construct[df$subscale == "property"] <- "Aggression"
df$construct[df$subscale == "subjective anger"] <- "Anger"
df$construct[df$subscale == "overt anger"] <- "Anger"
df$construct[df$subscale == "guilt"] <- NA
df$construct[df$subscale == "indirect"] <- "Aggression"
df$construct[df$subscale == "mean intensity"] <- "Aggression"
df$construct[df$subscale == "records"] <- "Aggression"
df$construct[df$subscale == "motor"] <- "Aggression"
df$construct[df$subscale == "extrapunitive hostility"] <- "Aggression & Hostility"
df$construct[df$construct == "Psychopathy"] <- "Aggression & Hostility"
df$construct[df$construct == ""] <- NA

# View(cbind(df$measure, df$subscale, df$construct))

macat(ztor=TRUE, var=z_v, es=z, mod=construct, data=df)

new.df = agg(id=old.id, r=r, n=n, mod=construct, data=df)
new.df$z = r_to_z(new.df$r)
new.df$z_v = var_z(new.df$n)
new.df$construct = new.df$mod
macat(ztor=TRUE, var=z_v, es=z, mod=construct, data=new.df)

x = as.character(new.df$id[new.df$construct == "Hostility"])
i = df[df$old.id %in% x,]
macat(ztor=TRUE, var=z_v, es=z, mod=src, data=i)
i$old.id[i$src == "other"]
df$old.id %in% x
View(new.df)

# summary(factor(df$subscale))


df.bdhi = subset(df.ss, measure == "BDHI")



View(df)

View(df$subscale)
df.ss$construct