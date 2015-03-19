source('./functions.r')
df = readRDS("./data/df.RDS")
df2 = readRDS("./data/df2.RDS")
df3 = readRDS("./data/df3.RDS")
df4 = readRDS("./data/dfvict.RDS")
over = readRDS("./data/over.RDS")
over = over[!is.na(over$bibtex),]
over.a = over
over.a$bibtex = paste(over.a$bibtex, "mixed", sep=".")
over.b = over
over.b$bibtex = paste(over.b$bibtex, "men", sep=".")
over.c = over
over.c$bibtex = paste(over.c$bibtex, "women", sep=".")
over = rbind(over.a, over.b, over.c)
over$gender = over$bibtex
over$gender = gsub("^.*[.]", "", over$gender)
#View(over)
x = merge(df, over, by.x="bibtex")
x = x[order(x$year),]
x = x[order(x$d),]
x$role = "perpetration"
x$substance = "alcohol"
# x$male = 0
# x$male[x$gender == "men"] = 1
# x$female = 0
# x$female[x$gender == "women"] = 1
# x$gen = factor(x$gender)
saveRDS(x, "./data/x.RDS")


x2 = merge(df2, over, by.x="bibtex")
x2 = x2[order(x2$year),]
x2 = x2[order(x2$d),]
x2$role = "perpetration"
x2$substance = "combined"


x3 = merge(df3, over, by.x="bibtex")
x3 = x3[order(x3$year),]
x3 = x3[order(x3$d),]
x3$role = "perpetration"
x3$substance = "drugs"

x4 = merge(df4, over, by.x="bibtex")
x4 = x4[order(x4$year),]
x4 = x4[order(x4$d),]
x4$role = "victim"
x4$substance = "alcohol"

xall = rbind(x, x2, x3, x4)

x$male = 0
x$male[x$gender == "men"] = 1
x$female = 0
x$female[x$gender == "women"] = 1
x$gen = factor(x$gender)
x$var = x$se * x$se
saveRDS(xall, "./data/xall.RDS")
saveRDS(x4, "./data/xvict.RDS")