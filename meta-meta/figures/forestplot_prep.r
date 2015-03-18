source('./functions.r')
df = readRDS("./data/df.RDS")
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
x$male = 0
x$male[x$gender == "men"] = 1
x$female = 0
x$female[x$gender == "women"] = 1
x$gen = factor(x$gender)
saveRDS(x, "./data/x.RDS")


x = readRDS("./data/x.RDS")
View(over)