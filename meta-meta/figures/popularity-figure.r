# Popularity Figure
library(ggplot2)
theme_minimal_cb_L <- function (base_size=12, base_family="", ...){
  modifyList(theme_minimal(base_size=base_size, base_family=base_family),
              list(axis.line=element_line(colour="black")))
}
x = import.google(paste0("https://docs.google.com/spreadsheet/pub?key=",
                  "0Ar2MWSi_hKI6dEt0RGNfalpkRUdPM3hqV2tlb",
                  "XRoVHc&single=true&gid=0&output=csv")
df = data.frame(year = x$Year, pubs = x$Citations)
df = df[df$year != 2014, ]
p = qplot(x=year, y=pubs, data=df, geom="blank") + geom_bar(stat="identity")
p = p + scale_y_continuous(breaks=seq(0, 1000, 100))
p = p+ scale_x_continuous(breaks=seq(1960, 2010, 5))
p = p + xlab('') + ylab("Articles Published Per Year")
p = p + theme_minimal() +theme(axis.line=element_line(colour="black"),
                               panel.grid.minor.x=element_blank(),
                               panel.grid.major.x=element_blank())
png("popularity.png", width=1050, height=750, res=150)
print(p)
dev.off()
