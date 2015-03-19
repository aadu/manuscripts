x = readRDS("./data/xall.RDS")
library(metafor)
rma(yi=d, vi=var, data=x, method="FE")
rma(yi=d, vi=var, data=x, method="FE")

x$role = factor(x$role)
xp = subset(x, role=="perpetration")

macat(g=d, var=var, mod=role, data=x)
macat(g=d, var=var, mod=substance, data=xp)
macatC(1, 2, g=d, var=var, mod=substance, data=xp)
macatC(1, 3, g=d, var=var, mod=substance, data=xp)
macatC(2, 3, g=d, var=var, mod=substance, data=xp)
xp$role

macatC(1, 2, g=d, var=var, mod=role, data=x)
macatC(1, 2, g=d, var=var, mod=factor(role), data=x)

xalc = subset(x, substance == "alcohol")
macatC(1, 2, g=d, var=var, mod=role, data=xalc)