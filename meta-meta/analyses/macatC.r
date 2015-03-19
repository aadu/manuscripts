macatC = function (x1, x2, g, var, mod, data, method = "random", type = "post.hoc") 
{
  call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  args <- match(c("x1", "x.2", "g", "var", "mod", "data"), 
                names(mf), 0)
  mf <- mf[c(1, args)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  mf.g <- mf[[match("g", names(mf))]]
  g <- eval(mf.g, data, enclos = sys.frame(sys.parent()))
  mf.var <- mf[[match("var", names(mf))]]
  var.g <- eval(mf.var, data, enclos = sys.frame(sys.parent()))
  mf.mod <- mf[[match("mod", names(mf))]]
  mod <- eval(mf.mod, data, enclos = sys.frame(sys.parent()))
  modsig <- macat(g = g, var = var.g, mod = mod, data = data, 
                  method = method)
  modsig$mod <- as.factor(modsig$Model$mod)
  com1 <- levels(modsig$mod)[x1]
  com2 <- levels(modsig$mod)[x2]
  x1.es <- modsig$Model[modsig$mod == com1, "estimate"]
  x2.es <- modsig$Model[modsig$mod == com2, "estimate"]
  x1.var <- modsig$Model[modsig$mod == com1, "var"]
  x2.var <- modsig$Model[modsig$mod == com2, "var"]
  g <- (-1) * x1.es + 1 * x2.es
  var <- (-1)^2 * x1.var + (1)^2 * x2.var
  df <- 2 - 1
  chi.sqr <- g^2/var
  if (type == "post.hoc") {
    z <- g/sqrt(var)
    z2 <- z^2
    levels <- length(levels(as.factor(mod)))
    df.post <- levels - 1
    p.value <- 1 - pchisq(z2, df.post)
    L.95ci <- g - 1.96 * sqrt(var)
    U.95ci <- g + 1.96 * sqrt(var)
    fit <- data.frame(g, var, p.value, L.95ci, U.95ci)
    names(fit) <- c("diff", "var.diff", "p", "lower", "upper")
  }
  if (type == "planned") {
    df <- 2 - 1
    chi.sqr <- g^2/var
    p.value <- 1 - pchisq(chi.sqr, df)
    L.95ci <- g - 1.96 * sqrt(var)
    U.95ci <- g + 1.96 * sqrt(var)
    fit <- data.frame(g, var, p.value, L.95ci, U.95ci)
    names(fit) <- c("diff", "var.diff", "p", "lower", "upper")
  }
  return(fit)
}