## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
load("GCEstim_Optim.RData")

## ----echo=TRUE,eval=TRUE,message=FALSE----------------------------------------
library(GCEstim)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.50.primal.solnp <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    support.signal = c(-50, 50),
    twosteps.n = 0,
    method = "primal.solnp"
  )

res.lmgce.50.primal.solnl <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    support.signal = c(-50, 50),
    twosteps.n = 0,
    method = "primal.solnl"
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.50.primal.solnp$convergence
res.lmgce.50.primal.solnl$convergence

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.50.dual.BFGS <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    support.signal = c(-50, 50),
    twosteps.n = 0,
    method = "dual.BFGS"
  )

res.lmgce.50.dual.CG <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    support.signal = c(-50, 50),
    twosteps.n = 0,
    method = "dual.CG"
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.50.dual.BFGS$convergence
res.lmgce.50.dual.CG$convergence

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.50.dual.BFGS$lambda

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.50.dual.BFGS$p

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.50.dual.BFGS$w

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# method.opt <-
#   c(
#     "primal.solnl",
#     "primal.solnp",
#     "dual.BFGS",
#     "dual.CG",
#     "dual.L-BFGS-B",
#     "dual.Rcgmin",
#     "dual.bobyqa",
#     "dual.newuoa",
#     "dual.nlminb",
#     "dual.nlm",
#     "dual.lbfgs",
#     "dual.lbfgsb3c"
#   )
# 
# compare_methods <- data.frame(
#   method = method.opt,
#   time = NA,
#   r.squared = NA,
#   error.measure = NA,
#   error.measure.cv.mean = NA,
#   beta.error = NA,
#   nep = NA,
#   nep.cv.mean = NA,
#   convergence = NA)

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# 
# for (i in 1:length(method.opt)) {
# start.time <- Sys.time()
# res.method <-
#   lmgce(
#     y ~ .,
#     data = dataThesis,
#     support.signal = c(-50,50),
#     twosteps.n = 0,
#     method = method.opt[i]
#     )
# 
# compare_methods$time[i] <- difftime(Sys.time(),
#                                     start.time,
#                                     units = "secs")
# compare_methods$r.squared[i] <- summary(res.method)$r.squared
# compare_methods$error.measure[i] <- res.method$error.measure
# compare_methods$error.measure.cv.mean[i] <- res.method$error.measure.cv.mean
# compare_methods$beta.error[i] <- accmeasure(coef(res.method), coef.dataThesis)
# compare_methods$nep[i] <- res.method$nep
# compare_methods$nep.cv.mean [i] <- res.method$nep.cv.mean
# compare_methods$convergence[i] <- res.method$convergence
# }
# 
# compare_methods_ordered <- compare_methods[order(compare_methods$time),]
# 
# compare_methods_ordered$convergence <- factor(compare_methods_ordered$convergence,
#                                               levels = c(0,1),
#                                               labels = c(TRUE, FALSE))

## ----echo=FALSE,eval=TRUE,results = 'asis'------------------------------------
kableExtra::kable(
  compare_methods_ordered[, c(1,2,4,5,6,9)],
  digits = 3,
  align = "rcccc",
  col.names = c("optimization method",
                "time (s)",
                "$RMSE_{\\widehat{y}}$",
                "$CV \\text{-} RMSE_{\\widehat{y}}$",
                "$RMSE_{\\widehat{\\beta}}$",
                "Convergence"),
  row.names = FALSE,
  booktabs = F)

