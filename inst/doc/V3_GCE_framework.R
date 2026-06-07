## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE,eval=TRUE,message=FALSE----------------------------------------
library(GCEstim)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.100.GME <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    support.signal = c(-100, 100),
    support.signal.points = 5,
    twosteps.n = 0,
    method = "dual.BFGS" # default
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(coef.res.lmgce.100.GME <- coef(res.lmgce.100.GME))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(support.signal.points.matrix <- 
  matrix(
    c(rep(1/5, 5),
      c(0.1, 0.1, 0.6, 0.1, 0.1),
      rep(1/5, 5),
      rep(1/5, 5),
      rep(1/5, 5)
      ),
    ncol = 5,
    byrow = TRUE))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.100.GCE <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    support.signal = c(-100, 100),
    support.signal.points = support.signal.points.matrix,
    twosteps.n = 0,
    method = "dual.BFGS" # default
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(coef.res.lmgce.100.GCE <- coef(res.lmgce.100.GCE))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(RMSE_beta.lmgce.100.GME <-
   GCEstim::accmeasure(coef.res.lmgce.100.GME, coef.dataThesis, which = "RMSE"))

(RMSE_beta.lmgce.100.GCE <-
    GCEstim::accmeasure(coef.res.lmgce.100.GCE, coef.dataThesis, which = "RMSE"))

