## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
coef.dataGCE <- c(1, 0, 0, 3, 6, 9)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
library(GCEstim)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.100.GME <-
  GCEstim::lmgce(
    y ~ .,
    data = dataGCE,
    cv = TRUE,
    cv.nfolds = 5,
    support.signal = c(-100, 100),
    support.signal.points = 5,
    twosteps.n = 0,
    seed = 230676
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(coef.res.lmgce.100.GME <- coef(res.lmgce.100.GME))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(support.signal.points.matrix <- 
  matrix(
    c(rep(1/5, 5),
      c(0.1, 0.1, 0.6, 0.1, 0.1),
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
    data = dataGCE,
    cv = TRUE,
    cv.nfolds = 5,
    support.signal = c(-100, 100),
    support.signal.points = support.signal.points.matrix,
    twosteps.n = 0,
    seed = 230676
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(coef.res.lmgce.100.GCE <- coef(res.lmgce.100.GCE))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(RMSE_beta.lmgce.100.GME <-
   GCEstim::accmeasure(coef.res.lmgce.100.GME, coef.dataGCE, which = "RMSE"))

(RMSE_beta.lmgce.100.GCE <-
    GCEstim::accmeasure(coef.res.lmgce.100.GCE, coef.dataGCE, which = "RMSE"))

