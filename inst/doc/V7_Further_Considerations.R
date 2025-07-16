## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
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
res.lmgce.100.GCE <-
  GCEstim::lmgce(
    y ~ .,
    data = dataGCE,
    cv = TRUE,
    cv.nfolds = 5,
    support.signal = c(-100, 100),
    support.signal.points =
      matrix(
        c(
          rep(1 / 5, 5),
          c(0.1, 0.1, 0.6, 0.1, 0.1),
          c(0.1, 0.1, 0.6, 0.1, 0.1),
          rep(1 / 5, 5),
          rep(1 / 5, 5),
          rep(1 / 5, 5)
        ),
        ncol = 5,
        byrow = TRUE
      ),
    twosteps.n = 0,
    seed = 230676
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
NormEnt(res.lmgce.100.GME)
NormEnt(res.lmgce.100.GCE)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
NormEnt(res.lmgce.100.GME, model = FALSE)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
NormEnt(res.lmgce.100.GCE, model = FALSE)

