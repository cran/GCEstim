## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
library(GCEstim)
load("GCEstim_Choosing_Supports.RData")

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef.dataGCE <- c(1, 0, 0, 3, 6, 9)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.rt.01 <- 
  ridgetrace(
    formula = y ~ X001 + X002 + X003 + X004 + X005,
    data = dataGCE)

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
plot(res.rt.01, coef = coef.dataGCE)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.rt.01

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef(res.rt.01, which = "max.abs")

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef(res.rt.01, which = "max.abs") > abs(c(1, 0, 0, 3, 6, 9))

## ----echo=TRUE,eval=TRUE------------------------------------------------------

(RidGME.support <- 
  matrix(c(-coef(res.rt.01, which = "max.abs"),
           coef(res.rt.01, which = "max.abs")),
         ncol = 2,
         byrow = FALSE))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.RidGME <-
  GCEstim::lmgce(
    y ~ .,
    data = dataGCE,
    support.signal = RidGME.support,
    twosteps.n = 0
  )

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.RidGME <-
#   GCEstim::lmgce(
#     y ~ .,
#     data = dataGCE,
#     support.method = "ridge",
#     support.signal = 1,
#     twosteps.n = 0
#   )

## ----echo=FALSE,eval=TRUE,results = 'asis'------------------------------------

kableExtra::kable(
  cbind(res.all[, -c(5,6)],
        c(round(GCEstim::accmeasure(fitted(res.lmgce.RidGME), dataGCE$y, which = "RMSE"), 3),
          round(res.lmgce.RidGME$error.measure.cv.mean, 3),
          round(GCEstim::accmeasure(coef(res.lmgce.RidGME), coef.dataGCE, which = "RMSE"), 3))),
  digits = 3,
  align = c(rep('c', times = 5)),
  col.names = c("$OLS$",
                "$GME_{(100000)}$",
                "$GME_{(100)}$",
                "$GME_{(50)}$",
                "$GME_{(RidGME)}$"),
  row.names = TRUE,
  booktabs = FALSE)


## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.rt.02 <- 
  ridgetrace(
    formula = y ~ .,
    data = dataincRidGME)

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
coef.dataincRidGME <- c(2.5, rep(0, 3), c(-8, 19, -13))
plot(res.rt.02, coef = coef.dataincRidGME)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef(res.rt.02, which = "max.abs") > abs(coef.dataincRidGME)

## ----echo=TRUE,eval=TRUE------------------------------------------------------

res.lmgce.RidGME.02.alpha1 <-
  GCEstim::lmgce(
    y ~ .,
    data = dataincRidGME,
    support.method = "ridge",
    support.signal = 1,
    twosteps.n = 0
  )

res.lmgce.RidGME.02.alpha2 <-
  GCEstim::lmgce(
    y ~ .,
    data = dataincRidGME,
    support.method = "ridge",
    support.signal = 2,
    twosteps.n = 0
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
summary(res.lmgce.RidGME.02.alpha1)$error.measure
summary(res.lmgce.RidGME.02.alpha2)$error.measure

## ----echo=TRUE,eval=TRUE------------------------------------------------------
summary(res.lmgce.RidGME.02.alpha1)$error.measure.cv.mean
summary(res.lmgce.RidGME.02.alpha2)$error.measure.cv.mean

## -----------------------------------------------------------------------------
round(GCEstim::accmeasure(coef(res.lmgce.RidGME.02.alpha1), coef.dataincRidGME, which = "RMSE"), 3) 
round(GCEstim::accmeasure(coef(res.lmgce.RidGME.02.alpha2), coef.dataincRidGME, which = "RMSE"), 3) 

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.RidGME.02 <-
  GCEstim::lmgce(
    y ~ .,
    data = dataincRidGME,
    support.method = "ridge",
    twosteps.n = 0
  )

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
plot(res.lmgce.RidGME.02, which = 2, NormEnt = FALSE)[[1]]

## ----echo=TRUE,eval=TRUE------------------------------------------------------
summary(res.lmgce.RidGME.02)

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
plot(res.lmgce.RidGME.02, which = 5, NormEnt = FALSE, coef = coef.dataincRidGME)[[1]]

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.RidGME.02.min <-
#   GCEstim::lmgce(
#     y ~ .,
#     data = dataincRidGME,
#     support.method = "ridge",
#     errormeasure.which = "min",
#     twosteps.n = 0
#   )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.RidGME.02.min <- 
  changesupport(res.lmgce.RidGME.02, "min")

## ----echo=TRUE,eval=TRUE------------------------------------------------------
summary(res.lmgce.RidGME.02.min)

## -----------------------------------------------------------------------------
round(GCEstim::accmeasure(coef(res.lmgce.RidGME.02), coef.dataincRidGME, which = "RMSE"), 3) 

round(GCEstim::accmeasure(coef(res.lmgce.RidGME.02.min), coef.dataincRidGME, which = "RMSE"), 3) 

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.RidGME.01.1se <-
  GCEstim::lmgce(
    y ~ .,
    data = dataGCE,
    support.method = "ridge",
    twosteps.n = 0
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.RidGME.01.min <- changesupport(res.lmgce.RidGME.01.1se, "min")

## ----echo=FALSE,eval=TRUE,results = 'asis'------------------------------------

kableExtra::kable(
  cbind(res.all[, -c(5,6)],
        c(round(GCEstim::accmeasure(fitted(res.lmgce.RidGME), dataGCE$y, which = "RMSE"), 3),
          round(res.lmgce.RidGME$error.measure.cv.mean, 3),
          round(GCEstim::accmeasure(coef(res.lmgce.RidGME), coef.dataGCE, which = "RMSE"), 3)),
        c(round(GCEstim::accmeasure(fitted(res.lmgce.RidGME.01.1se), dataGCE$y, which = "RMSE"), 3),
          round(res.lmgce.RidGME.01.1se$error.measure.cv.mean, 3),
          round(GCEstim::accmeasure(coef(res.lmgce.RidGME.01.1se), coef.dataGCE, which = "RMSE"), 3)),
        c(round(GCEstim::accmeasure(fitted(res.lmgce.RidGME.01.min), dataGCE$y, which = "RMSE"), 3),
          round(res.lmgce.RidGME.01.min$error.measure.cv.mean, 3),
          round(GCEstim::accmeasure(coef(res.lmgce.RidGME.01.min), coef.dataGCE, which = "RMSE"), 3))),
  digits = 3,
  align = c(rep('c', times = 5)),
  col.names = c("$OLS$",
                "$GME_{(100000)}$",
                "$GME_{(100)}$",
                "$GME_{(50)}$",
                "$GME_{(RidGME)}$",
                "$GME_{(incRidGME_{1se})}$",
                "$GME_{(incRidGME_{min})}$"),
  row.names = TRUE,
  booktabs = FALSE)


## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.1se <-
  GCEstim::lmgce(
    y ~ .,
    data = dataGCE,
    twosteps.n = 0
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.min <- changesupport(res.lmgce.1se, "min")

## ----echo=TRUE,eval=TRUE------------------------------------------------------
summary(res.lmgce.1se)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
summary(res.lmgce.min)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
all.data.2 <- cbind(
  res.all[, -c(3, 4, 5, 6)],
  c(
    round(GCEstim::accmeasure(
      fitted(res.lmgce.RidGME), dataGCE$y, which = "RMSE"
    ), 3),
    round(res.lmgce.RidGME$error.measure.cv.mean, 3),
    round(GCEstim::accmeasure(
      coef(res.lmgce.RidGME), coef.dataGCE, which = "RMSE"
    ), 3)
  ),
  c(
    round(GCEstim::accmeasure(
      fitted(res.lmgce.RidGME.01.1se), dataGCE$y, which = "RMSE"
    ), 3),
    round(res.lmgce.RidGME.01.1se$error.measure.cv.mean, 3),
    round(GCEstim::accmeasure(
      coef(res.lmgce.RidGME.01.1se), coef.dataGCE, which = "RMSE"
    ), 3)
  ),
  c(
    round(GCEstim::accmeasure(
      fitted(res.lmgce.RidGME.01.min), dataGCE$y, which = "RMSE"
    ), 3),
    round(res.lmgce.RidGME.01.min$error.measure.cv.mean, 3),
    round(GCEstim::accmeasure(
      coef(res.lmgce.RidGME.01.min), coef.dataGCE, which = "RMSE"
    ), 3)
  ),
  c(
    round(GCEstim::accmeasure(fitted(res.lmgce.1se), dataGCE$y, which = "RMSE"), 3),
    round(res.lmgce.1se$error.measure.cv.mean, 3),
    round(GCEstim::accmeasure(
      coef(res.lmgce.1se), coef.dataGCE, which = "RMSE"
    ), 3)
  ),
  c(
    round(GCEstim::accmeasure(fitted(res.lmgce.min), dataGCE$y, which = "RMSE"), 3),
    round(res.lmgce.min$error.measure.cv.mean, 3),
    round(GCEstim::accmeasure(
      coef(res.lmgce.min), coef.dataGCE, which = "RMSE"
    ), 3)
  )
)[, -1]

## ----echo=FALSE,eval=TRUE,results = 'asis'------------------------------------
kableExtra::kable(
  all.data.2,
  digits = 3,
  align = c(rep('c', times = 5)),
  col.names = c("$OLS$",
                "$GME_{(RidGME)}$",
                "$GME_{(incRidGME_{1se})}$",
                "$GME_{(incRidGME_{min})}$",
                "$GME_{(std_{1se})}$",
                "$GME_{(std_{min})}$"),
  row.names = TRUE,
  booktabs = FALSE)


