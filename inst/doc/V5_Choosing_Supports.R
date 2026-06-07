## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,eval=TRUE,message=FALSE---------------------------------------
library(GCEstim)
load("GCEstim_Choosing_Supports.RData")

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.rt.01 <- 
  ridgetrace(
    formula = y ~ X001 + X002 + X003 + X004,
    data = dataThesis,
    lambda.min = 10^-3, # default
    lambda.max = 10^3, # default
    lambda.n = 100 # default
  )

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
plot(res.rt.01, coef = coef.dataThesis)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.rt.01

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef(res.rt.01, which = "max.abs")

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef(res.rt.01, which = "max.abs") > abs(coef.dataThesis)

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
    data = dataThesis,
    support.signal = RidGME.support,
    twosteps.n = 0
  )
coef(res.lmgce.RidGME)

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.RidGME <-
#   GCEstim::lmgce(
#     y ~ .,
#     data = dataThesis,
#     support.method = "ridge",
#     support.method.ridge.symm = TRUE, # default
#     support.method.ridge.maxresid = FALSE,
#     support.signal = 1,
#     twosteps.n = 0
#   )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef(res.lmgce.RidGME)

## ----echo=FALSE,eval=TRUE,results = 'asis'------------------------------------

kableExtra::kable(
  cbind(res.all[, -c(5,6)],
        c(round(GCEstim::accmeasure(fitted(res.lmgce.RidGME), dataThesis$y, which = "RMSE"), 3),
          round(res.lmgce.RidGME$error.measure.cv.mean, 3),
          round(GCEstim::accmeasure(coef(res.lmgce.RidGME), coef.dataThesis, which = "RMSE"), 3))),
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
(coef.dataincRidGME <- c(2.5, rep(0, 3), c(-8, 19, -13)))

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
plot(res.rt.02, coef = coef.dataincRidGME)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef(res.rt.02, which = "max.abs") > abs(coef.dataincRidGME)

## ----echo=TRUE,eval=TRUE------------------------------------------------------

res.lmgce.RidGME.02.alpha1 <-
  GCEstim::lmgce(
    y ~ .,
    data = dataincRidGME,
    support.method = "ridge",
    support.method.ridge.symm = TRUE, # default
    support.method.ridge.maxresid = FALSE,
    support.signal = 1,
    twosteps.n = 0
  )

res.lmgce.RidGME.02.alpha2 <-
  GCEstim::lmgce(
    y ~ .,
    data = dataincRidGME,
    support.method = "ridge",
    support.method.ridge.symm = TRUE, # default
    support.method.ridge.maxresid = FALSE,
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

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.RidGME.02 <-
#   GCEstim::lmgce(
#     y ~ .,
#     data = dataincRidGME,
#     support.method = "ridge",
#     twosteps.n = 0
#   )

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
plot(res.lmgce.RidGME.02, which = 2, NormEnt = FALSE)$p2

## ----echo=TRUE,eval=TRUE------------------------------------------------------
summary(res.lmgce.RidGME.02)

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
summary(res.lmgce.RidGME.02.min)

## -----------------------------------------------------------------------------
round(GCEstim::accmeasure(coef(res.lmgce.RidGME.02), coef.dataincRidGME, which = "RMSE"), 3) 

round(GCEstim::accmeasure(coef(res.lmgce.RidGME.02.min), coef.dataincRidGME, which = "RMSE"), 3) 

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.RidGME.01.1se <-
#   GCEstim::lmgce(
#     y ~ .,
#     data = dataThesis,
#     support.method = "ridge",
#     twosteps.n = 0
#   )

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.RidGME.01.min <-
#   GCEstim::lmgce(
#     y ~ .,
#     data = dataThesis,
#     support.method = "ridge",
#     errormeasure.which = "min",
#     twosteps.n = 0
#   )

## ----echo=FALSE,eval=FALSE----------------------------------------------------
# res.all.1 <-
#   cbind(res.all[, -c(5,6)],
#         c(round(GCEstim::accmeasure(fitted(res.lmgce.RidGME), dataThesis$y, which = "RMSE"), 3),
#           round(res.lmgce.RidGME$error.measure.cv.mean, 3),
#           round(GCEstim::accmeasure(coef(res.lmgce.RidGME), coef.dataThesis, which = "RMSE"), 3)),
#         c(round(GCEstim::accmeasure(fitted(res.lmgce.RidGME.01.1se), dataThesis$y, which = "RMSE"), 3),
#           round(res.lmgce.RidGME.01.1se$error.measure.cv.mean, 3),
#           round(GCEstim::accmeasure(coef(res.lmgce.RidGME.01.1se), coef.dataThesis, which = "RMSE"), 3)),
#         c(round(GCEstim::accmeasure(fitted(res.lmgce.RidGME.01.min), dataThesis$y, which = "RMSE"), 3),
#           round(res.lmgce.RidGME.01.min$error.measure.cv.mean, 3),
#           round(GCEstim::accmeasure(coef(res.lmgce.RidGME.01.min), coef.dataThesis, which = "RMSE"), 3)))

## ----echo=FALSE,eval=TRUE,results = 'asis'------------------------------------

kableExtra::kable(
  res.all.1,
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


## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.1se <-
#   GCEstim::lmgce(
#     y ~ .,
#     data = dataThesis,
#     support.method = "standardize", # default
#     errormeasure.which = "1se", # default
#     twosteps.n = 0
#   )

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.min <- changesupport(res.lmgce.1se, "min")

## ----echo=FALSE,eval=FALSE----------------------------------------------------
# res.lmgce.1se.summary <- summary(res.lmgce.1se)
# res.lmgce.min.summary <- summary(res.lmgce.min)

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# summary(res.lmgce.1se)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
res.lmgce.1se.summary

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# summary(res.lmgce.min)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
res.lmgce.min.summary

## ----echo=FALSE,eval=FALSE----------------------------------------------------
# all.data.2 <- cbind(
#   res.all[, -c(3, 4, 5, 6)],
#   c(
#     round(GCEstim::accmeasure(
#       fitted(res.lmgce.RidGME), dataThesis$y, which = "RMSE"
#     ), 3),
#     round(res.lmgce.RidGME$error.measure.cv.mean, 3),
#     round(GCEstim::accmeasure(
#       coef(res.lmgce.RidGME), coef.dataThesis, which = "RMSE"
#     ), 3)
#   ),
#   c(
#     round(GCEstim::accmeasure(
#       fitted(res.lmgce.RidGME.01.1se), dataThesis$y, which = "RMSE"
#     ), 3),
#     round(res.lmgce.RidGME.01.1se$error.measure.cv.mean, 3),
#     round(GCEstim::accmeasure(
#       coef(res.lmgce.RidGME.01.1se), coef.dataThesis, which = "RMSE"
#     ), 3)
#   ),
#   c(
#     round(GCEstim::accmeasure(
#       fitted(res.lmgce.RidGME.01.min), dataThesis$y, which = "RMSE"
#     ), 3),
#     round(res.lmgce.RidGME.01.min$error.measure.cv.mean, 3),
#     round(GCEstim::accmeasure(
#       coef(res.lmgce.RidGME.01.min), coef.dataThesis, which = "RMSE"
#     ), 3)
#   ),
#   c(
#     round(GCEstim::accmeasure(fitted(res.lmgce.1se), dataThesis$y, which = "RMSE"), 3),
#     round(res.lmgce.1se$error.measure.cv.mean, 3),
#     round(GCEstim::accmeasure(
#       coef(res.lmgce.1se), coef.dataThesis, which = "RMSE"
#     ), 3)
#   ),
#   c(
#     round(GCEstim::accmeasure(fitted(res.lmgce.min), dataThesis$y, which = "RMSE"), 3),
#     round(res.lmgce.min$error.measure.cv.mean, 3),
#     round(GCEstim::accmeasure(
#       coef(res.lmgce.min), coef.dataThesis, which = "RMSE"
#     ), 3)
#   )
# )[, -1]

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


