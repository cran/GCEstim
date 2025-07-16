## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
library(GCEstim)
load("GCEstim_Quick_start.RData")

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef.dataGCE <- c(1, 0, 0, 3, 6, 9)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
summary(dataGCE)

## ----echo=FALSE,eval=TRUE, fig.width=6,fig.height=6,fig.align='center'--------
plot(dataGCE)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.v01 <-
  lmgce(
    formula = y ~ X001 + X002 + X003 + X004 + X005,
    data = dataGCE)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.v01

## ----echo=TRUE,eval=TRUE------------------------------------------------------
summary(res.lmgce.v01)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(coef.res.lmgce.v01 <- coef(res.lmgce.v01))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.v01$error.measure

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.v01$error.measure.cv.mean

## ----echo=TRUE,eval=TRUE------------------------------------------------------
confint(res.lmgce.v01, level = 0.95)

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
plot(res.lmgce.v01, which = 1)[[1]]

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.v01.confint <-
#   confint(
#     res.lmgce.v01,
#     level = 0.95,
#     method = "percentile",
#     boot.B = 1000,
#     boot.method = "residuals"
#   )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.v01.confint

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.v02 <- 
  update(res.lmgce.v01,
         boot.B = 1000,
         boot.method = "residuals")

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.v02.confint <-
  confint(
    res.lmgce.v02,
    level = 0.95,
    method = "percentile"
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.v02.confint

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4,fig.align='center'---------
plot(res.lmgce.v02, which = 1, ci.method = "percentile")[[1]]

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
res.lmgce.v01$support.stdUL #standardized

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
res.lmgce.v01$support.signal.1se

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
res.lmgce.v01$support.matrix #original scale

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4,fig.align='center'---------
plot(res.lmgce.v01, which = 2)[[1]]

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4,fig.align='center'---------
plot(res.lmgce.v01, which = 3)[[1]]

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
res.lmgce.v01$p0

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4,fig.align='center'---------
plot(res.lmgce.v01, which = 6)[[1]]

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
res.lmgce.v01$p

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# lmgce(y ~ X001 + X002 + X003 + X004 + X005,
#       data = dataGCE,
#       errormeasure.which = "min")
# #or
# update(res.lmgce.v01, errormeasure.which = "min)

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
res.lmgce.v01.min <- changesupport(res.lmgce.v01, "min")

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
summary(res.lmgce.v01.min)

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
plot(res.lmgce.v01.min)

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
data.frame("Supp_1se" = coef(res.lmgce.v01),
           "Supp_min" = coef(res.lmgce.v01.min),
           "OLS" = coef(res.lmgce.v01$results$OLS$res),
           "TRUE" = coef.dataGCE)

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
fitted(res.lmgce.v01)[1:5]

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
predict(res.lmgce.v01, dataGCE[1,])

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# lmgceAddin()

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.cv.lmgce <-
#   cv.lmgce(
#     y ~ X001 + X002 + X003 + X004 + X005,
#     data = dataGCE,
#     support.signal.points = c(3, 5, 7, 9, 11),
#     support.noise.points = c(3, 5, 7, 9, 11),
#     weight = c(0.1, 0.3, 0.5, 0.7, 0.9))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.cv.lmgce

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.cv.lmgce$results[order(res.cv.lmgce$results$error.measure.cv.mean),][1:10,-6] 

## ----echo=TRUE,eval=TRUE------------------------------------------------------
summary(res.cv.lmgce$best)

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=12,fig.align='center'---------
plot(res.cv.lmgce) 

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=6,fig.align='center'----------
plot(moz_ts)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.tsbootgce <- 
  tsbootgce(
    formula = CO2 ~ 1 + L(GDP, 1) + L(EPC, 1) + L(EU, 1),
    data = moz_ts
    )

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
plot(res.tsbootgce)[[1]]

## ----echo=TRUE,eval=TRUE,fig.width=8,fig.height=5,fig.align='center'----------
res.tsbootgce

## ----echo=TRUE,eval=TRUE,fig.width=8,fig.height=5,fig.align='center'----------
coef(res.tsbootgce)

## ----echo=TRUE,eval=TRUE,fig.width=8,fig.height=5,fig.align='center'----------
confint(res.tsbootgce)

## ----echo=TRUE,eval=TRUE,fig.width=8,fig.height=5,fig.align='center'----------
plot(res.tsbootgce,
     ci.levels = c(0.90, 0.95, 0.99),
     ci.method = c("hdr" #,"basic" #,"percentile"
       ))[[2]]

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.neagging.lmgce <- neagging(res.lmgce.v02)

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
plot(res.neagging.lmgce)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
which.min(res.neagging.lmgce$error)[[1]]

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
plot(res.neagging.lmgce, which = 2) 

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef(res.neagging.lmgce)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef(res.neagging.lmgce, which = ncol(res.neagging.lmgce$matrix))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
accmeasure(
  as.matrix(cbind(1,dataGCE.test[, - ncol(dataGCE.test)])) %*% as.matrix(coef(res.neagging.lmgce)),
  dataGCE.test$y)

accmeasure(
  predict(res.lmgce.v02, dataGCE.test),
  dataGCE.test$y)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.neagging.tsbootgce <- neagging(res.tsbootgce)

## ----echo=TRUE,eval=TRUE,fig.width=6,fig.height=4,fig.align='center'----------
plot(res.neagging.tsbootgce)

