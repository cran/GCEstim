## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
library(GCEstim)
load("GCEstim_Two_Steps.RData")

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef.dataGCE <- c(1, 0, 0, 3, 6, 9)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.1se.twosteps <-
  GCEstim::lmgce(
    y ~ .,
    data = dataGCE,
    twosteps.n = 10
  )

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4,fig.align='center'---------
plot(res.lmgce.1se.twosteps, which = 6)[[1]]

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4,fig.align='center'---------
plot(res.lmgce.1se.twosteps, which = 7, coef = coef.dataGCE)[[1]]

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.1se.twosteps.1 <-
#   GCEstim::lmgce(
#     y ~ .,
#     data = dataGCE
#   )

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.1se.twosteps.1 <- update(res.lmgce.1se.twosteps, twosteps.n = 1)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.1se.twosteps.1 <- changestep(res.lmgce.1se.twosteps, 1)

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4,fig.align='center'---------
plot(res.lmgce.1se.twosteps.1, which = 2)[[1]]

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4,fig.align='center'---------
plot(res.lmgce.1se.twosteps.1, which = 3)[[1]]

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.1se.twosteps.1$p0

## ----echo=TRUE,eval=TRUE, fig.width=6,fig.height=4----------------------------
res.lmgce.1se.twosteps.1$p

## ----echo=FALSE,eval=TRUE,results = 'asis'------------------------------------
kableExtra::kable(
  cbind(all.data.2,
        c(
    round(GCEstim::accmeasure(
      fitted(res.lmgce.1se.twosteps.1), dataGCE$y, which = "RMSE"
    ), 3),
    round(res.lmgce.1se.twosteps.1$error.measure.cv.mean, 3),
    round(GCEstim::accmeasure(
      coef(res.lmgce.1se.twosteps.1), coef.dataGCE, which = "RMSE"
    ), 3)
  )),
  digits = 3,
  align = c(rep('c', times = 5)),
  col.names = c("$OLS$",
                "$GME_{(RidGME)}$",
                "$GME_{(incRidGME_{1se})}$",
                "$GME_{(incRidGME_{min})}$",
                "$GME_{(std_{1se})}$",
                "$GME_{(std_{min})}$",
                "$GCE_{(std_{1se})}$"),
  row.names = TRUE,
  booktabs = FALSE)


