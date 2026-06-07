## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=TRUE,eval=TRUE, message = FALSE-------------------------------------
library(GCEstim)

## ----echo=FALSE,eval=TRUE, fig.width=5,fig.height=5,fig.align='center'--------

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr = usr))

  par(usr = c(usr[1:2], 0, 1.5))

  h <- hist(x, plot = FALSE)
  y <- h$counts / max(h$counts)

  rect(
    h$breaks[-length(h$breaks)], 0,
    h$breaks[-1], y,
    col = "lightgray",
    border = "white"
  )
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 1.2, ...) {
  usr <- par("usr")
  on.exit(par(usr = usr))

  par(usr = c(0, 1, 0, 1))

  r <- cor(x, y, use = "complete.obs")
  txt <- paste0(prefix, formatC(r, format = "f", digits = digits))

  text(0.5, 0.5, txt, cex = cex.cor)
}

pairs(
  dataThesis,
  upper.panel = panel.cor,
  diag.panel = panel.hist,
  lower.panel = panel.smooth
)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.100000 <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    support.signal = c(-100000, 100000),
    support.signal.points = 5, # default
    support.noise = NULL, # default
    support.noise.points = 3, # default
    twosteps.n = 0,
    method = "primal.solnp"
  )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(coef.res.lmgce.100000 <- coef(res.lmgce.100000))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lm <- lm(y ~ ., data = dataThesis)
(coef.res.lm <- coef(res.lm))

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.100000 <-
#   GCEstim::lmgce(
#     y ~ .,
#     data = dataThesis,
#     support.signal = c(-100000, 100000),
#     support.signal.points = 5, # default
#     support.noise = NULL, # default
#     support.noise.points = 3, # default
#     twosteps.n = 0,
#     method = "primal.solnp",
#     OLS = TRUE # default
#   )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
coef(res.lmgce.100000$results$OLS$res)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(RMSE_y.lmgce.100000 <- 
  GCEstim::accmeasure(fitted(res.lmgce.100000), dataThesis$y, which = "RMSE"))

# or
# res.lmgce.100000$error.measure

(RMSE_y.lm <-
    GCEstim::accmeasure(fitted(res.lm), dataThesis$y, which = "RMSE"))

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
# res.lmgce.100000 <-
#   GCEstim::lmgce(
#     y ~ .,
#     data = dataThesis,
#     cv = TRUE, # default
#     cv.nfolds = 5, # default
#     support.signal = c(-100000, 100000),
#     support.signal.points = 5, # default
#     support.noise = NULL, # default
#     support.noise.points = 3, # default
#     twosteps.n = 0,
#     method = "primal.solnp",
#     OLS = TRUE, # default
#     seed = 230676 # default
#   )

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(CV_RMSE_y.lmgce.100000 <- res.lmgce.100000$error.measure.cv.mean)
(CV_RMSE_y.lm <- mean(res.lmgce.100000$results$OLS$error))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
(RMSE_beta.lmgce.100000 <-
   GCEstim::accmeasure(coef.res.lmgce.100000, coef.dataThesis, which = "RMSE"))

(RMSE_beta.lm <-
    GCEstim::accmeasure(coef.res.lm, coef.dataThesis, which = "RMSE"))

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.100 <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    cv = TRUE, # default
    cv.nfolds = 5, # default
    support.signal = c(-100, 100),
    support.signal.points = 5, # default
    support.noise = NULL, # default
    support.noise.points = 3, # default
    twosteps.n = 0,
    method = "primal.solnp",
    OLS = TRUE, # default
    seed = 230676 # default
  )

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
coef.res.lmgce.100 <- coef(res.lmgce.100)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
RMSE_y.lmgce.100 <-
   GCEstim::accmeasure(fitted(res.lmgce.100), dataThesis$y, which = "RMSE")

RMSE_beta.lmgce.100 <-
    GCEstim::accmeasure(coef.res.lmgce.100, coef.dataThesis, which = "RMSE")

CV_RMSE_y.lmgce.100 <- 
    res.lmgce.100$error.measure.cv.mean

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
res.lmgce.50 <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    cv = TRUE, # default
    cv.nfolds = 5, # default
    support.signal = c(-50, 50),
    support.signal.points = 5, # default
    support.noise = NULL, # default
    support.noise.points = 3, # default
    twosteps.n = 0,
    method = "primal.solnp",
    OLS = TRUE, # default
    seed = 230676 # default
  )

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
coef.res.lmgce.50 <- coef(res.lmgce.50)

RMSE_y.lmgce.50 <-
   GCEstim::accmeasure(fitted(res.lmgce.50), dataThesis$y, which = "RMSE")

RMSE_beta.lmgce.50 <-
    GCEstim::accmeasure(coef.res.lmgce.50, coef.dataThesis, which = "RMSE")

CV_RMSE_y.lmgce.50 <- 
    res.lmgce.50$error.measure.cv.mean

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.apriori.centered.zero <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    support.signal = 
      matrix(
        c(-12, 12,
          -5, 5,
          -30, 30,
          -26, 26,
          -16, 16),
        ncol = 2,
        byrow = TRUE), 
    support.signal.points = 5, # default
    support.noise = NULL, # default
    support.noise.points = 3, # default
    twosteps.n = 0,
    method = "primal.solnp",
    OLS = TRUE, # default
    seed = 230676 # default
  )

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
coef.lmgce.apriori.centered.zero <- coef(res.lmgce.apriori.centered.zero)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
RMSE_y.lmgce.apriori.centered.zero <-
   GCEstim::accmeasure(fitted(res.lmgce.apriori.centered.zero), dataThesis$y, which = "RMSE")

RMSE_beta.lmgce.apriori.centered.zero <-
    GCEstim::accmeasure(coef.lmgce.apriori.centered.zero, coef.dataThesis, which = "RMSE")

CV_RMSE_y.lmgce.apriori.centered.zero <- 
    res.lmgce.apriori.centered.zero$error.measure.cv.mean

## ----echo=TRUE,eval=TRUE------------------------------------------------------
res.lmgce.apriori.centered.beta <-
  GCEstim::lmgce(
    y ~ .,
    data = dataThesis,
    support.signal = 
      matrix(
        c(-6, -2,
          -2, 2,
          -18, -14,
          -14, -10,
          -7, -3),
        ncol = 2,
        byrow = TRUE), 
    support.signal.points = 5,
    support.noise = NULL,
    support.noise.points = 3,
    twosteps.n = 0,
    method = "primal.solnp",
    OLS = TRUE,
    seed = 230676
  )

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
coef.lmgce.apriori.centered.beta <- coef(res.lmgce.apriori.centered.beta)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
RMSE_y.lmgce.apriori.centered.beta <-
   GCEstim::accmeasure(fitted(res.lmgce.apriori.centered.beta), dataThesis$y, which = "RMSE")

RMSE_beta.lmgce.apriori.centered.beta <-
    GCEstim::accmeasure(coef.lmgce.apriori.centered.beta, coef.dataThesis, which = "RMSE")

CV_RMSE_y.lmgce.apriori.centered.beta <- 
    res.lmgce.apriori.centered.beta$error.measure.cv.mean

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
res.all <- 
  data.frame(OLS = c(RMSE_y.lm,
                     CV_RMSE_y.lm,
                     RMSE_beta.lm),
           GCE_100000 = c(RMSE_y.lmgce.100000,
                          CV_RMSE_y.lmgce.100000,
                          RMSE_beta.lmgce.100000),
           GCE_100 = c(RMSE_y.lmgce.100,
                       CV_RMSE_y.lmgce.100,
                       RMSE_beta.lmgce.100),
           GCE_50 = c(RMSE_y.lmgce.50,
                       CV_RMSE_y.lmgce.50,
                       RMSE_beta.lmgce.50),
           GCE_apriori.centered.zero = c(RMSE_y.lmgce.apriori.centered.zero,
                                         CV_RMSE_y.lmgce.apriori.centered.zero,
                                         RMSE_beta.lmgce.apriori.centered.zero),
           GCE_apriori.centered.beta = c(RMSE_y.lmgce.apriori.centered.beta,
                                         CV_RMSE_y.lmgce.apriori.centered.beta,
                                         RMSE_beta.lmgce.apriori.centered.beta),
           row.names = c("Prediction RMSE",
                         "Prediction CV-RMSE",
                         "Precision RMSE")
  )

## ----echo=FALSE,eval=TRUE,results = 'asis'------------------------------------

kableExtra::kable(
  res.all,
  digits = 3,
  align = c(rep('c', times = 5)),
  col.names = c("$OLS$",
                "$GME_{(100000)}$",
                "$GME_{(100)}$",
                "$GME_{(50)}$",
                "$GME_{(apriori.centered.zero)}$",
                "$GME_{(apriori.centered.beta)}$"),
  row.names = TRUE,
  booktabs = FALSE)


