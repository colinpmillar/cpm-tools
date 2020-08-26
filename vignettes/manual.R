## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  cache=FALSE, cache.path='cache/',
  fig.path  ='tex/man-',
  fig.align='center', comment=NA,
  message=FALSE, warning=FALSE, echo=TRUE,
  fig.width=6, fig.height=4.5)

## -----------------------------------------------------------------------------
library(rct3)
data(pok)
head(pok)

## -----------------------------------------------------------------------------
fmla <- formula(age4 ~ ibtsq3)

res <- rct3(formula = fmla, data = pok, min.se = 0,
  range = nrow(pok)-1, old = FALSE, shrink = FALSE, power = 0)

summary(res)

## -----------------------------------------------------------------------------
plot(log(age4) ~ log(ibtsq3), pok, t = "p", pch = 20, col = 8)
pred <- res$rct3.summary

reg <- res$rct3$`yearclass:2015`
# mean observed value
abline(h = reg$prediction[2], col = 8, lty = 2) 
# fitted regression and predicted value
abline(a = reg$intercept[1], b = reg$slope[1], col = 1, lty = 2) 
points(reg$indices[1], pred$logWAP, pch = 20, col = 1)
# assumed value
points(reg$indices[1], log(81298))
legend("topleft", legend = c("Mean", "rct3 pred."), 
  lty = c(2,2), col = c(8,1), pch = c(NA, 20), bty = "n")

