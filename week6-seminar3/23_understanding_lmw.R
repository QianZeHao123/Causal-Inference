#   SGIA40F15 23/24
#       Seminar 02
#           Understanding the implied linear model weights
#   2024-01-28
#   Juraj Medzihorsky

rm(list = ls())

#   first install lmw
#   two options: CRAN and the authors' github
#
#   right now we'll use CRAN because of the definition
#   of the weights
#
#
#   this is the issue
#   https://github.com/ngreifer/lmw/commit/be53ff14b4dbef33d17b46db8da0f5bf902a59a5
#
#   the development version:
#   https://github.com/ngreifer/lmw
#   with devtools
#       devtools::package_install('ngreifer/lmw')
#   with pak
#       pak::pkg_install('ngreifer/lmw')

library(lmw)

#   check how to cite the installed version
citation('lmw')


#   let's simulate data with ATE=1 and var(ITE) = 1
N <- 2e2
set.seed(4202)
a0 <- rnorm(N) |> scale()
a1 <- rnorm(N) |> scale()
x <- runif(N,-3, 3) |> scale()
y0 <- (x + a0) |> scale()
y1 <- 1 + (x + a1) |> scale()
#   treatment assignment depends on a0
d <- sapply(plogis(a0 - 0.5, 0, 1), {
  \(i) rbinom(1, 1, i)
})
y <- ifelse(d %in% 0, y0, y1)

#   the result of this selection is that the ITE in
#   the treated group differs from that in the whole
#   sample
c('ATE' = mean(y1 - y0), 'ATT' = mean((y1 - y0)[d %in% 1])) |> round(2)

#   OLS estimates
lm(y ~ 1 + d + x + a0 + a1) |> coef() |> round(2)
#   we only care about this one
lm(y ~ 1 + d + x + a0 + a1)$coef['d'] |> round(2)

#   what are the weights?
mw <- lmw( ~ d + x + a0 + a1,
           treat = 'd',
           estimand = 'ATE',
           method = 'URI')
w <- mw$weights


#   the weights are used to compute the wieghted E[Y(D)]
#   compare
c(
  'E[Y|D=0]' = mean(y[d %in% 0]),
  'E_w[Y|D=0]' = sum((w * y)[d %in% 0]),
  'E[Y|D=1]' = mean(y[d %in% 1]),
  'E_w[Y|D=1]' = sum((w * y)[d %in% 1])
) |> round(3)

#   and in the way we can compute the OLS coeff
(sum((w * y)[d %in% 1]) - sum((w * y)[d %in% 0])) |> round(3)
lm(y ~ 1 + d + x + a0 + a1)$coef['d'] |> round(3)

#   this differs from the naive group means comparison
(mean(y[d %in% 1]) - mean(y[d %in% 0])) |> round(3)
lm(y ~ 1 + d)$coef['d'] |> round(3)


#   what do the weights look like?
summary(w[d %in% 0]) |> round(4)
summary(w[d %in% 1]) |> round(4)

#   the weights sum up to 1 in each D-group
sum(w[d %in% 0]) |> round(4)
sum(w[d %in% 1]) |> round(4)

#   the mean weight in each group is 1/N_d
c(mean(w[d %in% 0]), 1 / sum(d %in% 0)) |> round(4)
c(mean(w[d %in% 1]), 1 / sum(d %in% 1)) |> round(4)

#   some of the weights are negative
mean(w[d %in% 0] < 0)
mean(w[d %in% 1] < 0)


#   _______________________________________________________
#   this is to compare the weights under the old and new
#   version of lmw
#       ow <- readRDS('weights_github.rds')
#   how to transform the new weights back into the old ones
#   in case you're using a newer version of lmw that uses
#   the change linked at the beginning of this script
#       sum(ow$weights[d%in%0]) |> round(4)
#       sum(ow$weights[d%in%1]) |> round(4)
#       wo <- ifelse(d%in%0,
#                    ow$weights/sum(d%in%0),
#                    ow$weights/sum(d%in%1))
#       summary(wo-w) |> round(5)
#   _______________________________________________________

#   the weights achieve that the weighted D=0 and D=1
#   groups have the same means on the controls
#   remember, X, A0, A1 all have a mean of 0 overall

#   original
mean(x[d %in% 0])
mean(x[d %in% 1])

#   weighted
sum((x * w)[d %in% 0])
sum((x * w)[d %in% 1])

c(mean(a0), mean(a0[d %in% 0]), mean(a0[d %in% 1]),
  sum((a0 * w)[d %in% 0]), sum((a0 * w)[d %in% 1])) |> round(3)

c(mean(a1), mean(a1[d %in% 0]), mean(a1[d %in% 1]),
  sum((a1 * w)[d %in% 0]), sum((a1 * w)[d %in% 1])) |> round(3)

#   those weighted averages differ quite a bit from the
#   true ones


#   we can use a similar approach to keep the weights
#   in the D=1 group equal and reweight only D=0
#   this will allow us to better estimate ATT

#   a data.frame for easier subsetting
dat <- data.frame(y, d, x, a0, a1)


#   now the g-computation version
#   we model the {Y(0) | D=1, X=x, ...} and use it to
#   impute { Y(0) | D=1 }
M_imp <- lm(y ~ 1 + x + a0 + a1, data = subset(dat, d %in% 0))

#   predict Y0
imp_y0 <- M_imp |> predict(newdata = dat)

#   now compare the predicted E[ Y0 | D=1 ] with the
#   observed E[ Y0 | D=0 ]

#   quite a difference
mean(y[d %in% 0])
mean(imp_y0[d %in% 1])

#   now the ATT hat
mean(y[d %in% 1]) - mean(imp_y0[d %in% 1])

#   and how is the actual ATT? since we have simulated
#   the data, we can check
mean((y1 - y0)[d %in% 1])
#   in this case it's the same as the estimate

#   as it happens, since we got the true model, we have
#   even estimated the Y0 | D=1 excellently
summary(y0[d %in% 1] - imp_y0[d %in% 1]) |> round(4)


#   what are the weights?

mw2 <- lmw( ~ d + x + a0 + a1,
            treat = 'd',
            estimand = 'ATT',
            method = 'MRI')
w2 <- mw2$weights

#   now the D=1 units are wieghted unifromly
w2[d %in% 1] |> head(10) |> round(4)
#   but not in D=0
w2[d %in% 0] |> head(10) |> round(4)


#   check the sample means now
#   now the weighted D=0 matches D=1 exactly
colMeans(cbind(x, a0, a1)[d %in% 1, ])
colSums(cbind(x, a0, a1)[d %in% 0, ] * w2[d %in% 0])

#   how do original and the ATT weights for D=0 compare ?
#   they are pretty close
cor(w[d %in% 0], w2[d %in% 0])

#   but not exactly the same
par(mar = c(5, 5, 1, 1), las = 1)
plot(w2[d %in% 0],
     w[d %in% 0],
     asp = 1,
     pch = 16,
     col = rgb(0, 0, 1, 0.5))


#   the output from lmw() in fact contains this info on
#   balance and also on the effective sample sizes
#   a function with the formula
ess <- function(i)
  (sum(i) ^ 2) / sum(i ^ 2)

#   NB this is different from the formula in the Biometrika
#   paper: (sum(abs(i))^2)/sum(i^2)
#   Noah Greifer explained this to me this way:
#
#       "the ESS really is meant to reflect the variability
#   of the weights, as this is directly related to the
#   variability of the effect estimate. Using the absolute
#   value of the weights can lead to a situation in which
#   the weights have a larger variance, but because of large
#   negative weights, which can occur due to extrapolation,
#   the apparent ESS (using the absolute value of the
#   weights) will increase, whereas the true ESS will be
#   much smaller (because the weights are more variable).
#   That is, a set of weights in which one weight has a
#   value of, say, -1, will in fact yield less precision
#   than a set of weights where that same weight instead has
#   a value of 1, but the ESS calculated using the absolute
#   value will not reflect this difference."
#       "I would hesitate when using the ESS with lmw anyway
#   because the ESS assumes the weights are totally
#   uncorrelated with the outcome; in fact, the ESS is much
#   higher than the formula reveals because the weights are
#   as highly correlated with the outcome as they can be
#   under the linear projection used. That is, an unadjusted
#   estimator (uniform weights of 1) will have a higher
#   computed ESS than an adjusted estimator (variable
#   weights but some confounding reduced), but in fact the
#   adjusted estimator will be more precise. That is, the
#   ESS assumes only instruments (variables uncorrelated
#   with the outcome) were used to compute the weights,
#   which is unrealistic in practice."


#   for the one-regression ATEhat
c('D=0' = ess(w[d %in% 0]), 'D=1' = ess(w[d %in% 1])) |> round(1)
#   for the fit-and-predict ATThat
c('D=0' = ess(w2[d %in% 0]), 'D=1' = ess(w2[d %in% 1])) |> round(1)

#   that's what summary.lmw() shows
mw |> summary()
mw2 |> summary()


#   From the manual
#   • ‘SMD’ - the standardized mean difference (SMD) between
#     the treated and control groups
#   • TSMD Treated - the target standardized mean difference
#     (TSMD) between the treated group and target sample
#   • TSMD Control - the TSMD between between the control
#     group and target sample
#   • ‘KS’ - the Kolmogorov-Smirnov (KS) statistic between
#     the treated and control groups
#   • TKS Treated - the target KS (TKS) statistic between
#     the treated group and target sample
#   • TKS Control - the TKS statistic between the control
#     group and target sample

#   we can even check the plots
mw |> summary() |> plot()

#   and in more detail
plot(mw, type = 'weights')

#   negative weights are in red
plot(mw,
     type = 'extrapolation',
     var = 'x',
     data = dat)

#   from the manual:
#   ‘SIC = (N-1) * w * r /(1 - h)’, where ‘N’ is the sample
#   size, ‘w’ are the units' implied regression weights,
#   ‘r’ are the residuals, and ‘h’ are the hat values. SIC
#   values are scaled to have a maximum of 1.
plot(mw,
     type = 'influence',
     outcome = 'y',
     data = dat)


#   we can bootstrap the ATT hat and the ESS
#   stratified bootstrap so we keep the nominal sizes of
#   the D=0 and D=1 groups fixed; this is just for
#   illustration; whether the plain or the stratified
#   bootstrap makes more sense depends on the sampling
#   process; in this case in fact the plain one might be
#   better

oneboot <-
  function(md = dat)
  {
    n <- nrow(md)
    ii <- sample(1:n, replace = T)
    md_ <- md[ii,]
    M0 <- lm(y ~ 1 + x + a0 + a1, data = subset(md_, d %in% 0))
    y0_d1 <- predict(M0, newdata = subset(md_, d %in% 1))
    y1_d1 <- md_$y[md_$d %in% 1]
    att <- mean(y1_d1) - mean(y0_d1)
    w <- lmw(
      ~ d + x + a0 + a1,
      treat = 'd',
      data = md_,
      estimand = 'ATT',
      method = 'MRI'
    )$weights
    ess0 <- ess(w[md_$d %in% 0])
    n0 <- sum(md_$d %in% 0)
    return(c(
      'att' = att,
      'n0' = n0,
      'ess0' = ess0
    ))
  }

oneboot()

set.seed(2024)
B <- replicate(1e3, oneboot()) |> t()

#   pretty close to estimates in the actual data
colMeans(B) |> round(2)

#   uncertainty?
apply(B, 2, sd) |> round(2)
apply(B, 2, quantile, c(0.025, 0.975)) |> t() |> round(2)

#   what is ESS0 as a fraction of N0? about 1/2
summary(B[, 3] / B[, 2]) |> round(2)


#   PS: another wrapper for convenience
mw |> lmw_est(outcome = y) |> summary() |> print(2)
mw2 |> lmw_est(outcome = y) |> summary() |> print(2)

#   SCRIPT END
#   ========================================================
#   APPENDIX

#   What's up under the hood?

#   ________________________________________________________
#   remember that the 'by-hand' way of computing the OLS
#   estimates is simply the following

M <- cbind(1, d, x, a0, a1)
beta <- solve(t(M) %*% M) %*% t(M) %*% y
beta |> as.vector() |> round(2)
#   check
lm(y ~ 1 + d + x + a0 + a1) |> coef() |> round(2)

#   following page 6 in the lmw paper:
#   https://arxiv.org/abs/2303.08790

X <- cbind(x, a0, a1)
N0 <- sum(d %in% 0)
N1 <- sum(d %in% 1)
X0 <- X[d %in% 0,]
X1 <- X[d %in% 1,]
xbar <- colMeans(X[,])
xbar0 <- colMeans(X0[,])
xbar1 <- colMeans(X1[,])
V0 <- var(X0)
V1 <- var(X1)


#   prepare a vector
u <- rep(NA, N) |> as.numeric()

for (i in 1:N) {
  if (d[i] %in% 1) {
    u[i] <-
      1 / N1 + t(X[i, ] - xbar1) %*% solve(N1 * V1 + N0 * V0) %*% (xbar0 - xbar1)
  } else if (d[i] %in% 0) {
    u[i] <-
      1 / N0 + t(X[i, ] - xbar0) %*% solve(N1 * V1 + N0 * V0) %*% (xbar1 - xbar0)
  } else {
    u[i] <- NA
  }
}

#   basically the same thing, but something's different
cor(u, w)

lm(w ~ u) |> summary()
lm(w ~ u)$coef[2]

par(mar = c(5, 5, 1, 1), las = 1)
plot(
  w,
  u,
  asp = 1,
  pch = 16,
  col = grey(0.5, 0.5),
  xlab = 'By-hand weights',
  ylab = 'Weights from lmw 0.0.1'
)
abline(a = 0, b = 1, col = 'red')
abline(lm(w ~ u), col = 'blue')

#   looks like an issue with how the computer handles
#   big and small numbers
#   or, my incorrect implementation of the formula
range(u)
range(w)



#   let's compare the weights and residuals under URI
d_res <- lm(d ~ 1 + x + a0 + a1) |> resid()

cor(d_res, w)
cor(d_res[d %in% 0], w[d %in% 0])
cor(d_res[d %in% 1], w[d %in% 1])

#   so what's the transformation?
lm(d_res[d %in% 0] ~ 1 + w[d %in% 0]) |> coef() |> round(4)
lm(d_res[d %in% 1] ~ 1 + w[d %in% 1]) |> coef() |> round(4)

#   the D-residuals are divided by the same amount, but with
#   opposite signs

lm(d_res[d %in% 0] ~ 1 + u[d %in% 0]) |> coef() |> round(4)
lm(d_res[d %in% 1] ~ 1 + u[d %in% 1]) |> coef() |> round(4)

summary(d_res[d %in% 0] / w[d %in% 0])
summary(d_res[d %in% 1] / w[d %in% 1])

#   #   that doesn't apply to the by-hand weights u
#   summary(d_res[d%in%0]/u[d%in%0])
#   summary(d_res[d%in%1]/u[d%in%1])

#    so, within D-groups we may get the URI LMW by simply
d_res_w <- rep(NA, length(d_res)) |> as.numeric()
d_res_w[d %in% 0] <- d_res[d %in% 0] / sum(d_res[d %in% 0])
d_res_w[d %in% 1] <- d_res[d %in% 1] / sum(d_res[d %in% 1])

sum(d_res_w)

cor(d_res_w, w)

# -------------------------------------------------------------
