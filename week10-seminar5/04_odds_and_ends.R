#   SGIA40F15 23/24
#       Seminar 05
#           Odds and Ends
#               1.  Copulas
#               2.  Tobit: dealing with censored outcomes
#               3.  Location-scale models
#   2024-03-10
#   Juraj Medzihorsky

rm(list = ls())

library(MASS)
library(AER) # tobit
library(mgcv) # generalized additive location-scale models

#   ________________________________________________________
#   2. Copulas
#   Informally: a copula is what remains from a multivariate
#   distribution if we make the marginals uniform.
#   A bit more formally: multivariate Cumulative
#   Distribution function of a multivariate distribution
#   with uniform marginals.

#   A very simple illustration.
#   Simulate two bivariate normals. One negatively the other
#   positively correlated. And then make the second one a
#   log-normal [that is, exp(Normal())]

#   Sample size
N <- 4e2
#   Means: all zero
mu1 <- mu2 <- rep(0, 2)
#   Variance-covariance matrices
Sigma1 <- matrix(c(1, 0.6, 0.6, 1), ncol = 2)
Sigma2 <- matrix(c(1, -0.9, -0.9, 1), ncol = 2)

set.seed(19740330)
A1 <- mvrnorm(N, mu1, Sigma1)
A2 <- mvrnorm(N, mu2, Sigma2) |> apply(2, exp)

#   Sample correlations:
cor(A1) |> round(2)
cor(A2) |> round(2)

#   The joint distributions
par(mar = c(3, 3, 1, 1),
    mfrow = c(1, 2),
    las = 1)
plot(
  A1,
  col = 'blue3',
  xlab = F,
  ylab = F,
  asp = 1
)
plot(
  A2,
  col = 'red4',
  xlab = F,
  ylab = F,
  asp = 1
)

#   The marginals, as histograms
K <- 10
par(mar = c(3, 3, 1, 1),
    mfrow = c(2, 2),
    las = 1)
hist(A1[, 1],
     breaks = K,
     col = 'blue3',
     xlim = c(-1, 1) * 5)
hist(A1[, 2],
     breaks = K,
     col = 'blue3',
     xlim = c(-1, 1) * 5)
hist(A2[, 1],
     breaks = K,
     col = 'red4',
     xlim = c(0, 1) * 16)
hist(A2[, 2],
     breaks = K,
     col = 'red4',
     xlim = c(0, 1) * 16)

#   now strip the marginals, using the empirical cumulative
#   density functions
U1 <- apply(A1, 2, function(x)
  ecdf(x)(x))
U2 <- apply(A2, 2, function(x)
  ecdf(x)(x))

#   the joint distributions
par(mar = c(3, 3, 1, 1),
    mfrow = c(1, 2),
    las = 1)
plot(
  U1,
  col = 'blue3',
  xlab = F,
  ylab = F,
  asp = 1
)
rect(0, 0, 1, 1, border = 'green4', lty = 3)
plot(
  U2,
  col = 'red4',
  xlab = F,
  ylab = F,
  asp = 1
)
rect(0, 0, 1, 1, border = 'green4', lty = 3)

#   The marginals, as histograms
K <- 10
yl <- c(0, 16)
par(mar = c(3, 3, 1, 1),
    mfrow = c(2, 2),
    las = 1)
hist(U1[, 1],
     breaks = K,
     col = 'blue3',
     ylim = yl)
hist(U1[, 2],
     breaks = K,
     col = 'blue3',
     ylim = yl)
hist(U2[, 1],
     breaks = K,
     col = 'red4',
     ylim = yl)
hist(U2[, 2],
     breaks = K,
     col = 'red4',
     ylim = yl)

#   now the really fun part, use the U1 to combine it with
#   the marginals of A2
B <-
  data.frame(
    'b1' = quantile(x = A2[, 1], probs = U1[, 1], type = 2),
    'b2' = quantile(x = A2[, 2], probs = U1[, 2], type = 2)
  )

#   positively correlated, just like A1
cor(B) |> round(2)

#   but with log-normal marginals, just like A2
#   the joint distributions
par(mar = c(3, 3, 3, 1),
    mfrow = c(1, 2),
    las = 1)
plot(
  A2,
  col = 'red4',
  xlab = F,
  ylab = F,
  asp = 1
)
plot(
  B,
  col = 'purple',
  xlab = F,
  ylab = F,
  asp = 1
)
#   The marginals, as histograms
K <- 10
yl <- c(0, 16)
par(mar = c(3, 3, 1, 1),
    mfrow = c(2, 2),
    las = 1)
hist(A2[, 1],
     breaks = K,
     col = 'red4',
     ylim = yl)
hist(A2[, 2],
     breaks = K,
     col = 'red4',
     ylim = yl)
hist(B[, 1],
     breaks = K,
     col = 'purple',
     ylim = yl)
hist(B[, 2],
     breaks = K,
     col = 'purple',
     ylim = yl)

#   We've only scratched the surface here. Copulas are
#   extremely useful for modeling associations in joint
#   distributions, such as in Generalized Joint Response
#   Models that generalize the Heckman selection model. And
#   they have many, many more applications.

#   ________________________________________________________
#   2. Tobit
#   Censoring: values that are below or above some threshold
#   will not be observed at their actual values, but at the
#   value of the threshold.

#   just a quick descriptive simulation
set.seed(12321)
a <- rnorm(4e2, 0, 1) |> scale() |> as.vector()
#   three kinds of censoring:
#   only at the left side
#   only at the upper side
#   at both side
thresh_lo <- -1.5
thresh_hi <- 2
a_lo <- ifelse(a <= thresh_lo, thresh_lo, a)
a_hi <- ifelse(a >= thresh_hi, thresh_hi, a)
a_both <- ifelse(a_lo >= thresh_hi, thresh_hi, a_lo)

#   prepare gaussian kernel density estimates
d_a <- density(a)
d_lo <- density(a_lo, from = thresh_lo)
d_hi <- density(a_hi, to = thresh_hi)
d_both <- density(a_both, from = thresh_lo, to = thresh_hi)

par(mar = c(6, 1, 3, 1),
    las = 1,
    mfrow = c(3, 1))
plot(
  d_a,
  col = 'black',
  main = 'Left-sided censoting',
  xlab = '',
  ylab = '',
  frame = F,
  yaxt = 'n',
  lty = 3,
  lwd = 3,
  xlim = c(-4.5, 4.5)
)
polygon(
  c(d_lo$x, rev(d_lo$x)),
  c(d_lo$y, rep(0, 512)),
  col = rgb(1, 0, 0, 0.5),
  border = rgb(1, 0, 0)
)
plot(
  d_a,
  col = 'black',
  main = 'Right-sided censoring',
  xlab = '',
  ylab = '',
  frame = F,
  yaxt = 'n',
  lty = 3,
  lwd = 3,
  xlim = c(-4.5, 4.5)
)
polygon(
  c(d_hi$x, rev(d_hi$x)),
  c(d_hi$y, rep(0, 512)),
  col = rgb(0, 0, 1, 0.5),
  border = rgb(0, 0, 1)
)
plot(
  d_a,
  col = 'black',
  main = 'Both-sided censoring',
  xlab = '',
  ylab = '',
  frame = F,
  yaxt = 'n',
  lty = 3,
  lwd = 3,
  xlim = c(-4.5, 4.5)
)
polygon(
  c(d_both$x, rev(d_both$x)),
  c(d_both$y, rep(0, 512)),
  col = rgb(0, 0.5, 0, 0.5),
  border = rgb(0, 0.5, 0)
)


#   Let's look at a simulated example of how it can cause
#   issues.
#   For simplicity, one-sided left censoring
set.seed(32123)
N <- 1e3
#   single confounder, two non-confounders
x <- runif(N,-3, 3) |> scale(scale = F) |> as.vector()
e0 <- rnorm(N,-3, 3) |> scale() |> as.vector()
e1 <- rnorm(N,-3, 3) |> scale() |> as.vector()
#   treatment
d <- plogis(-x) |> sapply(function(i)
  rbinom(1, 1, i))
#   potential outcomes
y0 <- 0.0 + (1 * x + e0) |> scale() |> as.vector()
y1 <- 2.0 + (2 * x + e1) |> scale() |> as.vector()
#   observed outcome, the textbook way as opposed to ifelse
#   lower threshold
ystar <- (1 - d) * y0 + d * y1
thresh_1 <- 0.0
y <- ifelse(ystar < thresh_1, thresh_1, ystar)

#   compare actual and observed
ystar |> quantile(probs = seq(0, 1, by = 1e-1)) |> round(1)
y |> quantile(probs = seq(0, 1, by = 1e-1)) |> round(1)
#   actual proportion below the threshold?
mean(ystar < thresh_1) |> round(2)
mean(ystar < y) |> round(2)


#   ITE; true sample and population ATE = 2
ite <- y1 - y0
ite |> summary() |> round(2)
#   true sample and population ATT
ite |> subset(d %in% 1) |> mean() |> round(2)

#   naive ATE: not that off, in this case
(mean(y[d %in% 1]) - mean(y[d %in% 0])) |> round(2)

#   "OLS" controlling for x
lm(y ~ 1 + d + x)$coef['d'] |> round(2)

#   tobit from AER: much better, in this case
tobit(y ~ 1 + d + x, dist = 'gaussian')$coef['d'] |> round(2)

#   the summary is a bit more verbose
tobit(y ~ 1 + d + x, dist = 'gaussian') |> summary() |>
  print(digits = 2)


#   as usuall, let's run this simulation a bunch of times

onerun <-
  function(N = 1e3, thresh_1 = 0)
  {
    x <- runif(N,-3, 3) |> scale(scale = F) |> as.vector()
    e0 <- rnorm(N,-3, 3) |> scale() |> as.vector()
    e1 <- rnorm(N,-3, 3) |> scale() |> as.vector()
    d <- plogis(-x) |> sapply(function(i)
      rbinom(1, 1, i))
    y0 <- 0.0 + (1 * x + e0) |> scale() |> as.vector()
    y1 <- 2.0 + (2 * x + e1) |> scale() |> as.vector()
    ystar <- (1 - d) * y0 + d * y1
    y <- ifelse(ystar < thresh_1, thresh_1, ystar)
    est_naive <- (mean(y[d %in% 1]) - mean(y[d %in% 0]))
    est_lm <- lm(y ~ 1 + d + x)$coef['d']
    est_tobit <- tobit(y ~ 1 + d + x, dist = 'gaussian')$coef['d']
    est <- data.frame(est_naive, est_lm, est_tobit)
    rownames(est) <- NULL
    return(est)
  }

#   B iterations at a smaller sample size
B <- 1e3
set.seed(12121)
res <- do.call(rbind, replicate(B, onerun(N = 2e2), simplify = F))

#   bias: tobit is practically unbiased
colMeans(res - 2) |> round(2)

#   variance: about the same
apply(res, 2, sd) |> round(2)
apply(res, 2, var) |> round(2)


#   ________________________________________________________
#   2.  Location-scale models
#   "heteroskedasticity" - the residuals have unequal
#   variance over the support of the fitted values
#   Location-scale models are useful when we need the
#   residuals for something (think e.g. changes-in-changes).

rm(list = ls())

#   Simple descriptive example, so no potential outcomes
N <- 4e2
x <- seq(-3, 3, length.out = N)
#   estar will be rescaled to generate the true residuals
estar <- rnorm(N, 0, 1) |> scale() |> as.vector()
e <- (estar * (0.2 + 0.8 * pnorm(x))) |> scale() |> as.vector()
#   1 + x is the linear predictor
y <- 1 + x + e

par(mfrow = c(2, 1),
    mar = c(5, 5, 2, 2),
    las = 1)
plot(x, y, col = 'blue1')
abline(lm(y ~ 1 + x),
       col = 'red',
       lty = 3,
       lwd = 4)
plot(x, e, col = 'blue1')
abline(
  h = 0,
  col = 'red',
  lty = 3,
  lwd = 4
)

#   now "OLS"
mod1 <- lm(y ~ 1 + x)
#   fitted values
yhat1 <- fitted(mod1)
#   residuals
res1 <- resid(mod1)

#   same thing, now estimated
par(mfrow = c(1, 1),
    mar = c(5, 5, 2, 2),
    las = 1)
plot(yhat1, res1, col = 'blue1')
abline(
  h = 0,
  col = 'red',
  lty = 3,
  lwd = 4
)

#   mod1 above is a location-only model, it only models
#   the conditional location (in this case the mean),
#   conditional on the right-hand side,
#   within the restrictions imposed by the model

#   we can take those residuals, square them, and fit a
#   scale-only model to them

ressq1 <- res1 ^ 2

mods1 <- glm(ressq1 ~ 1 + x,
             family = quasi(variance = 'mu', link = 'log'))

fits1 <- fitted(mods1, type = 'link') |> exp() |> sqrt()

#   this is the fitted value of the residual SD given X
par(mfrow = c(1, 1),
    mar = c(5, 5, 2, 2),
    las = 1)
plot(x, fits1, col = 'blue1', ylim = c(0, 6))
abline(
  h = 0,
  col = 'red',
  lty = 3,
  lwd = 4
)

#   let's compare the original and the rescaled residuals
#   but first we need to rescale the observed ones
resscaled1 <- res1 / fits1

par(mfrow = c(2, 1),
    mar = c(5, 5, 2, 2),
    las = 1)
plot(yhat1, res1, col = 'blue1')
abline(
  h = 0,
  col = 'red',
  lty = 3,
  lwd = 4
)
plot(yhat1, resscaled1, col = 'blue1')
abline(
  h = 0,
  col = 'red',
  lty = 3,
  lwd = 4
)

#   now in one go with gam() from the lmls package
#   two formulas, as a list: 1. location 2. scale

mod_g_1 <- gam(list(y ~ 1 + x, ~ 1 + x), family = gaulss())

mod_g_1 |> summary() |> print(digits = 2)

#   check the fitted values
#   remember, here, x is just an order sequence
fit_g_1 <- mod_g_1 |> fitted()
fit_g_1 |> head() |> round(2)

par(mfrow = c(2, 1),
    mar = c(5, 5, 2, 2),
    las = 1)
plot(x, fit_g_1[, 1], col = 'blue1', ylab = 'Fitted location')
plot(x, fit_g_1[, 2], col = 'blue1', ylab = 'Fitted scale')



#   SCRIPT END
#   ________________________________________________________
