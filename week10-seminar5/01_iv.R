#   SGIA40F15 23/24
#       Seminar 05
#           Instrumental Variables
#   2024-03-10
#   Juraj Medzihorsky

rm(list = ls())

library(AER)
library(arm) # for se.coef
#   install.packages('systemfit')
library(systemfit)


#   Simulate the data
#   To make it simple, the instrument, Z will be binary,
#   and uniformly distributed. Imagine something like a
#   simple randomized mailing campaign. Having been sent
#   the mail is randomized, Z, but D, having read it,
#   is of main interest.
N <- 4e2
set.seed(432)
#   first a bunch of confounders
#   a somewhat more complex setup than usual
u <- runif(N,-3, 3) # an unobserved confounder
e0 <- rnorm(N, 0, 1) |> scale() |> as.vector()
e1 <- rnorm(N, 0, 1) |> scale() |> as.vector()
foo <- function(i, p = 0.4)
  p * pnorm(i) + (1 - p) * ecdf(i)(i)
aux <- 1 + foo(u)  |> scale() |> as.vector()
v0 <- 0.9 + 0.1 * (1 - foo(u))
v1 <- 0.9 + 0.1 * foo(u)
#   The Y potential outcomes
y0 <- 0.0 + (u + e0 * v0) |> scale(center = T) |> as.vector()
y1 <- aux + (u + e1 * v1) |> scale(center = T) |> as.vector()


#   The sample (and population) ATE is still exactly 1
mean(y1) - mean(y0)

#   Summarize the ITE
ite <- y1 - y0
ite |> summary() |> print(digits = 2)
ite |> sd() |> print(digits = 2)

#   The joint distribution {Y(0),Y(1)} looks like this
par(mar = c(5, 5, 1, 1), las = 1)
plot(
  y0,
  y1,
  pch = 16,
  col = 'blue3',
  xlab = 'Y(0)',
  ylab = 'Y(1)',
  asp = 1
)
abline(
  a = 0,
  b = 1,
  col = 'red3',
  lty = 3
)

#  {U, Y(1)-Y(0)}
par(mar = c(5, 5, 1, 1), las = 1)
plot(u,
     ite,
     col = 'blue',
     xlab = 'U',
     ylab = 'ITE = Y(1) - Y(0)')
abline(h = 0, col = 'black', lty = 3)
lm(ite ~ 1 + u) |> abline(col = 'red')

#   The four principal strata
#   To make things as simple as possible, we assume no
#   defiers
#
#   Generate the types
type_names <- c('always-taker', 'complier',
                'never-taker', 'defier')

#   An individual's probability of type membership depends
#   on their values of X and U through their ITEs
#   We can think of this as something like anticipation:
#   the individuals have some idea of what to expect in
#   terms of their ITEs, which in turn depend on X and U
#
#   Remember the softmax function?
#   we'll now add the 'temperature' parameter
softmax <- function(x, h = 1)
  exp(x * h) / sum(exp(x * h))

#   a matrix with a row for each individual, and as many
#   columns as there are categories, minus the last column
#   because we're assuming no defiers
type_df <- data.frame(
  'alw' = 0.8 * sign(ite) * abs(ite) ^ 1 / 2,
  'comp' = 0.25 * ite,
  'nev' = -0.8 * sign(ite) * (ite ^ 2)
)

type_df |> head() |> print(digits = 2)

#   now use softmax to compute those probablities
type_prob <- apply(type_df, 1, softmax, h = 4) |>
  t() |> as.data.frame()
#   add 0 for defiers
type_prob$def <- rep(0, nrow(type_prob))

type_prob |> head() |> print(digits = 2)

#   Draw the type for each individual
set.seed(765)
type <- apply(type_prob, 1,
              function(i)
                sample(type_names, 1, prob = i))

type |> table()

#   what are the ITEs by type?
#   the compliers are a bit different
type_LE <- aggregate(ite, by = list(type), FUN = summary)
#   these are summaries of the Local Effects, local with
#   respect to the four types. the 'x.Mean' column is the
#   LATE of each type
type_LE |> print(digits = 1)

#   Now the instrument
#   It's binary, to make it as simple as possible
set.seed(90210)
z <-
  sample(rep(0:1, c(round(N / 2), N - round(N / 2))), N, replace = F)

#   Now the D(Z=z) potential outcomes
#   the probability of D1 depends on Z
d0 <- d1 <- rep(NA, N) |> as.numeric()
d0[type %in% 'always-taker'] <- 1
d1[type %in% 'always-taker'] <- 1
d0[type %in% 'complier'] <- 0
d1[type %in% 'complier'] <- 1
d0[type %in% 'never-taker'] <- 0
d1[type %in% 'never-taker'] <- 0
d0[type %in% 'defier'] <- 1
d1[type %in% 'defier'] <- 0

#   use them to generate the observed D
d <- ifelse(z %in% 0, d0, d1)
#   and use that in turn to generate the observed y
y <- ifelse(d %in% 0, y0, y1)

#   True sample (and population) ATE is set to 1
mean(y1 - y0) |> round(3)

#   Naive ATE estimate is a bit off
(mean(y[d %in% 1]) - mean(y[d %in% 0]))  |> round(3)
lm(y ~ 1 + d)$coef['d'] |> round(3)

#   Inspect the principal stratification
table(z, d)

#   Let's make the same plots, but now color the points by
#   type
type_cols <-
  c(
    rgb(0, 158, 115, 255, max = 255),
    #   green
    rgb(0, 114, 178, 255, max = 255),
    #   dark blue
    rgb(213,  94,   0, 255, max = 255),
    #   red
    rgb(204, 121, 167, 255, max = 255)
  )  #   purple
names(type_cols) <- type_names

#  {U, Y(1)-Y(0)}
dev.off()
par(mar = c(4.5, 4.5, 3, 1),
    las = 1,
    mfrow = c(2, 2))
for (j in 1:3) {
  fil <- type %in% type_names[j]
  plot(
    u[fil],
    ite[fil],
    col = type_cols[j],
    yl = range(ite),
    xlab = 'U',
    ylab = 'ITE = Y(1) - Y(0)',
    pch = 16
  )
  mtext(type_names[j], col = type_cols[j], font = 2)
  abline(h = 0,
         col = 'black',
         lty = 3)
  lm(ite ~ 1 + u, subset = fil) |> abline(col = 'red')
}

#   Now let's try three different versions of the basic
#   two-step (2SLS) estimator

#   Stage 0: The allocation model, with only the instrument
md1 <- lm(d ~ 1 + z)
md1 |> summary() |> print(digits = 2)

#   Get the fitted D*, i.e. only the part of variation d
dstar <- fitted(md1)

#   Stage 1: The outcome model, with the fitted value from
#   S0
lm(y ~ 1 + dstar)$coef['dstar'] |> round(3)

#   2SLS using the function in AER
ivreg(y ~ 1 + d | z)$coef['d'] |> round(3)

#   Same point estimates, but different SEs!
#   The SE in LM is not the one we want
lm(y ~ 1 + dstar) |> summary() |>
  {
    \(x) x$coefficients['dstar', 1:3]
  }() |> round(3)

ivreg(y ~ 1 + d | z) |> summary() |>
  {
    \(x) x$coefficients['d', 1:3]
  }() |> round(3)

#   b_2sls() is a function that does 2SLS IV "by-hand"
#   same as IV reg, and you can read the SE formula in it
source('00_b_2sls.R')
b_2sls(y = matrix(y, ncol = 1),
       X = matrix(d, ncol = 1),
       Z = matrix(z, ncol = 1)) |> round(3)

#   Alternative two-step ways:
#   A: Wald estimator:
#   2 stages:
#       1. D | Z (as in 2SLS)
#       2. Y | Z (different from 2sls)
mw1 <- lm(d ~ 1 + z)
mw2 <- lm(y ~ 1 + z)

coef(mw1)['z']
coef(mw2)['z']

#   and the same thing
(coef(mw2)['z'] / coef(mw1)['z']) |> round(3)


#   B: controlling for Stage 1 d-residuals, i.e. variation
#   in d that does not come from the instrument
dres <- residuals(md1)
lm(y ~ 1 + d + dres) |> summary() |>
  {
    \(x) x$coefficients['d', 1:3]
  }() |> round(3)

#   Truly one step way: a bivariate model
#   using a function for fitting
#   Seemingly Unrelated Regressions in the systemfit library
#   it can also do 2SLS and a bunch of other things
#
#   From the manual:
#       Fits a set of linear structural equations using
#       Ordinary Least Squares (OLS), Weighted Least Squares
#       (WLS), Seemingly Unrelated Regression (SUR),
#       Two-Stage Least Squares (2SLS), Weighted Two-Stage
#       Least Squares (W2SLS) or Three-Stage Least Squares
#       (3SLS).

sur <- systemfit(list(y ~ d, d ~ z),
                 method = 'SUR',
                 control = systemfit.control(maxiter = 1e4))

#   same point estimate but very different SE
summary(sur)$coefficients[2, 1:3] |> round(3)

#   we can also do as above with ivreg
tsls <- systemfit(y ~ d, inst =  ~ z, method = '2SLS')
summary(tsls)$coefficients[2, 1:3] |> round(3)

#   ________________________________________________________
