#   SGIA40F15 23/24
#       Seminar 02
#           A Diff-in-Diff Horror Story:
#           `Fixed Effects won't Save You`
#   2024-01-28
#   Juraj Medzihorsky

# ---------------------------------------------------------------------
# clear the environment var area
rm(list = ls())
# clear all plots
graphics.off()
# clear the console area
cat("\014")

library(lmw)
#   a helper to compute the ESS from LMW
ess <- function(i)
  (sum(i) ^ 2) / sum(i ^ 2)
ess_old <- function(i)
  (sum(abs(i)) ^ 2) / sum(i ^ 2)

#   this is an illustration of a non-intuitive problem
#   in diff-in-diff, using a recent high-profile article
#   it is anonymized, since we haven't yet notified the
#   authors of the flaw in their analysis that overturns
#   their result
#   the overturn is complete, it's not that the SEs would
#   change and what was `sig` is no longer `sig`
#   rather, the coeff changes signs

#   the data comes from a cross-country survey
#   it's not panel data, i.e. there aren't the same units
#   twice, before and after, rather, before and after
#   are different individuals

md <- read.csv('anonymized.csv')

head(md)

#   let's add the t:g interaction
md$d <- with(md, t * g)

#   how many countries are there? 9
table(md$ctry) |> length()
table(md$ctry)

#   add a (T,G) stratum indicator for convenience
md$strt <- with(md, paste0(g, t))

#   we see big differences
#   this is because the periods are not equally common
#   and this varies over countries
with(md, table(strt, ctry))

#   let's check the ATT estimates reported result in the
#   original paper
#   _p means pooled, i.e. all countries together
#   _f means fixed effects, i.e., with country intercepts
att_p <- lm(y ~ 1 + t * g, data = md)$coef['t:g']
att_f <- lm(y ~ 1 + t * g + factor(ctry), data = md)$coef['t:g']

#   both are positive, and only differ a bit
c(att_p, att_f) |> round(3)

#   now what happens if we estimate the model separately for
#   each country?
countries <- md$ctry |> unique() |> sort()

onecountry <-
  function(i)
  {
    lm(y ~ 1 + t * g, data = subset(md, ctry %in% i))$coef['t:g']
  }

att_s <- sapply(countries, onecountry)

#   it is negative in all but country!
att_s |> round(3)

#   average: still negative
mean(att_s) |> round(3)
#   same with country-weighted average
#   not that different
(sum(att_s * table(md$ctry)) / nrow(md)) |> round(3)

#   so what's up?
#   long story short, despite what many choose to believe
#   `fixed effects` are just intercepts, and adding them
#   won't automatically solve all problems
#   (neither do cluster-corrected SEs, which only change the
#   SE and not the point estimates)

#   let's first look at a "corrected" spec
#   time interacts with country, in effect giving each
#   country two separate intercepts for each group
lm(y ~ 1 + t + t:g + g * factor(ctry), data = md)$coef['t:g']

#   one way to think about it is that the FE model lies
#   in between the pooled and the stratified estimator
#   adding interactions moves the model towards the
#   stratified estimator

#   now let's look at how the weights change with
#   conditioning

W_p <-
  lmw(
    ~ 1 + t + g + d,
    treat = 'd',
    data = md,
    estimand = 'ATT',
    method = 'URI'
  )
W_f <-
  lmw(
    ~ 1 + t + g + d + factor(ctry),
    treat = 'd',
    data = md,
    estimand = 'ATT',
    method = 'URI'
  )
W_i <-
  lmw(
    ~ 1 + t + d + g * factor(ctry),
    treat = 'd',
    data = md,
    estimand = 'ATT',
    method = 'URI'
  )


#   getting the weights under stratification is a bit more
#   involving

aux_w_s <- rep(NA, nrow(md)) |> as.numeric()

for (i in countries) {
  aux_w_s[md$ctry %in% i] <-
    lmw(
      ~ 1 + t + g + d,
      treat = 'd',
      data = subset(md, ctry %in% i),
      estimand = 'ATT',
      method = 'URI'
    )$weights * mean(md$ctry %in% i)
}


#   check the effective sample sizes
sizes <-
  data.frame(
    what = c('--', 'Pooled', 'FE', 'G-C', 'Strat'),
    ess_0 = c(
      sum(md$d %in% 0),
      ess(W_p$weights[md$d %in% 0]),
      ess(W_f$weights[md$d %in% 0]),
      ess(W_i$weights[md$d %in% 0]),
      ess(aux_w_s[md$d %in% 0])
    ),
    ess_1 = c(
      sum(md$d %in% 1),
      ess(W_p$weights[md$d %in% 1]),
      ess(W_f$weights[md$d %in% 1]),
      ess(W_i$weights[md$d %in% 1]),
      ess(aux_w_s[md$d %in% 1])
    )
  )

#   despite the large nominal sample size in D=0
#   (remember, that includes GT {00, 01, 10}), the ESS
#   is not impresive and shrinks quickly with complexity
sizes

#   recover the ATT estimates
sum((md$y * W_p$weights)[md$d %in% 1]) - sum((md$y * W_p$weights)[md$d %in%
                                                                    0])
sum((md$y * W_f$weights)[md$d %in% 1]) - sum((md$y * W_f$weights)[md$d %in%
                                                                    0])
sum((md$y * W_i$weights)[md$d %in% 1]) - sum((md$y * W_i$weights)[md$d %in%
                                                                    0])
sum((md$y * aux_w_s)[md$d %in% 1]) - sum((md$y * aux_w_s)[md$d %in% 0])

w_p <-
  aggregate(W_p$weights,
            by = list(
              't' = md$t,
              'g' = md$g,
              'ctry' = md$ctry
            ),
            FUN = sum)
w_f <-
  aggregate(W_f$weights,
            by = list(
              't' = md$t,
              'g' = md$g,
              'ctry' = md$ctry
            ),
            FUN = sum)
w_i <-
  aggregate(W_i$weights,
            by = list(
              't' = md$t,
              'g' = md$g,
              'ctry' = md$ctry
            ),
            FUN = sum)
w_s <-
  aggregate(aux_w_s,
            by = list(
              't' = md$t,
              'g' = md$g,
              'ctry' = md$ctry
            ),
            FUN = sum)


# add them together
w <- w_p[1:3]
w$EY <-
  aggregate(md$y,
            by = list(
              't' = md$t,
              'g' = md$g,
              'ctry' = md$ctry
            ),
            FUN = mean)$x
w$N <-
  aggregate(md$y,
            by = list(
              't' = md$t,
              'g' = md$g,
              'ctry' = md$ctry
            ),
            FUN = length)$x
w$w_p <- w_p$x
w$w_f <- w_f$x
w$w_i <- w_i$x
w$w_s <- w_s$x

w |> print(digits = 1)

#   how do the weights correlate?
cor(w[, c('w_p', 'w_f', 'w_i', 'w_s')]) |> round(2)

#   it's gotta be ess within D-group

fil <- md$ctry %in% 'a'

sapply(countries, function(i)
  ess(aux_w_s[(md$ctry %in% i) & (md$d %in% 1)])) |> sum()
sapply(countries, function(i)
  ess(aux_w_s[(md$ctry %in% i) & (md$d %in% 0)])) |> sum()

sapply(countries, function(i)
  ess(W_i$weights[(md$ctry %in% i) & (md$d %in% 1)])) |> sum()
sapply(countries, function(i)
  ess(W_i$weights[(md$ctry %in% i) & (md$d %in% 0)])) |> sum()

sapply(countries, function(i)
  ess(W_p$weights[(md$ctry %in% i) & (md$d %in% 1)])) |> sum()
sapply(countries, function(i)
  ess(W_p$weights[(md$ctry %in% i) & (md$d %in% 0)])) |> sum()

sapply(countries, function(i)
  ess(aux_w_s[md$ctry %in% i]))
sapply(countries, function(i)
  ess(aux_w_s[md$ctry %in% i])) |> sum()


#      t g ctry    EY    N    w_p    w_f    w_i   w_s
#   1  0 0    a  15.9  552 -0.122 -0.123 -0.010 -0.08
#   2  1 0    a   2.4    9  0.004  0.004  0.010  0.08
#   3  0 1    a -16.3  560  0.123  0.122  0.008  0.08
#   4  1 1    a -47.8    8  0.003  0.003  0.008  0.08
#   5  0 0    b  34.1  281 -0.062 -0.066 -0.236 -0.17
#   6  1 0    b  12.3  907  0.356  0.350  0.236  0.17
#   7  0 1    b  31.0  300  0.066  0.062  0.234  0.17
#   8  1 1    b  -0.3  876  0.340  0.346  0.234  0.17
#   9  0 0    c   3.5  287 -0.063 -0.064 -0.233 -0.16
#   10 1 0    c  18.7  806  0.316  0.320  0.233  0.16
#   11 0 1    c -15.2  283  0.062  0.062  0.221  0.16
#   12 1 1    c  -7.6  830  0.322  0.318  0.221  0.16
#   13 0 0    d -17.8  143 -0.032 -0.033 -0.108 -0.07
#   14 1 0    d -16.9  315  0.124  0.122  0.108  0.07
#   15 0 1    d -20.9  164  0.036  0.034  0.113  0.07
#   16 1 1    d -23.0  314  0.122  0.123  0.113  0.07
#   17 0 0    e  26.3  793 -0.175 -0.168 -0.176 -0.14
#   18 1 0    e  24.4  200  0.078  0.082  0.176  0.14
#   19 0 1    e  10.2  753  0.166  0.172  0.184  0.14
#   20 1 1    e   6.9  229  0.089  0.085  0.184  0.14
#   21 0 0    f   7.4  283 -0.062 -0.065 -0.132 -0.07
#   22 1 0    f  -1.6  208  0.082  0.081  0.132  0.07
#   23 0 1    f -10.3  306  0.067  0.065  0.130  0.07
#   24 1 1    f -18.9  209  0.081  0.081  0.130  0.07
#   25 0 0    g   9.2 1138 -0.251 -0.251 -0.034 -0.16
#   26 1 0    g  20.2   32  0.013  0.013  0.034  0.16
#   27 0 1    g -10.3 1140  0.251  0.251  0.032  0.16
#   28 1 1    g -14.7   31  0.012  0.012  0.032  0.16
#   29 0 0    h  31.7  515 -0.114 -0.111 -0.017 -0.07
#   30 1 0    h  38.7   16  0.006  0.006  0.017  0.07
#   31 0 1    h  16.3  500  0.110  0.113  0.022  0.07
#   32 1 1    h   4.9   22  0.009  0.008  0.022  0.07
#   33 0 0    i  23.1  542 -0.120 -0.119 -0.055 -0.08
#   34 1 0    i  24.2   55  0.022  0.022  0.055  0.08
#   35 0 1    i -14.1  541  0.119  0.119  0.055  0.08
#   36 1 1    i -22.7   58  0.023  0.022  0.055  0.08
