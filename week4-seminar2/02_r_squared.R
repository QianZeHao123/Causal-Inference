#   SGIA40F15 23/24 
#       Seminar 02
#           What about the R-squared? 
#   2024-01-29
#   Juraj Medzihorsky

rm(list=ls())

#   let's simulate data with ATE=1 and var(ITE) = 1
N <- 1e3
set.seed(4202)
y0 <- rnorm(N) |> scale()
tau <- 1 + rnorm(N) |> scale()
y1 <- y0 + tau
d <- sample(rep(0:1, c(round(N/2),N-round(N/2))), replace=F)
y <- ifelse(d%in%0, y0, y1)

table(d)
mean(tau)
var(tau)

#   since we have simulated the data, we know the true model
#   R^2 is poor, 0.12
#   meaning the deterministic component, in effect the two
#   intercepts, captures 1/8 of the variance in Y
lm(y ~ 1 + d) |> summary() |> print(digits=3)

#   and yet, the individual treatment effects contribute to
#   half of the variance in y1
(var(tau)/var(y1)) |> round(3)

#   as seen in the observed Y|D=1
(var(tau[d%in%1])/var(y[d%in%1])) |> round(3)

#   let's check the residuals
r <- lm(y ~ 1 + d) |> resid()

#   the squared correlation between the ITE and the 
#   residuals is about 0.5
cor(r[d%in%1], tau[d%in%1])^2 |> round(3)

#   for D=1 cases, tau - ATE simply ends in the
#   residuals

#   SCRIPT END
