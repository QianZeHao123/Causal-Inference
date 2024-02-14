#   SGIA40F15 23/24 
#       Seminar 02
#           g-computation
#   2024-01-28
#   Juraj Medzihorsky

#   G-computation (a.k.a. regression imputation) of ATT:
#       1. fitting an outcome Y(0) model in D=0
#       2. use the fitted model to impute the Y(0) in D=1
#       3. compare the observed Y(1)|D=1 with the imputed
#          Y(0)|D=1

library(mgcv)

set.seed(234)
N <- 1e3
x <- runif(N, -3, 3)
a0 <- rnorm(N) |> scale()
a1 <- rnorm(N) |> scale()
y0 <- sin(x) + a0 
y1 <- sin(x) + a1 + 1
mean(y1-y0)
d <- sapply(plogis(y1-y0), function(i) rbinom(1, 1, i))
y <- ifelse(d%in%0, y0, y1)

md <- data.frame(y,d,x,a0,a1)


#   two imputation models, GLM and GAM, both with Gaussian
#   residuals

M1 <- lm(y ~ 1 + x, data=md, subset=md$d%in%0)
M2 <- gam(y ~ 1 + s(x), data=md, subset=md$d%in%0)

y0_1 <- predict(M1, newdata=subset(md, d%in%1))
y0_2 <- predict(M2, newdata=subset(md, d%in%1))

att_0 <- mean(y[d%in%1]) - mean(y[d%in%0])
att_1 <- mean(y[d%in%1]) - mean(y0_1)
att_2 <- mean(y[d%in%1]) - mean(y0_2)
ols_b <- lm(y ~ 1 + d + x, data=md)$coef['d']

est <- 
    data.frame(
        att=c('True', 'GC-OLS', 'GC-GAM', 'OLS'),
        value=c(att_0, att_1, att_2, ols_b))

est |> print(digits=3)

#   this is just for illustration, many other options for
#   the imputation model are possible

#   There is a lot more to G-computation (a.k.a. G-formula).

#   We could for example estimate the ATE instead of the ATT
#   by imputing Y(0) and Y(1) for everone.
M_0 <- gam(y ~ 1 + s(x), data=md, subset=md$d%in%0)
M_1 <- gam(y ~ 1 + s(x), data=md, subset=md$d%in%1)

imp_y0 <- predict(M_0, newdata=md)
imp_y1 <- predict(M_1, newdata=md)

mean(imp_y1) - mean(imp_y0)

#   G-computation becomes especially useful when dealing 
#   with time-series data with time-varying confounders.

#   SCRIPT END   
#   ========================================================
