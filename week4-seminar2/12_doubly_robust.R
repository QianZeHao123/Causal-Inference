#   SGIA40F15 Causal Inference
#       Seminar 2
#           Doubly Robust Estimation
#   Juraj Medzihorsky
#   2024-01-25

#   https://matheusfacure.github.io/python-causality-handbook/12-Doubly-Robust-Estimation.html

rm(list=ls())

#   a helper function to tranform a variable from 
#   [0,1] to (0,1), this is just for the simulation
foo <- function(x, adj=1e-3) adj + x*(1-2*adj)
foo(c(0,1))

#   simulate the data
N <- 1e3
set.seed(1969)
x <- runif(N, -2, 2)
a0 <- runif(N, -5, 5)
a1 <- runif(N, -5, 5)
y0_ <- x + a0
y1_ <- x + a1
#   potential outcomes
y0 <- qnorm(ecdf(y0_)(y0_) |> foo(), 0, 1) |> scale()
y1 <- qnorm(ecdf(y1_)(y1_) |> foo(), 1, 1) |> scale() + 1
#   individual treatment effects
tau <- y1 - y0
#   D depends on A0 and is thus associated with Y(0)
p <- plogis(a0, 0, 1)
d <- sapply(p, function(i) rbinom(1,1,i))
#   observed outcomes
y <- ifelse(d%in%0, y0, y1)
#   bind into a data frame for easier manipulation
dat <- data.frame(y,d,x,a0,a1)

#   first let's get the propensity scores
dat$ps <- glm(d ~ 1 + x + a0 + a1, data=dat,
              family=binomial('logit')) |> fitted('response')

#   now we need to fit two models, 
#   regressing Y on everything_but_D
#   separately in D=0 and D=1

mod_0 <- lm(y ~ 1 + x + a0 + a1, data=dat, subset=d%in%0)
mod_1 <- lm(y ~ 1 + x + a0 + a1, data=dat, subset=d%in%1)

#   compare the coefs
data.frame('M0'=coef(mod_0), 'M1'=coef(mod_1)) |> round(2)
#   informally, the difference between the intercepts
#   is one of the things that will go into the DRE ATE hat

#   and we use those models to predict Y(1) and Y(0)
#   for every unit
dat$mu0 <- mod_0 |> predict(newdata=dat)
dat$mu1 <- mod_1 |> predict(newdata=dat)

#   we will also take the residuals and scale them
#   with IPWs, giving more weight to residuals
#   of units whose D_i is more "surprising" give their
#   PS
#   we can see that the mean weigthed residuals are not 0
dat |> subset(d%in%1) |> with((y-mu1)/ps) |> summary() |> round(2)
dat |> subset(d%in%0) |> with((y-mu0)/(1-ps)) |> summary() |> round(2)



#   E[] Y(1) and Y(0)
E_Y1 <- with(dat, sum(d*(y-mu1)/ps + mu1))*(1/N)
E_Y0 <- with(dat, sum((1-d)*(y-mu0)/(1-ps) + mu0))*(1/N)
#   ATE hat
E_Y1 - E_Y0
#   not bad, compare
lm(y ~ 1 + d + x + a0 + a1, data=dat) |> coef() |> {\(x) x[2]}()


#   we could think of the following as the imputed POs
imp_y1 <- with(dat, d*(y-mu1)/ps + mu1)
imp_y0 <- with(dat, (1-d)*(y-mu0)/(1-ps) + mu0)

#   not bad, not great
cor(y1, imp_y1)
cor(y0, imp_y0)

ks.test(imp_y1, imp_y0, simulate.p.value=T)

#   as usual, let's try a simulation


onerun <-
    function(N=1e3)
    {
        x <- runif(N, -2, 2)
        a0 <- runif(N, -5, 5)
        a1 <- runif(N, -5, 5)
        y0_ <- x + a0
        y1_ <- x + a1
        y0 <- qnorm(ecdf(y0_)(y0_) |> foo(), 0, 1) |> scale()
        y1 <- qnorm(ecdf(y1_)(y1_) |> foo(), 1, 1) |> scale() + 1
        p <- plogis(a0, 0, 1)
        d <- sapply(p, function(i) rbinom(1,1,i))
        y <- ifelse(d%in%0, y0, y1)
        dat <- data.frame(y,d,x,a0,a1)
        dat$ps <- glm(d ~ 1 + x + a0 + a1, data=dat,
                      family=binomial('logit')) |> fitted('response')
        dat$mu0 <- lm(y ~ 1 + x + a0 + a1, data=dat, subset=d%in%0) |>
            predict(newdata=dat)
        dat$mu1 <- lm(y ~ 1 + x + a0 + a1, data=dat, subset=d%in%1) |>
            predict(newdata=dat)
        E_Y1 <- with(dat, sum(d*(y-mu1)/ps + mu1))*(1/N)
        E_Y0 <- with(dat, sum((1-d)*(y-mu0)/(1-ps) + mu0))*(1/N)
        dre <- E_Y1 - E_Y0
        ols <- coef(lm(y ~ 1 + d + x + a0 + a1, data=dat))['d']
        names(dre) <- names(ols) <- NULL
        return(c('ols'=ols, 'dre'=dre))
    }

onerun()

set.seed(1972)
b <- replicate(1e3, onerun()) |> t()

#   DRE is a bit less biased
colMeans(b-1) |> round(3)
#   and variant
apply(b, 2, sd) |> round(3)


#   but what about the SEs?
#   let's use a simple bootstrap as before

oneboot <-
    function(y_=y, d_=d, x_=x, a0_=a0, a1_=a1)
    {
        ii <- sample(1:length(y_), length(y_), replace=TRUE)
        y <- y_[ii]
        d <- d_[ii]
        x <- x_[ii]
        a0 <- a0_[ii]
        a1 <- a1_[ii]
        dat <- data.frame(y,d,x,a0,a1)
        dat$ps <- glm(d ~ 1 + x + a0 + a1, data=dat,
                      family=binomial('logit')) |> fitted('response')
        dat$mu0 <- lm(y ~ 1 + x + a0 + a1, data=dat, subset=d%in%0) |>
            predict(newdata=dat)
        dat$mu1 <- lm(y ~ 1 + x + a0 + a1, data=dat, subset=d%in%1) |>
            predict(newdata=dat)
        E_Y1 <- with(dat, sum(d*(y-mu1)/ps + mu1))*(1/N)
        E_Y0 <- with(dat, sum((1-d)*(y-mu0)/(1-ps) + mu0))*(1/N)
        dre <- E_Y1 - E_Y0
        ols <- coef(lm(y ~ 1 + d + x + a0 + a1, data=dat))['d']
        names(dre) <- names(ols) <- NULL
        return(c('ols'=ols, 'dre'=dre))
    }

oneboot()

set.seed(939)
system.time( B <- t(replicate(1e3, oneboot())) )

#   a bit different
colMeans(B) |> round(2)

#   DRE is a bit less variant too
apply(B, 2, sd) |> round(2)
apply(B, 2, quantile, c(0.025, 0.975)) |> t() |> round(2)

#   plot
par(mfrow=c(2,1), las=1, mar=c(3,4,1,1), oma=c(0,0,2,0))
xl <- range(as.vector(B))
hist(B[,1], col='lightblue3', main='OLS', xlim=xl)
abline(v=1, col='tomato3', lty=3)
hist(B[,2], col='lightblue3', main='DML', xlim=xl)
abline(v=1, col='tomato3', lty=3)
mtext('Bootstrap Replicates', outer=T, font=2, line=0.5, cex=1.25)


#   we can think of DRE as a kind of LEGO 
#   in which we can replace the parts
#   for example, we can replace all models with GAMs

library(mgcv)

#   first, let's clone the original data
dat2 <- data.frame(y,d,x,a0,a1)

dat2$ps <- gam(d ~ 1 + s(x) + s(a0) + s(a1), data=dat2,
               family=binomial('logit')) |> fitted('response')

M0 <- gam(y ~ 1 + s(x) + s(a0) + s(a1), data=dat2, subset=d%in%0)
M1 <- gam(y ~ 1 + s(x) + s(a0) + s(a1), data=dat2, subset=d%in%1)
dat2$mu0 <- M0 |> predict(newdata=dat2)
dat2$mu1 <- M1 |> predict(newdata=dat2)

E_Y1_ <- with(dat2, sum(d*(y-mu1)/ps + mu1))*(1/N)
E_Y0_ <- with(dat2, sum((1-d)*(y-mu0)/(1-ps) + mu0))*(1/N)
#   ATE hat
E_Y1_ - E_Y0_
#   compare
E_Y1 - E_Y0

#   in this single case it is worse
#   feel free to rerun the simulation on your own
#   to see if this holds more generally

#   SCRIPT END
