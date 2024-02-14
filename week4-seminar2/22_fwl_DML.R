#   SGIA40F15 Causal Inference
#       Seminar 2
#           From the FWL Theorem to Double Machine Learning
#   Juraj Medzihorsky
#   2024-01-28

library(mgcv) # for gam

rm(list=ls())

#   just some colors
my_red <- rgb(205, 79, 57, 150, max=255)
my_blue <- rgb(104, 131, 139, 150, max=255)


#   a helper function to tranform a variable from 
#   [0,1] to (0,1), this is just for the simulation
foo <- function(x, adj=1e-3) adj + x*(1-2*adj)
foo(c(0,1))


#   simulate the data
N <- 1e3
set.seed(1234)
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

#   D depends on f(a0)
#   associated with Y(0)
p <- pnorm(sin(a0), 0, 0.7)
d <- sapply(p, function(i) rbinom(1,1,i))
#   inspect
table(d)
#   vector with color based on D
col_d <- ifelse(d%in%0, my_red, my_blue)

y <- ifelse(d%in%0, y0, y1)

#   what does it look like?
par(mar=c(5,5,1,1), las=1)
curve(pnorm(sin(x), 0, 0.7), min(a0), max(a0), ylim=c(0,1),
      xlab='A0', ylab='P[D=1]', col='pink4', lty=3)
rug(a0[d%in%0], side=1, col=my_blue)
rug(a0[d%in%1], side=3, col=my_red)

#   plain control
lm(y ~ 1 + d + x + a0 + a1)$coef['d']


#   Double ML with a GAM

test_gam <- gam(y ~ 1 + s(x) + s(a0) + s(a1))

summary(test_gam)

par(mfrow=c(2,2), las=1, mar=c(4,4,1,1))
plot.gam(test_gam, col='blue3', select=1)
plot.gam(test_gam, col='blue3', select=2)
plot.gam(test_gam, col='blue3', select=3)


y_res <- gam(y ~ 1 + s(x) + s(a0) + s(a1), family=gaussian()) |> resid()
d_res <- gam(d ~ 1 + s(x) + s(a0) + s(a1), family=gaussian()) |> resid()

#   and the estimate
lm(y_res ~ 1 + d_res) |> {\(x) coef(x)[2]}()
#   compare
lm(y ~ 1 + d + x + a0 + a1) |> coef() |> {\(x) x[2]}()

#   now let's use logistic regression for the other
d_res_2 <- gam(d ~ 1 + s(x) + s(a0) + s(a1), family=binomial('logit')) |> resid(type='response')
#   a bit different
lm(y_res ~ 1 + d_res_2) |> {\(x) coef(x)[2]}()


#   now simulate K draws and see how they compare

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
        p <- pnorm(sin(a0), 0, 0.7)
        d <- sapply(p, function(i) rbinom(1,1,i))
        y <- ifelse(d%in%0, y0, y1)
        y_res <- gam(y ~ 1 + s(x) + s(a0) + s(a1)) |> resid()
        d_res <- d - gam(d ~ 1 + s(x) + s(a0) + s(a1),
                         family=binomial('probit')) |> fitted(type='response')
        est_dml <- coef(lm(y_res ~ 1 + d_res))[2]
        est_ols <- coef(lm(y ~ 1 + d + x + a0 + a1))[2]
        names(est_dml) <- names(est_ols) <- NULL
        return(c('ols'=est_ols, 'dml'=est_dml))
    }

#   as we don't have the time now, this is just 100 
#   iterations, feel free to rerun it on your own with more 
set.seed(4321)
est <- replicate(1e2, onerun()) |> t()

#   slight bias of the OLS estimates
colMeans(est-1) |> round(3)
#   but the DML estimate is unsurprisingly noisier
apply(est, 2, sd) |> round(3)


#   now let's see the CIs using a simple bootstrap 

oneboot <-
    function(y_=y, d_=d, x_=x, a0_=a0, a1_=a1)
    {
        ii <- sample(1:length(y_), length(y_), replace=TRUE)
        y <- y_[ii]
        d <- d_[ii]
        x <- x_[ii]
        a0 <- a0_[ii]
        a1 <- a1_[ii]
        y_res <- gam(y ~ 1 + s(x) + s(a0) + s(a1)) |> resid()
        d_res <- d - gam(d ~ 1 + s(x) + s(a0) + s(a1),
                         family=binomial('probit')) |> fitted(type='response')
        est_dml <- coef(lm(y_res ~ 1 + d_res))[2]
        est_ols <- coef(lm(y ~ 1 + d + x + a0 + a1))[2]
        names(est_dml) <- names(est_ols) <- NULL
        return(c('ols'=est_ols, 'dml'=est_dml))
    }

#   we don't have the time to do an useful number of iterations
#   in class, so feel free to retun it with at least 5e2 later
set.seed(905)
system.time( B <- t(replicate(1e2, oneboot())) )

#   a bit different
colMeans(B) |> round(2)
#  compare 
lm(y ~ 1 + d + x + a0 + a1) |> coef() |> {\(x) x[2]}()
lm(y_res ~ 1 + d_res_2) |> {\(x) coef(x)[2]}()

#   the SD is twice as large under DML
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

#   NO FREE LUNCH

#   This is just an illustration. We could have fitted
#   just one GAM, or GLM with polynomials.
#   You can try this by modifying the est_ols line in 
#   onerun()
#   DLM becomes more interesting if we are in a data-rich
#   environment and use more complex regularized models.

#   SCRIPT END
