#   SGIA40F15 Causal Inference
#       Seminar 3
#           From the FWL Theorem to Double Machine Learning
#   Juraj Medzihorsky
#   2024-02-16

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

#   bind the data for easier manipulation
dat <- data.frame(y, d, x, a0, a1)
#   split it into two folds
set.seed(4321)
fi <- sample(rep(c(1,2), c(floor(N/2), N-floor(N/2))), N, replace=F) 

#   now cross-fit-and-predict to get the residuals

mod_y_1 <- gam(y ~ 1 + s(x) + s(a0) + s(a1),
               family=gaussian(), data=dat[fi%in%1, ]) 
mod_y_2 <- gam(y ~ 1 + s(x) + s(a0) + s(a1),
               family=gaussian(), data=dat[fi%in%2, ]) 

mod_d_1 <- gam(d ~ 1 + s(x) + s(a0) + s(a1),
               family=gaussian(), data=dat[fi%in%1, ]) 
mod_d_2 <- gam(d ~ 1 + s(x) + s(a0) + s(a1),
               family=gaussian(), data=dat[fi%in%2, ]) 

res_y_1 <- dat$y[fi%in%1] - predict(mod_y_2, newdata=dat[fi%in%1, ])
res_y_2 <- dat$y[fi%in%2] - predict(mod_y_1, newdata=dat[fi%in%2, ])

res_d_1 <- dat$d[fi%in%1] - predict(mod_d_2, newdata=dat[fi%in%1, ])
res_d_2 <- dat$d[fi%in%2] - predict(mod_d_1, newdata=dat[fi%in%2, ])

y_res <- c(res_y_1, res_y_2)
d_res <- c(res_d_1, res_d_2)


#   and the estimate
lm(y_res ~ 1 + d_res)$coef['d_res']
#   compare
lm(y ~ 1 + d + x + a0 + a1)$coef['d']


#   SCRIPT END
