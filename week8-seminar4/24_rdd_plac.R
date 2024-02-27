#   SGIA40F15 23/24 
#       Seminar 04
#            RDD minimal example with a placebo cutoff test
#   2024-02-24
#   Juraj Medzihorsky

library(rdrobust)
library(arm) # for se.coeff()

rm(list=ls())

#   simulate the data, same setup as the previous script
#   larger sample size though
N <- 6e2
set.seed(19740330)
x <- runif(N, -2, 4)
u0 <- rnorm(N) |> scale()
u1 <- 2 + rnorm(N) |> scale()   #   ATE is 2!
y0 <- 0.5*x - 0.3*x^2 + 0.2*x^3 + u0
y1 <- 0.5*x - 0.3*x^2 + 0.2*x^3 + u1
cutoff <- 1
d <- as.numeric(x>=cutoff) 
y <- ifelse(d%in%0, y0, y1)

#   what's the relationship?
par(mar=c(5,5,1,1), las=1)
plot.new()
plot.window(xlim=range(x), ylim=range(c(y0,y1)))
axis(1) ; axis(2)
abline(v=cutoff, lty=3, col=grey(0.5), lwd=0.75)
curve(0.5*x - 0.3*x^2 + 0.2*x^3 + 1,
      from=cutoff, to=max(x), col='blue3', add=T)
curve(0.5*x - 0.3*x^2 + 0.2*x^3 + 0,
      from=min(x), to=cutoff, col='red3', add=T)

#   simplest RDD estimate
#   first, we center X on the cutoff
xc <- x-cutoff


#   3-rd degree polynomial
#   the true model
#   there it is
lm(y ~ 1 + xc + I(xc^2) + I(xc^3) + d)$coeff['d'] |> round(2)

#   robust local polynomial RDD from rdrobust
rob <- rdrobust(y=y, x=x, c=cutoff)
#   get the coeff
rob$coef |> round(3)

#   placebo cutoffs
pc <- seq(0.0, 2.0, by=1e-2)

#   let's only get the robust estimate from drdrobust,
#   to make it simpler
getcoeff <- 
    function(tc)
    {
        x_ <- x - tc
        d_ <- (x_ > 0) |> as.numeric()
        m1 <- lm(y ~ 1 + x_ + I(x_^2) + I(x_^3) + d_)
        b1 <- m1$coef['d_']
        se <- se.coef(m1)['d_']
        rob <- rdrobust(y=y, x=x, c=tc)
        return(c('lm_est'=b1, 
                 'lm_lo'=b1-1.96*se, 'lm_hi'=b1+1.96*se,
                 'rrdd_est'=as.vector(rob$coef)[3], 
                 'rrdd_lo'=rob$ci[3,1], 'rrdd_hi'=rob$ci[3,2]))
    }

getcoeff(1) 

plac <- sapply(pc, getcoeff) |> t() |> as.data.frame()

plac |> head() |> round(2)

#   plot it
par(mar=c(5,5,1,1), las=1)
plot.new()
plot.window(xlim=range(pc), ylim=c(-2,2))
axis(1, seq(0.0, 2.0, by=0.5)) ; axis(2)
abline(h=0, lty=3, col=grey(0.5))
abline(v=1, lty=3, col=grey(0.5))
points(pc, plac$lm_est, pch=16, col='blue3') 
points(pc, plac$rrdd_est, pch=16, col='red3') 

#   now with the CIs, separately for easier reading
par(mar=c(5,5,1,1), las=1, mfrow=c(1,2), xpd=T)
#
plot.new()
plot.window(xlim=range(pc), ylim=c(-1,1)*3)
axis(1, seq(0.0, 2.0, by=0.5)) ; axis(2)
abline(h=0, lty=3, col=grey(0.5))
abline(v=1, lty=3, col=grey(0.5))
points(pc, plac$lm_est, pch=16, col='blue3') 
for (i in 1:nrow(plac)) {
    lines(c(pc[i],pc[i]), c(plac$lm_lo[i], plac$lm_hi[i]),
          col='blue3', lend='butt')
}
#
plot.new()
plot.window(xlim=range(pc), ylim=c(-1,1)*3)
axis(1, seq(0.0, 2.0, by=0.5))
abline(h=0, lty=3, col=grey(0.5))
abline(v=1, lty=3, col=grey(0.5))
points(pc, plac$rrdd_est, pch=16, col='red3') 
for (i in 1:nrow(plac)) {
    lines(c(pc[i],pc[i]),
          c(plac$rrdd_lo[i], plac$rrdd_hi[i]),
          col='red3', lend='butt')
}

#   now let's zoom in on robust RDD
#   color by 0 in the interval
par(mar=c(5,5,1,1), las=1, mfrow=c(1,1), xpd=T)
#
plot.new()
plot.window(xlim=range(pc), ylim=c(-1,1)*3)
axis(1, seq(0.0, 2.0, by=0.5)) ; axis(2)
abline(h=0, lty=3, col=grey(0.5))
abline(v=1, lty=3, col=grey(0.5))
zero_in <- (plac$rrdd_lo < 0) & (plac$rrdd_hi > 0)
mk <- ifelse(zero_in, 'pink2', 'red4')
points(pc, plac$rrdd_est, pch=16, col=mk) 
for (i in 1:nrow(plac)) {
    lines(c(pc[i],pc[i]),
          c(plac$rrdd_lo[i], plac$rrdd_hi[i]),
          col=mk[i], lend='butt')
}



