#   SGIA40F15 23/24 
#       Seminar 04
#            RDD minimal example with residualisation
#   2024-02-23
#   Juraj Medzihorsky

rm(list=ls())

#   simulate the data, extremely simple setup
#   with an added confounder Z
#   that has an effect on Y
#   that differs for Y0 and Y1
N <- 2e2
set.seed(19740330)
x <- runif(N, -2, 4)
z <- runif(N, -2, 2)
u0 <- rnorm(N) |> scale()
u1 <- 1 + rnorm(N) |> scale()
y0 <- 0.5*x + u0 + -1.5*z  
y1 <- 0.5*x + u1 +  1.3*z 
cutoff <- 1
d <- as.numeric(x>=cutoff) 
y <- ifelse(d%in%0, y0, y1)

#   true ATE is 1
mean(y1-y0)
#   ITE vary quite a bit though
summary(y1-y0)


#   plot it
#   just by looking at it, it doesn't seem that much
col0 <- rgb(1,0,0,0.8)
col1 <- rgb(0,0,1,0.8)
pcol <- ifelse(d%in%0, col0, col1)
plot(x, y, las=1, pch=16, col=pcol)


#   yet, if we could see the potential outcomes
#   but separately
par(mfrow=c(1,2), mar=c(4,4,1,1), las=1)
plot(x, y0, pch=16, col=col0, ylim=range(y))
plot(x, y1, pch=16, col=col1, ylim=range(y))


#   simplest RDD estimate
#   first, we center X on the cutoff
xc <- x-cutoff

#   now just add D
#   not that great
lm(y ~ 1 + xc + d) |> summary()

#   so what about Z?

#   let's plot it, separately
#   hard to say, really
par(mfrow=c(1,2), mar=c(4,4,1,1), las=1)
plot(z, y0, pch=16, col=col0, ylim=range(y))
plot(z, y1, pch=16, col=col1, ylim=range(y))

#   what about if we just control for Z?
lm(y ~ 1 + xc + d + z)$coeff |> round(4)

#   better
lm(y ~ 1 + xc + d + z + z:d)$coeff |> round(4)




#   so, let's residualise
#   we fit a model where Y ~ Z + Z^2  
#   to allow for different effect on Y0 and Y1 

#   without the intercept!
Mz <- lm(y ~ -1 + z + z:d)
Mz$coeff |> round(4)

yres <- resid(Mz)

#   now let's just plug those into the basic spec 
#   not bad
lm(yres ~ 1 + xc + d) |> summary()

#   but this was just a single run
#   let's try all three in a bunch of simulations as usual


onerun <-
    function(N=2e2)
    {
        x <- runif(N, -2, 4)
        z <- runif(N, -2, 2)
        u0 <- rnorm(N) |> scale()
        u1 <- 1 + rnorm(N) |> scale()
        y0 <- 0.5*x + u0 + -1.5*z  
        y1 <- 0.5*x + u1 +  1.3*z 
        cutoff <- 1
        d <- as.numeric(x>=cutoff) 
        y <- ifelse(d%in%0, y0, y1)
        xc <- x - cutoff
        est_plain <- lm(y ~ 1 + xc + d)$coeff['d'] 
        est_cont <- lm(y ~ 1 + xc + d + z + z:d)$coeff['d'] 
        yres <- lm(y ~ -1 + z + z:d) |> resid()
        est_res <- lm(yres ~ 1 + xc + d)$coeff['d']
        return(c('plain'=est_plain, 
                 'cont'=est_cont, 
                 'res'=est_res))
    }

onerun()

B <- 1e3
set.seed(30034791)
r <- replicate(B, onerun(N=2e2)) |> t()

summary(r) |> print(digits=3)

#   how bad is the bias?
#   the true value is always 1
#   not bad in any of the cases, actually
colMeans(r-1) |> round(3)

#   how is the variance?
#   much less if we control one way or the other
apply(r-1, 2, var) |> round(3)

#   remember the Crash Course in Good and Bad Controls:
#   Z doesn't affect D, so if we're ignoring it
#   we're not having issues with bias, only with precision
#   D depends only on X, and in a deterministic manner
#   plus here Z does not associate with X

lm(d ~ 1 + z) |> summary()

cor(x,z) |> round(3)

#   we haven't yet tried bandwidth selection or anything 
#   else that's expected, see the next scripts

#   ________________________________________________________
#   APPENDIX


#   let's compare LMW weights for the two specs

library(lmw)

wx <- lmw(~xc+d, treat='d', estimand='ATE', method='URI')
wz <- lmw(~xc+d+z+z:d, treat='d', estimand='ATE', method='URI')

#   practically the same
cor(wx$weights, wz$weights) |> round(3)
plot(wx$weights, wz$weights, asp=1, las=1, col=grey(0,0.5))
abline(a=0, b=1, col='red')

#   comparing the D-residuals gives the same pic
dresx <- lm(d~xc)$resid
dresz <- lm(d~xc+z+z:d)$resid

cor(dresx, dresz) |> round(3)

plot(dresx, dresz, asp=1, las=1, col=grey(0,0.5))
abline(a=0, b=1, col='red')


#   SCRIPT END
