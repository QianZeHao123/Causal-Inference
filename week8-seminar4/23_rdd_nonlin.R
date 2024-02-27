#   SGIA40F15 23/24 
#       Seminar 04
#            RDD minimal example with a GAM
#   2024-02-24
#   Juraj Medzihorsky

library(mgcv)
library(rdrobust)

rm(list=ls())

#   simulate the data, extremely simple setup
N <- 2e2
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


#   plot it
#   just by looking at it, it doesn't seem that much
col0 <- rgb(1,0,0,0.8)
col1 <- rgb(0,0,1,0.8)
pcol <- ifelse(d%in%0, col0, col1)
plot(x, y, las=1, pch=16, col=pcol)

#   yet, if we could see the potential outcomes
plot(x, y0, las=1, pch=16, col=col0)
points(x, y1, pch=18, col=col1)
abline(v=cutoff, lty=3)

#   simplest RDD estimate
#   first, we center X on the cutoff
xc <- x-cutoff

#   now just add D
#   not good at all
lm(y ~ 1 + xc + d)$coeff['d'] |> round(2)

#   different slopes, perhaps?
#   not really
lm(y ~ 1 + xc + d + xc:d)$coeff['d'] |> round(2)

#   3-rd degree polynomial?
#   there it is
lm(y ~ 1 + xc + I(xc^2) + I(xc^3) + d)$coeff['d'] |> round(2)

#   even more complex
#   happens to be worse
lm(y ~ 1+xc+I(xc^2)+I(xc^3)+d+ d:xc+d:I(xc^2)+d:I(xc^3))$coeff['d'] |> round(2)

#   now a GAM
#   not bad
gam(y ~ 1 + s(xc) + d) |> coef() |> {function(x) x['d']}() |> round(2)

#   there's many different splines
#   let's change the default (thin plate) to Gaussian 
#   Process, just for illustration
gam(y ~ 1 + s(xc, bs='gp') + d) |> coef() |> {function(x) x['d']}() |> round(2)

#   robust local polynomial RDD from rdrobust
rob <- rdrobust(y=y, x=x, c=cutoff)

#   integrated bandwidth selection
#   robust SE as an option
rob |> summary()

#   get the coeff
rob |> str()

#   three coeffs
rob$coef

#   as usuall, lets simulate

onerun <- 
    function(N=2e2)
    {
        x <- runif(N, -2, 4)
        u0 <- rnorm(N) |> scale()
        u1 <- 2 + rnorm(N) |> scale()   #   ATE is 2!
        y0 <- 0.5*x - 0.3*x^2 + 0.2*x^3 + u0
        y1 <- 0.5*x - 0.3*x^2 + 0.2*x^3 + u1
        cutoff <- 1
        d <- as.numeric(x>=cutoff) 
        y <- ifelse(d%in%0, y0, y1)
        xc <- x-cutoff
        b1 <-lm(y ~ 1 + xc + d)$coeff['d'] 
        b2 <- lm(y ~ 1 + xc + d + xc:d)$coeff['d'] 
        b3 <- lm(y ~ 1 + xc + I(xc^2) + I(xc^3) + d)$coeff['d'] 
        b4 <- lm(y ~ 1+xc+I(xc^2)+I(xc^3)+d+ d:xc+d:I(xc^2)+d:I(xc^3))$coeff['d'] 
        b5 <- gam(y ~ 1 + s(xc, bs='tp') + d) |> coef() |> {function(x) x['d']}() 
        b6 <- gam(y ~ 1 + s(xc, bs='gp') + d) |> coef() |> {function(x) x['d']}() 
        rr <- rdrobust(y=y,x=x,c=cutoff)$coef |> as.vector()
        b7 <- rr[1]
        b8 <- rr[2]
        b9 <- rr[3]
        return(c('lin'=b1, 'dlin'=b2, 'poly'=b3, 
                 'dpoly'=b4, 'gam_tp'=b5, 'gam_gp'=b6,
                 'rob_c'=b7, 'rob_bc'=b8, 'rob_r'=b9))
    }

onerun(4e2) |> round(2)


#   500 simulations at a higher sample size
B <- 5e2
set.seed(30034791)
system.time(
    r <- replicate(B, onerun(N=6e2)) |> t()
    )

summary(r) |> print(digits=3)

#   how bad is the bias?
#   the true value is always 2
colMeans(r-2) |> round(3)

#   how is the variance?
apply(r-2, 2, var) |> round(3)

#   lm polynomials have done pretty well, not surprising
#   that is the true model
#   robust local polynomial RDD didn't do bad either,
#   though the cost is in higher variance
#   perhaps, with more data and some more tunable spline
#   a GAM could approximate this performance

#   we haven't yet tried bandwidth selection or anything 
#   else that's expected, see the next scripts

#   SCRIPT END

