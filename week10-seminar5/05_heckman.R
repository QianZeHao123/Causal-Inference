#   SGIA40F15 23/24 
#       Seminar 05
#           Heckman correction 
#   2024-03-11
#   Juraj Medzihorsky

rm(list=ls())

#   install.packages('sampleSelection')
#   install.packages('GJRM')
library(sampleSelection)
library(GJRM)
library(MASS)

#   simulate the data
#   What's different now is that D will be continuous. This
#   is because it's easier to show it that way. We won't
#   simulate the potential outcomes explicitely as we
#   otherwise do in the seminars. Rather, we will think of
#   an equation as a generator function Y(D=d).
N <- 4e2
set.seed(32123)
mu <- c(0,0)
Sigma <- matrix(c(1,-0.9,-0.9,1), 2, 2)
eps <- mvrnorm(N, mu, Sigma) |> scale() 
d <- (0.5*runif(N, -1, 1)) |> scale(scale=F) |> as.vector()
z <- rnorm(N, 0, 0.5) |> scale(scale=F) |> as.vector()
#   selection stage
sel <- 0 < (1.0 + z -d*4 + eps[,1])
#   now the Y(D=d) generator
    b <- 4
y_D <- b*d  + eps[,2]
#   finally, the observed y
y <- ifelse(sel, y_D, NA) 
#   #   Let's plot what is observed and what is not
#   #   {D, Y(D)}
#   par(mar=c(5,5,1,1), las=1, mfrow=c(1,1))
#   plot(d, y_D, pch=ifelse(sel, 16, 1), col='blue3')
#   legend('top', horiz=T, bty='n', pch=c(16,1), 
#          legend=c('observed', 'missing'))
#   abline(a=0, b=b, col='red')
#   abline(lm(y ~ d), col='pink3', lty=3, lwd=2)
#   #   now check the naive ATE estimate
#   #   that's just the LATE for the observed
#   lm(y ~ 1 + d)$coef['d'] |> round(2)
#   lm(y_D ~ 1 + d)$coef['d'] |> round(2)
#   #   sample selection model ('Tobit 2', the original Heckman)
#   #   as implemented in sampleSelection
#   mod_s1 <- selection(sel~1+d+z, y~1+d)
#   coef(mod_s1) |> {\(x) x[which(names(x)%in%'d')[2]]}() |>
#       print(digits=2)


#   let's go step-by-step to understand what have we
#   generated
#   eps[,] are the two vectors of unobservables, they are
#   (negatively) correlated; the causal intepretation of
#   that is that there are some unobserved things that
#   affect both Y and the selection into reporting/observing 
#   Y, in this case in opposite ways
head(eps) |> round(2)
cor(eps) |> round(2)

par(mar=c(5,5,1,1), las=1, mfrow=c(1,1))
plot(eps, asp=1, col='blue3',
     xlab='Selection-stage unobservables',
     ylab='Outcome-stage unobservables')

#   sel is a logical that states whether Y_D was reported as
#   Y or the value is not observed
table(sel)

#   Let's plot what is observed and what is not
#   {D, Y(D)}: How the missing cases are pulling the
#   estimate
par(mar=c(5,5,1,1), las=1, mfrow=c(1,1))
plot(d, y_D, pch=ifelse(sel, 16, 1), col='blue3')
legend('top', horiz=T, bty='n', pch=c(16,1), 
       legend=c('observed', 'missing'))
abline(lm(y_D ~ d), col='red2', lty=3, lwd=2)
abline(lm(y ~ d), col=grey(0.2), lty=3, lwd=3)

#   {Z, Y(D)}: How the selection works 
par(mar=c(5,5,1,1), las=1, mfrow=c(1,1))
plot(z, y_D, pch=ifelse(sel, 16, 1), col='purple')
legend('top', horiz=T, bty='n', pch=c(16,1), 
       legend=c('observed', 'missing'))

#   The ITE are not stated explicitely, because eps[,2]
#   contains both the unobservables and the individual
#   variation around the ATE

#   check the true sample ATE by fitting the true model: +4
lm(y_D ~ 1 + d)$coef['d'] |> round(2)
#   LATE for observed: +3: the estimate on observed-only
lm(y_D ~ 1 + d, subset=sel)$coef['d'] |> round(2)
#   LATE for unobserved: +2 
lm(y_D ~ 1 + d, subset=!sel)$coef['d'] |> round(2)

#   How come? The missing cases have a higher average Y,
#   i.e. the intercept
par(mar=c(5,5,1,1), las=1, mfrow=c(1,1))
plot(d, y_D, pch=ifelse(sel, 16, 1),
     col=ifelse(sel, 'darkorange3', 'blue4'))
abline(lm(y_D ~ d), col='black', lty=1, lwd=1)
abline(lm(y_D ~ d, subset=sel), col='darkorange3', lty=3, lwd=2)
abline(lm(y_D ~ d, subset=!sel), col='blue4', lty=2, lwd=2)


#   sample selection model ('Tobit 2', the original Heckman)
#   as implemented in sampleSelection
mod_s1 <- selection(sel~1+d+z, y~1+d)
mod_s1 |> summary() |> print(digits=2)

#   just the estimate of interest: not bad
summary(mod_s1)$estimate[5,1:3] |> round(2)

#   using GJRM
#   generalized joint response model, Heckman's special case

mod_g1 <- gjrm(list(sel~1+d+z, y~1+d), model='BSS',
               gamlssfit=T, extra.regI='t', 
               margins=c('probit', 'N'), copula='N')  
#   same thing
mod_g1 |> summary() |> print(digits=2)


#   Heckman's original two-step procedure by hand
#   well, almost, we will use the convenient invMillsRatio()
#   from sampleSelection
#   The IMR is the ratio of a PDF over 1-CDF where both the
#   PDF and the CDF are of the same distribution

mod_sel <- glm(sel ~ 1 + d + z, family=binomial('probit')) 

imr <- invMillsRatio(mod_sel)

#   IMR1 is the thing we want
#   from the manual:
#       If a univariate probit estimation is provided, the
#       variables ‘IMR1’ and ‘IMR0’ are the Inverse Mill's
#       Ratios to correct for a sample selection bias of y =
#       1 and y = 0, respectively. Accordingly, 'delta1' and
#       'delta0' are the corresponding delta values.
imr |> str()

#   and now we plug it into the outcome model ... instead of
#   the intercept
mod_out <- lm(y ~ -1 + d + imr$IMR1)

#   not bad at all
mod_out$coef['d'] |> round(2)


#   now we can check what the IMR as control does
#   we can't use LMW as the D is not categorical
#   but we can still check the residuals
res0 <- lm(y ~ 1 + d) |> resid()
res1 <- lm(y ~ -1 + d + imr$IMR1) |> resid() 

#   almost the same thing, and yet
cor(res0, res1) |> round(2)

(res1-res0) |> summary()

par(mar=c(5,5,1,1), las=1, mfrow=c(1,1))
plot(res0, res1, pch=1, col='brown', asp=1)
abline(a=0, b=1, lty=3)

par(mar=c(5,5,1,1), las=1, mfrow=c(1,1))
plot(res1-res0, na.omit(y), pch=1, col='green4', ylab='Y')
abline(h=0, lty=3)

#   let's compare the real "residuals"
par(mar=c(5,2,5,2), las=1, mfrow=c(3,1))
K <- 16
xl <- c(-4,4)
plot(density(eps[,2]), col='blue2', lwd=2,
     frame=F, yaxt='n', xlim=xl,
     xlab='', ylab='', main='All')
plot(density(eps[sel,2]), col='blue4',  lwd=2,
     frame=F, yaxt='n', xlim=xl,
     xlab='', ylab='', main='Observed')
plot(density(eps[!sel,2]), col='black',
     frame=F, yaxt='n', xlim=xl,
     xlab='', ylab='', main='Missing')

#   ________________________________________________________

