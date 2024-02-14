#   SGIA40F15 23/24 
#       Seminar 02
#           Frisch-Waugh-Lovell Theorem
#   2023-01-28
#   Juraj Medzihorsky

rm(list=ls())

library(MASS) # for simulating draws from the mv normal

#   ________________________________________________________
#       Example 2
#   now suppose there is a binary treatment D and an 
#   simulate the data
#   there is no caucastion explicitely simulated here
N <- 4e1
set.seed(19740120)
S <- matrix(c(1,0.6,0.6,1), ncol=2)
X <- mvrnorm(N, c(0,0), S) 
e <- rnorm(N, 0, 1) |> scale()
y <- 1 + X %*% c(-2,0.5) + e

#   OLS fit
lm(y ~ 1 + X) |> coef() |> round(digits=3)

#   residuals from
#       Y ~ X2
#       X1 ~ X2
yres <- lm(y ~ 1 + X[,2]) |> resid() 
xres <- lm(X[,1] ~ 1 + X[,2]) |> resid() 

#   residuals ~ residuals regression
lm(yres ~ 1 + xres) |> coef() |> round(digits=3)
#   xres coef above is the same as the X1 coef here
lm(y ~ 1 + X) |> coef() |> round(digits=3)


#   ________________________________________________________
#       Example 2
#   now suppose there is a binary treatment D and an 
#   observed confounder u
#   simulate new data

rm(list=ls())

N <- 1e2
u <- runif(N, -1, 1) |> scale()
ee <- mvrnorm(N, c(0,0), diag(2)) |>
    svd() |> (\(x) x$u)() |> apply(2, scale)
y0 <- ee[,1]
y1 <- u + ee[,2] + 1
tau <- y1 - y0
d <- u |> plogis(0, 1) |> sapply((\(x) rbinom(1,1,x)))
y <- ifelse(d%in%0, y0, y1)

#   OLS fits with and without controls
lm(y ~ 1 + d) |> coef() |> round(3)
lm(y ~ 1 + d + u) |> coef() |> round(3)

#   residuals from the two regressions
#       Y ~ U
#       D ~ U
yres <- lm(y ~ 1 + u) |> resid()
dres <- lm(d ~ 1 + u) |> resid()

#   residuals on residuals fit
#   again, the res ~ res coef is the same as the D coef
#   above in Y ~ D + U
lm(yres ~ 1 + dres) |> coef() |> round(3)

#   and here's the transformation
res <- lm(y ~ 1 + d + u) |> resid()
rres <- lm(yres ~ 1 + dres) |> resid()

#   the ratio of the difference between the Y~D+U residuals
#   and the res~res residuals over the D~U residuals is 
#   constant
(yres-rres)/dres


#   what do the individual treatment effects among the
#   treated only look like?
mean(tau[d%in%1])

#   let's do an IPW-weighted OLS fit

#   we need a function that will transform the PS = P[D_i=1]
#   into the IPW; remember that
#   if D=0 IPW_i = 1/P[D=0] = 1/(1-PS_i)
#   if D=1 IPW_i = 1/P[D_i=1] = 1/PS_i
ps2ipw <- (\(x) ifelse(d%in%0, 1/(1-x), 1/x))

#   now fit a Binomial-logistic GLM to get the PS and 
#   transform them to the IPWs
ipw <- glm(d ~ 1 + u, binomial) |> fitted() |> ps2ipw()
    
#   weighted OLS fit without U in the formula gives the
#   exact same estimate for D because the confounding by U
#   enters through the IPWs
lm(y ~ 1 + d, weights=ipw)$coef['d'] |> round(3)
#   compare with the simple IPW
lm(y ~ 1 + d + u)$coef['d'] |> round(3)

#   ________________________________________________________
#   Aronow-Samii weights
#   those weights apply if we have the right (well, close
#   enough) model
#   then the contribution of an observation's individual
#   treatment effect to the regression coefficient for the
#   treatment will be proportional to the square of the 
#   weights, which in turn is just the residual from the
#   allocation regression D~controls, but this only works
#   asymptotically, that is as sample sizes get large

#   the wieghts are just the square of the allocation 
#   residuals from D~U, i.e., treatment~controls regression
w <- dres^2

par(mar=c(5,5,1,1), las=1)
plot(u, w, lwd=2, type='n')
text(u, w, d, font=2, col=ifelse(d, 'red', 'blue'), 
     xlab='Confounder U', ylab='Aronow-Samii weights')
#


#   and these are the weighted ITEs
mean(w*tau)/mean(w)

#   quite far from the estimate
lm(y ~ 1 + d + u)$coef['d'] |> round(3)

#   the sample size is rather small,, N=100
N

#   so let's try simulating the same with  

onerun <-
    function(N=1e3)
    {
        u <- runif(N, -1, 1) |> scale() |> as.vector()
        ee <- mvrnorm(N, c(0,0), diag(2)) |>
            svd() |> (\(x) x$u)() |> apply(2, scale)
        y0 <- u + ee[,1]
        y1 <- u + ee[,2] + 1
        tau <- y1 - y0
        d <- u |> plogis(0, 1) |> sapply((\(x) rbinom(1,1,x)))
        y <- ifelse(d%in%0, y0, y1)
        #   est_00 <- lm(y ~ 1 + d) |> coef() |> (\(x) x['d'])()
        est_lm <- lm(y ~ 1 + d + u) |> coef() |> (\(x) x['d'])()
        w <- lm(d ~ 1 + u) |> resid() |> (\(x) x^2)()
        ipw <- glm(d ~ 1 + u, binomial) |> fitted() |> 
            (\(x) ifelse(d%in%0, 1/(1-x), 1/x))()
        est_ipw <- lm(y ~ 1 + d, weights=ipw)$coef['d']
        tau_as <- mean(w*tau)/mean(w)
        att <- mean(tau[d%in%1])
        return(c('att'=att, 'tau_as'=tau_as,
                 'est_lm'=est_lm, 'est_ipw'=est_ipw)) 
    }

onerun(1e3)

#   let's get B replicates at different sample sizes
B <- 4e3
set.seed(1000)
sample_sizes <- sample(4e1:1e3, B, replace=T) 
system.time(r <- do.call(rbind, lapply(sample_sizes, onerun)))

str(r)

#   is the Aronow-Samii tau converging to the true ATE=1?
#   it kinda is ....
par(mar=c(5,4,2,1), las=1, mfrow=c(1,1))
plot(sample_sizes, r[,2], col=rgb(0,0,1,0.5),
     xlab='Sample Size', ylab='',
     main='Aronow-Samii weighted average of ITE')
abline(h=1, col='red', lwd=2)

#   are the OLS coefs and the AS tau average converging?
#   kinda
par(mar=c(5,4,2,1), las=1, mfrow=c(1,1))
plot(sample_sizes, r[,3]-r[,2], col=rgb(0,0,1,0.5),
     xlab='Sample Size', ylab='', Main='OLS coeff - AS tau')
abline(h=0, col='red', lwd=2)

#   and what about the estimates? about the same
par(mfrow=c(2,1), mar=c(5,5,2,1), las=1)
plot(sample_sizes, r[,3], col=rgb(0,0,1,0.5), ylim=c(0,2),
     xlab='', ylab='', 
     main='OLS with direct controling')
abline(h=1, col='red', lwd=2)
plot(sample_sizes, r[,4], col=rgb(0,0,1,0.5), ylim=c(0,2),
     xlab='Sample Size', ylab='', 
     main='OLS with IPW')
abline(h=1, col='red', lwd=2)



#   SCRIPT END
