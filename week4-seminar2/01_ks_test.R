#   SGIA40F15 23/24 
#       Seminar 02
#           Same and yet different        
#   2024-01-24
#   Juraj Medzihorsky

rm(list=ls())

#   let's simulate data in which the ATE is 0, but
#   Y(0) and Y(1) look rather different.
N <- 2e3
set.seed(2024)
y0 <- rlnorm(N,0,1) |> scale()
y1 <- rt(N, df=20) |> scale()
d <- sample(rep(0:1, c(1e3,1e3)), replace=F)
y <- ifelse(d%in%0, y0, y1)

par(mfrow=c(2,1), las=1)
hist(y0, col='lightblue4', breaks=2e1)
hist(y1, col='lightblue4', breaks=2e1)

#   ATE
mean(y1-y0) |> round(3)
#   ATE hat     
(mean(y[d%in%1]) - mean(y[d%in%0])) |> round(3)

t.test(y[d%in%0], y[d%in%1]) |> print(digits=2)

lm(y ~ 1 + d) |> summary() |> print(digits=3)

#   so, no difference ...
#   in means, but ... the distributions look different

#   non-parametric density estimates
par(mfrow=c(1,1), las=1)
plot(density(y[d%in%0]), col='blue3', xlim=range(y),
     main='Gaussian KDE', ylab='')
lines(density(y[d%in%1]), col='red3')

#   another way is to look at the empirical CDFs
par(mfrow=c(1,1), las=1)
plot.new()
plot.window(xlim=range(y), ylim=c(0,1))
axis(1) ; axis(2) ; mtext('Empirical CDFs')
plot(ecdf(y[d%in%0]), add=T, col='blue3')
plot(ecdf(y[d%in%1]), add=T, col='red3')


#   KS rejects the null that these two samples
#   come from the same distribution. It summarizes
#   their difference with a single number, the test
#   statistic D, which measures the largest absolute
#   vertical distance between the two empirical CDFs.
ks.test(y[d%in%0], y[d%in%1], simulate.p.value=T)


#   SCRIPT END
