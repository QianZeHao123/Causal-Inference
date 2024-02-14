#   SGIA40F15 23/24 
#       Juraj Medzihorsky
#   Seminar 1
#   2024-01-13

rm(list=ls())

#   Let's simulate a randomized experiment.

#   The size of the experimental population                                    
N <- 4e1
set.seed(123)
xb <- rnorm(N, 0, 2)
xe <- rnorm(N, 0, 1) |> scale()
#   make the potential outcomes
y0 <- xb
y1 <- y0 + 1 + xe

cor(y1, y0)

par(mar=c(5,5,1,1), las=1)
plot(y0, y1, pch=16, col=rgb(0,0,1,0.5), frame=F)
abline(a=0, b=1, col='pink3', lty=3)

#   individual treatment effect
ite <- y1 - y0
#   average treatment effect
ate <- mean(ite)

#   ite summary
summary(ite)
#   proportion negative
mean(ite<0)

#   now simulate randomized treatment assignment
#   20 into control (D=0) and 20 into treatment (D=1)
set.seed(2314)
d_a <- sample(rep(c(0,1), c(2e1,2e1)), N, replace=F)
#   inspect
table(d_a)
#   generate the observed outcome y_a
y_a <- ifelse(d_a%in%0, y0, y1)

#   now compute the sample ATE
mean(y_a[d_a%in%1]) - mean(y_a[d_a%in%0])
#   this is of course the same as
lm(y_a ~ 1 + d_a)

#   so, this experiment generates an underestimate
#   now let's repeat the experiment 100 times

onexp <-
    function(y0_=y0, y1_=y1)
    {
        n_ <- length(y0_)
        n0 <- round(n_/2) 
        n1 <- n_ - n0
        d_ <- sample(rep(c(0,1), c(n0,n1)), n_, replace=F)
        y_ <- ifelse(d_%in%0, y0_, y1_)
        mean(y_[d_%in%1]) - mean(y_[d_%in%0])
    }

#   test it: reproduce the experiment above
set.seed(2314)
onexp()

#   now rerun it a 100 times
set.seed(2314)
ee <- replicate(1e2, onexp())
#   inspect the estimated ATEs
summary(ee)
#   that is all over the place

#   now rerun the experiment 100 times under different sample sizes
#   with the true ATE=1
#   first define a function that simulates the potential outcomes and
#   then executes K experiments

manyexps <-
    function(N, K=1e2)
    {
        xb <- rnorm(N, 0, 2)
        xe <- rnorm(N, 0, 1) |> scale()
        y0 <- xb
        y1 <- y0 + 1 + xe
        ate_ <- replicate(K, onexp(y0_=y0, y1_=y1))
        return(ate_)
    }

#   test
set.seed(123)
manyexps(4e1, 1e2) |> summary()

#   use these four sample sizes
s_sizes <- c(6, 12, 24, 48)^2

#   simulate the 5 x 1e2 experiments
set.seed(6105)
em <- lapply(s_sizes, manyexps, K=1e2)

#   inspect
sapply(em, mean)
sapply(em, sd)

#   let's plot this
par(mar=c(5,5,3,1), las=1)
plot.new()
plot.window(xlim=c(-1,3), ylim=c(1,4)) 
    mtext('4x100 Simulated Experiments', side=3, font=2)
    mtext('Estimated ATE', side=1, line=3)
    abline(v=0, lty=3, lwd=0.5)
    abline(v=1, col='red3', lwd=0.5)
    axis(1)
    axis(2, 1:4, s_sizes)
for (i in 1:4) {
    points(em[[i]],
           y=rep(i, length(em[[i]])), pch='|', col=rgb(0,0,1,0.5))
}


#   the sqrt(N) rule at work:
sd(em[[1]]) / sd(em[[2]])
sqrt(s_sizes[2]) / sqrt(s_sizes[1])

sd(em[[2]]) / sd(em[[3]])
sqrt(s_sizes[3]) / sqrt(s_sizes[2])

sd(em[[3]]) / sd(em[[4]])
sqrt(s_sizes[4]) / sqrt(s_sizes[3])


#   now let's repeat it 10,000 times with random even sample size

set.seed(675)
s_ <- sample(5:6e2)*2
er <- sapply(s_, manyexps, K=1)

par(mar=c(5,5,3,1), las=1)
plot(er, s_, pch=16, col=rgb(0,0,1,0.5),
     xlab='Estimated ATE', ylab='Sample Sizes')
    abline(v=0, lty=3, lwd=0.5)
    abline(v=1, col='red3', lwd=0.5)








#   SCRIPT END

#   ----------------------------------------------------------------------------
#   side note:
#       the average of a difference is the same as the difference of the 
#       averages, but the same doesn't hold for quantiles
    mean(y1-y0) == mean(y1) - mean(y0)
    median(y1-y0) == median(y1) - median(y0)
#   ----------------------------------------------------------------------------
