#   SGIA40F15 23/24 
#       Juraj Medzihorsky
#   Seminar 1
#   2024-01-13

rm(list=ls())

#   Let's simulate a  more complex setting with stratified random sampling

#   same as before
N <- 4e1
set.seed(5600)
xb <- rnorm(N, 0, 1) |> scale()
x0_ <- rnorm(N, 0, 1) 
x1_ <- rnorm(N, 0, 1)   
#   decorrelate x0 and x1, scale them
aux <- svd(cbind(x0_, x1_))
x0 <- aux$u[,1] |> scale(center=T, scale=T)  
x1 <- aux$u[,2] |> scale(center=T, scale=T)  
#   the potential outcomes
y0 <- x0 + xb
y1 <- x1 + xb + 1


#   now stratify treatment assignment based on the quartile of x1
q1 <- quantile(x1, c(0.25, 0.5, 0.75, 1.0))
s1 <- cut(x1, c(-1e3, q1)) |> as.numeric()
#   inspect
table(s1)

#   now sample into D=1 so that within each stratum it's 5:5 (50:50)
srs <-
    function(x_)
    {
        q_ <- quantile(x_, c(0.25, 0.5, 0.75, 1.0))
        s_ <- cut(x_, c(-1e6, q_)) |> as.numeric()
        t_ <- table(s_)
        v <- names(t_)
        l <- length(t_)
        n <- t_
        n0 <- round(n/2)
        n1 <- n - n0
        d_ <- rep(NA, sum(n)) |> as.numeric()
        for (i in 1:l) {
            d_[s_%in%v[i]] <- sample(rep(0:1, c(n0[i],n1[i])), n[i], replace=F)
        }
        return(d_)
    }

#   test
set.seed(132)
d_a <- srs(x1)
set.seed(675)
d_b <- srs(x1)

table(d_a, s1)
table(d_b, s1)
table(d_a, d_b)


#   now compute the two ate estimates
y_a <- ifelse(d_a%in%0, y0, y1)
y_b <- ifelse(d_b%in%0, y0, y1)
#   not bad
mean(y_a[d_a%in%1]) - mean(y_a[d_a%in%0])
mean(y_b[d_b%in%1]) - mean(y_b[d_b%in%0])


#   now try the same many times for different sample sizes
#   and compare simple randomized and stratified randomized assignment


oneexp <-
    function(N)
    {
        xb <- rnorm(N, 0, 1) |> scale()
        x0_ <- rnorm(N, 0, 1) 
        x1_ <- rnorm(N, 0, 1)   
        aux <- svd(cbind(x0_, x1_))
        x0 <- aux$u[,1] |> scale(center=T, scale=T)  
        x1 <- aux$u[,2] |> scale(center=T, scale=T)  
        y0 <- x0 + xb
        y1 <- x1 + xb + 1
        n0 <- round(N/2) 
        n1 <- N - n0
        d_a <- sample(rep(0:1, c(n0,n1)), N, replace=F)
        d_b <- srs(x1)
        y_a <- ifelse(d_a%in%0, y0, y1)
        y_b <- ifelse(d_b%in%0, y0, y1)
        ate_a <- mean(y_a[d_a%in%1]) - mean(y_a[d_a%in%0])
        ate_b <- mean(y_b[d_b%in%1]) - mean(y_b[d_b%in%0])
        return(c(ate_a, ate_b))
    }

set.seed(657)
s_ <- sample(5:2e2, 4e3, replace=T)*2
er <- sapply(s_, oneexp) |> t() 

head(er)

summary(er)


# increasing the sample size won't help
par(mfrow=c(2,1), mar=c(5,5,3,1), las=1)
plot(er[,1], s_, pch=16, col=rgb(0,0,1,0.5), xlim=c(-1, 3),
     main='Simple Random Sampling',
     xlab='Estimated ATE', ylab='Sample Sizes')
    abline(v=0, lty=3, lwd=0.5)
    abline(v=1, col='red3', lwd=0.5)
plot(er[,2], s_, pch=16, col=rgb(0,0,1,0.5), xlim=c(-1, 3),
     main='Stratified Random Sampling',
     xlab='Estimated ATE', ylab='Sample Sizes')
    abline(v=0, lty=3, lwd=0.5)
    abline(v=1, col='red3', lwd=0.5)


#   let's show it a little bit clearer

dev <- (er-1)^2
ns_ <- data.frame(s_=1e1:4e2)

mod_1 <- glm(dev[,1] ~ 1 + poly(s_, 3), family=quasi(variance='mu', link='log'))
mod_2 <- glm(dev[,2] ~ 1 + poly(s_, 3), family=quasi(variance='mu', link='log'))
dev_1 <- sqrt(exp(predict(mod_1, newdata=ns_, type='link'))) 
dev_2 <- sqrt(exp(predict(mod_2, newdata=ns_, type='link'))) 

par(mfrow=c(1,1), mar=c(5,5,1,1), las=1)
plot(ns_[,1], dev_1, type='l', col='red3', frame=F,
     xlab='Sample Size', ylab='Square root of squared deviation')
lines(ns_[,1], dev_2, col='blue3', lty=2)
legend('topright', lty=c(1,2), col=c('red3', 'blue3'),
       legend=c('Simple Random Sampling', 'Stratified Random Sampling'))

#   SCRIPT END    

