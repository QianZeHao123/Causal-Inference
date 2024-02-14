#   SGIA40F15 23/24 
#       Juraj Medzihorsky
#   Seminar 1
#   2024-01-13

rm(list=ls())

#   Let's simulate a  more complex setting with selection bias from
#   selection into treatment


N <- 4e1
set.seed(5600)
xb <- rnorm(N, 0, 1) |> scale()
x0_ <- rnorm(N, 0, 1) 
x1_ <- rnorm(N, 0, 1)   
#   decorrelate x0 and x1, scale them
aux <- svd(cbind(x0_, x1_))
x0 <- aux$u[,1] |> scale(center=T, scale=T)  
x1 <- aux$u[,2] |> scale(center=T, scale=T)  
#   check
cor(x0_, x1_)
cor(x0, x1)

y0 <- x0 + xb
y1 <- x1 + xb + 1

cor(y0, y1)

#   now let's make selection into treatment depend on xb
pd <- pnorm(xb)

curve(pnorm, xlim=c(-1,1)*3, col='red', las=1)

#   draw a random beta, then sort them and the first 20 go into D=0
set.seed(732)
pb <- sapply(pd, function(i) rbeta(1, i*2, (1-i)*2))
d_a <- ifelse(rank(pb)<=2e1, 0, 1)

plot(y0, y1, pch=16, asp=1,
     col=ifelse(d_a%in%0, rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

y_a <- ifelse(d_a%in%0, y0, y1)

#   estimate the ate
mean(y_a[d_a%in%1]) - mean(y_a[d_a%in%0])

#   quite an over-estimate

#   now let's repeat it many times for different sample sizes as before

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
        b_ <- sapply(pnorm(xb), function(i) rbeta(1, i*2, (1-i)*2))
        d_ <- ifelse(rank(b_)<=n0, 0, 1)
        y_ <- ifelse(d_%in%0, y0, y1)
        ate_ <- mean(y_[d_%in%1]) - mean(y_[d_%in%0])
        return(ate_)
    }

set.seed(657)
s_ <- sample(5:6e2)*2
er <- sapply(s_, oneexp) 


# increasing the sample size won't help
par(mar=c(5,5,3,1), las=1)
plot(er, s_, pch=16, col=rgb(0,0,1,0.5), xlim=c(0, 3),
     xlab='Estimated ATE', ylab='Sample Sizes')
    abline(v=0, lty=3, lwd=0.5)
    abline(v=1, col='red3', lwd=0.5)



#   SCRIPT END
