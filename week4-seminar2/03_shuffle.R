#   SGIA40F15 23/24 
#       Seminar 02
#           The marginals and the joint 
#   2024-01-24
#   Juraj Medzihorsky

rm(list=ls())

#   simulate data
N <- 1e2
set.seed(124)
y0 <- rnorm(N) |> scale()
y1 <- 1 + rnorm(N) |> scale()
tau <- y1 - y0


mean(y1) 
mean(y0)
mean(y1)-mean(y0)
summary(tau)
var(tau)

#   now create data with the exact same marginals, 
#   but much less variance in ITE
a0 <- sort(y0)
a1 <- sort(y1)

#   same ATE
mean(y1-y0)
mean(a1-a0)

#   but very different var(ITE)
sd(y1-y0)^2 |> round(2)
sd(a1-a0)^2 |> round(2)

#   and another dataset, with an moderate negative
#   correlation between Y(0) an Y(1)
u <- rnorm(N) |> scale()
u0 <- u*-1.0 + rnorm(N) |> scale()
u1 <- u*+1.0 + rnorm(N) |> scale()
cor(u0,u1) |> round(2)

b0 <- sort(y0)[rank(u0)] 
b1 <- sort(y1)[rank(u1)] 

cor(b0,b1) |> round(2)


#   same ATE
mean(y1-y0)
mean(a1-a0)
mean(b1-b0)

#   but very different var(ITE)
sd(y1-y0)^2 |> round(2)
sd(a1-a0)^2 |> round(2)
sd(b1-b0)^2 |> round(2)


par(mfrow=c(2,2), mar=rep(0.5,4), oma=rep(1,4), lwd=0.5)
xyl <- range(c(y0,y1))
plot.new() ; plot.window(xlim=xyl,ylim=xyl,asp=1) ; box()
points(y0, y1, pch=16, col=grey(0.4))
plot.new() ; plot.window(xlim=xyl,ylim=xyl,asp=1) ; box()
points(a0, a1, pch=16, col='red3')
plot.new() ; plot.window(xlim=xyl,ylim=xyl,asp=1) ; box()
points(b0, b1, pch=16, col='blue3')



#   SCRIPT END
