#   SGIA40F15 23/24 
#       Seminar 04
#            RDD minimal example
#   2024-02-23
#   Juraj Medzihorsky

rm(list=ls())

#   simulate the data, extremely simple setup
N <- 2e2
set.seed(19740330)
x <- runif(N, -2, 4)
u0 <- rnorm(N) |> scale()
u1 <- 1 + rnorm(N) |> scale()
y0 <- x*0.5 + u0
y1 <- x*0.5 + u1
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

#   just Y ~ X; ok fit without D too
lm(y~1+x) |> summary() 


#   yet, if we could see the potential outcomes
plot(x, y0, las=1, pch=16, col=col0)
points(x, y1, pch=18, col=col1)


#   simplest RDD estimate
#   first, we center X on the cutoff
xc <- x-cutoff

#   now just add D
#   pretty close
lm(y ~ 1 + xc + d) |> summary()

#   since we simulated the data, we know that the X-slope
#   is the same in D=0 and D=1
#   allowing it to vary by adding an interaction between
#   Xc and D doesn't change much
lm(y ~ 1 + xc + d + xc:d) |> summary()


#   we haven't yet tried bandwidth selection or anything 
#   else that's expected, see the next scripts

#   SCRIPT END
