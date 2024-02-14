#   SGIA40F15 23/24 
#       Juraj Medzihorsky
#   Seminar 1
#   2024-01-14

rm(list=ls())

#   Matching

N <- 4e1
set.seed(620)
# stratum
K <- 6
s <- sample(1:K, N, replace=T)
sx <- runif(K, -1, +1)
x1 <- sx[s] + pnorm(0, 1) 
ite <- 1 + x1 |> scale()
x0 <- rnorm(N, 0, 1) |> scale()
xb <- rnorm(N, 0, 1) |> scale()
#   potential outcomes
y0 <- xb + x0 
y1 <- xb + ite
#   selection into treatment
pd <- plogis(x1)
pd[ s %in% sample(1:K,2,replace=F) ] <- 0
d_o <- sapply(pd, function(i) rbinom(1, 1, i))
#   observed outcome
y_o <- ifelse(d_o%in%0, y0, y1)

#   naive mean difference
mean(y_o[d_o%in%1]) - mean(y_o[d_o%in%0])

plot(y0, y1, asp=1, col=ifelse(d_o%in%0, 'red3', 'blue3'))
abline(a=0, b=1, col=grey(0.2), lty=3)


#   suppose we know that treatment assignment relates to the strata
#   we can observe S but not xs
#   now we want to estimate the ATT: average treatment effect on the treated

#   plot
par(mar=c(5,5,2,1), las=1)
plot(s, y_o, 
     lwd=2, xlab='Stratum', ylab='Y',
     pch=ifelse(d_o%in%0, 0, 1), 
     col=ifelse(d_o%in%0, rgb(0,0,1,0.5), rgb(1,0,0,0.5)))

#   now suppose we match 1:1 so that only those units who have an exact match
#   in S stay

#   we can do this in many ways
#   including randomization
#   from each stratum, only as many individuals will stay as is the number of
#   those from the group that is the least common in the stratum

#   there are:
table(d_o, s)
#   there will be from each group
s_n <- apply(table(d_o, s), 2, min)
rbind(s_n, s_n)
#   now sample them
#   prepare a grid
g <- as.data.frame(table(s, d_o))
g$n <- s_n[g$s]
g

#   helper function
onestratum <-
    function(os=s, od=d_o, s_, d_, n_)
    {
        id <- which((os%in%s_)&(od%in%d_))
        idm <- sample(id, n_, replace=F)
        return(idm)
    }


set.seed(286)
r_a <- apply(g, 1, function(i) onestratum(s_=i['s'], d_=i['d_o'], n_=i['n']))
r_a
i_a <- unlist(r_a)

#   plot
par(mar=c(5,5,2,1), las=1)
plot(s, y_o, 
     lwd=2, xlab='Stratum', ylab='Y',
     pch=ifelse(d_o%in%0, 0, 1), 
     col=ifelse(d_o%in%0, rgb(0,0,1,0.2), rgb(1,0,0,0.2)))
points(s[i_a], y_o[i_a], 
       pch=ifelse(d_o[i_a]%in%0, 15, 16), 
       col=ifelse(d_o[i_a]%in%0, rgb(0,0,1,1), rgb(1,0,0,1)))

#   what's the estimate?
y_a <- y_o[i_a]
d_a <- d_o[i_a]
#   compare
mean(y_a[d_a%in%1]) - mean(y_a[d_a%in%0])
mean(y_o[d_o%in%1]) - mean(y_o[d_o%in%0])

#   or, we can weigh the units based on their conditional probabilities of
#   being in D=1 as opposed to D=0 given their S=s
#   these are their propensity scores
ps <- glm(d_o ~ 1 + as.factor(s), family=binomial('logit')) |> fitted()
#   these are just the P(D=1|S=s)
ps |> round(digits=3)
#   now compute the weights, which are inverse
psw <- ifelse(d_o%in%0, 1/(1-ps), 1/ps)

#   now compute the estimate
sum ( (y_o*psw)[d_o%in%1] ) / sum(psw[d_o%in%1]) - 
    sum ( (y_o*psw)[d_o%in%0] ) / sum(psw[d_o%in%0])

#   the same as:
lm(y_o ~ 1 + d_o, weights=psw)

#   for completness, let's check the gaussian-linear regression estimate
#   using stratum intercepts (a.k.a. 'fixed effects')
lm(y_o ~ 1 + d_o + as.factor(s))

#   now let's rerun the same thing with M replicated datasets



onerun <-
    function(N=4e1, K=6)
    {
        s <- sample(1:K, N, replace=T)
        sx <- runif(K, -1, +1)
        x1 <- sx[s] + pnorm(0, 1) 
        ite <- 1 + x1 |> scale()
        x0 <- rnorm(N, 0, 1) |> scale()
        xb <- rnorm(N, 0, 1) |> scale()
        y0 <- xb + x0 
        y1 <- xb + ite
        pd <- plogis(x1)
        pd[ s %in% sample(1:K,2,replace=F) ] <- 0
        d_o <- sapply(pd, function(i) rbinom(1, 1, i))
        y_o <- ifelse(d_o%in%0, y0, y1)
        s_n <- apply(table(d_o, s), 2, min)
        g <- as.data.frame(table(s, d_o))
        g$n <- s_n[g$s]
        r_a <- apply(g, 1, function(i)
                     onestratum(os=s, od=d_o,
                                s_=i['s'], d_=i['d_o'], n_=i['n']))
        i_a <- unlist(r_a)
        y_a <- y_o[i_a]
        d_a <- d_o[i_a]
        ps <- glm(d_o ~ 1 + as.factor(s), family=binomial('logit')) |> fitted()
        psw <- ifelse(d_o%in%0, 1/(1-ps), 1/ps)
        att_nai <- mean(y_o[d_o%in%1]) - mean(y_o[d_o%in%0])
        att_rem <- mean(y_a[d_a%in%1]) - mean(y_a[d_a%in%0])
        att_psw <- coef(lm(y_o ~ 1 + d_o, weights=psw))['d_o']
        att_fex <- coef(lm(y_o ~ 1 + d_o + as.factor(s)))['d_o']
        names(att_psw) <- names(att_fex) <- NULL
        return(c(#'truth'=mean(y1)-mean(y0),
                 'att_nai'=att_nai, 'att_rem'=att_rem, 
                 'att_psw'=att_psw, 'att_fex'=att_fex))
    }

onerun() |> round(digits=2)

#   now 1e3 replications
R <- 1e3
set.seed(657)
e <- replicate(R, onerun(N=4e1, K=6)) |> t()

#   inspect
#   note the missing values
summary(e)

data.frame('mean_dev'=apply(1-e, 2, mean, na.rm=T),
           'var'=apply(e, 2, var, na.rm=T)) |> round(digits=2)


att_labels <- 
    c('Naive means comparison',
      'Randomized exact matching',
      'Propensity score weights',
      'Stratum intercepts')

par(mar=c(5,14,3,1), las=1)
xl <- range(as.vector(e), na.rm=T) + c(-1,1)*0.5
plot.new()
plot.window(xlim=xl, ylim=c(4.5,0.5))
mtext('ATT estimates', font=2)
axis(1)
axis(2, 1:4, att_labels, lwd=0, lwd.ticks=1)
for (i in 1:4) {
    set.seed(i+100)
    points(e[,i], rep(i,R)+runif(R,-0.3,0.3), pch=16, col='lightblue3')
    lines(rep(mean(e[,i],na.rm=T),2), i+c(-1,1)*0.4, col='darkblue', lwd=2)
}
abline(v=1, lty=3, col='tomato3')

#   and how often is each the best?
apply(abs(1-e), 1, which.min) |> table()


#   SCRIPT END

