#   SGIA40F15 23/24 
#       Seminar 02
#           A Diff-in-Diff Horror Story:
#           `Fixed Effects won't Save You`
#   2024-01-28
#   Juraj Medzihorsky

rm(list=ls())

library(lmw)
#   a helper to compute the ESS from LMW
ess <- function(i) (sum(i)^2)/sum(i^2)
ess_old <- function(i) (sum(abs(i))^2)/sum(i^2)

#   this is an illustration of a non-intuitive problem 
#   in diff-in-diff, using a recent high-profile article
#   it is anonymized, since we haven't yet notified the
#   authors of the flaw in their analysis that overturns
#   their result
#   the overturn is complete, it's not that the SEs would
#   change and what was `sig` is no longer `sig`
#   rather, the coeff changes signs 

#   the data comes from a cross-country survey
#   it's not panel data, i.e. there aren't the same units
#   twice, before and after, rather, before and after
#   are different individuals

md <- read.csv('anonymized.csv')

head(md)

#   let's add the t:g interaction
md$d <- with(md, t*g)

#   how many countries are there? 9
table(md$ctry) |> length()
table(md$ctry)

#   add a (T,G) stratum indicator for convenience
md$strt <- with(md, paste0(g,t))

#   we see big differences
#   this is because the periods are not equally common
#   and this varies over countries
with(md, table(strt, ctry))

#   let's check the ATT estimates reported result in the
#   original paper
#   _p means pooled, i.e. all countries together
#   _f means fixed effects, i.e., with country intercepts
att_p <- lm(y ~ 1 + t*g, data=md)$coef['t:g']
att_f <- lm(y ~ 1 + t*g + factor(ctry), data=md)$coef['t:g']

#   both are positive, and only differ a bit
c(att_p, att_f) |> round(3)

#   now what happens if we estimate the model separately for
#   each country?
countries <- md$ctry |> unique() |> sort()

onecountry <- 
    function(i) 
    {
        lm(y~1+t*g, data=subset(md, ctry%in%i))$coef['t:g']
    }

att_s <- sapply(countries, onecountry)

#   it is negative in all but country!
att_s |> round(3)

#   average: still negative
mean(att_s) |> round(3)
#   same with country-weighted average 
#   not that different
(sum( att_s * table(md$ctry) ) / nrow(md)) |> round(3)

#   so what's up?
#   long story short, despite what many choose to believe
#   `fixed effects` are just intercepts, and adding them
#   won't automatically solve all problems
#   (neither do cluster-corrected SEs, which only change the
#   SE and not the point estimates)

#   let's first look at a "corrected" spec
#   time interacts with country, in effect giving each 
#   country two separate intercepts for each group
lm(y ~ 1+t+t:g + g*factor(ctry), data=md)$coef['t:g']

#   one way to think about it is that the FE model lies
#   in between the pooled and the stratified estimator
#   adding interactions moves the model towards the
#   stratified estimator

#   now let's look at how the weights change with 
#   conditioning

W_p <- lmw(~1+t+g+d, treat='d', data=md, estimand='ATT', method='URI')
W_f <- lmw(~1+t+g+d+factor(ctry), treat='d', data=md, estimand='ATT', method='URI')
W_i <- lmw(~1+t+d+g*factor(ctry), treat='d', data=md, estimand='ATT', method='URI')


#   getting the weights under stratification is a bit more 
#   involving

aux_w_s <- rep(NA,nrow(md)) |> as.numeric()

for (i in countries) {
    aux_w_s[md$ctry%in%i] <- 
          lmw(~1+t+g+d, treat='d', 
              data=subset(md, ctry%in%i),
              estimand='ATT',
              method='URI')$weights * mean(md$ctry%in%i)
}


#   check the effective sample sizes
sizes <- 
    data.frame(what=c('--', 'Pooled', 'FE', 'G-C', 'Strat'),
               ess_0 =c(sum(md$d%in%0),
                        ess(W_p$weights[md$d%in%0]),
                        ess(W_f$weights[md$d%in%0]),
                        ess(W_i$weights[md$d%in%0]),
                        ess(aux_w_s[md$d%in%0])),
               ess_1 =c(sum(md$d%in%1),
                        ess(W_p$weights[md$d%in%1]),
                        ess(W_f$weights[md$d%in%1]),
                        ess(W_i$weights[md$d%in%1]),
                        ess(aux_w_s[md$d%in%1])))

#   despite the large nominal sample size in D=0
#   (remember, that includes GT {00, 01, 10}), the ESS
#   is not impresive and shrinks quickly with complexity
sizes

#   recover the ATT estimates
sum((md$y*W_p$weights)[md$d%in%1]) - sum((md$y*W_p$weights)[md$d%in%0])
sum((md$y*W_f$weights)[md$d%in%1]) - sum((md$y*W_f$weights)[md$d%in%0])
sum((md$y*W_i$weights)[md$d%in%1]) - sum((md$y*W_i$weights)[md$d%in%0])
sum((md$y*aux_w_s)[md$d%in%1]) - sum((md$y*aux_w_s)[md$d%in%0])

w_p <- aggregate(W_p$weights, by=list('t'=md$t, 'g'=md$g, 'ctry'=md$ctry), FUN=sum)
w_f <- aggregate(W_f$weights, by=list('t'=md$t, 'g'=md$g, 'ctry'=md$ctry), FUN=sum)
w_i <- aggregate(W_i$weights, by=list('t'=md$t, 'g'=md$g, 'ctry'=md$ctry), FUN=sum)
w_s <- aggregate(aux_w_s, by=list('t'=md$t, 'g'=md$g, 'ctry'=md$ctry), FUN=sum)


# add them together
w <- w_p[1:3]
w$EY <- aggregate(md$y, by=list('t'=md$t, 'g'=md$g, 'ctry'=md$ctry), FUN=mean)$x
w$N <- aggregate(md$y, by=list('t'=md$t, 'g'=md$g, 'ctry'=md$ctry), FUN=length)$x
w$w_p <- w_p$x
w$w_f <- w_f$x
w$w_i <- w_i$x
w$w_s <- w_s$x

w |> print(digits=1)

#   how do the weights correlate?
cor(w[,c('w_p', 'w_f', 'w_i', 'w_s')]) |> round(2)

#   it's gotta be ess within D-group

fil <- md$ctry %in% 'a'

sapply(countries, function(i) ess(aux_w_s[(md$ctry%in%i)&(md$d%in%1)])) |> sum()
sapply(countries, function(i) ess(aux_w_s[(md$ctry%in%i)&(md$d%in%0)])) |> sum()

sapply(countries, function(i) ess(W_i$weights[(md$ctry%in%i)&(md$d%in%1)])) |> sum()
sapply(countries, function(i) ess(W_i$weights[(md$ctry%in%i)&(md$d%in%0)])) |> sum()

sapply(countries, function(i) ess(W_p$weights[(md$ctry%in%i)&(md$d%in%1)])) |> sum()
sapply(countries, function(i) ess(W_p$weights[(md$ctry%in%i)&(md$d%in%0)])) |> sum()

sapply(countries, function(i) ess(aux_w_s[md$ctry%in%i]))
sapply(countries, function(i) ess(aux_w_s[md$ctry%in%i])) |> sum()


