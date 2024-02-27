#   SGIA40F15 23/24 
#       Seminar 04
#            a.k.a. the Linear Probability Model
#   2024-02-26
#   Juraj Medzihorsky

library(mgcv)

#   Suppose we have a binary outcome Y?
#   Then the right choice is a GLM with a Binomial sampling
#   distribution, right?
#   Well, that depends on what we're trying to estimate.
#   Remember, the ATE / ATT are *averages*
#       ATE = E[Y(1)-Y(0)] = E[Y(1)] - E[Y(0)]
#       ATT = E[Y(1)-Y(0)|D=1] = E[Y(1)|D=1] - E[Y(0)|D=1]
#   If Y is binary, then this is equivalent to
#       ATE = P[Y(1)=1] - P[Y(0)=1]
#       ATT = P[Y(1)=1|D=1] - P[Y(0)=1|D=1]
#   When we use logistic regression, i.e. Binomial GLM with
#   logistic link function, what is it that we're trying to
#   estimate?
#   The Causal Odds Ratio:
#       COR = ( P[Y(1)=1]/P[Y(1)=0] ) / ( P[Y(0)=1]/P[Y(0)=0] )
#   Conditional Causal Odds Ratio:
#       CCOR(x) = ( P[Y(1)=1|X=x]/P[Y(1)=0|X=x] ) / ( P[Y(0)=1|X=x]/P[Y(0)=0|X=x] )
#   In practice, the interest is usually in the logarithms 
#   of COR and CCOR, remember
#       log(x < 1) < 0
#       log(x = 1) = 0
#       log(x > 1) > 1
#   In this way, the sign has an analogous meaning to that
#   of the ATE, negative sign means a negative effect etc.


#   When we're estimating the ATE, ATT or some variant of 
#   them, we're not trying to estimate the whole process.
#   We may in fact be quite agnostic about it.
#   The main priority is to get a good estimate of the
#   effect. And the effect is just one aspect of the joing
#   distribution of the potential outcomes {Y(1), Y(0)}.

#   That's why some fields where causal inference with
#   observational data is prioritized use linear regression
#   with normal residuals (a.k.a. OLS)often even if the 
#   outcome is binary. Think of Diff-in-Diff or RDD. In this
#   application it is known as the Linear Probability Model.
#   
#   Many scholars have strong views in favor or against LPM
#   vs. logit or probit. Often, such views are based on
#   misunderstandings. Some stem from the fact that they 
#   do not first define the causal quantity of interest in 
#   population terms (e.g., using the Potential Outcomes
#   framework). Others from the fact that they only care
#   about the sings of effects instead of their magnitude.

#   Let's illustrate a couple points using simulations

#   1) big randomized experiment

N <- 1e3
set.seed(1974)
dat1 <- 
    data.frame(
       y0 = rbinom(N, 1, prob=0.3), 
       y1 = rbinom(N, 1, prob=0.8), 
       d  = sample(rep(0:1, c(floor(N/2), N-floor(N/2))), 
                   N, replace=F)
    )
dat1$y <- with(dat1, ifelse(d%in%1, y1, y0))

with(dat1, table(d,y))

#   population ATE
with(dat1, mean(y1)-mean(y0)) |> round(3)
#   population Causal Odds Ratio | COR
with(dat1,
     (mean(y1==1)/mean(y1==0))/(mean(y0==1)/mean(y0==0))) |>
    round(3)
#   and its log
with(dat1,
     (mean(y1==1)/mean(y1==0))/(mean(y0==1)/mean(y0==0))) |>
    log() |> round(3)

#   now estimate
#       LPM
lm(y ~ 1 + d, data=dat1)$coef['d']
#   logit, remember this is the log, so need to exponetiate
glm(y ~ 1 + d, data=dat1, family=binomial('logit'))$coef['d']
glm(y ~ 1 + d, data=dat1, family=binomial('logit'))$coef['d'] |> exp()

#   Simply:
#   Different quantities of interest may need different 
#   estimators. But if we only care about the sign, then
#   the choice between Binomial GLMs and the LPM is a lot 
#   less of an issue. Because both are sensitive to 
#   Simpson's paradox in the same way.

#   2) Just for completeness, let's try a simple example 
#      with confounding. 
#       The true link is cauchit, not logit

N <- 1e3
set.seed(1974)
dat2 <- 
    data.frame(
       x  = runif(N, -2, 2)        
    )

dat2$py0 <- with(dat2, ifelse(x<0, 0.1, 0.6))
dat2$py1 <- with(dat2, ifelse(x<0, 0.3, 0.9))
dat2$y0 <- with(dat2, sapply(py0, function(i) rbinom(1, 1, prob=i)))
dat2$y1 <- with(dat2, sapply(py1, function(i) rbinom(1, 1, prob=i)))
dat2$pd <- with(dat2, ifelse(x<0, 0.2, 0.8))
dat2$d <- with(dat2, sapply(pd, function(i) rbinom(1, 1, prob=1-i)))
dat2$y <- with(dat2, ifelse(d%in%1, y1, y0))

#   basic Simpson's paradox
#   Y(1) beats Y(0) in all X-strata, but the strata that are
#   overall worseoff both in Y(1) and Y(0) select into D=1
#   and the other out of it
with(dat2, table(d,y))
with(dat2, table(d,y,x<0))

#   population ATE
with(dat2, mean(y1)-mean(y0)) |> round(3)
#   population log Causal Odds Ratio | COR
with(dat2,
     (mean(y1==1)/mean(y1==0))/(mean(y0==1)/mean(y0==0))) |>
    log() |> round(3)

#   naive ATE and LCOR estimates
#   wrong sign
lm(y ~ 1 + d, data=dat2)$coef['d'] |> round(3)
glm(y ~ 1 + d, data=dat2, family=binomial('logit'))$coef['d'] |> round(3)
#   well that's a bit off, isn't it

#   controlled ATE and LCOR estimates
lm(y ~ 1 + d + x, data=dat2)$coef['d'] |> round(3)
glm(y ~ 1 + d + x, data=dat2, family=binomial('logit'))$coef['d'] |> round(3)

#   now let's try to generalize both with GAMs
#   much better
gam(y ~ 1 + d + s(x), data=dat2)$coefficients['d'] |> round(3)
gam(y ~ 1 + d + s(x), data=dat2,
    famil=binomial('logit'))$coefficients['d'] |> round(3)


#   3) What if the outcome is from a Binomial with a size
#      that varies over units?

#   as above, with a small twist 
N <- 1e3
set.seed(1974)
dat3 <- 
    data.frame(
       x = runif(N, -2, 2)       
    )
dat3$n <- with(dat3, sapply(x, function(i) 1+rpois(1, exp(2+i/4))))
dat3$py0 <- with(dat3, ifelse(x<0, 0.1, 0.6))
dat3$py1 <- with(dat3, ifelse(x<0, 0.3, 0.9))
dat3$y0 <- sapply(1:N, function(i) rbinom(1, size=dat3$n[i], prob=dat3$py0[i]))
dat3$y1 <- sapply(1:N, function(i) rbinom(1, size=dat3$n[i], prob=dat3$py1[i]))
dat3$pd <- with(dat3, ifelse(x<0, 0.2, 0.8))
dat3$d <- with(dat3, sapply(pd, function(i) rbinom(1, 1, prob=1-i)))
dat3$y <- with(dat3, ifelse(d%in%1, y1, y0))

#   what's the distribution of the no of trials?
with(dat3, table(n))

#   now the population causal quantities are defined in terms
#   of P[Y(d)_i==1] 
#   we need to add probability of Y==1
dat3$pyis1 <- with(dat3, y/n)

#   population ATE
with(dat3, mean(y1/n)-mean(y0/n)) |> round(3)
#   population log Causal Odds Ratio | COR
with(dat3,
     (mean(y1/n)/mean(1-y1/n))/(mean(y0/n)/mean(1-y0/n))) |>
    log() |> round(3)


#   logit is easy to spec
glm(cbind(y,n-y) ~ 1 + d + x, data=dat3,
    family=binomial('logit'))$coef['d'] |> round(3)

#   LPM without weights
lm(pyis1 ~ 1 + d + x, data=dat3)$coef['d'] |> round(3)
#   LPM, using the no trials as weights
lm(pyis1 ~ 1 + d + x, data=dat3,
   weights=dat3$n)$coef['d'] |> round(3)

#   same with the GAM version
#   which again does much better here
gam(pyis1 ~ 1 + d + s(x), data=dat3, 
    weights=dat3$n)$coefficients['d'] |> round(3)


#   Remember, there are many other, potentially more useful
#   options than LPM / Binomial GLMs/GAMs                

#   SCRIPT END
