#   SGIA40F15 23/24 
#       Seminar 02
#            Diff-in-Diff
#   2024-01-27
#   Juraj Medzihorsky

rm(list=ls())

library(lmw)


#   this is the original data from Card & Kruger (1994) AER
#   specifically for Table 4 in the paper
ck <- read.csv('CardKrueger_Tab4_data.csv')

head(ck)

#   to replicate their results exactly, we need to drop
#   cases with missing data on any of the variables

#   Each row is a fast food restaurant
#   the columns we'll focus on are
#   nj      ... New Jersey, the treated state
#   emptot  ... employment before D=1 took effect in NJ
#   emptot2 ... employment after D=1 took effect in NJ
#   emptot_diff = emptot2 - emptot 

#   different ways of computing the DiD estimate
#   we have four groups
#   PA before ... S=0 T=0 D=0
#   PA after  ... S=0 T=1 D=0
#   NJ before ... S=1 T=0 D=0
#   NJ after  ... S=1 T=1 D=1

#   0. 'by hand' taking the four averages
EY_S0T0 <- with(ck, mean(emptot[nj%in%0]))
EY_S0T1 <- with(ck, mean(emptot2[nj%in%0]))
EY_S1T0 <- with(ck, mean(emptot[nj%in%1]))
EY_S1T1 <- with(ck, mean(emptot2[nj%in%1]))

#   after - before difference in the control group
EY_S0T1 - EY_S0T0
#   after - before difference in the treated group
EY_S1T1 - EY_S1T0
#   difference in differences 
((EY_S1T1 - EY_S1T0) - (EY_S0T1 - EY_S0T0)) |> round(2)

#   1. regressing the within-restaurant after - before diff
#   on the treatment group dummy
lm(emptot_diff ~ 1 + nj, data=ck) |> coef() |> round(2)

#   2. fitting the full diff-in-diff with interaction

#   but first we need to reformat the data
#   add a unique restaurant id
ck$id <- 1:nrow(ck)
#   new 'elongated' data.frame
lck <- data.frame(id=c(ck$id, ck$id),
                  emp=c(ck$emptot, ck$emptot2),
                  s=c(ck$nj, ck$nj),
                  t=c(rep(0,nrow(ck)), rep(1,nrow(ck))))
lck$d <- with(lck, ifelse( (s%in%1) & (t%in%1) ,1,0))
lck$gid <- with(lck, paste0(s,t))

#   now the interaction version
lm(emp ~ 1 + s*t, data=lck) |> coef() |> round(2)
#   same as
lm(emp ~ 1 + s + t + d, data=lck) |> coef() |> round(2)

#   side note: we can of course get the same estimate using
#   the FWL-based two-step approach
    yres <- lm(emp ~ 1 + s + t, data=lck) |> resid()
    dres <- lm(d ~ 1 + s + t, data=lck) |> resid()
    lm(yres ~ 1 + dres)$coef[2] |> round(2)

#   3. using OLS to impute E[Y(0)|D=1] = E[Y(0)|S=1,T=1]
M0 <- lm(emp ~ 1 + s + t, data=subset(lck, d%in%0))
imp_y0_11 <- predict(M0, newdata=subset(lck, d%in%1))

#   inspect: it's just the average for each observation
imp_y0_11 |> head() |> round(2)
#   now the ATT hat
(EY_S1T1 - mean(imp_y0_11)) |> round(2)

#   now what about the weights in the three cases?

W1 <- lmw(~1+nj, treat='nj', data=ck, estimand='ATT', method='URI')
W2 <- lmw(~1+s+t+d, treat='d', data=lck, estimand='ATT', method='URI')
W3 <- lmw(~1+s+t+d, treat='d', data=lck, estimand='ATT', method='MRI')

#   remember, the definition of the control group differs
#   between W1 and W2, W3
#   W1 = PA
#   W2, W3 = PA beofre and after and NJ before

#   in either case, the control group is much smaller than
#   the treatment group in terms of the ESS
#   W1 weights equally, W2 and W3 do not
W1 |> summary()
W2 |> summary()
W3 |> summary()

#   the W1 weights are equivalent to 1/N0 or 1/N1
table(ck$nj, W1$weights)
1/table(ck$nj)

#   the W2,W3 weights are a bit surprising
table(lck$gid, W2$weights)
table(lck$gid, W3$weights)

#   the S=0, T=0 group (PA before) gets negative weights
#   but this is because we are substracting it

#   check the four group means
gm <- aggregate(W2$weights*lck$emp, by=list(lck$gid), sum)
gm

#   therese are the same as the EY from the beginning
#   except (00) is flipped
gm$EY <- c(EY_S0T0, EY_S0T1, EY_S1T0, EY_S1T1)
gm

#   this is the imputed EY0_S1T1
gm$x[1]+gm$x[2]+gm$x[3]
#   subtract it from the EY1_S1T1, and it's the ATThat
(EY_S1T1 - (gm$x[1]+gm$x[2]+gm$x[3])) |> round(2)

#   negative weights mean extrapolating outside of the 
#   sample. 

#   how do the weights compare to the AS weights?
#   the AS weights are just the squares of the residuals
#   from the allocation regression
summary(dres[lck$d%in%0]/W2$weights[lck$d%in%0])
summary(dres[lck$d%in%1]/W2$weights[lck$d%in%1])


#   --------------------------------------------------------
#   now let's try the same with a control

#   remember, we have two time periods
#   so in principle we could have the controls change values
#   in time; however, what if a post-treatment covariate is
#   affected by the treatment? then controlling for it would
#   introduce the risk of post-treatment bias. and what if 
#   it is not? well, even then it cannot affect treatment
#   assignment, so it's not a confounder

#   let's use these binary indicator covariates
#       bk + kfc + roys + co_owned
#   first add it to the elongated data
lck$bk <- c(ck$bk, ck$bk)
lck$kfc <- c(ck$kfc, ck$kfc)
lck$roys <- c(ck$roys, ck$roys)
lck$co_owned <- c(ck$co_owned, ck$co_owned)


#   now the different specs

#   this is Table 4 model (ii)
lm(emptot_diff ~ 1 + nj + bk + kfc + roys + co_owned,
   data=ck) |> coef() |> round(2)

#   now here the estimate didn't change, pre-treatment
#   covariates cannot change the ATThat, only its SE
lm(emp ~ 1 + s + t + d + bk + kfc + roys + co_owned,
   data=lck) |> coef() |> round(2)

#   and MRI: the explicit imputation approach
Mc <- lm(emp ~ 1 + s + t + bk + kfc + roys + co_owned,
         data=subset(lck, d%in%0))
imp_y0 <- predict(Mc, newdata=subset(lck, d%in%1))
#   and the same ATThat as above
(EY_S1T1 - mean(imp_y0)) |> round(2)

#   now the 2x4 binary covariates create 2^4 = 16 strata,
#   so there are up to 16 possible Y(0) values in S=1 T=1
#   but bk, kfc, roys sum up to one, so there are only
#   4*2 possible values
#   check the strata
lck$strt <- with(lck, paste0(bk, kfc, roys, co_owned))
with(lck, table(strt, gid))
#
table(imp_y0)
table(imp_y0) |> length()

#   and what about the weights?
W1c <- lmw(~1+nj+bk+kfc+roys+co_owned, treat='nj',
           data=ck, estimand='ATT', method='URI')
W2c <- lmw(~1+s+t+d+bk+kfc+roys+co_owned, treat='d',
           data=lck, estimand='ATT', method='URI')
W3c <- lmw(~1+s+t+d+bk+kfc+roys+co_owned, treat='d',
           data=lck, estimand='ATT', method='MRI')

#   adding the covariates changed the weights for W1
W1 |> summary()
W1c |> summary()

#   weights cross tabs (rounded for readability)
table(round(W1$weights, 3), round(W1c$weights, 3), ck$nj)

ck$strt <- with(ck, paste0(bk, kfc, roys, co_owned))
table(ck$strt, round(W1c$weights, 3), ck$nj)

#   but not for W2
table(round(W2$weights, 3), round(W2c$weights, 3))

#   and W3
table(round(W3$weights, 3), round(W3c$weights, 3))



#   SCRIPT END
#   ========================================================

