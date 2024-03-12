#   SGIA40F15 23/24 
#       Seminar 05
#           Instrumental Variables           
#               Sesame Street Example
#   2024-03-10
#   Juraj Medzihorsky

library(AER) #ivreg command

rm(list=ls())

#   Background: A Sesame Street experiment
#   A randomized experiment, in which some kids were
#   encouraged to watch the educational-entertainment show
#   Sesame Street, and were tested before and after on their
#   letters and numbers skills and knowledge
#   There's several papers, this one is the most relevant
#   for today 
#   https://files.eric.ed.gov/fulltext/ED122800.pdf
#   it's from 1971

sesame <- read.csv('Sesame.csv')

sesame |> dim()

sesame |> head()
#   age     ... in months
#   sex     ... M/F
#   prelet  ... letters test before
#   prenumb ... numbers test before
#   postlet ... letters test after
#   postnumb... numbers test after 
#   encour  ... randomized encouragement
#   regular ... did they watch it regularly

#   what do the outcomes look like?

#   letters test (with a tiny bit of random jitter)
par(mar=c(5,5,2,2), las=1)
set.seed(123)
with(sesame,
     plot(prelet+runif(240, -1,1)*0.3,
          postlet+runif(240, -1,1)*0.3,
          col=ifelse(encour%in%1, 'blue3', 'green4'),
          pch=ifelse(regular%in%1, 0, 1),
          asp=1, main='Letters Test Scores',
          xlab='Before', ylab='After'))
abline(a=0, b=1, col='red3', lty=3, lwd=1)

#   numbers test (with a tiny bit of random jitter)
par(mar=c(5,5,2,2), las=1)
set.seed(123)
with(sesame,
     plot(prenumb+runif(240, -1,1)*0.3,
          postnumb+runif(240, -1,1)*0.3,
          col=ifelse(encour%in%1, 'blue3', 'green4'),
          pch=ifelse(regular%in%1, 0, 1),
          asp=1, main='Numbers Test Scores',
          xlab='Before', ylab='After'))
abline(a=0, b=1, col='red3', lty=3, lwd=1)



#   If we care about the effect of encouragement, as that is
#   what is on the table policy-wise, we can simply use a 
#   diff-in-diff.
#       As usual in the policy domain, we wouldn't care only
#   about the direction of the effect (it would be weird if
#   there was none at all for anyone) but the size, to
#   decide whether the intervention is worth the time, money
#   and effort.

#   Using the difference-in-outcome shortcut
#   without and with age and sex as controls
sesame$diflet <- sesame |> with(postlet-prelet)
sesame$difnumb <- sesame |> with(postnumb-prenumb)

did_let <- lm(diflet ~ 1 + encour, data=sesame)
did_numb <- lm(difnumb ~ 1 + encour, data=sesame)

did_let_c <- lm(diflet ~ 1 + encour + age + sex, data=sesame)
did_numb_c <- lm(difnumb ~ 1 + encour + age + sex, data=sesame)

summary(did_let)$coef['encour', 1:3] |> round(2)
summary(did_numb)$coef['encour', 1:3] |> round(2)

summary(did_let_c)$coef['encour', 1:3] |> round(2)
summary(did_numb_c)$coef['encour', 1:3] |> round(2)


#   But what if we also care about the effect of regularly
#   watching the show itself? Perhaps just to understand, or
#   to promote it more. Then we have the issue that though
#   the encouragement was randomized, some kids would or
#   wouldn't watch  it anyway

#   Did the encouragement work?
#   It was randomized, so we can simply:
lm(regular ~ 1 + encour, data=sesame) |> summary() |>
    {\(x) x$coef['encour', 1:3]}() |> round(2)

#   That's a large point estimate with a narrow 95% interval
#   +.36 (0.26, 0.46)
#   So, if you just want to encourage kids to watch the show
#   it works. But does the show deliver the education
#   outcomes?

#   Let's start with the Z-by-D table with a principal
#   stratification.
#
#   Type            D(Z=0)   D(Z=1)   
#   Always-takers       1       1     
#   Compliers           0       1         
#   Never-takers        0       0     
#   Defiers             0       0     

zdtab <- with(sesame, table(encour, regular))

#   and we only care about the proportions
zdprop <- zdtab/sum(zdtab)

#   let's inspect it in rounder percents
round(1e2*zdprop)

#   The table only has three parameters, because
#   it sums up to 1 (100%). Once you know three of the cells
#   you know exactly what's in the fourth one. 

#       Z   D   Types                   P(D,Z)
#       0   0   Never-takers, Compliers  .17
#       0   1   Always-takers, Defiers   .20
#       1   0   Never-takers, Defiers    .06
#       1   1   Always-takers, Compliers .57

#   We can bring partial identification into the picture,
#   and compute sharp bounds, a.k.a. Manski bounds on the
#   proportions of the four classes.
#   Let's start with  only always true assumptions.

#   Always-true sharp bounds on type frequencies:
#   Type                Min       Max   
#   Always-takers        0    .20 +  .57 =  .77 
#   Compliers            0    .17 +  .57 =  .74  
#   Never-takers         0    .06 +  .17 =  .23
#   Defiers              0    .06 +  .20 =  .26


#   Conventionally, we assume 
#       a)  that there are no defiers, 
#       b)  that the encouragement was successfully
#           randomized 

#   How does a) no defiers change the bounds?
#   Always-true sharp bounds:
#   Type                Min         Max   
#   Always-takers         .20       .77 
#   Compliers            0          .74  
#   Never-takers          .06       .23
#   Defiers              0         0

#   Those are still substantial ranges.

#   Assumption a) tells us that 
#       -   there are only Always-takers in the second row 
#           of the first column
#       -   there are only Never-takers in the first row of
#           the second column
#   Assumption b) tells us that (asymptotically), we can
#   expect 
#       -   the number of Never-takers to be the same in
#           both rows of the first column
#       -   the number of Always-takers to be the same in
#           both rows of the second column
#   Taken together we can try computing the numbers by
#   hand from the table: 
#
#       Z   D   Types                   P(D,Z)
#       0   0   Never-takers, Compliers  .17
#       0   1   Always-takers            .20
#       1   0   Never-takers             .06
#       1   1   Always-takers, Compliers .57
#
#   There are only two cells with two classes now. One thing
#   to remember is that the rows (Z-allocation) are not
#   uniform:
pz <- rowSums(zdprop)
round(1e2*pz)

#   Try to get the P[Complier] from cells (0,0) and (1,0)
#   about 36%
(zdprop['0','0']/pz['0'] - zdprop['1','0']/pz['1']) |>
    round(2)
#   now from cells (1,1) and (0,1)
#   also about 36%
(zdprop['1','1']/pz['1'] - zdprop['0','1']/pz['0']) |>
    round(2)
#   I.e. under these (admittedly possibly false) assumptions
#   we can take the estimate of
#       E[D(Z=1)] - E[D(Z=0)] 
#   to be the proption of compliers
lm(regular ~ 1 + encour, data=sesame) |> summary() |>
    {\(x) x$coef['encour', 1:3]}() |> round(2)

#   What are the sharp Manski bounds on this "OLS" estimate
#   of the ATE of Z on D?
c('lo'=-(zdprop['0','1']+zdprop['1','0']), 
  'hi'=(zdprop['0','0']+zdprop['1','1'])) |> round(2)
#   In such cases the basic bounds are always wide 1:
#      lo    hi 
#   -0.26  0.74 
#   But we also know that it is not possible for a negative
#   proportion of compliers to exist [old joke about a
#   closed hot dog stand]. So we can narrow it down to
#      lo    hi 
#    0.    0.74 
#   Which is the same thing that we already got here
#
#   Always-true sharp bounds on type frequencies:
#   Type                Min       Max   
#   Always-takers        0    .20 +  .57 =  .77 
#   Compliers            0    .17 +  .57 =  .74  
#   Never-takers         0    .06 +  .17 =  .23
#   Defiers              0    .06 +  .20 =  .26
#
#   And as we already saw the no-defiers assumption does not
#   narrow them down further.


#   ========================================================
#   Now let's compute the IV estimates

# 2-step "by hand":
# 1st stage again (proportion of compliers)
fit_s1 <- lm(regular ~ encour, data = sesame)

summary(fit_s1)$coef['encour',1:3] |> round(2)

# 2nd stage (ITT, Intention to Treat)
fit_s2_numb <- lm(postnumb ~ encour, data = sesame)
fit_s2_let <- lm(postlet ~ encour, data = sesame)

summary(fit_s2_numb)$coef['encour',1:3] |> round(2)
summary(fit_s2_let)$coef['encour',1:3] |> round(2)

# Wald estimator
iv_numb_1 <- coef(fit_s2_numb)['encour'] / coef(fit_s1)['encour']
iv_let_1 <- coef(fit_s2_let)['encour'] / coef(fit_s1)['encour']

#   the LATE estimates
iv_numb_1 |> round(2)
iv_let_1 |> round(2)

## two-stage least squares
# numbers outcome
iv_numb_2 <- ivreg(postnumb ~ regular | encour, data = sesame)
iv_let_2 <- ivreg(postlet ~ regular | encour, data = sesame)

#   the LATE estimates
summary(iv_numb_2)$coef['regular',1:3] |> round(2)
summary(iv_let_2)$coef['regular',1:3] |> round(2)

#   the ITT effects are smaller than the LATE
summary(did_let)$coef['encour', 1:3] |> round(2)
summary(did_numb)$coef['encour', 1:3] |> round(2)

#   OK, but what do those numbers mean substantively?
with(sesame, sd(postnumb)) |> round(2)
with(sesame, sd(postlet)) |> round(2)

#   The SDs in the after-treatment test scores are about 13
with(sesame, sd(postnumb)) |> round(2)
with(sesame, sd(postlet)) |> round(2)

#  about half the SD for numbers, 0.6 for letters 
(iv_numb_1/with(sesame, sd(postnumb))) |> round(2)
(iv_let_1/with(sesame, sd(postlet))) |> round(2)

#   How about a (cowboy) placebo test?
#   Pre-treatment test scores should not be affecte by D*
#   as that happened afterwards:

#   one of the placebo estimate is large, but check the SE
iv_numb_2p <- ivreg(prenumb ~ regular | encour, data = sesame)
iv_let_2p <- ivreg(prelet ~ regular | encour, data = sesame)
summary(iv_numb_2p)$coef['regular',1:3] |> round(2)
summary(iv_let_2p)$coef['regular',1:3] |> round(2)

#   let's try SUR for completeness:
library(systemfit)

sur_numb <- systemfit(list(postnumb~regular, regular~encour), 
                      data=sesame, method='SUR',
                      control=systemfit.control(maxiter=1e4))
sur_let <- systemfit(list(postlet~regular, regular~encour), 
                     data=sesame, method='SUR',
                     control=systemfit.control(maxiter=1e4))

#   same point estimate but very different SE
#   again, much smaller SEs, which isn't necessarily a good
#   thing unless one is a p-hacker
summary(sur_numb)$coefficients[2,1:3] |> round(2)
summary(sur_let)$coefficients[2,1:3] |> round(2)

#   ________________________________________________________
