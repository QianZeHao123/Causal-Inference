

library(rdd)
library(rdrobust)
library(rddensity)
options(stringsAsFactors=F)

#   simplified from
#   https://mixtape.scunning.com/06-regression_discontinuity#replicating-a-popular-design-the-close-election

#   original source Lee, Moretti, and Butler (2004) QJE
#   https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=64f2ac60d723a37c3d0ccd040f591022b2e49002

#   data downloaded from
#   https://mixtape.scunning.com/06-regression_discontinuity

lmb <- read.csv('lmb-data-mini.csv')

vars_to_take <- c('score', 'democrat', 'lagdemocrat', 'demvoteshare')

d <- na.omit(lmb[, vars_to_take])

#   14k elections in the US,1948-1990 
dim(d)
with(lmb, range(year))


#   the main outcome of interest:
#       score how liberal are the legislative votes of the 
#       elected representative 
with(d, summary(score))

#   treatment:
#       is the elected rep a member of the Democratic party?
with(d, table(demvoteshare<0.5, democrat))
with(d, table(democrat))


#   the running/forcing variable:
#       what as the share of the Dem candidate in the
#       elections
with(d, summary(demvoteshare))

#   plot everything
par(mfrow=c(2,1), las=1, mar=c(4,4,1,1))
plot(d$demvoteshare, d$score, pch=1, 
     xlab='', ylab='Score',
     col=ifelse(d$democrat%in%1, 'blue', 'red'))
    abline(v=0.5, lty=3)
plot(d$demvoteshare, d$score, pch=1, 
     xlab='Dem vote share', ylab='Score',
     col=ifelse(d$democrat%in%1, 'blue', 'red'))
    abline(v=0.5, lty=3)


#   first center X
d$demvoteshare_c <- d$demvoteshare - 0.5

#   the simplest estimates
#   m1a <- lm(score ~ lagdemocrat + demvoteshare_c, data = d)
m2a <- lm(score ~ democrat + demvoteshare_c, data = d)
#   m3a <- lm(democrat ~ lagdemocrat + demvoteshare_c, data = d)

#   coef(m1a)[2] |> round(2)
coef(m2a)[2] |> round(2)
#   coef(m3a)[2] |> round(2)

#   with interaction, i.e. different slopes for D=0 and D=1
#   m1b <- lm(score ~ lagdemocrat * demvoteshare_c, data = d)
m2b <- lm(score ~ democrat * demvoteshare_c, data = d)
#   m3b <- lm(democrat ~ lagdemocrat * demvoteshare_c, data = d)

#   coef(m1b)[2] |> round(2)
coef(m2b)[2] |> round(2)
#   coef(m3b)[2] |> round(2)

#   add a cubic polynomial
d$demvoteshare_c_sq <- d$demvoteshare_c^2

#   m1c <- lm(score ~ lagdemocrat*demvoteshare_c + lagdemocrat*demvoteshare_c_sq, data=d)
m2c <- lm(score ~ democrat*demvoteshare_c + democrat*demvoteshare_c_sq, data=d)
#   m3c <- lm(democrat ~ lagdemocrat*demvoteshare_c + lagdemocrat*demvoteshare_c_sq, data=d)

#   coef(m1c)[2] |> round(2)
coef(m2c)[2] |> round(2)
#   coef(m3c)[2] |> round(2)

#   a 0.05, i.e. 5% bandwidth
#   m1d <- lm(score ~ lagdemocrat*demvoteshare_c + lagdemocrat*demvoteshare_c_sq,
#             data=d, subset=(demvoteshare>0.45)&(demvoteshare<0.55))
m2d <- lm(score ~ democrat*demvoteshare_c + democrat*demvoteshare_c_sq,
          data=d, subset=(demvoteshare>0.45)&(demvoteshare<0.55))
#   m3d <- lm(democrat ~ lagdemocrat*demvoteshare_c + lagdemocrat*demvoteshare_c_sq,
#             data=d, subset=(demvoteshare>0.45)&(demvoteshare<0.55))

#   coef(m1d)[2] |> round(2)
coef(m2d)[2] |> round(2)
#   coef(m3d)[2] |> round(2)

#   find a good bandwidth with IKbandwidth()
h_ik <- IKbandwidth(X=d$demvoteshare, Y=d$score,
                    cutpoint=0.5, verbose=T, kernel="triangular")
h_ik
m2e <- lm(score ~ democrat*demvoteshare_c + democrat*demvoteshare_c_sq,
          data=d, subset=(demvoteshare>(0.5-h_ik))&(demvoteshare<(0.5+h_ik)))

coef(m2e)[2] |> round(2)

#   non-linear, using rdrobust::rdrobust() 
m1r <- rdrobust(y=d$score, x=d$demvoteshare, c=0.5)

DCdensity(d$demvoteshare, cutpoint = 0.5)

den <- rddensity(d$demvoteshare, c = 0.5)
rdplotdensity(den, d$demvoteshare)




