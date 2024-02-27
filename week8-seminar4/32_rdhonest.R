#   SGIA40F15 23/24 
#       Seminar 04
#            RDD minimal example with a placebo cutoff test
#   2024-02-24
#   Juraj Medzihorsky


#   library(devtools)
#   install_github('kolesarm/RDHonest')
#   https://github.com/kolesarm/RDHonest
library(RDHonest)
library(rdrobust)

rm(list=ls())

#   same data as before
lmb <- read.csv('lmb-data-mini.csv')
vars_to_take <- c('score', 'democrat', 'lagdemocrat', 'demvoteshare')
d <- na.omit(lmb[, vars_to_take])
d$demvoteshare_c <- d$demvoteshare - 0.5

#   with nearest neighbor SEs: 0.63
RDHonest(score ~ demvoteshare, cutoff=0.5, data=d, 
         M=0.1, kern='triangular', se.method='nn')

#   with EHW SEs: larger, 0.75
RDHonest(score ~ demvoteshare, cutoff=0.5, data=d, 
         M=0.1, kern='triangular', se.method='EHW')

#   compare with rdrobust from before: 1.24 convetional
#   width of the robust ci is 
49.052-43.293

#   how would that translate into SE (this is cowboying,
#   not serious): about 0.73
(49.052-43.293)/(4*1.96)

rdrobust(y=d$score, x=d$demvoteshare, c=0.5) |> summary()


#   SCRIPT END
