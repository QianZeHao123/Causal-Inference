#### Figures 1-3 in Erikson and Rader "Much Ado about Nothing: RDD and the Incumbency Advantage"

#### Code for Figures 1-3 adapted from replication code for Caughey and Sekhon (2015)

rm(list = ls(all = T))


library(foreign)
library(coin)

data.dir <- getwd()
plot.dir <- paste(getwd(), "/plotdir", sep = '')


setwd(data.dir)
rd <- read.dta("./RD_IP.dta")

use <- rd$Use == 1
dinc <- rd$DWinPrv == 1 & !is.na(rd$DWinPrv)
rinc <- rd$DWinPrv == 0 & !is.na(rd$DWinPrv)
close.5 <- rd$close05          ## defining close by inc party margin
use.5 <- use & rd$close05
use.d <- use & rd$close05 & rd$DWinPrv
use.r <- use & rd$close05 & rd$DWinPrv == 0




######################################################################
#### Figure 1
#### Balance Plot by Dem Party margin for Dem Inc Party only
######################################################################


## Pre-treatment covariates
#### must exclude Dem win t-1, Rep inc in race, Rep held open seat, open seat
covs <- matrix(
  c(
    'DPctPrv',
    'Dem % t - 1',
    'DifDPPrv',
    'Dem % Margin t - 1',
    'IncDWNOM1',
    'Inc\'s D1 NOMINATE',
    'DemInc',
    'Dem Inc in Race',
    'PrvTrmsD',
    'Dem\'s # Prev Terms',
    'PrvTrmsO',
    'Rep\'s # Prev Terms',
    'RExpAdv',
    'Rep Experience Adv',
    'DExpAdv',
    'Dem Experience Adv',
    'ElcSwing',
    'Partisan Swing',
    'CQRating3',
    'CQ Rating {-1, 0, 1}',
    'DSpndPct',
    'Dem Spending %',
    'DDonaPct',
    'Dem Donation %',
    'SoSDem',
    'Dem Sec of State',
    'GovDem',
    'Dem Governor',
    'DifPVDec',
    'Dem Pres % Margin',
    ## average over decade
    'DemOpen',
    'Dem-held Open Seat',
    'VtTotPct',
    'Voter Turnout %',
    'GovWkPct',
    'Pct Gov\'t Worker',
    'UrbanPct',
    'Pct Urban',
    'BlackPct',
    'Pct Black',
    'ForgnPct',
    'Pct Foreign Born'
  ),
  ncol = 2,
  byrow = TRUE
)
## Parameters for plotting
r <- 10000
varline <- 6
nline <- 4
tline <- 2
cline <- 0

## Balance plot
setwd(plot.dir)
pdf(
  paste("DemIP_BalancePlot", Sys.Date(), ".pdf", sep = ""),
  width = 7,
  height = 8
)
par(mar = c(2, 12, 1, 0))
plot(
  x = NULL,
  y = NULL,
  xlim = c(0, 1),
  ylim = c(1, nrow(covs)),
  ylab = '',
  xlab = '',
  xaxt = "n",
  yaxt = "n",
  bty = 'n'
)
mtext(
  text = c(
    'Variable\nName',
    'Valid\nCases',
    'Treated\nMean',
    'Control\nMean'
  ),
  side = 2,
  font = 2,
  line = c(varline + 3, nline + .7, tline + .7, cline + 0.3),
  adj = .5,
  las = 2,
  at = 22,
  cex = .7
)
## For each covariate...
for (i in 1:nrow(covs)) {
  print(covs[i, 2])
  print(sum(is.na(rd[use.d, covs[i, 1]])))
  aty <- nrow(covs) - i + 1
  print(aty)
  mtext(
    text = covs[i, 2],
    side = 2,
    line = varline,
    adj = 1,
    las = 2,
    at = aty,
    cex = .7
  )
  ## Number of valid cases
  nonna <- sum(!is.na(rd[use.d, covs[i, 1]]))
  ## Mean of treated
  meanT <- signif(mean(rd[(use.d & rd$DemWin == 1), covs[i, 1]],
                       na.rm = TRUE),
                  digits = 2)
  ## Adding/subtracting digits for presentation purposes
  if (abs(meanT) < 0.1) {
    meanT <- signif(meanT, digits = 1)
  }
  if (meanT %% 1 == 0 & abs(meanT) < 10) {
    meanT <- paste(meanT, '.0', sep = '')
  }
  if (abs(as.numeric(meanT)) >= .1 & abs(as.numeric(meanT)) < 1 &
      nchar(meanT) == 3) {
    meanT <- paste(meanT, '0', sep = '')
  }
  ## Mean of control
  meanC <- signif(mean(rd[(use.d & rd$DemWin == 0), covs[i, 1]],
                       na.rm = TRUE),
                  digits = 2)
  ## Presentation adjustments
  if (abs(meanC) < 0.1) {
    meanC <- signif(meanC, digits = 1)
  }
  if (meanC %% 1 == 0 & abs(meanC) < 10) {
    meanC <- paste(meanC, '.0', sep = '')
  }
  if (as.numeric(meanC) %% .1 == 0 & abs(as.numeric(meanC)) < 1) {
    meanC <- paste(meanC, '0', sep = '')
  }
  mtext(
    text = c(nonna, meanT, meanC),
    side = 2,
    line = c(nline, tline, cline),
    adj = 1,
    las = 2,
    at = aty,
    cex = .7
  )
  ## Gray bands
  if (aty %% 2 == 1) {
    polygon(
      x = c(0, 0, 1, 1),
      y = c(aty - .5, aty + .5, aty + .5, aty - .5),
      border = FALSE,
      col = 'lightgray'
    )
  }
  ## If the variable has three or more levels
  if (length(levels(factor(rd[, covs[i, 1]]))) >= 3) {
    p1 <- pvalue(wilcox_test(rd[, covs[i, 1]][use.d] ~
                               factor(rd$DemWin[use.d]),
                             distribution = 'exact'))
    print(p1)
    sym <- 18
  } else
    ## If the variable is dichotomous
  {
    p1 <- fisher.test(x = factor(rd[, covs[i, 1]][use.d]),
                      y = factor(rd$DemWin[use.d]))$p.value
    sym <- 20
  }
  ## Plot p-value
  points(pch = sym, x = p1, y = aty)
}

segments(x0 = 0,
         x1 = 0,
         y0 = .5,
         y1 = 21.5)
segments(x0 = 0,
         x1 = 1,
         y0 = .49,
         y1 = .49)
segments(
  x0 = c(.05, .1),
  x1 = c(.05, .1),
  y0 = .5,
  y1 = 21.5,
  lty = 'dotted'
)
mtext(
  side = 1,
  at = c(0, .05, .1, 1),
  text = c('0', '.05', '.1', '1'),
  cex = .7,
  line = -.75
)
mtext(side = 1, at = .5, text = 'p-value')
dev.off()




######################################################################
#### Figure 2
#### Balance Plot by Dem Party margin for Rep Inc Party only
######################################################################

## Pre-treatment covariates

#### must exclude Dem win t-1, Dem inc in race, Dem held open seat, open seat
covs <- matrix(
  c(
    'DPctPrv',
    'Dem % t - 1',
    'DifDPPrv',
    'Dem % Margin t - 1',
    'IncDWNOM1',
    'Inc\'s D1 NOMINATE',
    'NonDInc',
    'Rep Inc in Race',
    'PrvTrmsD',
    'Dem\'s # Prev Terms',
    'PrvTrmsO',
    'Rep\'s # Prev Terms',
    'RExpAdv',
    'Rep Experience Adv',
    'DExpAdv',
    'Dem Experience Adv',
    'ElcSwing',
    'Partisan Swing',
    'CQRating3',
    'CQ Rating {-1, 0, 1}',
    'DSpndPct',
    'Dem Spending %',
    'DDonaPct',
    'Dem Donation %',
    'SoSDem',
    'Dem Sec of State',
    'GovDem',
    'Dem Governor',
    'DifPVDec',
    'Dem Pres % Margin',
    ## average over decade
    'NonDOpen',
    'Rep-held Open Seat',
    'VtTotPct',
    'Voter Turnout %',
    'GovWkPct',
    'Pct Gov\'t Worker',
    'UrbanPct',
    'Pct Urban',
    'BlackPct',
    'Pct Black',
    'ForgnPct',
    'Pct Foreign Born'
  ),
  ncol = 2,
  byrow = TRUE
)
## Parameters for plotting
r <- 10000
varline <- 6
nline <- 4
tline <- 2
cline <- 0

## Balance plot
setwd(plot.dir)
pdf(
  paste("RepIP_BalancePlot", Sys.Date(), ".pdf", sep = ""),
  width = 7,
  height = 8
)
par(mar = c(2, 12, 1, 0))
plot(
  x = NULL,
  y = NULL,
  xlim = c(0, 1),
  ylim = c(1, nrow(covs)),
  ylab = '',
  xlab = '',
  xaxt = "n",
  yaxt = "n",
  bty = 'n'
)
mtext(
  text = c(
    'Variable\nName',
    'Valid\nCases',
    'Treated\nMean',
    'Control\nMean'
  ),
  side = 2,
  font = 2,
  line = c(varline + 3, nline + .7, tline + .7, cline + 0.3),
  adj = .5,
  las = 2,
  at = 22,
  cex = .7
)
## For each covariate...
for (i in 1:nrow(covs)) {
  print(covs[i, 2])
  print(sum(is.na(rd[use.r, covs[i, 1]])))
  aty <- nrow(covs) - i + 1
  print(aty)
  mtext(
    text = covs[i, 2],
    side = 2,
    line = varline,
    adj = 1,
    las = 2,
    at = aty,
    cex = .7
  )
  ## Number of valid cases
  nonna <- sum(!is.na(rd[use.r, covs[i, 1]]))
  ## Mean of treated
  meanT <- signif(mean(rd[(use.r & rd$DemWin == 1), covs[i, 1]],
                       na.rm = TRUE),
                  digits = 2)
  ## Adding/subtracting digits for presentation purposes
  if (abs(meanT) < 0.1) {
    meanT <- signif(meanT, digits = 1)
  }
  if (meanT %% 1 == 0 & abs(meanT) < 10) {
    meanT <- paste(meanT, '.0', sep = '')
  }
  if (abs(as.numeric(meanT)) >= .1 & abs(as.numeric(meanT)) < 1 &
      nchar(meanT) == 3) {
    meanT <- paste(meanT, '0', sep = '')
  }
  ## Mean of control
  meanC <- signif(mean(rd[(use.r & rd$DemWin == 0), covs[i, 1]],
                       na.rm = TRUE),
                  digits = 2)
  ## Presentation adjustments
  if (abs(meanC) < 0.1) {
    meanC <- signif(meanC, digits = 1)
  }
  if (meanC %% 1 == 0 & abs(meanC) < 10) {
    meanC <- paste(meanC, '.0', sep = '')
  }
  if (as.numeric(meanC) %% .1 == 0 & abs(as.numeric(meanC)) < 1) {
    meanC <- paste(meanC, '0', sep = '')
  }
  mtext(
    text = c(nonna, meanT, meanC),
    side = 2,
    line = c(nline, tline, cline),
    adj = 1,
    las = 2,
    at = aty,
    cex = .7
  )
  ## Gray bands
  if (aty %% 2 == 1) {
    polygon(
      x = c(0, 0, 1, 1),
      y = c(aty - .5, aty + .5, aty + .5, aty - .5),
      border = FALSE,
      col = 'lightgray'
    )
  }
  ## If the variable has three or more levels
  if (length(levels(factor(rd[, covs[i, 1]]))) >= 3) {
    p1 <- pvalue(wilcox_test(rd[, covs[i, 1]][use.r] ~
                               factor(rd$DemWin[use.r]),
                             distribution = 'exact'))
    print(p1)
    sym <- 18
  } else
    ## If the variable is dichotomous
  {
    p1 <- fisher.test(x = factor(rd[, covs[i, 1]][use.r]),
                      y = factor(rd$DemWin[use.r]))$p.value
    sym <- 20
  }
  ## Plot p-value
  points(pch = sym, x = p1, y = aty)
}

segments(x0 = 0,
         x1 = 0,
         y0 = .5,
         y1 = 21.5)
segments(x0 = 0,
         x1 = 1,
         y0 = .49,
         y1 = .49)
segments(
  x0 = c(.05, .1),
  x1 = c(.05, .1),
  y0 = .5,
  y1 = 21.5,
  lty = 'dotted'
)
mtext(
  side = 1,
  at = c(0, .05, .1, 1),
  text = c('0', '.05', '.1', '1'),
  cex = .7,
  line = -.75
)
mtext(side = 1, at = .5, text = 'p-value')
dev.off()




######################################################################
#### Figure 3
#### Balance Plot by Inc Party margin
######################################################################

## Pre-treatment covariates
covs <- matrix(
  c(
    'IPPctPrv',
    'Inc Party % t - 1',
    'DifIPPPrv',
    'Inc Party % Margin t - 1',
    'IncDWNOM1',
    'Inc\'s D1 NOMINATE',
    'IPInc',
    'Inc in Race',
    'DemInc',
    'Dem Inc in Race',
    'NonDInc',
    'Rep Inc in Race',
    'PrvTrmsIP',
    'Inc Party\'s # Prev Terms',
    'IPExpAdv',
    'Inc Experience Adv',
    'IPSwing',
    'Inc Party Swing',
    'CQRatingIP',
    'CQ Rating Inc Party {-1, 0, 1}',
    'IPSpndPct',
    'Inc Party Spending %',
    'IPDonaPct',
    'Inc Party Donation %',
    'SoSIP',
    'Inc Party Sec of State',
    'GovIP',
    'Inc Party Governor',
    'DifPVIP',
    'Inc Party Pres % Margin',
    ## average over decade
    'DemOpen',
    'Dem-held Open Seat',
    'NonDOpen',
    'Rep-held Open Seat',
    'VtTotPct',
    'Voter Turnout %',
    'GovWkPct',
    'Pct Gov\'t Worker',
    'UrbanPct',
    'Pct Urban',
    'BlackPct',
    'Pct Black',
    'ForgnPct',
    'Pct Foreign Born'
  ),
  ncol = 2,
  byrow = TRUE
)
## Parameters for plotting
r <- 10000
varline <- 6
nline <- 4
tline <- 2
cline <- 0



## Balance plot
setwd(plot.dir)
pdf(
  paste("IPBalancePlot", Sys.Date(), ".pdf", sep = ""),
  width = 7.3,
  height = 8
)
par(mar = c(2, 14, 1, 0))
plot(
  x = NULL,
  y = NULL,
  xlim = c(0, 1),
  ylim = c(1, nrow(covs)),
  ylab = '',
  xlab = '',
  xaxt = "n",
  yaxt = "n",
  bty = 'n'
)
mtext(
  text = c(
    'Variable\nName',
    'Valid\nCases',
    'Treated\nMean',
    'Control\nMean'
  ),
  side = 2,
  font = 2,
  line = c(varline + 3, nline + .7, tline + .7, cline + 0.3),
  adj = .5,
  las = 2,
  at = 23,
  cex = .7
)
## For each covariate...
for (i in 1:nrow(covs)) {
  print(covs[i, 2])
  print(sum(is.na(rd[use.5, covs[i, 1]])))
  aty <- nrow(covs) - i + 1
  print(aty)
  mtext(
    text = covs[i, 2],
    side = 2,
    line = varline,
    adj = 1,
    las = 2,
    at = aty,
    cex = .7
  )
  ## Number of valid cases
  nonna <- sum(!is.na(rd[use.5, covs[i, 1]]))
  ## Mean of treated
  meanT <- signif(mean(rd[(use.5 & rd$IPwin == 1), covs[i, 1]],
                       na.rm = TRUE),
                  digits = 2)
  ## Adding/subtracting digits for presentation purposes
  if (abs(meanT) < 0.1) {
    meanT <- signif(meanT, digits = 1)
  }
  if (meanT %% 1 == 0 & abs(meanT) < 10) {
    meanT <- paste(meanT, '.0', sep = '')
  }
  if (abs(as.numeric(meanT)) >= .1 & abs(as.numeric(meanT)) < 1 &
      nchar(meanT) == 3) {
    meanT <- paste(meanT, '0', sep = '')
  }
  ## Mean of control
  meanC <- signif(mean(rd[(use.5 & rd$IPwin == 0), covs[i, 1]],
                       na.rm = TRUE),
                  digits = 2)
  ## Presentation adjustments
  if (abs(meanC) < 0.1) {
    meanC <- signif(meanC, digits = 1)
  }
  if (meanC %% 1 == 0 & abs(meanC) < 10) {
    meanC <- paste(meanC, '.0', sep = '')
  }
  if (as.numeric(meanC) %% .1 == 0 & abs(as.numeric(meanC)) < 1) {
    meanC <- paste(meanC, '0', sep = '')
  }
  mtext(
    text = c(nonna, meanT, meanC),
    side = 2,
    line = c(nline, tline, cline),
    adj = 1,
    las = 2,
    at = aty,
    cex = .7
  )
  ## Gray bands
  if (aty %% 2 == 1) {
    polygon(
      x = c(0, 0, 1, 1),
      y = c(aty - .5, aty + .5, aty + .5, aty - .5),
      border = FALSE,
      col = 'lightgray'
    )
  }
  ## If the variable has three or more levels
  if (length(levels(factor(rd[, covs[i, 1]]))) >= 3) {
    p1 <- pvalue(wilcox_test(rd[, covs[i, 1]][use.5] ~
                               factor(rd$IPwin[use.5]),
                             distribution = 'exact'))
    print(p1)
    sym <- 18
  } else
    ## If the variable is dichotomous
  {
    p1 <- fisher.test(x = factor(rd[, covs[i, 1]][use.5]),
                      y = factor(rd$IPwin[use.5]))$p.value
    sym <- 20
  }
  ## Plot p-value
  points(pch = sym, x = p1, y = aty)
}

segments(x0 = 0,
         x1 = 0,
         y0 = .5,
         y1 = 22.5)
segments(x0 = 0,
         x1 = 1,
         y0 = .49,
         y1 = .49)
segments(
  x0 = c(.05, .1),
  x1 = c(.05, .1),
  y0 = .5,
  y1 = 22.5,
  lty = 'dotted'
)
#segments(x0 = 0, x1 = 1, y0 = 25.5, y1 = 25.5, lty = 'dashed')
mtext(
  side = 1,
  at = c(0, .05, .1, 1),
  text = c('0', '.05', '.1', '1'),
  cex = .7,
  line = -.75
)
mtext(side = 1, at = .5, text = 'p-value')
dev.off()
