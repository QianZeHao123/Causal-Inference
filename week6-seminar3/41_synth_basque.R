#   SGIA40F15 23/24
#       Seminar 02
#           Synthetic Control Method with Synth
#           Terrorism in Basque country example
#   2024-02-16
#   Juraj Medzihorsky

#   script adapted from
#       https://carlos-mendez.quarto.pub/r-synthetic-control-tutorial/


rm(list = ls())

#   install.packages('Synth')
library('Synth')

#   #   devtools::install_github('bcastanho/SCtools')
#   #   pak::pkg_install('bcastanho/SCtools')
#   library(SCtools)

#   the data is included in the Synth package
#   the goal of the researchers was to estimate the
#   economic effects of terrorism in Basque country in Spain
#   The 2003 article:
#   https://economics.mit.edu/sites/default/files/publications/The%20Economic%20Costs%20of%20Conflict.pdf

data('basque')

str(basque)

#   quite a bit of missing data
head(basque)

#   42 years
range(basque$year)

#   in 1+17 potential donor regions
#   indexed by name and id no
table(basque$regionname)
table(basque$regionno)
#   the treated unit has id no 17
#   unit no 1 is the whole Spain

#   the outcome of interest is GDP per capita
summary(basque$gdpcap)

#   let's plot what it looks like
par(mar = c(3, 3, 1, 1), las = 1)
plot.new()
plot.window(xlim = c(1950, 2000), ylim = c(0, max(basque$gdpcap)))
axis(1)
axis(2)
for (i in 2:18) {
  rf <- basque$regionno %in% i
  mk <- ifelse(i %in% 17, rgb(0, 0, 1, 0.8), grey(0.7, 0.8))
  lines(basque$year[rf],
        basque$gdpcap[rf],
        col = mk,
        lwd = 1.5)
}
rf <- basque$regionno %in% 1
lines(basque$year[rf], basque$gdpcap[rf], lty = 3, lwd = 1.5)

#   Synth uses the dataprep function that sorts the data
#   into pre-and post and treatment and control

# Prepare the data for analysis
dat <- dataprep(
  foo = basque,
  # the dataset to be prepared
  # the predictor variables to be used in the model
  predictors = c(
    "school.illit",
    "school.prim",
    "school.med",
    "school.high",
    "school.post.high",
    "invest"
  ),
  predictors.op = "mean",
  # operation to be applied on the predictors
  time.predictors.prior = 1964:1969,
  # pre-treatment time period
  # special predictors with their respective time periods and operations
  special.predictors = list(
    list("gdpcap", 1960:1969, "mean"),
    list("sec.agriculture", seq(1961, 1969, 2), "mean"),
    list("sec.energy", seq(1961, 1969, 2), "mean"),
    list("sec.industry", seq(1961, 1969, 2), "mean"),
    list("sec.construction", seq(1961, 1969, 2), "mean"),
    list("sec.services.venta", seq(1961, 1969, 2), "mean"),
    list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
    list("popdens", 1969, "mean")
  ),
  dependent = "gdpcap",
  # the outcome
  unit.variable = "regionno",
  # unit ids
  unit.names.variable = "regionname",
  # unit names
  time.variable = "year",
  # time index
  treatment.identifier = 17,
  # treatment group id no
  controls.identifier = c(2:16, 18),
  # control pool id nos
  time.optimize.ssr = 1960:1969,
  # the period for estimating the weights
  time.plot = 1955:1997  # the time period for the plot
)

#   X indexes the predictors, Z the outcome
str(dat, max.level = 1)

#   treated unit pre-treatment predictors:
dat$X1


out <- synth(data.prep.obj = dat, method = 'BFGS')

str(out, max.level = 1)

# we have to compute the post-treatment outcome by hand

#   the observed Y values in the treated unit
str(dat$Y1plot)

#   the observed Y values in the donor units
str(dat$Y0plot)

#   the estimated weights
str(out$solution.w)

#   inspect: highly sparse, just two donors stay in (10, 14)
round(1e2 * out$solution.w)

#   the fitted values for the treated unit
Yhat <- dat$Y0plot %*% out$solution.w

#   let's plot what it looks like
par(mar = c(3, 3, 1, 1), las = 1)
plot.new()
plot.window(xlim = c(1950, 2000), ylim = c(0, max(basque$gdpcap)))
axis(1)
axis(2)
#   observed Basque country
rf <- basque$regionno %in% 17
lines(basque$year[rf],
      basque$gdpcap[rf],
      col = rgb(0, 0, 1, 0.8),
      lwd = 1.5)
#   fitted Basque country
lines(
  basque$year[rf],
  Yhat,
  col = rgb(0, 0, 1, 0.8),
  lwd = 1.5,
  lty = 3
)
#   and the two donors
for (i in c(10, 14)) {
  rf <- basque$regionno %in% i
  lines(
    basque$year[rf],
    basque$gdpcap[rf],
    col = rgb(1, 0, 0, 0.5),
    lwd = 1.0,
    lty = '11'
  )
}


#   what about the difference?
gaps <- dat$Y1plot - Yhat
gaps |> round(2)


tab <- synth.tab(dataprep.res = dat, synth.res = out)

str(tab, max.level = 1)

#   balance
tab$tab.pred |> round(digits = 1)

#   predictor weights
tab$tab.v

#   a simplex .... almost
unlist(tab$tab.v) |> sum()

#   donor unit wights
print(tab$tab.w, digits = 2)

#   values of the objectivees
tab$tab.loss


#   a convenient way to get the previous plot
#   plot in levels (treated and synthetic)
path.plot(dataprep.res = dat, synth.res = out)

#   plot the gaps (treated - synthetic)
gaps.plot(dataprep.res = dat, synth.res = out)


#   now the placebo tests
#   let's loop through units 2:16, 18, always designating
#   one as the fake treated unit, and compute the gap
#   and save it

oneplac <-
  function(j)
  {
    pool <- c(2:18)[!(c(2:18) %in% j)]
    dat_ <- dataprep(
      foo = basque,
      predictors = c(
        "school.illit",
        "school.prim",
        "school.med",
        "school.high",
        "school.post.high",
        "invest"
      ),
      predictors.op = "mean",
      time.predictors.prior = 1964:1969,
      special.predictors = list(
        list("gdpcap", 1960:1969, "mean"),
        list("sec.agriculture", seq(1961, 1969, 2), "mean"),
        list("sec.energy", seq(1961, 1969, 2), "mean"),
        list("sec.industry", seq(1961, 1969, 2), "mean"),
        list("sec.construction", seq(1961, 1969, 2), "mean"),
        list("sec.services.venta", seq(1961, 1969, 2), "mean"),
        list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
        list("popdens", 1969, "mean")
      ),
      dependent = "gdpcap",
      unit.variable = "regionno",
      unit.names.variable = "regionname",
      time.variable = "year",
      treatment.identifier = j,
      controls.identifier = pool,
      time.optimize.ssr = 1960:1969,
      time.plot = 1955:1997
    )
    out_ <- synth(data.prep.obj = dat_,
                  method = 'BFGS',
                  verbose = F)
    gaps <- dat_$Y1plot - (dat_$Y0plot %*% out_$solution.w)
    return(gaps)
  }

oneplac(2)

#   will take a bit
plac <- do.call(cbind, lapply(2:18, oneplac))

str(plac)

#   let's just add the placebo gaps to the estimated gap
par(las = 1)
gaps.plot(dataprep.res = dat, synth.res = out)
for (j in 1:ncol(plac)) {
  lines(1955:1997, plac[, j], col = grey(0.5, 0.5), lty = '22')
}
