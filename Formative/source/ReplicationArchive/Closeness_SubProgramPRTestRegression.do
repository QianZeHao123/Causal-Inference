
prtest voted_2014_primary `modelrestriction', by(close350not2500)
local prop350: display %4.3f `r(P_2)'
local prop2500: display %4.3f `r(P_1)'
local diffproptemp = `r(P_2)'-`r(P_1)'
local diffprop: display %4.3f `diffproptemp'
local diffpropsetemp = sqrt(((`r(P_1)' * (1 - `r(P_1)')) / `r(N_1)') + ((`r(P_2)' * (1 - `r(P_2)')) / `r(N_2)'))
local diffpropse: display %4.3f `diffpropsetemp'
local n350 `r(N_2)'
local n2500 `r(N_1)'

qui reg voted_2014_primary close350not2500 i.strata Z_* `modelrestriction' [pweight = weight_allstatestreats], robust
local coef: display %4.3f _b[close350not2500]
local se: display %4.3f _se[close350not2500]

qui logit voted_2014_primary close350not2500 i.strata Z_* `modelrestriction' [pweight = weight_allstatestreats], robust iter(500)
local loglik: display %8.3f `e(ll)'
local pr2: display %4.3f `e(r2_p)'
if `e(ic)' == 500 {
  local warning = "Convergence Not Achieved!"
}
else {
  local warning = " "
}

putexcel A`row' = "`modelname'" B`row' = "`prop350'" C`row' = "`prop2500'" D`row' = "`diffprop' [`diffpropse']" E`row' = "`coef' [`se']" F`row' = "(`n350',`n2500')"

reg voted_2014_primary close350not2500 i.strata Z_* `modelrestriction' [pweight = weight_allstatestreats], robust
if `modelcounter' == 1 {
  outreg2 using Tables/TableA3-FullRegressionsForTable3, excel se bracket dec(3) label ctitle("`modelname'") title("Table A3: Full Regression Results for Table 3") addnote("Note: OLS regression coefficients with robust standard errors in brackets. Dependent variable is voted in 2014 primary election (Yes = 1, No = 0). All models include state x voter history x district competitiveness fixed effects. Weighted analysis. ***p<0.01; **p<0.05; *p<0.1.") replace
}
else {
  outreg2 using Tables/TableA3-FullRegressionsForTable3, excel se bracket dec(3) label ctitle("`modelname'") append
}

logit voted_2014_primary close350not2500 i.strata Z_* `modelrestriction' [pweight = weight_allstatestreats], robust iter(500)
if `modelcounter' == 1 {
  outreg2 using Tables/TableA5-LogitVersionsOfTableA3, excel se bracket dec(3) label ctitle("`modelname'") addtext(Log-Likelihood, "`loglik'", Pseudo-R-squared, "`pr2'", Warning, "`warning'" ) title("Table A5: Logistic Regression Versions of OLS Models from Table A3") addnote("Note: Logistic regression coefficients with robust standard errors in brackets. Dependent variable is voted in 2014 primary election (Yes = 1, No = 0). All models include state x voter history x district competitiveness fixed effects. Weighted analysis. ***p<0.01; **p<0.05; *p<0.1.") replace
}
else {
  outreg2 using Tables/TableA5-LogitVersionsOfTableA3, excel se bracket dec(3) label ctitle("`modelname'") addtext(Log-Likelihood, "`loglik'", Pseudo-R-squared, "`pr2'", Warning, "`warning'" ) append
}

********************************************************************************
** Interaction of treatment with indicator for under 50
********************************************************************************

reg voted_2014_primary close350not2500##ageunder50 i.strata Z_* `modelrestriction' [pweight = weight_allstatestreats], robust
if `modelcounter' == 1 {
  outreg2 using Tables/TableA7-TreatmentsInteractedWithAge, excel se bracket dec(3) label ctitle("`modelname'") title("Table A7: Interaction of Treatment Effects with Indicator for Age Under 50") addnote("Note: OLS regression coefficients with robust standard errors in brackets. Dependent variable is voted in 2014 primary election (Yes = 1, No = 0). All models include state x voter history x district competitiveness fixed effects. Weighted analysis. ***p<0.01; **p<0.05; *p<0.1.") replace
}
else {
  outreg2 using Tables/TableA7-TreatmentsInteractedWithAge, excel se bracket dec(3) label ctitle("`modelname'") append
}



