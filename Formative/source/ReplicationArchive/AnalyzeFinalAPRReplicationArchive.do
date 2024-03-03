/* Replication code for:

 "Messages Designed to Increase Perceived Electoral Closeness Increase Turnout"
 Daniel R. Biggers, David J. Hendry, and Gregory A. Huber
 American Politics Research
 
 This is version 1.0
 10/8/2023
  
*******************************************************

 This Stata .do file performs all of the analyses
 reported in the main text and recreates all the 
 tables in the supporting information.
 
 It uses the subprogram do file "Closeness_SubProgramPRTestRegression" 
 included in the replication materials to produce many of the model results. 
 
******************************************************/

clear
set more off

*set working directory and start log file.
cd ""
log using Logs/APR_Final_ReplicationLog.txt, replace text

use APRCloseElections_Final_Publication_Replication_Dataset.dta, clear


********************************************************************************
** Appendix Table A1. Balance Tests
********************************************************************************

mlogit a_phone_treat_relplacebo_passed d_yearssincereg d_yearssincereg_miss ///
  d_electiondayage d_gender_male d_gender_unknown d_race_black ///
  d_race_latino d_race_miss d_race_other d_genvotes d_primvotes d_specvotes ///
  [pweight = weight_allstatestreats], robust baseoutcome(1)
local mlogitp = e(p)
local mlogitdf = e(df_m)
local mlogitchi = e(chi2)

local tablenotes2: display "Note: Cell entries are means with standard " ///
  "deviations in brackets. Multinomial logit was used to predict treatment " ///
  "assignment with all variables in the table used as predictors. The " ///
  "chi-squared test for all covariates predicting assignment is not " ///
  "significant (χ2(" %2.0f `mlogitdf' ") = " %3.2f `mlogitchi' ", p = " ///
  %3.2f `mlogitp' ")."

display "`tablenotes2'"

putexcel set Tables/TableA1-BalanceTests, replace
putexcel A1 = "Table A1: Tests of Balance for Experiment Treatment Assignment"
putexcel A2 = "Variable" B2 = "Treatment = Placebo" C2 = "Treatment = Information Only" D2 = "Treatment = Closeness (350 Votes)" E2 = "Treatment = Closeness (2500) Votes"
local row 3
foreach var of varlist d_yearssincereg d_yearssincereg_miss d_electiondayage ///
  d_gender_male d_gender_unknown d_race_black d_race_latino d_race_miss ///
  d_race_other d_genvotes d_primvotes d_specvotes {
    foreach treat of varlist t_placebo t_info_only t_closeness_1 t_closeness_2 {
	  qui sum `var' [aweight = weight_allstatestreats] if `treat' == 1
	  local `treat'mean: display %4.3f `r(mean)'
	  local `treat'sdtemp: display %4.3f `r(sd)'
	  local `treat'sd: display "[" `t_placebosdtemp' "]"
	}
	local varlab: var label `var'
	putexcel A`row' = "`varlab'" B`row' = `t_placebomean' C`row' = `t_info_onlymean' D`row' = `t_closeness_1mean' E`row' = `t_closeness_2mean'
    local ++row
	putexcel B`row' = "`t_placebosd'" C`row' = "`t_info_onlysd'" D`row' = "`t_closeness_1sd'" E`row' = "`t_closeness_2sd'"
    local ++row
}
qui sum t_placebo if t_placebo == 1
local t_placebon `r(N)'
qui sum t_info_only if t_info_only == 1
local t_info_onlyn `r(N)'
qui sum t_closeness_1 if t_closeness_1 == 1
local t_closeness_1n `r(N)'
qui sum t_closeness_2 if t_closeness_2 == 1
local t_closeness_2n `r(N)'
putexcel A`row' = "Observations" B`row' = "`t_placebon'" C`row' = "`t_info_onlyn'" D`row' = "`t_closeness_1n'" E`row' = "`t_closeness_2n'"
local ++row
putexcel A`row' = "`tablenotes2'"


********************************************************************************
** Table 2. Differences in Election and Turnout Context Across States
********************************************************************************

sum voted_2014_primary if t_placebo == 1 & vf_state ==  "MA"
local ma_prop: display %4.3f `r(mean)' * 100

sum voted_2014_primary if t_placebo == 1 & vf_state ==  "MI"
local mi_prop: display %4.3f `r(mean)' * 100

sum voted_2014_primary if t_placebo == 1 & vf_state ==  "MN"
local mn_prop: display %4.3f `r(mean)' * 100

sum voted_2014_primary if t_placebo == 1 & vf_state ==  "MO"
local mo_prop: display %4.3f `r(mean)' * 100

sum voted_2014_primary if t_placebo == 1 & vf_state ==  "NH"
local nh_prop: display %4.3f `r(mean)' * 100

sum voted_2014_primary if t_placebo == 1 & vf_state ==  "TN"
local tn_prop: display %4.3f `r(mean)' * 100

sum voted_2014_primary if t_placebo == 1 & vf_state ==  "WI"
local wi_prop: display %4.3f `r(mean)' * 100

putexcel set Tables/Table02-StateContext, replace
putexcel A1 = "Table 2. Differences in Election and Turnout Contexts Across States"
putexcel E2 = "Number of Contested Primaries" G2 = "Number of Uncontested Primaries"
putexcel A3 = "State" B3 = "Primary Date" C3 = "Turnout Rate Among Placebo Subjects" D3 = "Number of Congressional Districts" E3 = "Democratic" F3 = "Republican" G3 = "Democratic" H3 = "Republican" 
putexcel A4 = "Massachusetts" B4 = "September 9" C4 = "`ma_prop'" D4 = "9" E4 = "2" F4 = "1" G4 = "7" H4 = "2"
putexcel A5 = "Michigan" B5 = "August 5" C5 = "`mi_prop'" D5 = "14" E5 = "5" F5 = "8" G5 = "9" H5 = "6"
putexcel A6 = "Minnesota" B6 = "August 12" C6 = "`mn_prop'" D6 = "8" E6 = "1" F6 = "2" G6 = "2" H6 = "1"
putexcel A7 = "Missouri" B7 = "August 5" C7 = "`mo_prop'" D7 = "8" E7 = "4" F7 = "6" G7 = "3" H7 = "1"
putexcel A8 = "New Hampshire" B8 = "September 9" C8 = "`nh_prop'" D8 = "2" E8 = "0" F8 = "2" G8 = "0" H8 = "0"
putexcel A9 = "Tennessee" B9 = "August 7" C9 = "`tn_prop'" D9 = "9" E9 = "3" F9 = "8" G9 = "5" H9 = "1"
putexcel A10 = "Wisconsin" B10 = "August 12" C10 = "`wi_prop'" D10 = "8" E10 = "3" F10 = "5" G10 = "0" H10 = "0"


********************************************************************************
** Table 3, Table A3, and Table A5. Difference of Proportions and Regressions
** Table A7. Interaction of Treatment Effects with Indicator for Age Under 50
********************************************************************************

gen close350not2500 = .
replace close350not2500 = 0 if t_closeness_2 == 1
replace close350not2500 = 1 if t_closeness_1 == 1
label var close350not2500 "Closeness = 350 votes (not 2500)"

gen ageunder50 = d_electiondayage < 50
label var ageunder50 "Age Under 50 (Yes = 1)"

xtset strata

local modelcounter 1
local row 4

putexcel set Tables/Table03-DifferenceOfProportionsAndRegressions, replace
putexcel A1 = ///
  "Table 3. Turnout by Closeness Experimental Condition in Phone Field Experiment"
putexcel B2 = "(1)" C2 = "(2)" D2 = "(3)" E2 = "(4)" F2 = "(5)"
putexcel A3 = "Sample" B3 = "Proportion Voting, 350 Votes Treatment" ///
  C3 = "Proportion Voting, 2500 Votes Treatment" ///
  D3 = "Difference of Proportions (350 Votes - 2500 Votes) [Standard Error]" ///
  E3 = "Regression Estimate of Difference (350 Votes - 2500 Votes) [Standard Error]" ///
  F3 = "Number of Observations (350 Votes, 2500 Votes)"

local modelname = "Entire Sample"
local modelrestriction = `""'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "State = Massachusetts"
local modelrestriction = `"if vf_state == "MA""'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "State = Michigan"
local modelrestriction = `"if vf_state == "MI""'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "State = Minnesota"
local modelrestriction = `"if vf_state == "MN""'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "State = Missouri"
local modelrestriction = `"if vf_state == "MO""'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "State = New Hampshire"
local modelrestriction = `"if vf_state == "NH""'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "State = Tennessee"
local modelrestriction = `"if vf_state == "TN""'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "State = Wisconsin"
local modelrestriction = `"if vf_state == "WI""'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "No Competitive House Primary"
local modelrestriction = `"if comp_either_primary == 0"'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "Either House Primary Competitive"
local modelrestriction = `"if comp_either_primary == 1"'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "Ever Voters (Have Voted Before)"
local modelrestriction = `"if (strata_sum != 1 & strata_sum != 2)"'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "Have Voted in Primary"
local modelrestriction = `"if strata_sum == 5 | strata_sum == 6"'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "Have Voted, but Never in Primary"
local modelrestriction = `"if strata_sum == 3 | strata_sum == 4"'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "No Prior History of Voting"
local modelrestriction = `"if strata_sum == 1 | strata_sum == 2"'
include Closeness_SubProgramPRTestRegression.do

local ++row
local ++modelcounter

local modelname = "Entire Sample, No Covariates"

reg voted_2014_primary close350not2500 i.strata [pweight = weight_allstatestreats], robust
outreg2 using Tables/TableA3-FullRegressionsForTable3, excel se bracket dec(3) label ctitle("`modelname'") append

reg voted_2014_primary close350not2500##ageunder50 i.strata [pweight = weight_allstatestreats], robust
outreg2 using Tables/TableA7-TreatmentsInteractedWithAge, excel se bracket dec(3) label ctitle("`modelname'") append

qui logit voted_2014_primary close350not2500 i.strata [pweight = weight_allstatestreats], robust
local loglik: display %8.3f `e(ll)'
local pr2: display %4.3f `e(r2_p)'
if `e(ic)' == 500 {
  local warning = "Convergence Not Achieved!"
}
else {
  local warning = " "
}

logit voted_2014_primary close350not2500 i.strata [pweight = weight_allstatestreats], robust
outreg2 using Tables/TableA5-LogitVersionsOfTableA3, excel se bracket dec(3) label ctitle("`modelname'") addtext(Log-Likelihood, "`loglik'", Pseudo-R-squared, "`pr2'", Warning, "`warning'" ) append

putexcel A`row' = "Note: The estimates in column (4) were generated from regression models including strata (state × vote history × district competitiveness) fixed effects and state interacted with indicators for age, year of registration, sex, race/ethnicity, and the number of times voted in general, primary, and special elections (complete model results are reported in Online Appendix Table A3)." 


********************************************************************************
** Endnote 9 and Table A2. Analysis of Intent to Vote in Election
********************************************************************************
prtest intend_to_vote_maybeYes, by(close350not2500)
reg intend_to_vote_maybeYes close350not2500 i.strata Z_* [pweight = weight_allstatestreats], robust
  outreg2 using Tables/TableA2-VoteIntent, excel se bracket dec(3) label ctitle("Intend to Vote in 2014 Primary Election (Yes=1)") title("Table A2. Intention to Vote in 2014 Primary Election") addnote("Note: OLS regression coefficients with robust standard errors in brackets. Dependent variable is reported intent to vote in 2014 primary election (Yes = 1, No = 0). Model includes state x voter history x district competitiveness fixed effects. Weighted analysis. ***p<0.01; **p<0.05; *p<0.1.") replace


********************************************************************************
** Figure 1 and Table A4. Comparative Effectiveness of Different Treatments
********************************************************************************

reg voted_2014_primary t_info_only t_closeness_1 t_closeness_2 i.strata Z_* ///
  [pweight = weight_allstatestreats], robust

 sum voted_2014_primary [aweight = weight_allstatestreats] if ///
  a_phone_treat_relplacebo_passed == 0 & e(sample)

qui reg voted_2014_primary t_info_only t_closeness_1 t_closeness_2 i.strata ///
  Z_* [pweight = weight_allstatestreats], robust
outreg2 using Tables/TableA4-RegressionsResultsForFigure1, excel se bracket ///
  dec(3) label ctitle("Voted In 2014 Primary Election (Yes = 1)") ///
  title("Table A4. Full Regression Results for Figure 1") ///
  addnote("Note: Cell entries are OLS regression coefficients with robust standard errors in brackets. Dependent variable is voted in 2014 primary election (Yes = 1, No = 0). Model includes state x voter history x district competitiveness fixed effects. Weighted analysis. ***p<0.01; **p<0.05; *p<0.1.") replace

lincom t_info_only-t_closeness_1 
lincom t_info_only-t_closeness_2

gen tvar = .
gen beta = .
gen beta_lowci = .
gen beta_hici = .

replace tvar = 3 in 1
replace beta = _b[t_info_only] in 1
replace beta_lowci = (_b[t_info_only] - 1.96 * _se[t_info_only]) in 1
replace beta_hici = (_b[t_info_only] + 1.96 * _se[t_info_only]) in 1

replace tvar = 1 in 2
replace beta = _b[t_closeness_1] in 2
replace beta_lowci = (_b[t_closeness_1] - 1.96 * _se[t_closeness_1]) in 2
replace beta_hici = (_b[t_closeness_1] + 1.96 * _se[t_closeness_1]) in 2

replace tvar = 2 in 3
replace beta = _b[t_closeness_2] in 3
replace beta_lowci = (_b[t_closeness_2] - 1.96 * _se[t_closeness_2]) in 3
replace beta_hici = (_b[t_closeness_2] + 1.96 * _se[t_closeness_2]) in 3

label define tvar_lab 3 "Election Reminder" 1 "Closeness (350 Votes)" 2 ///
  "Closeness (2500 Votes)"
label values tvar tvar_lab

gen betaastext = string(beta, "%9.3f")
replace betaastext = "" if beta == .
gen temp2 = .002

twoway (bar beta tvar) (rcap beta_hici beta_lowci tvar, lcolor(black) ///
  msize(medium)) (scatter temp2 tvar, mlabel(betaastext) msymbol(none) ///
  mlabcolor(black) mlabposition(0)), ///
  ytitle("Estimated Treatment Effect" "(Relative to Placebo)" " ") ///
  yscale(range(0 .04)) ylabel(0(.01).04) yline(0) ///
  xtitle("") xlabel(1 `" "Closeness" "(350 Votes)" "' 2 ///
  `" "Closeness" "(2500 Votes)" "' 3 `" "Election" "Reminder" "', ///
  noticks labcolor(black) labgap(small)) ///
  legend(off) scheme(s1mono) xsize(3) ysize(2) scale(1) name(Figure1, replace)
graph export Figures/Figure01.pdf, as(pdf) replace

drop tvar-beta_hici betaastext temp2

display "Among Those Contacted, N = `modelntext'. Placebo group turnout is `placeboturnoutastext'%."


********************************************************************************
** Table A6. Proportion Voting by Experimental Conditions, State, and Strata
********************************************************************************

putexcel set Tables/TableA6-ProportionVotingByConditionStateAndStrata, replace
putexcel A1 = ///
  "Table A6. Proportion Voting by Experimental Conditions, State, and Strata"
putexcel B2 = "Placebo" D2 = "Election Reminder" F2 = "Closeness 350" H2 = "Closeness 2500"
putexcel B3 = "Proportion Voting" C3 = "N" D3 = "Proportion Voting" E3 = "N" F3 = "Proportion Voting" G3 = "N" H3 = "Proportion Voting" I3 = "N"
putexcel A4 = "Entire Sample"
putexcel A5 = "State=Massachusetts"
putexcel A6 = "State=Michigan"
putexcel A7 = "State=Minnesota"
putexcel A8 = "State=Missouri"
putexcel A9 = "State=New Hampshire"
putexcel A10 = "State=Tennessee"
putexcel A11 = "State=Wisconsin"
putexcel A12 = "No Competitive House Primary"
putexcel A13 = "Either House Primary Competitive"
putexcel A14 = "Ever Voters (Have Voted Before)"
putexcel A15 = "Have Voted in Primary"
putexcel A16 = "Have Voted, but Never in Primary"
putexcel A17 = "No Prior History of Voting"
foreach var in t_placebo t_info_only t_closeness_1 t_closeness_2 {
  sum voted_2014_primary if `var' == 1
  local prop_`var' `r(mean)'
  local n_`var' `r(N)'
}
putexcel B4 = `prop_t_placebo' C4 = `n_t_placebo' D4 = `prop_t_info_only' E4 = `n_t_info_only' F4 = `prop_t_closeness_1' G4 = `n_t_closeness_1' H4 = `prop_t_closeness_2' I4 = `n_t_closeness_2'
local row = 5
foreach var1 in MA MI MN MO NH TN WI {
  foreach var2 in t_placebo t_info_only t_closeness_1 t_closeness_2 {
    sum voted_2014_primary if `var2' == 1 & vf_state == "`var1'"
	local prop_`var2' `r(mean)'
	local n_`var2' `r(N)'
  }
  putexcel B`row' = `prop_t_placebo' C`row' = `n_t_placebo' D`row' = `prop_t_info_only' E`row' = `n_t_info_only' F`row' = `prop_t_closeness_1' G`row' = `n_t_closeness_1' H`row' = `prop_t_closeness_2' I`row' = `n_t_closeness_2'
  local ++row
}
local row = 12
forvalues j = 0/1 {
  foreach var2 in t_placebo t_info_only t_closeness_1 t_closeness_2 {
    sum voted_2014_primary if `var2' == 1 & comp_either_primary == `j'
	local prop_`var2' `r(mean)'
	local n_`var2' `r(N)'
  }
  putexcel B`row' = `prop_t_placebo' C`row' = `n_t_placebo' D`row' = `prop_t_info_only' E`row' = `n_t_info_only' F`row' = `prop_t_closeness_1' G`row' = `n_t_closeness_1' H`row' = `prop_t_closeness_2' I`row' = `n_t_closeness_2'
  local ++row
}
foreach var in t_placebo t_info_only t_closeness_1 t_closeness_2 {
  sum voted_2014_primary if `var' == 1 & strata_sum != 1 & strata_sum != 2
  local prop_`var' `r(mean)'
  local n_`var' `r(N)'
}
putexcel B14 = `prop_t_placebo' C14 = `n_t_placebo' D14 = `prop_t_info_only' E14 = `n_t_info_only' F14 = `prop_t_closeness_1' G14 = `n_t_closeness_1' H14 = `prop_t_closeness_2' I14 = `n_t_closeness_2'

foreach var in t_placebo t_info_only t_closeness_1 t_closeness_2 {
  sum voted_2014_primary if `var' == 1 & (strata_sum == 5 | strata_sum == 6)
  local prop_`var' `r(mean)'
  local n_`var' `r(N)'
}
putexcel B15 = `prop_t_placebo' C15 = `n_t_placebo' D15 = `prop_t_info_only' E15 = `n_t_info_only' F15 = `prop_t_closeness_1' G15 = `n_t_closeness_1' H15 = `prop_t_closeness_2' I15 = `n_t_closeness_2'

foreach var in t_placebo t_info_only t_closeness_1 t_closeness_2 {
  sum voted_2014_primary if `var' == 1 & (strata_sum == 3 | strata_sum == 4)
  local prop_`var' `r(mean)'
  local n_`var' `r(N)'
}
putexcel B16 = `prop_t_placebo' C16 = `n_t_placebo' D16 = `prop_t_info_only' E16 = `n_t_info_only' F16 = `prop_t_closeness_1' G16 = `n_t_closeness_1' H16 = `prop_t_closeness_2' I16 = `n_t_closeness_2'

foreach var in t_placebo t_info_only t_closeness_1 t_closeness_2 {
  sum voted_2014_primary if `var' == 1 & (strata_sum == 1 | strata_sum == 2)
  local prop_`var' `r(mean)'
  local n_`var' `r(N)'
}
putexcel B17 = `prop_t_placebo' C17 = `n_t_placebo' D17 = `prop_t_info_only' E17 = `n_t_info_only' F17 = `prop_t_closeness_1' G17 = `n_t_closeness_1' H17 = `prop_t_closeness_2' I17 = `n_t_closeness_2'

gen treat_cat = t_placebo
replace treat_cat = 2 if t_info_only == 1
replace treat_cat = 3 if t_closeness_1 == 1
replace treat_cat = 4 if t_closeness_2 == 1
label define treatments 1 "Placebo" 2 "Election Reminder" 3 "Closeness 350" 4 "Closeness 2500"

bysort treat_cat: sum voted_2014_primary

*local modelname = "State = Massachusetts"
bysort treat_cat: sum voted_2014_primary if vf_state == "MA" 

*local modelname = "State = Michigan"
bysort treat_cat: sum voted_2014_primary if vf_state == "MI"

*local modelname = "State = Minnesota"
bysort treat_cat: sum voted_2014_primary if vf_state == "MN"

*local modelname = "State = Missouri"
bysort treat_cat: sum voted_2014_primary if vf_state == "MO"

*local modelname = "State = New Hampshire"
bysort treat_cat: sum voted_2014_primary if vf_state == "NH"

*local modelname = "State = Wisconsin"
bysort treat_cat: sum voted_2014_primary if vf_state == "WI"

*local modelname = "No Competitive House Primary"
bysort treat_cat: sum voted_2014_primary if comp_either_primary == 0

*local modelname = "Either House Primary Competitive"
bysort treat_cat: sum voted_2014_primary if comp_either_primary == 1

*local modelname = "Ever Voters (Have Voted Before)"
bysort treat_cat: sum voted_2014_primary if (strata_sum != 1 & strata_sum != 2)

*local modelname = "Have Voted in Primary"
bysort treat_cat: sum voted_2014_primary if strata_sum == 5 | strata_sum == 6

*local modelname = "Have Voted, but Never in Primary"
bysort treat_cat: sum voted_2014_primary if strata_sum == 3 | strata_sum == 4

*local modelname = "No Prior History of Voting"
bysort treat_cat: sum voted_2014_primary if strata_sum == 1 | strata_sum == 2


********************************************************************************
** Table A8. Relationship Between Intention to Vote and Actual Turnout
********************************************************************************

reg voted_2014_primary intend_to_vote_maybeYes##close350not2500 i.strata Z_* [pweight = weight_allstatestreats], robust
  outreg2 using Tables/TableA8-VoteIntent_Turnout, excel se bracket dec(3) label ctitle("Voted In 2014 Primary Election (Yes = 1)") title("Table A8. Effect of Intention to Vote on Turnout, by Experimental Condition") addnote("Note: OLS regression coefficients with robust standard errors in brackets. Dependent variable is voted in 2014 primary election (Yes = 1, No = 0). Model includes state x voter history x district competitiveness fixed effects. Weighted analysis. ***p<0.01; **p<0.05; *p<0.1.") replace

log close
