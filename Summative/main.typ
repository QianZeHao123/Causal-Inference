#import "template.typ": *

// Take a look at the file `template.typ` in the file panel
// to customize this template and discover how it works.
#let today = datetime.today()
#show: project.with(
  title: "Causal Inference Summative",
  subtitle: "Analyzing the Impact of Election Swings on Democratic Vote Percentage",
  authors: ((name: "Z0195806", email: "bjsn39@durham.ac.uk"),),
  // Insert your abstract after the colon, wrapped in brackets.
  // Example: `abstract: [This is my abstract...]`
  abstract: "This thesis explores the effects of electoral swings on voter turnout within U.S. House elections from 1932 to 2008. Employing Difference in Differences (DiD) and Propensity Score Matching (PSM), the study analyzes how shifts exceeding five percentage points influence the Democratic vote percentage (DemPct). Results indicate that both positive and negative swings significantly affect voter turnout. The analysis reveals a nuanced response from voters to different types of electoral changes, contributing new insights into voter behavior and electoral dynamics. These findings have practical implications for political strategists and underscore the complexity of electoral influence on voter participation. The rigorous methodological approach strengthens the study's contributions to political science, particularly in understanding causal relationships where experimental methods are impractical.",
  date: today.display("[month repr:long] [day], [year]"),
)

// We generated the example code below so you can see how
// your document will look. Go ahead and replace it with
// your own content!
// ------------------------------------------------
// outline part
#show link: underline
#show outline.entry.where(level: 1): it => {
  v(12pt, weak: true)
  strong(it)
}
#outline(indent: auto)
// ------------------------------------------------

= Introduction

== Study Overview

Negative Election Swings are pivotal events that can reshape the political
landscape, influencing both the strategic decisions of parties and the
participation rates of voters. This thesis examines the impact of negative
election swings—defined as significant shifts away from the incumbent or
previously dominant party—on voter turnout in U.S. House elections from 1932 to
2008@dvn567RS6_2016. Utilizing a Difference in Differences (DiD) approach
complemented by Propensity Score Matching (PSM), this research aims to robustly
isolate the effects of such electoral dynamics from other variables affecting
voter engagement.

== Theoretical Framework

This research is grounded in the potential outcomes framework, which allows for
a structured exploration of causal relationships by hypothesizing different
potential realities—one where the intervention (negative election swing)
occurred and one where it did not. This approach is critical for estimating the
specific effect of negative campaigning on voter turnout by comparing observed
outcomes with estimated counterfactuals.

== Research Questions and Hypotheses

*"How does negative election swing affect voter turnout in U.S. House elections?"* Based
on the dataset, the hypothesis posits that negative election swing has a
statistically significant impact on voter turnout, considering the potential for
voter disillusionment and fatigue.

== Study Quantification and Protenial Result Framework

- *Definition of Negative Swing*: Defining negative swings quantitatively with
  threshold instead of a float number.
- *DiD Application*: Implementing Difference in Differences to assess changes in
  voter turnout between districts experiencing negative swings and those without
  such swings, across the front and back election cycles.
- *PSM Utilization*: Using Propensity Score Matching to control for confounding
  variables that could affect turnout, such as demographic shifts, economic
  changes, etc., ensuring that the treatment and control groups are well-matched.

= Theory

== Election Swing

An electoral swing analysis (or swing) shows the extent of change in voter
support, typically from one election to another, expressed as a positive or
negative percentage@swing_politics. In this study, I use One Party Swing, the
math formula is as follows:
$ "OnePartySwing" = "CurrentVotePercentage" - "PreviousVotePercentage" $

== Voter Turnout and Political Swing Research

Here is some literature review about the relationship between voter turnout and
political swing. According to the correlates of voter turnout, Richard W. delved
into different metrics for calculating voter turnout such as the Voting Eligible
Population (VEP) and Citizen Voting Age Population (CVAP)@frank2023correlates.
Another research from the MIT Election Lab examines voter turnout over time,
providing a comprehensive historical perspective@mit_electionlab_voterturnout.

Thomas Cao's study (2023) conducted during the 2022 midterm elections
investigated how information about bipartisan oversight of the electoral process
affects voter confidence and turnout@mit-electionlab-voterconfidence. Duquette
discusses the disproportionate influence of swing states due to the
winner-take-all nature of the U.S. Electoral College@SwingStates2017.

Political researchers also put there attention on the election analysis with
causal inference. In his work, "The Statistics of Causal Inference: A View from
Political Methodology," Luke Keele provides a comprehensive overview of the
statistics used in causal inference within political science. He discusses the
necessary assumptions for attributing a causal interpretation to statistical
estimates and emphasizes the importance of identification assumptions in
statistical analyses concerning causal effects@Keele_2015. Robert in his work
using RDD to prove there is no causal effect of Incumbency Advantage in the U.S.
House Elections@Erikson_Rader_2017.

== Donation in this Study

In my research, I delve into the impact of negative electoral swings on voter
turnout, building on existing research on electoral swings and voting behavior.
At the same time, reference was made to the variables used by predecessors. Most
previous studies have focused on the impact of electoral swings on party
policies and election results, but few studies have analyzed in detail how
negative swings affect voters' voting participation from the perspective of
causal inference. By combining difference-in-differences (DiD) methods and
propensity score matching (PSM), my study not only fills this gap but also
improves the rigor of the study, allowing us to more accurately estimate the
impact of this swing on voter behavior practical impact.

= Data

The dataset@dvn567RS6_2016 used in this study comprises a selection of variables
derived from the U.S. House Elections spanning from 1932 to 2008. The data was
sourced from a comprehensive electoral database, ensuring a robust foundation
for the analysis. Here is a brief description of each variable included in the
dataset:

+ *Main Variable for Causal Inference*
  - DWinNxt (Nominal Scale): Indicates whether the Democrat wins the next election
    (Yes = 1, No = 0). It measures the subsequent success of the Democratic
    candidate, providing insights into election trends and the persistence of
    political support.
  - DemWin (Nominal Scale): Indicates whether the Democrat won the current election
    (Yes = 1, No = 0). This variable is critical for understanding immediate
    electoral outcomes.
  - DPctNxt (Ratio Scale): Represents the percentage of the vote the Democratic
    candidate receives in the next election, providing a quantifiable measure of
    electoral support over successive election cycles.
  - DemPct (Ratio Scale): Represents the percentage of the vote the Democratic
    candidate receives in the current election, offering insights into the immediate
    electoral landscape.
   
  #figure(
    image("./imG/hist.png"),
    caption: [Histogram of the Turnout Rate in current year and next year],
  )
  
  Based on these four variables, the tables of Before-treatment and Post-treatment
  can be assembled. TThis can help us build the *DiD* model.
 
+ *Intervention*
  - ElcSwing (Interval Scale): Measures the change in the percentage of votes
    between current and previous elections for the Democratic candidate. It
    quantifies the magnitude and direction of electoral shifts, providing a key
    indicator of political change.
    #figure(image("./imG/EW.png"), caption: [Electoral Swings Before Conversion])
  
  By observing the distribution of electoral swing, manually set the value less
  than -5 to *TreatmentNeg=1* and the value greater than 5 to *TreatmentPos=1*.

+ *Other covarites inspried by the previous research*
  - *Social Data*
    - UrbanPct (Ratio Scale): The percentage of the population living in urban areas
      within the state. This demographic factor is essential for analyzing voter
      behavior across different urbanization levels.
    - GovWkPct (Ratio Scale): The percentage of the state's workforce employed in
      government jobs. This variable can indicate the economic structure of a region
      and its potential influence on voting behavior.
  - *Economic Data*
    - DSpndPct (Ratio Scale): Reflects the percentage of total campaign expenditure
      attributed to the Democratic candidate. This financial metric helps gauge the
      intensity and scale of campaign efforts.
    - DDonaPct (Ratio Scale): Measures the percentage of total donations received by
      the Democratic candidate, offering insight into financial backing and supporter
      enthusiasm.
  - *Political Data*
    - YearElec (Interval Scale): The year in which the election took place. This
      temporal variable allows for the analysis of trends and patterns over time.
    - StAlphCd (Nominal Scale): The state's alphabetical code, which is crucial for
      geographic comparisons and understanding regional political dynamics.
  
  These covariates are used to refit the model when DiD fails.

*Data Set Build*

The data set originally did not contain time, but by extracting the two data from last year and this year, and then marking them with postTrt, the time factor can be cleverly extracted to facilitate DiD analysis.

*This is current year data Before-treatment*
#align(
  center,
)[
  #table(
    columns: (auto, auto, auto, auto, auto, auto),
    inset: 10pt,
    align: horizon,
    table.header([*StAlphCd*], [*DemWin*], [*DemPct*], [*Treatment*], [*Other...*], [*PostTrt*]),
    $...$,
    $...$,
    $...$,
    $...$,
    $...$,
    $0$,
  )]*This is next year data Post-treatment*
#align(
  center,
)[
   
  #table(
    columns: (auto, auto, auto, auto, auto, auto),
    inset: 10pt,
    align: horizon,
    table.header([*StAlphCd*], [*DWinNxt*], [*DPctNxt*], [*Treatment*], [*Other...*], [*PostTrt*]),
    $...$,
    $...$,
    $...$,
    $...$,
    $...$,
    $1$,
  )]

Other exploratory data analyzes and their codes on these variables are placed in
*Appendix 2*.

== Methodology: Difference in Differences (DiD)

*Why DiD?* DiD is advantageous because it helps control for unobserved
confounders that are constant over time, as well as common shocks that affect
all units within the dataset. By comparing the changes in outcomes over time
between a treatment group and a control group, DiD isolates the "difference"
that is attributable to the intervention (election swing), assuming that these
groups would have followed *parallel paths* in the absence of the treatment.

=== Mathematical Representation of DiD
$ Y_"ij"=beta_0+beta_1T_i+beta_2P_t+beta_3(T_i times P_t) + X_"it" beta + epsilon_"it" $

Where:
- $Y_"ij"$: DemPct for district i in year j.
- $T_i$: High negative election swing as the Treatment.
- $P_t$: A binary variable indicating whether the year is a post-election swing
  year.
- $X_"it"$: A vector of control variables.
- $epsilon_"it"$: The error term.

=== Treatment Definition

Positive/Negative Value of Election Swing: Districts are separately categorized
for positive swings (greater than +5 points) and negative swings (less than -5
points) to examine differing impacts.

=== First Differences

- First, calculate the average Democratic percentage (DemPct) by time and
  treatment group to establish a baseline and post-treatment outcome measurement.
#align(center)[
   
  #table(
    columns: (auto, auto, auto),
    inset: 10pt,
    align: horizon,
    table.header([*Post-treatment*], [*Treatment*], [*DemPct*]),
    $0$,
    $0$,
    $51.30630$,
    $0$,
    $1$,
    $49.51980$,
    $1$,
    $0$,
    $51.08934$,
    $1$,
    $1$,
    $53.36237$,
  )]

- This step simplifies the model by focusing on aggregate changes, reducing the
  influence of outliers or extreme values.

=== Average Treatment Effect (ATT)

The ATT is computed by taking the difference in outcomes between pre-treatment
and post-treatment periods across the treatment and control groups. This figure
represents the net effect of the election swing.

$ "ATE"_"DiD" = (hat(Y)_"T,post"-hat(Y)_"T,pre")-(hat(Y)_"C,post"-hat(Y)_"C,pre") $

In our case, the ATT represents the change in Democratic vote percentage is
4.059529.

=== Counterfactual Outcome and Test the Parallel Trend Assumption

Constructs a counterfactual scenario for what the outcome would have been in the
absence of the treatment, enhancing the robustness of causal inference by
providing a baseline against which actual outcomes can be compared.

// #image("./img/not.png")
#figure(
  image("./img/not.png"),
  caption: [Graphical Representation of the Average Treatment Effect],
)

As can be seen from the image of ATE, due to the existence of covariates, Treatment does not meet the parallel hypothesis, so next we will add covariates to the linear model and PSM for analysis.

== Methodology: Propensity Score Matching (PSM)

*Why Use PSM?* PSM helps in balancing the observed covariates between the treated and control groups, reducing bias due to confounding variables. By ensuring that the treatment and control groups are comparable on all observed covariates, PSM makes it more plausible that differences in outcomes (like voter turnout) are due to the treatment (negative swing) rather than other factors. In observational studies where the treatment is not randomly assigned, PSM is a powerful tool to mimic randomization, thereby strengthening the validity of causal conclusions.

=== Model Building

I begin by estimating the propensity scores using a logistic regression model (glm) where the probability of being exposed to a negative swing (TreatmentNeg) is modeled as a function of several covariates (TreatmentPos, DemWin, DWinPrv, DSpndPct, DDonaPct, GovWkPct, UrbanPct). These covariates are chosen based on their potential to influence both the likelihood of experiencing a negative swing and the outcome (voter turnout). *Appednix 5.1*

=== Propensity Score Calculation

The propensity scores are calculated using the fitted values from the logistic regression, predicting the likelihood that each unit in the dataset receives the treatment based on observed characteristics. *Appendix 5.2*

=== Matching

With the MatchIt package, you use these propensity scores to match units in the treatment group (those experiencing negative swings) with similar units in the control group (those not experiencing negative swings). The nearest neighbor matching method is used, which pairs units with the closest propensity scores in a one-to-one fashion without replacement. *Appendix 5.3*

== Linear Model with Covariates

After matching, I fit a linear model that includes the covariates used in the propensity score model to estimate the effect of negative swings on voter turnout while controlling for potential confounders. This model allows us to assess the impact of negative swings on voter turnout more accurately by accounting for the influence of other variables. *Appendix 3.3*

After doing the robustness check, we can get the final best model.

*DemPct \~ Time:TreatmentNeg + TreatmentPos + DemWin + DWinPrv + DSpndPct + DDonaPct + GovWkPct + UrbanPct*

= Fingdings

== DiD Results

Since the parallelism assumption is not satisfied, DiD continues to the part where ATE is calculated. Then turn more energy to finding covariates.

== Analysis based on the summary of PSM (Appendix 5)

*Main Obervation Items*
+ Std. Mean Diff.: This is the standardized mean difference for each covariate between the treatment and control groups. Values closer to zero indicate a good balance.
+ Var. Ratio: The variance ratio between the treated and control groups. A ratio close to 1 indicates similar variance.
+ eCDF Mean and Max: These are the empirical cumulative distribution function mean and maximum differences, respectively. Smaller values suggest that the distribution of covariates between groups is more similar.

*From initial model*
- Variables such as DSpndPct, DDonaPct, and GovWkPct show reasonable balance in terms of mean differences and variance ratios, suggesting initial comparability.
However, significant differences are noted in distance, and the treatment presence (TreatmentPos) is significantly different, indicating a need for matching to better balance these characteristics.

*Post-Matching Balance*
- The distance metric's standardized mean difference significantly decreases, highlighting the effectiveness of the matching in aligning the propensity scores between groups.
- DemWin, DWinPrv, and demographic variables like GovWkPct and UrbanPct show improved balance, although some, like GovWkPct, still exhibit relatively larger differences, which may need further investigation or additional covariate adjustment.

*Analysis and Interpretation*
- The improvements in balance metrics post-matching suggest that the matching process effectively reduces the bias due to observed confounding variables. This enhances the credibility of causal inferences about the effect of negative election swings.
- The matched dataset can now be used to estimate the causal impact of negative swings more reliably, as it minimizes the influence of confounding variables that could otherwise skew the analysis.

== Linear Model Results
- This model effectively quantifies how both positive and negative election swings, along with other political and demographic factors, impact Democratic vote percentages. The significance and effect size of the interaction term (Time:TreatmentNeg) highlight the particular influence of temporal dynamics combined with negative swings, offering insights into how electoral contexts evolve and affect voter behavior over time.
- These findings not only enhance our understanding of electoral dynamics but also provide actionable insights for political strategists and policymakers looking to understand or influence electoral outcomes.

= Discussion and Conclusion
== Summary of Findings
- This study utilized Difference in Differences (DiD) and Propensity Score Matching (PSM) to explore the impact of negative and positive election swings on voter turnout. Key findings include:
- The presence of negative election swings is associated with a significant, albeit smaller, increase in DemPct over time, suggesting a complex response by voters to shifts in electoral sentiment.
- Other factors such as prior electoral wins, campaign spending, and donations also play substantial roles in influencing DemPct.
== Theoretical Implications
The results affirm the theory that both positive and negative electoral dynamics can mobilize voters, possibly due to heightened political engagement or increased campaign efforts in swing districts. These findings enrich the understanding of electoral behavior, suggesting that voters respond not just to the direction of the swing but to the magnitude and context of the change.
== Practical Implications
For political strategists and campaigners, recognizing the nuanced effects of electoral swings can inform more targeted and effective campaign strategies. For policymakers, understanding these dynamics could guide efforts to enhance voter engagement and turnout, particularly in swing regions.
== Limitations and Future Research
One limitation of this study is its reliance on historical electoral data, which may not fully capture the contemporary political landscape's nuances. Additionally, while PSM improves the robustness of causal inference, it cannot account for unobserved confounders that might still affect the outcomes.

Future research could extend these findings by incorporating more granular data on voter demographics or by examining the role of digital media in influencing voter behavior in swing districts. Longitudinal studies could also assess the long-term effects of repeated swings in the same districts to understand cumulative impacts on voter turnout.


#bibliography("references.bib")
