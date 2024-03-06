#import "template.typ": *

// Take a look at the file `template.typ` in the file panel
// to customize this template and discover how it works.
#show: project.with(
  title: "Causal Inference Formative",
  authors: ((name: "Zehao Qian", email: "zehao.qian.cn@gmail.com"),),
  // Insert your abstract after the colon, wrapped in brackets.
  // Example: `abstract: [This is my abstract...]`
  abstract: "In Formative, I'm trying to extend the reproduction of 'Messages Designed to Increase Perceived Electoral Closeness Increase Turnout'. This paper investigates the impact of perceived electoral closeness on voter turnout. It discusses the theoretical underpinnings related to voter behavior and electoral closeness, and addresses the limitations of previous studies by conducting a large-scale field experiment across seven states. The experiment involved sending telephone messages to registrants, comparing those who received messages suggesting elections are usually decided by a smaller number of votes (thus closer) with those who received messages suggesting a larger deciding vote margin. The findings indicate that messages emphasizing the closeness of elections significantly increase voter turnout, suggesting that perceived electoral closeness can indeed motivate more people to vote.",
  date: "March 6, 2024",
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

In democratic societies, voter turnout is not merely a metric of participation;
it is a cornerstone for legitimacy and representation. The vitality of democracy
is often gauged by the electorate's engagement, where higher voter turnout is
synonymous with a more vibrant democratic process. Within this context, the
concept of perceived electoral closeness emerges as a pivotal factor influencing
voter behavior. The decision-theoretic Downsian model@Downsian, a seminal
framework in political science, posits that the closer an election is perceived
to be, the higher the likelihood of voter turnout. This premise suggests that
the perceived competitiveness of an election can significantly motivate voters
to cast their ballots, underpinning the intrinsic connection between electoral
closeness and democratic participation.

Despite the Downsian framework's intuitive appeal in explaining voter turnout,
empirical research faces challenges in linking perceived electoral closeness
with voting behavior due to methodological and framing issues. This
study@Biggers2024 I replicated aims to clarify how perceptions of electoral
closeness affect voter engagement by adopting innovative experimental and
analytical approaches to distinguish the effects of perceived closeness from
other factors. This research not only enriches academic understanding but also
offers practical insights for enhancing voter mobilization and democratic
participation, highlighting the critical influence of perceived electoral
closeness in the democratic process.

= Research Background

== Conception of Election Closeness and Participation

The "Election Closeness and Participation" discusses the theoretical connection
between perceived electoral closeness and voter turnout, referencing various
theoretical frameworks that suggest closer elections should motivate higher
turnout due to factors like intrinsic voting rewards, strategic elite behavior,
and social or group benefits. Despite supportive empirical evidence from
aggregate studies and meta-analyses suggesting that closer contests indeed
correlate with higher turnout, the causal mechanisms remain ambiguous. This
ambiguity arises from potential confounding factors such as media attention and
campaign activities in anticipated close contests, as well as voter strategies
like minimax regret that do not directly relate perceived closeness to the
decision to vote. The section highlights the need for experimental methods to
clearly understand how perceived closeness directly influences voter behavior.

== Limitations of previous studies

The "Election Closeness and Participation" section of the paper outlines several
theoretical approaches that suggest a relationship between perceived electoral
closeness and voter turnout, such as the intrinsic rewards of voting, strategic
behavior of elites in tight contests, and social or group benefits derived from
voting. However, the paper identifies several limitations in empirical tests of
this relationship:

+ *Ambiguity in Causal Mechanisms*: The mechanisms explaining the relationship
  between election closeness and voter turnout are not clear. For example, close
  contests may receive more media attention and campaign activities, which could
  influence voter turnout. Additionally, strategies like minimax regret, where
  voters don't assign probabilities to perceived closeness but turn out in a
  manner that appears consistent with concerns about closeness, have not been
  conclusively linked to actual voter behavior.
+ *Potential Confounding Factors*: The paper discusses how close elections might
  be close because of high voter turnout or due to other unconsidered factors that
  affect turnout. This ambiguity about the mechanisms through which perceived
  closeness influences turnout suggests a need for experimental manipulation to
  clarify these effects.
+ *Generalization and Credibility Issues*: Previous observational and experimental
  tests have struggled with issues of generalizability and credible inference.
  Prior field experiments have either conflated changes in closeness perceptions
  with the framing effects of voter encouragement messages or have focused on
  specific races, potentially altering beliefs about the election's
  characteristics beyond its closeness.

= Methodology

In the methodology section of their 2014 Seven-State Field Experiment, Biggers
et al. describe conducting a field experiment during the primary elections in
Massachusetts, Michigan, Minnesota, Missouri, New Hampshire, Tennessee, and
Wisconsin. The experiment targeted all registered voters eligible to vote in at
least one party's primary election. The team obtained comprehensive lists of
registered voters in each state and, prior to treatment assignment, excluded
certain records likely to be invalid or individuals who could not be reached by
phone. In households with multiple registrants, one was randomly chosen for the
study.

Participants were randomly assigned to one of four groups: a placebo group, a
traditional election reminder group, a close elections message group, and a
less-close elections message group. This assignment was stratified by state, the
competitiveness of the district's House race, and the individual's past voting
record. The messages were delivered via telephone by a professional survey
vendor in the four days leading up to each state's primary election, starting
with a standard question to confirm the respondent's state residency. This
approach ensured that the inclusion in the analysis was not influenced by
variations in the treatment content.

The effectiveness of the interventions was measured by voter turnout as recorded
in updated state voter files in spring 2015, with individuals classified as
having voted based on their listing in the official record. The experiment aimed
to isolate the effect of perceived electoral closeness on voter turnout by
comparing the turnout among those who received messages suggesting the elections
were very close (Closeness 350) versus somewhat close (Closeness 2500),
alongside a traditional election reminder and a placebo control with no
electoral content.

== Multinomial Logistic Regression

Used for balance tests to assess whether the treatment assignment (e.g.,
placebo, information only, closeness with varying vote margins) is balanced
across various covariates such as years since registration, age on election day,
gender, race, and voting history. This ensures that the experimental groups are
comparable across these characteristics.

$ log(P("Treatment"_i=k))/P("Treatment"_i="base")=beta_"0k"+sum_"j=1"^n beta_"jk" X_"ji" $

== Descriptive Statistics and Proportions

Employed to calculate turnout rates and other summary statistics for various
subsets of the data, such as by treatment group or state. This provides a basic
understanding of the data distribution and the context of the elections across
different states.

$ P_s=1/n_s sum_"i=1"^n_s I("State"_i=s and "Treatment"_i = "placebo" and "Voted"_i) $

== Ordinary Least Squares Regression

Used to estimate the effect of the closeness treatments on voter turnout,
controlling for other factors. This model is applied to different subsets of the
data, including by state and voter history, to examine the treatment effects
across various segments.

$ "Voted"_"2014_primary"_i=beta_0+beta_1"Close350not2500"_i+sum_j beta_j X_"ji"+epsilon_i $

== Logistic Regression

Similar to OLS regression but used for binary outcome variables, in this case,
voter turnout (voted or not). This model is suitable for binary response data
and provides estimates of the odds ratios for the predictors.

$ log(P("Voted"_"2014_primary"_i=1)/(1-P("Voted"_"2014_primary"_i=1))) = beta_0+beta_1"Close350not2500"_i+sum_j beta_j X_"ji" $

== Interaction Effects, Comparative Effectiveness Analysis

Includes the generation of interaction terms (e.g., close350not2500, ageunder50)
to examine how the effect of closeness messages on voter turnout varies by age.
Regression models are also used to compare the effectiveness of different
treatments (e.g., election reminder, closeness messages with different vote
margins) on voter turnout. ($"Close350not2500"="CN"$, $"AgeUnder50"="AU"$)

$ "Voted"_"2014_primary"_i = beta_0+beta_1 "CN"_i +beta_2 "AU"_i +beta_3("CN"_i times "AU"_i) +sum_j beta_j X_"ji"+epsilon_i $

$ "Voted"_"2014_primary"_i = beta_0 + beta_1 "InfoOnly"_i + beta_2 "Close350"_i + beta_3 "Close2500"_i +sum_j beta_j X_"ji"+epsilon_i $

== Intent to Vote Analysis

Examines the relationship between individuals' stated intention to vote and
their actual turnout, using regression mod ($"IntedToVote" = "ITV"$).

$ "Voted"_"2014_primary"_i = beta_0 + beta_1 "ITV"_i + beta_2 "CN"_i + beta_3 ("CN"_i times "ITV"_i) +sum_j beta_j X_"ji"+epsilon_i $

== DiD and Instrumental Variables

*Difference-in-Differences* can be used to compare the pre- and post-treatment outcomes in treated and control groups to isolate the treatment effect from other time-varying factors. In the context of the study, a DiD approach could compare voter turnout rates before and after receiving the closeness messages (treatment) across the different treatment and control groups.

*Instrumental Variables* can be utilized to address potential endogeneity issues, such as self-selection into treatment groups, by using instruments that affect the treatment status but are not directly related to the outcome variable. In this study, an instrument could be a variable that predicts whether an individual receives a closeness message but is not directly related to their likelihood of voting.

= Code Reproduction with R

"Messages Designed to Increase Perceived Electoral Closeness Increase Turnout" was studied using Stata originally, and I rewrote it in R when I reproduced the experiment. I used RMarkdown to render and export the file to PDF format in the appendix part.

= Results and Conclusion

== Results Analysis

After exploring the effect of perceived electoral closeness on voter turnout, particularly comparing the impact of two different treatments: the "Closeness 350" treatment, which implied a very close election, and the "Closeness 2500" treatment, which suggested a less close but still competitive election. The analysis revealed that individuals in the "Closeness 350" condition were more likely to vote compared to those in the "Closeness 2500" condition, with a turnout difference of approximately 1.6 percentage points, which was statistically significant (p = 0.02). This difference persisted even after controlling for various covariates and accounting for potential biases due to variations in treatment rates across different strata. The regression estimate indicated a 1.2 percentage point increase in turnout among those who received the "Closeness 350" message compared to the "Closeness 2500" message, translating to about a 5% higher turnout.

#image("./img/ETE.png")

Further analysis across different states, electoral contexts, and voters' past history showed the consistency of the effect, indicating that the "Closeness 350" message was more effective in most cases. Moreover, when comparing the closeness messages to a standard election reminder and a placebo message, the "Closeness 350" message proved to be the most effective in increasing voter turnout, showing a 13.1% increase compared to the placebo group's turnout rate.

== Conclusion

The final concludesion is communication emphasizing the potential closeness of an election can significantly increase voter participation. This suggests that individuals are more likely to vote when they believe their vote could be decisive in a close election. The findings underscore the importance of perceived electoral closeness in mobilizing voters and highlight the effectiveness of specifically crafted messages in increasing turnout. However, the study also acknowledges potential limitations in generalizability and the need for further research to explore the underlying mechanisms and broader applicability of these findings.

#bibliography("references.bib")
