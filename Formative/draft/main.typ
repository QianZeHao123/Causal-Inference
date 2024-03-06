#import "template.typ": *

// Take a look at the file `template.typ` in the file panel
// to customize this template and discover how it works.
#show: project.with(
  title: "Causal Inference Formative",
  authors: ((name: "Zehao Qian", email: "zehao.qian.cn@gmail.com"),),
  // Insert your abstract after the colon, wrapped in brackets.
  // Example: `abstract: [This is my abstract...]`
  abstract: lorem(59),
  date: "March 2, 2024",
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

= Some links

#link("https://github.com/erikgahner/PolData")[
  PolData: Political Data Sets and Functions
]

#link(
  "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/RAHKNJ",
)[Replication Data for: 'Visual Inference and Graphical Representation in
  Regression Discontinuity Designs']

#link(
  "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PGXO5O",
)[Replication Data for: Regression Discontinuity Designs Based on Population
  Thresholds: Pitfalls and Solutions]

#link(
  "https://www.science.org/doi/10.1126/sciadv.abg2652",
)[How to make causal inferences using texts]

#link(
  "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/B2ATF8",
)[Replication Data for: Deeper Roots: Historical Causal Inference and the
  Political Legacy of Slavery]

#link(
  "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HJDUMY",
)[Replication Data for: Projection in the Face of Centrism: Voter Inferences about
  Candidates' Party Affiliation in Low-information Contexts]

#link(
  "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VYTZAI",
)[Replication Data for: Messages Designed to Increase Perceived Electoral
  Closeness Increase Turnout]

= Introduction

#lorem(60)@Biggers2024

#lorem(60)

== In this paper
#lorem(20)

=== Contributions
#lorem(40)

= Related Work

== Work 1

== Work 2

#lorem(500)



#bibliography("references.bib")
