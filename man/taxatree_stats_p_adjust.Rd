% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxatree_stats_p_adjust.R
\name{taxatree_stats_p_adjust}
\alias{taxatree_stats_p_adjust}
\title{Adjust p values in taxatree_stats dataframe}
\usage{
taxatree_stats_p_adjust(
  data,
  method,
  grouping = "rank",
  p = "p.value",
  new_var = NULL
)
}
\arguments{
\item{data}{psExtra with taxatree_stats dataframe, or just the dataframe}

\item{method}{any method from \code{stats::p.adjust.methods}}

\item{grouping}{defines grouping of p-values into families for adjustment, see details.}

\item{p}{name of variable containing p values for adjustment}

\item{new_var}{name of new variable created for adjusted p values
(automatically inferred by default)}
}
\value{
psExtra with dataframe of statistics, or just the data.frame
}
\description{
Apply a p value adjustment method from \code{stats::p.adjust.methods},
such as false-discovery rate adjustment with "BH",
or more conservative family-wise error rate controlling methods such as "holm" or "bonferroni".
}
\details{
Define how to group the p values for adjustment with the \code{grouping} argument.
The default is to adjust the p values in groups at each taxonomic rank,
but you could also adjust per "taxon" or per "term".
Or even group by a combination of rank and term with c("rank", "term").
You should specify the name of the new variable containing the adjusted
p values in the new_var argument. If left as NULL the new variable name will
be created by pasting together p.adj, the method, and the grouping variable(s)
separated by ".".
}
\examples{
# This example is an abbreviated excerpt from article on taxon modelling on
# the microViz documentation website

library(corncob)
library(dplyr)
data("ibd", package = "microViz")

# We'll keep only the Ulcerative Colitis and Healthy Control samples, to
# simplify the analyses for this example. We'll also remove the Species
# rank information, as most OTUs in this dataset are not assigned to a
# species. We'll also use `tax_fix` to fill any gaps where the Genus is
# unknown, with the family name or whatever higher rank classification is
# known.

phylo <- ibd \%>\%
  ps_filter(DiseaseState \%in\% c("UC", "nonIBD")) \%>\%
  tax_mutate(Species = NULL) \%>\%
  tax_fix()

# Let's make some sample data variables that are easier to use and compare
# in the statistical modelling ahead. We will convert dichotomous
# categorical variables into similar binary variables (values: 1 for true,
# or 0 for false). We will also scale and center the numeric variable for
# age.

phylo <- phylo \%>\%
  ps_mutate(
    UC = ifelse(DiseaseState == "UC", yes = 1, no = 0),
    female = ifelse(gender == "female", yes = 1, no = 0),
    antibiotics = ifelse(abx == "abx", yes = 1, no = 0),
    steroids = ifelse(steroids == "steroids", yes = 1, no = 0),
    age_scaled = scale(age, center = TRUE, scale = TRUE)
  )

bb_models <- phylo \%>\%
  tax_fix() \%>\%
  tax_prepend_ranks() \%>\%
  tax_filter(min_prevalence = 0.3) \%>\%
  taxatree_models(
    type = corncob::bbdml,
    ranks = c("Phylum", "Class", "Order"),
    variables = c("UC", "female", "antibiotics", "steroids", "age_scaled")
  )

bb_stats <- bb_models \%>\%
  taxatree_models2stats(param = "mu") \%>\%
  taxatree_stats_p_adjust(method = "BH", grouping = "rank")

bb_stats

bb_stats \%>\% taxatree_stats_get()

# you can also directly modify the dataframe,
# and choose a different variable name
bb_stats \%>\%
  taxatree_stats_get() \%>\%
  taxatree_stats_p_adjust(
    method = "holm", grouping = "taxon", new_var = "p_adj_holm"
  )

# see all available adjustment methods
stats::p.adjust.methods
}
\seealso{
\code{\link{taxatree_models2stats}}

\code{\link{taxatree_models}}

\code{stats::\link[stats]{p.adjust}}
}
