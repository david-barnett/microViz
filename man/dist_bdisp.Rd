% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_bdisp.R
\name{dist_bdisp}
\alias{dist_bdisp}
\title{Wrapper for vegan::betadisper()}
\usage{
dist_bdisp(
  data,
  variables,
  method = c("centroid", "median")[[1]],
  complete_cases = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{data}{psExtra output from dist_calc}

\item{variables}{list of variables to use as group}

\item{method}{centroid or median}

\item{complete_cases}{drop samples with NAs in any of the variables listed}

\item{verbose}{sends messages about progress if true}
}
\value{
psExtra containing betadisper results
}
\description{
Takes the output of dist_calc function. Or use with the result of the permanova function to ensure the results correspond to exactly the same input data.
Runs betadisper for all categorical variables in variables argument.
See help('betadisper', package = 'vegan').
}
\examples{
library(phyloseq)
library(vegan)
data("dietswap", package = "microbiome")

# add some missings to demonstrate automated removal
sample_data(dietswap)$sex[3:6] <- NA
# create a numeric variable to show it will be skipped with a warning
dietswap <- ps_mutate(dietswap, timepoint = as.numeric(timepoint))

# straight to the betadisp
bd1 <- dietswap \%>\%
  tax_agg("Genus") \%>\%
  dist_calc("aitchison") \%>\%
  dist_bdisp(variables = c("sex", "bmi_group", "timepoint")) \%>\%
  bdisp_get()
bd1$sex
# quick vegan plotting methods
plot(bd1$sex$model, label.cex = 0.5)
boxplot(bd1$sex$model)

# compute distance and use for both permanova and dist_bdisp
testDist <- dietswap \%>\%
  tax_agg("Genus") \%>\%
  dist_calc("bray")

PERM <- testDist \%>\%
  dist_permanova(
    variables = c("sex", "bmi_group"),
    n_processes = 1, n_perms = 99
  )
str(PERM, max.level = 1)

bd <- PERM \%>\% dist_bdisp(variables = c("sex", "bmi_group"))
bd
}
