% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tax_prepend_ranks.R
\name{tax_prepend_ranks}
\alias{tax_prepend_ranks}
\title{Add rank prefixes to phyloseq tax_table values}
\usage{
tax_prepend_ranks(ps, sep = ": ", nchar = 1)
}
\arguments{
\item{ps}{phyloseq object}

\item{sep}{characters to paste in between rank initial and taxon name}

\item{nchar}{number of characters to use from start of rank_names}
}
\value{
phyloseq
}
\description{
Prepend the start of rank names to each taxon at each rank
(useful particularly in case of duplicated taxa names across ranks, e.g. dietswap dataset)
}
\examples{
data("dietswap", package = "microbiome")
phyloseq::tax_table(dietswap) \%>\% head()
dietswap \%>\%
  tax_prepend_ranks() \%>\%
  phyloseq::tax_table() \%>\%
  head()
}
\seealso{
\code{\link{tax_fix}} for fixing other tax_table problems
}
