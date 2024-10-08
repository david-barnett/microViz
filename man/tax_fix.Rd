% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tax_fix.R
\name{tax_fix}
\alias{tax_fix}
\title{Replace unknown, NA, or short tax_table values}
\usage{
tax_fix(
  ps,
  min_length = 4,
  unknowns = NA,
  suffix_rank = "classified",
  sep = " ",
  anon_unique = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{ps}{phyloseq or tax_table (taxonomyTable)}

\item{min_length}{replace strings shorter than this, must be integer > 0}

\item{unknowns}{also replace strings matching any in this vector,
NA default vector shown in details!}

\item{suffix_rank}{"classified" (default) or "current", when replacing an entry,
should the suffix be taken from the lowest classified rank
for that taxon, "classified", or the "current" unclassified rank?}

\item{sep}{character(s) separating new name and taxonomic rank level suffix (see suffix_rank)}

\item{anon_unique}{make anonymous taxa unique by replacing unknowns with taxa_name?
otherwise they are replaced with paste("unknown", first_rank_name),
which is therefore the same for every anonymous taxon, meaning they will be merged if tax_agg is used.
(anonymous taxa are taxa with all unknown values in their tax_table row,
i.e. cannot be classified even at highest rank available)}

\item{verbose}{emit warnings when cannot replace with informative name?}
}
\value{
object same class as ps
}
\description{
Identifies phyloseq tax_table values as unknown or uninformative and
replaces them with the first informative value from a higher taxonomic rank.
\itemize{
\item Short values in phyloseq tax_table are typically empty strings or " ", or "g__" etc.
so it is helpful to replace them. (If this is unwanted: set \code{min_length} = 0 to avoid filtering on length.)
\item Values in \code{unknowns} are also removed, even if longer than \code{min_length}.
It is up to the user to specify sensible values in \code{unknowns} if their dataset has other unwanted values.
\item NA values are also replaced.
}

See this article for an extended discussion of tax_table fixing.
\url{https://david-barnett.github.io/microViz/articles/web-only/tax-fixing.html}
}
\details{
By default (unknowns = NA), unknowns is set to a vector containing:

's__' 'g__' 'f__' 'o__' 'c__' 'p__' 'k__' 'S__' 'G__' 'F__' 'O__' 'C__' 'P__' 'K__' 'NA' 'NaN' ' ' ''
'unknown' 'Unknown' 's__unknown' 's__Unknown' 's__NA' 'g__unknown' 'g__Unknown' 'g__NA'
'f__unknown' 'f__Unknown' 'f__NA' 'o__unknown' 'o__Unknown' 'o__NA' 'c__unknown' 'c__Unknown' 'c__NA'
'p__unknown' 'p__Unknown' 'p__NA' 'k__unknown' 'k__Unknown' 'k__NA' 'S__unknown' 'S__Unknown' 'S__NA'
'G__unknown' 'G__Unknown' 'G__NA' 'F__unknown' 'F__Unknown' 'F__NA' 'O__unknown' 'O__Unknown' 'O__NA'
'C__unknown' 'C__Unknown' 'C__NA' 'P__unknown' 'P__Unknown' 'P__NA' 'K__unknown' 'K__Unknown' 'K__NA'
}
\examples{
library(dplyr)
library(phyloseq)

data(dietswap, package = "microbiome")
ps <- dietswap

# create unknowns to test filling
tt <- tax_table(ps)
ntax <- ntaxa(ps)
set.seed(123)
g <- sample(1:ntax, 30)
f <- sample(g, 10)
p <- sample(f, 3)
tt[g, 3] <- "g__"
tt[f, 2] <- "f__"
tt[p, 1] <- "p__"
tt[sample(1:ntax, 10), 3] <- "unknown"
# create a row with only NAs
tt[1, ] <- NA

tax_table(ps) <- tax_table(tt)

ps
# tax_fix with defaults should solve most problems
tax_table(ps) \%>\% head(50)

# this will replace "unknown"s as well as short values including "g__" and "f__"
tax_fix(ps) \%>\%
  tax_table() \%>\%
  head(50)

# This will only replace short entries, and so won't replace literal "unknown" values
ps \%>\%
  tax_fix(unknowns = NULL) \%>\%
  tax_table() \%>\%
  head(50)

# Change rank suffix and separator settings
tax_fix(ps, suffix_rank = "current", sep = " - ") \%>\%
  tax_table() \%>\%
  head(50)

# by default, completely unclassified (anonymous) taxa are named by their
# taxa_names / rownames at all ranks.
# This makes anonymous taxa distinct from each other,
# and so they won't be merged on aggregation with tax_agg.
# If you think your anonymous taxa should merge on tax_agg,
# or you just want them to be named the all same for another reason,
# set anon_unique = FALSE (compare the warning messages)
tax_fix(ps, anon_unique = FALSE)
tax_fix(ps, anon_unique = TRUE)

# here's a larger example tax_table shows its still fast with 1000s rows,
# from microbiomeutilities package
# library(microbiomeutilities)
# data("hmp2")
# system.time(tax_fix(hmp2, min_length = 1))
}
\seealso{
\code{\link{tax_fix_interactive}} for interactive tax_fix help
}
