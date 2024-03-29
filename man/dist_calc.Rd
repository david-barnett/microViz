% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_calc.R
\name{dist_calc}
\alias{dist_calc}
\title{Calculate distances between pairs of samples in phyloseq object}
\usage{
dist_calc(data, dist = "bray", gunifrac_alpha = 0.5, ...)
}
\arguments{
\item{data}{psExtra object, e.g. output from tax_transform()}

\item{dist}{name of distance to calculate between pairs of samples}

\item{gunifrac_alpha}{setting alpha value only relevant if gunifrac distance used}

\item{...}{optional distance-specific named arguments passed to phyloseq::distance()}
}
\value{
psExtra object including distance matrix and name of distance used
}
\description{
Can compute various sample-sample distances using the microbiota composition of your samples:
\itemize{
\item Bray Curtis ('bray') or any other ecological distance from phyloseq::distance() / vegan::vegdist()
\item UniFrac distances (using the GUniFrac package)
\itemize{
\item generalised: 'gunifrac' (optionally set weighting alpha in gunifrac alpha)
\item unweighted: 'unifrac'
\item weighted: 'wunifrac'
}
\item Aitchison distance (Euclidean distance after centered log ratio transform clr, see details)
\item Euclidean distance
}

Use dist_calc with psExtra output of tax_transform (or tax_agg).
It returns a psExtra object containing the phyloseq and the name of the distance used
in addition to the distance matrix itself.
The resulting object is intended to be piped into ord_calc or dist_permanova functions.
Alternatively you can directly access the distance matrix with dist_get().
}
\section{Aitchison distance note}{


You should EITHER:
\enumerate{
\item skip the dist_calc function and call ord_calc(method = "PCA") directly on an object with taxa transformed with tax_transform(trans = "clr")
\item pass an object with untransformed (or 'identity' transformed) taxa to the data argument of dist_calc() and specify dist = "aitchison".
}

If ordination plots with taxon loading vectors are desired, users require option 1.
If the distance matrix is required for permanova, users require option 2.
}

\section{Binary Jaccard distance note}{


Jaccard distance can be computed on abundances, but often in microbiome
research it is the Binary Jaccard distance that is desired. So remember to
first perform a "binary" transformation with \code{tax_transform("binary")},
OR pass an additional argument to \code{dist_calc("jaccard", binary = TRUE)}
}

\examples{
# bray curtis distance on genera-level features
data("dietswap", package = "microbiome")
bc <- dietswap \%>\%
  tax_agg("Genus") \%>\%
  dist_calc("bray")
bc
class(bc)

# gunifrac distance using phyloseq input
data("esophagus", package = "phyloseq")
gunifrac <- esophagus \%>\%
  dist_calc("gunifrac") \%>\%
  dist_get()
class(gunifrac)
}
\seealso{
\code{\link{tax_transform}} for the function to use before dist_calc

\code{\link{ord_calc}}

\code{\link{ord_plot}}

\code{\link{dist_permanova}}

\code{phyloseq::\link[phyloseq:distance]{distance}}

\code{vegan::\link[vegan:vegdist]{vegdist}}
}
