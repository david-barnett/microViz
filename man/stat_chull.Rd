% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_chull.R
\name{stat_chull}
\alias{stat_chull}
\title{Draw convex hull for a set of points on a ggplot}
\usage{
stat_chull(
  mapping = NULL,
  data = NULL,
  geom = "polygonHollow",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{geom}{The geometric object to use to display the data, either as a
\code{ggproto} \code{Geom} subclass or as a string naming the geom stripped of the
\code{geom_} prefix (e.g. \code{"point"} rather than \code{"geom_point"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}
}
\description{
Draws a (convex) polygon around the outermost points of a set of points.
Useful as a visual aid for identifying groups of points on a scatterplot,
such as an ordination plot.
}
\details{
This is a ggplot2 extension - slightly modified from the original code found here:

\url{https://CRAN.r-project.org/package=ggplot2/vignettes/extending-ggplot2.html}
}
\examples{
library(ggplot2)
microViz::ibd \%>\%
  tax_fix() \%>\%
  tax_transform(rank = "Genus", trans = "clr") \%>\%
  ord_calc(method = "PCA") \%>\%
  ord_plot(colour = "DiseaseState", shape = "DiseaseState", alpha = 0.5) +
  stat_chull(aes(colour = DiseaseState))

microViz::ibd \%>\%
  tax_fix() \%>\%
  tax_transform(rank = "Genus", trans = "clr") \%>\%
  ord_calc(method = "PCA") \%>\%
  ord_plot(colour = "DiseaseState", shape = "DiseaseState", alpha = 0.5) +
  stat_chull(aes(colour = DiseaseState, fill = DiseaseState), alpha = 0.1)
}
\seealso{
\code{ggplot2::\link{stat_ellipse}}

\code{\link{ord_plot}}
}
