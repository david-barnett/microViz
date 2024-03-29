% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxatree_funs.R
\name{taxatree_funs}
\alias{taxatree_funs}
\alias{taxatree_nodes}
\alias{taxatree_edges}
\title{Create node and edge dataframes for taxatree_plots}
\usage{
taxatree_nodes(
  ps,
  fun = list(sum = sum),
  ranks = "all",
  .sort = NULL,
  .use_counts = TRUE
)

taxatree_edges(nodes_df)
}
\arguments{
\item{ps}{phyloseq object or psExtra}

\item{fun}{function to calculate for each taxon/node}

\item{ranks}{selection of taxonomic ranks to make nodes for ("all", or names)}

\item{.sort}{sort nodes by "increasing" or "decreasing" values of fun function result}

\item{.use_counts}{use count data if available (instead of transformed data)}

\item{nodes_df}{dataframe output from taxatree_nodes}
}
\description{
Mostly you will not have to use these functions directly:
instead call \code{taxatree_plots} with the output of \code{taxatree_stats}
\itemize{
\item \code{taxatree_nodes} creates taxon nodes and calculates a summary statistic
about each taxon (given by \code{fun}). Takes a psExtra or phyloseq object.
\item \code{taxatree_edges} uses the output of \code{taxatree_nodes} to create a
dataframe of edges.
}
}
\details{
\code{taxatree_nodes} makes nodes for taxa at all ranks or for a list of
consecutive ranks (plus a root rank if tree is not rooted).
}
