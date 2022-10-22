#' Add taxa_names as last column in phyloseq tax_table
#'
#' The taxa names in your phyloseq may specify a further unique classification
#' of your taxa, e.g. ASVs, that is not otherwise represented in the tax_table itself.
#' This function fixes that, and allows you to include this level in taxatree_plots for example.
#'
#' `tax_names2tt` is the old name of `tax_names2rank`.
#' The use of `tax_names2tt` is deprecated.
#'
#' @param data phyloseq object, or ps_extra or tax_table (taxonomyTable)
#' @param colname name of new rank to add at right side of tax_table
#'
#' @return same class object as passed in to data
#' @export
tax_names2rank <- function(data, colname = "unique") {
  # get tt
  tt <- tt_get(data)

  # return data unchanged if already last column matches colname
  if (identical(colname, rev(phyloseq::rank_names(tt))[[1]])) {
    return(data)
  }

  # modify tt
  new <- matrix(data = rownames(tt), ncol = 1, dimnames = list(NULL, colname))
  tt <- cbind(tt, new)
  tt <- phyloseq::tax_table(tt)

  # return object
  if (is(data, "taxonomyTable")) {
    return(tt)
  }
  if (is(data, "psExtra")) {
    data@tax_table <- tt
  } else if (is(data, "phyloseq")) {
    phyloseq::tax_table(data) <- tt
  } else if (inherits(data, "ps_extra")) {
    phyloseq::tax_table(data$ps) <- tt
  } else {
    stop("Bad data - should never reach this line")
  }
  return(data)
}

#' @export
#' @rdname tax_names2rank
tax_names2tt <- function(data, colname = "unique") {
  .Deprecated(new = "tax_names2rank")
  out <- tax_names2rank(data, colname = colname)
  return(out)
}
