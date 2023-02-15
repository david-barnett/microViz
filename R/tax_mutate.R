#' Modify or compute new taxonomic ranks in phyloseq
#'
#' Add or overwrite tax_table ranks.
#' Use dplyr::mutate() syntax.
#'
#' @param ps phyloseq object with a tax_table, or just a tax_table
#' @param ... passed straight to dplyr::mutate (see examples and dplyr::mutate help)
#'
#' @return phyloseq object with modified tax_table
#' @export
#'
#' @seealso \code{\link[dplyr]{mutate}}
#' @seealso \code{\link{ps_mutate}}
#'
#' @examples
#' library(phyloseq)
#' library(dplyr)
#' data("dietswap", package = "microbiome")
#'
#' # compute new rank
#' tax_mutate(dietswap, loud_genus = toupper(Genus)) %>%
#'   tt_get() %>%
#'   head()
#'
#' # overwrite a current rank
#' tax_mutate(dietswap, Genus = toupper(Genus)) %>%
#'   tt_get() %>%
#'   head()
#'
#' # overwrite all ranks
#' tax_mutate(dietswap, across(everything(), .fns = toupper)) %>%
#'   tt_get() %>%
#'   head()
#'
#' # add a new rank at the beginning
#' tax_mutate(dietswap, Root = "Bacteria", .before = 1) %>%
#'   tt_get() %>%
#'   head()
#'
#' # this is an error as ranks can't be any other class than character
#' # tax_mutate(dietswap, Genus = 1:ntaxa(dietswap))
tax_mutate <- function(ps, ...) {
  stopif_ps_extra(ps, argName = "ps")
  if (is(ps, "psExtra")) {
    warning("ps argument is a psExtra, but only a phyloseq will be returned")
    ps <- ps_get(ps)
  }
  # get tt
  tt <- tt_get(ps)

  # convert to dataframe
  tt <- as.data.frame(tt, stringsAsFactors = FALSE)

  # mutate, ensuring rownames saved
  saved_rownames <- rownames(tt)
  tt <- dplyr::mutate(tt, ...)
  rownames(tt) <- saved_rownames

  # check mutation has not introduced non-character columns
  taxtabCheckAllChar(tt, fun = "tax_mutate")

  # coerce to taxonomy table
  tt <- as.matrix.data.frame(tt)
  tt <- phyloseq::tax_table(tt)

  if (methods::is(ps, "taxonomyTable")) {
    return(tt)
  }
  if (methods::is(ps, "phyloseq")) {
    phyloseq::tax_table(ps) <- tt
    return(ps)
  }
}


# helper to check taxtable doesn't contain non-character variables now
taxtabCheckAllChar <- function(df, fun = "tax_mutate") {
  colNames <- colnames(df)
  if (length(colNames) < 1) stop("you cannot create a tax_table with no ranks")
  classes <- lapply(X = df, FUN = class)
  isChar <- sapply(classes, function(x) identical(x, "character"))
  if (!all(isChar)) {
    bad <- classes[!isChar]
    bad_classes <- sapply(bad, paste, collapse = " ")
    bad_out <- paste0(names(bad), ": ", bad_classes)
    bad_out <- paste(bad_out, collapse = "\n   * ")
    stop(
      call. = FALSE,
      paste("\n -", fun, "created a non-character rank in taxonomy table\n"),
      " - ranks in taxonomy table must all be of class 'character'\n",
      " - non-character ranks are:\n",
      paste0("   * ", bad_out)
    )
  }
}
