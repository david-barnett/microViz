#' Modify or compute new taxonomic ranks in phyloseq
#'
#' Add or overwrite tax_table ranks.
#' Use dplyr::mutate() syntax.
#'
#' @param data phyloseq object with a tax_table, or just a tax_table
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
#' tax_mutate(dietswap, dplyr::across(.fns = toupper)) %>%
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
tax_mutate <- function(data, ...) {
  if (inherits(data, "ps_extra")) {
    warning("data is ps_extra but only a phyloseq will be returned")
    data <- ps_get(data)
  }
  # get tt
  tt <- tt_get(data)

  # convert to dataframe
  tt <- as.data.frame(tt)

  # mutate, ensuring rownames saved
  saved_rownames <- rownames(tt)
  tt <- dplyr::mutate(tt, ...)
  rownames(tt) <- saved_rownames

  # check mutation has not introduced non-character columns
  taxMutateCheckAllChar(tt)

  # coerce to taxonomy table
  tt <- as.matrix.data.frame(tt)
  tt <- phyloseq::tax_table(tt)

  if (methods::is(data, "taxonomyTable")) {
    return(tt)
  }
  if (methods::is(data, "phyloseq")) {
    phyloseq::tax_table(data) <- tt
    return(data)
  }
}


# helper to check taxtable doesn't contain non-character variables now
taxMutateCheckAllChar <- function(df) {
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
      "\n - tax_mutate created a non-character rank in taxonomy table\n",
      " - ranks in taxonomy table must all be of class 'character'\n",
      " - non-character ranks are:\n",
      paste0("   * ", bad_out)
    )
  }
}
