#' Subset phyloseq object by (partial) taxa names
#'
#' @description
#' Convenient name-based taxa selection/filtering of phyloseq object, including approximate name matching.
#' Takes a phyloseq with tax table and a (partial) taxonomic name, or a list/vector of taxonomic names (full or partial matches).
#'
#' @details
#' tax_select will also search the otu names/rownames, BUT only for perfect matches.
#'
#' @param ps phyloseq object
#' @param tax_list e.g. c('g__Bifidobacterium', 'g__Akkermansia', 'g__Bacteroides', 'g__Streptococcus')
#' @param ranks_searched 'all' or a list of which taxonomic ranks should be searched for the names in tax_list?
#' @param strict_matches only perfect full name matches allowed if TRUE
#' @param n_typos how many typos to allow in each name? uses agrep approximate matching if > 0
#' @param deselect if TRUE, the matching taxa will be REMOVED instead!
#'
#' @return phyloseq object with fewer taxa
#' @export
#'
#' @seealso \code{\link{ps_select}} for selecting variables in phyloseq sample_data
#' @seealso \code{\link{agrep}} for the function that powers the approximate matching in tax_select
#'
#' @examples
#' # Get example phyloseq object data
#' data("dietswap", package = "microbiome")
#' pSeq <- dietswap
#'
#' # SELECTION EXAMPLES #
#' a <- pSeq %>% tax_select(tax_list = "Bif", n_typos = 0, ranks_searched = "Genus")
#' b <- pSeq %>% tax_select(tax_list = "Bifidobacterium", n_typos = 0)
#' c <- pSeq %>% tax_select(tax_list = "Bif", n_typos = 1)
#' identical(a, b) # TRUE
#' identical(a, c) # FALSE
#'
#' pSeq %>% tax_select(tax_list = "Bifidobactrium") # default 1 typo allowed
#' one <- pSeq %>% tax_select(tax_list = "Akkarmensia", n_typos = 2)
#' two <- pSeq %>% tax_select(tax_list = "Akkermansia", n_typos = 0)
#' identical(one, two) # TRUE
#'
#' # DESELECTION EXAMPLE # #
#' pSeq %>% tax_select(tax_list = "Bif", strict_matches = FALSE, deselect = TRUE)
#' # Incorrect example
#' # pSeq %>% tax_select(tax_list = "Bif", strict_matches = TRUE) # fails
tax_select <- function(ps,
                       tax_list,
                       ranks_searched = "all",
                       strict_matches = FALSE,
                       n_typos = 1,
                       deselect = FALSE) {
  # Input checks ----
  if (!rlang::is_list(tax_list) && !rlang::is_character(tax_list)) {
    rlang::abort("tax_list must be a character vector or a list of strings")
  }
  if (rlang::is_list(tax_list) && !all(sapply(tax_list, is.character))) {
    rlang::abort("tax_list list must contain only character strings")
  }
  if (!is.logical(strict_matches) || !is.logical(deselect)) {
    rlang::abort("strict_matches and deselect must be logical values")
  }
  if (!rlang::is_scalar_integerish(n_typos) || n_typos < 0) {
    rlang::abort("n_typos must be an integer greater than or equal to 0")
  }
  check_is_phyloseq(ps)

  # End of checks, start of main function body ----
  ps <- ps_get(ps)

  # Collapse tax_list to a string of regex OR patterns
  taxaString <- paste(unlist(tax_list), collapse = "|")

  # Get tax table to search
  Taxa <- phyloseq::tax_table(ps)

  # Check valid taxonomic ranks given for searching
  if (!identical(ranks_searched, "all")) {
    if (any(!ranks_searched %in% phyloseq::rank_names(ps))) {
      rlang::abort(
        "Invalid rank names given: ", paste(ranks_searched, collapse = " "),
        "\n- Should be any/some of: ", paste(phyloseq::rank_names(ps), collapse = "/")
      )
    }
    Taxa <- Taxa[, ranks_searched, drop = FALSE]
  }

  # Calculate selection vector based on strict_matches and n_typos
  if (isTRUE(strict_matches)) {
    taxon_row_matching_fun <- function(r) any(r %in% tax_list)
  } else if (n_typos == 0) {
    taxon_row_matching_fun <- function(r) any(grepl(taxaString, r))
  } else if (n_typos > 0) {
    taxon_row_matching_fun <- function(r) {
      any(sapply(tax_list, FUN = function(x) {
        agrepl(x, r, max.distance = n_typos)
      }))
    }
  }
  selectionVec <- apply(Taxa, MARGIN = 1, FUN = taxon_row_matching_fun)

  # Include exact rownames/taxa names matches
  ROWNAMES <- rownames(Taxa)
  selectionVec <- selectionVec | ROWNAMES %in% tax_list

  # Stop with error if no taxa matched! (but not "in deselect mode")
  if (isFALSE(deselect) && !any(selectionVec)) {
    rlang::abort("No taxa matched.")
  }

  # If taxa DEselection is requested, invert the logical vector, to cause removal of matching taxa
  if (isTRUE(deselect)) selectionVec <- !selectionVec

  ps <- phyloseq::prune_taxa(x = ps, taxa = selectionVec)

  return(ps)
}
