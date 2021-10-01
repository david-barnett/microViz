#' Set unique taxa_names for phyloseq object
#'
#' @description
#' If your current taxa_names aren't what you want (e.g. they are long DNA sequences),
#' this function will help you set sensible unique names.
#'
#' It combines:
#'
#' - a prefix like tax, asv, or otu (pick an appropriate prefix or set your own)
#' - a unique (sequential) number
#' - classification information from a chosen taxonomic rank (optional)
#'
#' @details
#' Don't confuse this with the phyloseq function taxa_names().
#'
#' @param ps phyloseq object
#' @param prefix e.g. 'tax', 'asv', or 'otu' (or set your own)
#' @param rank
#' name of taxonomic rank from which to use classifications in new names
#' @param pad_number should unique numbers have zeros added to the front
#' (e.g. 001, 002) to be made the same number of characters?
#' @param sep
#' character to separate the unique number and any taxonomic classification
#' info (relevant if rank given)
#'
#' @return phyloseq object
#' @export
#'
#' @seealso \code{phyloseq::\link[phyloseq]{taxa_names}} for accessing and manually setting names
#'
#' @examples
#' library(phyloseq)
#' # get example data
#' data("enterotype")
#' ps <- enterotype
#' head(taxa_names(ps)) # these are mostly fine (except the -1), but imagine you wanted new names
#'
#' # consider storing the original names for reference (e.g. if they are DNA sequences)
#' old_taxa_names <- taxa_names(ps)
#'
#' ps <- tax_name(ps)
#' taxa_names(ps) %>% head()
#'
#' # probably better to include the genus info to make these names more informative
#' ps <- tax_name(ps, rank = "Genus")
#' taxa_names(ps) %>% head()
#'
#' # store new names with old names in dataframe for reference
#' names_df <- tibble::tibble(old = old_taxa_names, new = taxa_names(ps))
#'
#' # alternative settings
#' tax_name(ps, pad_number = FALSE) %>%
#'   taxa_names() %>%
#'   head()
#' tax_name(ps, prefix = "whateveryoulike") %>%
#'   taxa_names() %>%
#'   head()
#' tax_name(ps, rank = "Genus", sep = "-") %>%
#'   taxa_names() %>%
#'   head()
tax_name <- function(ps,
                     prefix = c("tax", "asv", "otu")[1],
                     rank = NA,
                     pad_number = TRUE,
                     sep = "_") {
  if (!inherits(prefix, "character")) {
    stop("prefix must be a character string, not: ", class(prefix))
  }
  if (inherits(ps, "ps_extra")) {
    warning(
      "ps is class ps_extra, returning only phyloseq!",
      "\nNaming should be done on phyloseq before starting analyses."
    )
  }
  ps <- ps_get(ps)

  taxtab <- phyloseq::access(ps, "tax_table")

  # create numbers
  ntax <- phyloseq::ntaxa(ps)
  numbers <- seq_len(length.out = ntax)
  if (isTRUE(pad_number)) {
    numbers <-
      formatC(x = numbers, width = nchar(ntax), format = "d", flag = "0")
  }

  # create names (using rank info if requested)
  name_vec <- paste0(prefix, numbers)
  if (!identical(rank, NA)) {
    if (identical(taxtab, NULL)) {
      stop("Your phyloseq has no tax_table, so only rank = NA will work")
    }
    if (!rank %in% phyloseq::rank_names(ps)) {
      stop(
        rank, " is not in rank_names(ps) :\n",
        paste(phyloseq::rank_names(ps), collapse = " / ")
      )
    }
    rank_vec <- taxtab[, rank]
    rank_vec[is.na(rank_vec)] <- "NA"
    name_vec <- paste(name_vec, rank_vec, sep = sep)
  }

  phyloseq::taxa_names(ps) <- name_vec

  return(ps)
}
