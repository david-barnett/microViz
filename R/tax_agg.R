#' Aggregate taxa and create ps_extra
#'
#' `tax_agg` sums the abundances of the phyloseq taxa at the given rank.
#' It records the tax_agg rank argument in the info of the ps_extra object output.
#' This ps_extra object tracks aggregation, and any further transformations and scaling,
#' to help you keep track of what you have done with your phyloseq object.
#' tax_agg allows you to pass NA or "unique" to the rank argument which will NOT aggregate the taxa.
#' It always adds a "unique" rank, which matches the tax_table row names (after any aggregation performed).
#'
#' This function is inspired by microbiome::aggregate_taxa.
#' However if microbiome::aggregate_taxa is used, microViz cannot track this aggregation.
#' Except for the ordering of taxa, the resulting phyloseq objects are identical for aggregating a phyloseq with n ranks up to the n-1 rank.
#' Aggregating at rank n with tax_agg still generates a "unique" column, but aggregate_taxa does not do this.
#'
#' @param ps phyloseq object
#' @param rank NA or "unique" or name of valid taxonomic rank (try phyloseq::rank_names(ps))
#' @param sort_by if not NA, how should the taxa be sorted, uses tax_sort(), takes same options as `by` arg
#'
#' @return ps_extra list object including phyloseq and tax_agg rank info
#' @export
#'
#' @examples
#' library(microbiome)
#' data("dietswap", package = "microbiome")
#'
#' tax_agg(ps = dietswap, "Phylum") %>%
#'   ps_get() %>%
#'   tax_table()
#' tax_agg(ps = dietswap, "Family") %>%
#'   ps_get() %>%
#'   tax_table()
#'
#' # this won't aggregate taxa,
#' # but just add a new rank called unique, equal to taxa_names
#' tax_agg(ps = dietswap, NA)
#' identical(tax_agg(dietswap, NA), tax_agg(dietswap, "unique")) # TRUE
#'
#' # create some missing values
#' tax_table(dietswap)[3:7, "Genus"] <- "g__"
#' # this will produce an error, instructing the user to use fill_unknowns
#' # tax_agg(ps = dietswap, "Genus")
#' # this will then work:
#' dietswap %>%
#'   tax_fill_unknowns() %>%
#'   tax_agg("Genus")
#'
#' # you can replace long unknown values
#' tax_table(dietswap)[13:17, "Family"] <- "some_unknown_family"
#' # default tax_fill_unknowns settings won't catch it (too long)
#' dietswap %>%
#'   tax_fill_unknowns(unknowns = "some_unknown_family") %>%
#'   tax_agg("Family")
tax_agg <- function(ps,
                    rank = "unique",
                    sort_by = NA) {
  if (inherits(ps, "ps_extra")) {
    # currently just reset info
    warning(
      "class of ps is ps_extra: any extra info is lost (transform, dist, etc.)"
    )
    ps <- ps_get(ps)
  }
  # store taxa orientation info for restoration to original setup before return
  taxa_were_rows <- phyloseq::taxa_are_rows(ps)

  # set up messages sent by multiple stop calls
  unknowns_message <-
    "\nTo fix, try fill_unknowns = TRUE or `tax_fill_unknowns(ps)`"

  ranks <- phyloseq::rank_names(ps)
  bad_rank_message <-
    paste0(
      "\nrank should be NA, 'unique' or one of:\n",
      paste(ranks, collapse = " / ")
    )

  # only do most things if rank is not NA/unique
  if (identical(rank, NA)) rank <- "unique"
  if (identical(rank, "unique")) {
    ps <- tax_names2tt(ps, colname = rank)
    return(
      new_ps_extra(ps = ps, info = new_ps_extra_info(tax_agg = rank))
    )
  }

  # these elements don't make sense any more, with aggregated taxa (ever?)
  ps@phy_tree <- NULL
  ps@refseq <- NULL

  # check valid rank and get rank index
  if (!identical(length(rank), 1L) && !inherits(rank, "character")) {
    stop(bad_rank_message)
  }
  rank_index <- base::match(x = rank, table = ranks, nomatch = 0L)
  if (identical(rank_index, 0L)) {
    stop("rank is: ", rank, bad_rank_message)
  }

  # remove lower tax_table ranks
  tt <- unclass(phyloseq::tax_table(ps))
  tt <- tt[, seq_len(rank_index), drop = FALSE]
  tt_df <- as.data.frame.matrix(tt, optional = TRUE, make.names = FALSE)
  tt_df[[".taxID."]] <- tt_df[, rank_index]

  # check NAs in chosen rank
  if (anyNA(tt_df[, rank_index])) {
    stop("NAs in `tax_table(ps)[, rank]`", unknowns_message)
  }

  tt_distinct <- dplyr::distinct(tt_df)
  # unique names needed as factor levels for .taxID. column
  # otherwise grouped summarise later reorders otu table to alphabetical...
  new_tax_names <- base::unique(tt_df[, rank_index])

  # tt must be unique at given rank by this point
  unique_at_rank <- identical(length(new_tax_names), y = nrow(tt_distinct))
  if (!unique_at_rank) stop("Taxa not unique at given rank.", unknowns_message)

  # get otu table with taxa as rows (like tt)
  otu <- t(unclass(otu_get(ps)))
  otu_df <- as.data.frame.matrix(
    x = otu, optional = TRUE, make.names = FALSE, stringsAsFactors = FALSE
  )
  otu_df[[".taxID."]] <- factor(
    x = tt_df[, rank_index],
    levels = new_tax_names,
    ordered = TRUE
  )

  # aggregate tax abundance values in samples by summing within taxID groups
  otu_grouped <- otu_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(".taxID.")))
  otu_agg <- otu_grouped %>%
    dplyr::summarise(
      dplyr::across(
        where(is.numeric),
        .fns = sum, na.rm = TRUE,
        .names = "{.col}"
      ),
      .groups = "drop"
    )

  # build new phyloseq
  otu_new <- otu_agg %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames(var = ".taxID.") %>%
    phyloseq::otu_table(taxa_are_rows = TRUE)


  tt_new <- tt_distinct %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames(var = ".taxID.") %>%
    as.matrix.data.frame() %>%
    phyloseq::tax_table()

  # add unique rank that matches taxa/rownames
  # (like how microbiome aggregate_taxa works, in case any fun uses that col)
  tt_new <- tax_names2tt(tt_new, colname = "unique")

  # return the otu_table as it was originally
  if (!taxa_were_rows) otu_new <- phyloseq::t(otu_new)

  # phyloseq
  ps_agg <- phyloseq::phyloseq(phyloseq::sample_data(ps), tt_new, otu_new)

  # sort phyloseq taxa
  if (!identical(sort_by, NA)) ps <- tax_sort(ps, by = sort_by)

  # ps_extra
  ps_extra <- new_ps_extra(
    ps = ps_agg, info = new_ps_extra_info(tax_agg = rank)
  )

  return(ps_extra)
}

# https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables("where")

# helper function adds unique names column to end of tax_table
tax_names2tt <- function(data, colname = "unique") {
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
  if (methods::is(data, "taxonomyTable")) {
    return(tt)
  }
  if (methods::is(data, "phyloseq")) {
    phyloseq::tax_table(data) <- tt
    return(data)
  }
  if (inherits(data, "ps_extra")) {
    phyloseq::tax_table(data$ps) <- tt
    return(data)
  }
}

