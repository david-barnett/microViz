#' Aggregate taxa and create ps_extra
#'
#' @description
#' `tax_agg` sums the abundances of the phyloseq taxa at the given rank.
#' It records the tax_agg rank argument in the info of the ps_extra object output.
#' This ps_extra object tracks aggregation, and any further transformations and scaling,
#' to help you keep track of what you have done with your phyloseq object and automatically caption ordination plots.
#'
#' Instead of tax_agg, consider using `tax_transform()` with a rank argument instead, to both aggregate and transform the taxa.
#' This is also useful when you want to aggregate but not transform the taxa,
#' and yet still log the "identity" transformation in ps_extra for captioning your ordination plots.
#' e.g. `tax_transform(rank = "Genus", transformation = "identity")`
#'
#' tax_agg allows you to pass NA or "unique" to the rank argument which will NOT aggregate the taxa.
#' If you use rank = "unique" or add_unique = TRUE, it will add a new rank called unique, identical to the taxa_names (after any aggregation)
#'
#' Be aware: you should not use the top_N argument yourself without good reason.
#' top_N provides a feature inspired by the deprecated microbiome function aggregate_top_taxa
#' which is primarily useful for decluttering compositional barplots.
#' microViz comp_barplot (and ord_plot_iris) already run tax_agg with a top_N argument for you, so you should not.
#' The tax_table produced when using top_N is otherwise INVALID FOR MOST OTHER ANALYSES.
#'
#' @details
#' This function is inspired by `microbiome::aggregate_taxa`.
#' However if `microbiome::aggregate_taxa` is used, microViz cannot track this aggregation.
#'
#' Comparing aggregate_taxa and tax_agg:
#'
#' Except for the ordering of taxa, and the addition of a "unique" rank being optional,
#' the resulting phyloseq objects are identical for aggregating a phyloseq with no ambiguous taxa.
#' Taxa are ambiguous when the tax_table converges at a lower rank after branching,
#' such as if two different genera share the same species (e.g. "s__").
#' `microbiome::aggregate_taxa` handles ambiguous taxa by creating a "unique" rank with all
#'  of the taxonomic rank info pasted together into one, often very long, name.
#' `tax_agg` throws an error, and directs the user to `tax_fix()` to fix the ambiguous taxa before aggregation,
#' which should then result in (much) shorter unique names at the aggregation rank.
#'
#' @param ps phyloseq object
#' @param rank
#' NA (for tax_names level) or name of valid taxonomic rank (try phyloseq::rank_names(ps)) or "unique"
#' @param sort_by if not NA, how should the taxa be sorted, uses tax_sort(), takes same options as `by` arg
#' @param top_N
#' NA does nothing, but if top_N is a number, it creates an extra tax_table column called top,
#' which is the same as the unique column for the first top_N number of taxa, and "other" otherwise.
#' @param force
#' If TRUE, this forces aggregation at chosen rank to occur regardless of if the output will be sensible!
#' This avoids the "Taxa not unique at rank: ..." error, but may allow very inappropriate aggregation to occur.
#' Do not use force = TRUE unless you know why you are doing this, and what the result will be.
#' If you are getting an error with force = FALSE, it is almost certainly better to examine the tax_table and fix the problem.
#' force = TRUE is similar to microbiome::aggregate_taxa,
#' which also does not check that the taxa are uniquely defined by only the aggregation level.
#' @param add_unique if TRUE, adds a rank named unique, identical to the rownames after aggregation
#'
#' @return ps_extra list object including phyloseq and tax_agg rank info
#' @export
#'
#' @seealso \code{\link{tax_fix}}
#' @seealso \code{\link{tax_transform}}
#'
#' @rdname tax_agg
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
#' # this won't aggregate taxa, but just adds a new rank called unique, equal to taxa_names
#' tax_agg(ps = dietswap, rank = NA, add_unique = TRUE)
#' identical(tax_agg(dietswap, NA, add_unique = TRUE), tax_agg(dietswap, "unique")) # TRUE
#'
#' # create some missing values
#' tax_table(dietswap)[3:7, "Genus"] <- "g__"
#' # this will produce an error, instructing the user to use fill_unknowns
#' # tax_agg(ps = dietswap, "Genus")
#' # this will then work:
#' dietswap %>%
#'   tax_fix() %>%
#'   tax_agg("Genus")
#'
#' # you can replace unknown values with `tax_fix()`
#' # which will fix most problems, like the common "g__" and "s__"
#' # but default tax_fix settings won't catch this long unknown
#' tax_table(dietswap)[13:17, "Family"] <- "some_unknown_family"
#' dietswap %>%
#'   tax_fix(unknowns = "some_unknown_family") %>%
#'   tax_agg("Family")
#'
#' # try tax_fix_interactive() for more help
tax_agg <- function(ps,
                    rank = NA,
                    sort_by = NA,
                    top_N = NA,
                    force = FALSE,
                    add_unique = FALSE) {
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
    "\nTo fix, try tax_fix() or tax_fix_interactive() before this function."

  ranks <- phyloseq::rank_names(ps)
  bad_rank_message <-
    paste0(
      "\nrank should be NA, 'unique' or one of:\n",
      paste(ranks, collapse = " / ")
    )

  # only do most things if rank is not NA/unique
  if (identical(rank, NA)) rank <- "unique"
  if (identical(rank, "unique")) {
    # any sorting and/or creation of "top" rank is done
    ps_agg <- ps
  } else {

    # these elements don't make sense any more with aggregated taxa (ever?)
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
      stop("NAs in tax_table at rank: ", rank, unknowns_message)
    }

    tt_distinct <- dplyr::distinct(tt_df)
    # unique names needed as factor levels for .taxID. column
    # otherwise grouped summarise later reorders otu table to alphabetical...
    new_tax_names <- base::unique(tt_df[, rank_index])

    # tt must be unique at given rank by this point
    if (!isTRUE(force)) {
      uniq_at_rank <- identical(length(new_tax_names), y = nrow(tt_distinct))
      if (!uniq_at_rank) {
        dupe_rows <- which(
          duplicated(tt_distinct[[rank]]) |
            duplicated(tt_distinct[[rank]], fromLast = TRUE)
        )
        message(
          "Problematic values detected in tax_table:\n",
          paste(unique(tt_distinct[[rank]][dupe_rows]), collapse = " ")
        )
        message("-")
        tax_names <- rownames(tt_distinct)
        message(paste(c("taxa_name", ranks), collapse = " / "))
        for (r in dupe_rows) {
          message(paste(
            c(tax_names[r], tt_distinct[r, -(rank_index + 1)]),
            collapse = " / "
          ))
        }
        message("-")
        stop(
          "Taxa not unique at rank: ", rank,
          "\nSee last messages for convergent taxa rows.", unknowns_message
        )
      }
    } else {
      # FORCED aggregation
      tt_distinct <- dplyr::distinct(
        # keeps first rows encountered
        .data = tt_distinct, .keep_all = TRUE, # keeps all ranks
        dplyr::across(dplyr::all_of(rank)) # only uses rank to make distinct
      )
    }

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

    # return the otu_table as it was originally
    if (!taxa_were_rows) otu_new <- phyloseq::t(otu_new)

    # phyloseq
    ps_agg <- phyloseq::phyloseq(phyloseq::sample_data(ps), tt_new, otu_new)
  }
  if (isTRUE(add_unique) || identical(rank, "unique")) {
    # add unique rank that matches taxa/rownames
    # (like how microbiome aggregate_taxa works, in case any fun uses that col)
    ps_agg <- tax_names2tt(ps_agg, colname = "unique")
  }

  # if top_N set, set a default sort_by of sum, if necessary
  if (!identical(top_N, NA) && identical(sort_by, NA)) {
    stopifnot(length(top_N) == 1 && is.numeric(top_N))
    sort_by <- function(x) sum(x, na.rm = TRUE)
  }

  # sort phyloseq taxa
  if (!identical(sort_by, NA)) {
    ps_agg <- tax_sort(ps_agg, by = sort_by, at = "names")
  }
  # create "top" rank column if requested by top_N
  if (!identical(top_N, NA)) {
    phyloseq::tax_table(ps_agg) <-
      tt_add_topN_var(phyloseq::tax_table(ps_agg), N = top_N, other = "other")
  }

  # ps_extra
  ps_extra <- new_ps_extra(
    ps = ps_agg, info = new_ps_extra_info(tax_agg = rank)
  )

  return(ps_extra)
}

# https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables("where")

#' Add taxa_names as column in phyloseq tax_table
#'
#' The taxa names in your phyloseq may specify a further unique classification
#' of your taxa, e.g. ASVs, that is not otherwise represented in the tax_table itself.
#' This function fixes that, and allows you to include this level in taxatree_plots for example.
#'
#' @param data phyloseq object, or ps_extra or tax_table (taxonomyTable)
#' @param colname name of new rank to add at right side of tax_table
#'
#' @return same class object as passed in to data
#' @export
#' @rdname tax_nammes2tt
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
