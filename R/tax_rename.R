#' Make new phyloseq taxa names from classification and taxon abundance info
#'
#' Pairs classification at the given rank with a
#' numeric ranking suffix (based on abundance or prevalence data)
#' to automatically create informative taxa names.
#'
#' e.g. "Bacteroides 003" for the third most abundant Bacteroides OTU or ASV.
#'
#' Taxa are returned in original order, and otu_table is returned un-transformed.
#'
#' @param ps phyloseq object
#' @param rank name of rank to use in new taxa names
#' @param sort_by
#' how to sort taxa for numbering within rank-based groups (a tax_sort option)
#' @param pad_digits how long should the numeric suffixes be? see details
#' @param sep character to separate the rank prefixes from numeric suffixes
#' @param transform_for_sort
#' named of transformation to apply to taxa before sorting
#' @param ... additional arguments passed to tax_sort
#'
#' @details
#' pad_digits options:
#'  - "auto" --> minimum digits to have equal length numbers within groups
#'  - "max" --> minimum digits to have equal length numbers across all groups
#'  - A number: e.g.
#'      - 3 --> 001, 002, ..., 042, ..., 180, ...
#'      - 1 --> 1, 2, ..., 42, ..., 180, ...
#'
#' @return phyloseq object
#' @export
#'
#' @seealso \code{phyloseq::\link[phyloseq]{taxa_names}} for accessing and manually setting names
#'
#' @examples
#' library(phyloseq)
#' data("ibd_phylo", package = "corncob")
#'
#' ps <- ibd_phylo %>%
#'   tax_filter(min_prevalence = 3) %>%
#'   tax_fix()
#'
#' # show a few of the current, uninformative names
#' taxa_names(ps) %>% head(15)
#' taxa_names(ps) %>% tail(15)
#'
#' # change names to genus classification plus number
#' psNewNames <- ps %>% tax_rename(rank = "Genus")
#'
#' taxa_names(psNewNames) %>% head(15)
#' taxa_names(psNewNames) %>% tail(15)
#'
#' # demonstrate some alternative argument settings
#' psNewNames2 <- ps %>% tax_rename(
#'   rank = "Family", sort_by = prev, pad_digits = "max", sep = "-"
#' )
#'
#' taxa_names(psNewNames2) %>% head(15)
#' taxa_names(psNewNames2) %>% tail(15)
#'
#' ps %>%
#'   tax_rename(rank = "Genus", pad_digits = 2) %>%
#'   taxa_names() %>%
#'   head(15)
#'
#' # naming improvement on plots example
#' library(ggplot2)
#' library(patchwork)
#'
#' # Overly aggressive OTU filtering to simplify and speed up example
#' psExample <- ps %>% tax_filter(min_prevalence = 0.4)
#'
#' # before OTU renaming
#' before <- psExample %>%
#'   ps_filter(activity == "inactive") %>%
#'   tax_names2rank("Taxon") %>%
#'   comp_barplot(
#'     tax_level = "Taxon", n_taxa = 12, other_name = "Other",
#'     merge_other = FALSE, bar_outline_colour = "grey60"
#'   ) +
#'   coord_flip() +
#'   ggtitle("Original taxon names :(")
#'
#' # after OTU renaming
#' after <- psExample %>%
#'   ps_filter(activity == "inactive") %>%
#'   tax_rename(rank = "Genus", pad_digits = "max") %>%
#'   tax_names2rank("Taxon") %>%
#'   comp_barplot(
#'     tax_level = "Taxon", n_taxa = 12, other_name = "Other",
#'     merge_other = FALSE, bar_outline_colour = "grey60"
#'   ) +
#'   coord_flip() +
#'   ggtitle("New taxon names :)", "tax_rename(rank = 'Genus', sort_by = sum)")
#'
#' before + after & theme(legend.text = element_text(size = 8))
#'
#' # ordination example
#' psExample %>%
#'   tax_rename(rank = "Genus", sort_by = sum) %>%
#'   tax_names2rank("otu") %>%
#'   tax_transform("clr", rank = "otu") %>%
#'   ord_calc() %>%
#'   ord_plot(
#'     size = 2, colour = "ibd", shape = "circle", alpha = 0.5,
#'     plot_taxa = 1:10,
#'     tax_vec_length = 0.5,
#'     tax_lab_style = tax_lab_style(
#'       type = "text", max_angle = 90, check_overlap = TRUE,
#'       size = 2.5, fontface = "bold"
#'     ),
#'     tax_vec_style_all = vec_tax_all(alpha = 0.1)
#'   ) +
#'   coord_fixed(clip = "off") +
#'   stat_chull(aes(colour = ibd)) +
#'   scale_colour_brewer(palette = "Dark2") +
#'   theme(panel.grid = element_line(size = 0.1))
tax_rename <- function(ps,
                       rank,
                       sort_by = sum,
                       transform_for_sort = "identity",
                       pad_digits = "auto",
                       sep = " ",
                       ... # for tax_sort
) {
  # check inputs #
  if (!inherits(ps, "phyloseq")) stop("ps must be a phyloseq object")
  psCheckRanks(ps = ps, rank = rank, varname = "rank")
  if (!rlang::is_scalar_integerish(pad_digits) || pad_digits < 0L) {
    if (!rlang::is_string(x = pad_digits, string = c("auto", "max"))) {
      stop("pad_digits must be 'auto', 'max', or a positive integer")
    }
  }
  stopifnot(rlang::is_string(sep))
  stopifnot(rlang::is_string(transform_for_sort))
  # end of input checks #

  psSorted <- tax_sort(
    data = ps, by = sort_by, at = "names", trans = transform_for_sort,
    tree_warn = FALSE, ...
  )
  ttSorted <- as.data.frame.matrix(tt_get(psSorted))
  ttList <- split.data.frame(x = ttSorted, f = ttSorted[, rank, drop = TRUE])

  # calculate consistent padding across groups if requested
  if (pad_digits == "max") pad_digits <- nchar(max(sapply(ttList, nrow)))

  # create new names per classification group
  newNames <- lapply(ttList, function(df) {
    numbers <- as.character(seq_len(nrow(df)))
    if (identical(pad_digits, "auto")) pad_digits <- nchar(nrow(df))
    numbers <- stringr::str_pad(
      string = numbers, pad = "0", side = "left", width = pad_digits
    )
    newNames <- paste(df[[rank]], numbers, sep = sep)
    names(newNames) <- rownames(df)
    return(newNames)
  })

  # make one vector of new names, named by old names
  newNames <- Reduce(f = c, x = newNames)
  oldNames <- phyloseq::taxa_names(ps)
  # change the order of the new names to match old names, and assign them to ps
  phyloseq::taxa_names(ps) <- as.character(newNames[oldNames])

  return(ps)
}
