#
#' Make a fixed taxa colour palette e.g. for comp_barplot
#'
#' Makes a named palette vector from your phyloseq dataset
#' considering overall abundance to assign colours (or some other sorting)
#'
#' @param data phyloseq or ps_extra
#' @param rank taxonomic rank name or "unique"
#' @param n number of colours / taxa (not including "other")
#' @param by tax sorting method for tax_sort e.g. sum
#' @param pal palette name from distinct_palette function
#' @param add name = value pairs appended to end of output
#' @param ... other args are passed to tax_sort
#'
#' @return named character vector
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(dietswap, package = "microbiome")
#'
#' myPal <- tax_palette(dietswap, rank = "Genus", pal = "brewerPlus", n = 40)
#' myPal %>% tax_palette_plot() # just to check the palette
#'
#' # plot one subset of data
#' dietswap %>%
#' ps_filter(nationality == "AFR", timepoint == 1, sex == "male") %>%
#'   comp_barplot(
#'     tax_level = "Genus", n_taxa = 15,
#'     bar_outline_colour = NA, bar_width = 0.7,
#'     palette = myPal, label = NULL
#'   )
#'
#' # plot a different subset of data (top taxa differ but colours are the same)
#' dietswap %>%
#' ps_filter(nationality != "AFR", timepoint == 1, sex == "male") %>%
#'   comp_barplot(
#'     tax_level = "Genus", n_taxa = 15,
#'     bar_outline_colour = NA, bar_width = 0.7,
#'     palette = myPal, label = NULL
#'   )
#'
tax_palette <- function(data, # phyloseq or ps_extra
                        rank, # e.g. "Genus"
                        n, # n colours / taxa not including other
                        by = sum, # method for tax_sort
                        pal = "brewerPlus", # palette name from distinct_palette
                        add = c(other = "lightgrey"), # name = value pairs appended to end of output
                        ... # other args passed to tax_sort
) {
  # input checks
  if (!is.null(add) && !is.character(add) || !rlang::is_named2(add)) {
    stop("add must be null or named vector of colours")
  }

  taxa <- tax_top(data = data, rank = rank, n = n, by = by, ...)
  taxColours <- distinct_palette(n = n, pal = pal, add = NA)

  names(taxColours) <- taxa
  taxColours <- c(taxColours, add)
  return(taxColours)
}


#' tax_palette plotting helper function
#'
#' Check the named palette colour vector you created with tax_palette()
#'
#' @param named_pal_vec
#' vector of colours named by taxa (e.g. tax_palette output)
#' @param max_n NA to display all colours, or limit this
#'
#' @return ggplot
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(dietswap, package = "microbiome")
#'
#' myPal <- tax_palette(dietswap, rank = "Genus", pal = "brewerPlus", n = 40)
#' myPal %>% tax_palette_plot() # just to check the palette
#'
tax_palette_plot <- function(named_pal_vec, max_n = NA) {

  stopifnot(rlang::is_named(named_pal_vec)) # all colours need names
  stopifnot(identical(max_n, NA) || rlang::is_scalar_integerish(max_n))
  if (!is.na(max_n)) named_pal_vec <- utils::head(named_pal_vec, max_n)

  df <- data.frame(taxon = names(named_pal_vec), hex = unname(named_pal_vec))
  df[["taxon"]] <- factor(df[["taxon"]], levels = rev(df[["taxon"]]))

  p <- ggplot2::ggplot(
    data = df, mapping = ggplot2::aes_string(y = "taxon", fill = "hex")
  ) +
    ggplot2::geom_raster(mapping = ggplot2::aes(x = "")) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal()
  return(p)
}
