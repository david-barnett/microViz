#' Arbitrary expansion of the "Paired" colorbrewer palette
#'
#' Good for generating palettes for large sets of categories, e.g. in the plot_comp_bar function
#'
#' @param n number of colours to return
#'
#' @return vector of colours
#' @export
#'
distinct_palette <- function(n) {
  c(
    RColorBrewer::brewer.pal(n = 12, name = 'Paired'),
    # list below generated with iwanthue all colours soft kmeans 20
    "#4b6a53",
    "#b249d5",
    "#7edc45",
    "#5c47b8",
    "#cfd251",
    "#c84895",
    "#69c86c",
    "#cd3e50",
    "#83d5af",
    "#da6130",
    "#5e79b2",
    "#c29545",
    "#532a5a",
    "#5f7b35",
    "#c497cf",
    "#773a27",
    "#7cb9cb",
    "#383233",
    "#d3c4a8",
    "#c17e7f"
  )[1:n]
}
