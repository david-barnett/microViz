#' Arbitrary expansion of the "Paired" colorbrewer palette
#'
#' Good for generating palettes for large sets of categories,
#' e.g. in the comp_barplot function
#'
#' @param n number of colours to return
#'
#' @return vector of colours
#' @export
#' @examples
#' pal <- distinct_palette()
#' scales::show_col(c(pal, "lightgrey"))
distinct_palette <- function(n = NA) {
  pal <-
    c(
      # first 12 colours generated with:
      # RColorBrewer::brewer.pal(n = 12, name = "Paired")
      "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
      "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928",
      # vivid interlude
      "#00FF00", # lime
      "#1ff8ff", # a bright blue
      "#FDFF00", # lemon
      "white",
      # next 8 colours generated with:
      # RColorBrewer::brewer.pal(n = 8, "Dark2")
      "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
      "#66A61E", "#E6AB02", "#A6761D", "#666666",
      # list below generated with iwanthue: all colours soft kmeans 20
      # with a couple of arbitrary tweaks by me
      "#4b6a53",
      "#b249d5",
      "#7edc45",
      "#5c47b8",
      "#cfd251",
      "hotpink",
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
      "#594e50",
      "#d3c4a8",
      "#c17e7f"
    )
  if (!identical(NA, n)) pal <- pal[1:n]

  return(pal)
}
