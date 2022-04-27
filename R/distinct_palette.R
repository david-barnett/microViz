# Ref https://eleanormaclure.files.wordpress.com/2011/03/colour-coding.pdf
# Ref http://www.iscc-archive.org/pdf/PC54_1724_001.pdf
# Ref https://github.com/EmilHvitfeldt/r-color-palettes
# Ref https://graphicdesign.stackexchange.com/q/3682

#' @title Colour palettes suitable for 20+ categories
#'
#' @description
#' Available palettes (max colors) are "brewerPlus" (41), "kelly" (20) and "greenArmytage" (25).
#'
#' - "brewerPlus" is an arbitrary expansion of the "Paired" and "Dark2"
#' colorbrewer palettes. The philosophy behind this expansion was to ensure
#' that similar colours are far apart, and the earlier colours are attractive.
#' - "kelly" is based on the 22-colour palette developed by Kenneth Kelly but
#'  with white and black starting colours removed. This palette is ordered
#'  such that the first colours are most distinct.
#' - "greenArmytage" is based on a 26-colour palette proposed by Paul
#' Green-Armytage, with black removed. This palette is not ordered by maximum
#' contrast.
#'
#' @details
#' Hex color codes for 'kelly' and 'greenArmytage' palettes are copied and
#' slightly modified from the Polychrome R package:
#' i.e. Polychrome::kelly.colors() and Polychrome::green.armytage.colors()
#'
#' Please consider also citing Coombes 2019
#' \doi{10.18637/jss.v090.c01}
#' if you use either of these palettes.
#'
#' See the Polychrome reference manual for more information:
#' \url{https://CRAN.R-project.org/package=Polychrome}
#'
#' @param n number of colours to return
#' @param pal palette name, one of "brewerPlus", "kelly", "greenArmytage"
#' @param add
#' colour to append to end of palette, as colour n+1,
#' lightgrey by default for the use as "other" taxa in comp_barplot,
#' or NA for no additional colour.
#'
#' @return vector of colours
#' @export
#' @examples
#' brewerPlus <- distinct_palette()
#' scales::show_col(brewerPlus)
#'
#' kelly <- distinct_palette(pal = "kelly")
#' scales::show_col(kelly)
#'
#' greenArmytage <- distinct_palette(pal = "greenArmytage")
#' scales::show_col(greenArmytage)
distinct_palette <- function(n = NA, pal = "brewerPlus", add = "lightgrey") {
  stopifnot(rlang::is_string(pal))
  stopifnot(rlang::is_scalar_integerish(n) || identical(n, NA))
  stopifnot(rlang::is_na(add) || is.character(add) || is.numeric(add))

  # define valid palettes matched to retrieval functions
  palList <- list(
    brewerPlus = palBrewerPlus,
    kelly = palKelly,
    greenArmytage = palGreenArmytage
  )

  # match palette request
  pal <- rlang::arg_match0(arg = pal, values = names(palList))

  # get full palette
  palFun <- palList[[pal]]
  palCols <- palFun()

  # get n colors
  if (!rlang::is_na(n)) {
    if (n > length(palCols)) {
      stop("Palette '", pal, "' has ", length(palCols), " colors, not ", n)
    }
    palCols <- palCols[seq_len(n)]
  }

  # add last colour e.g. lightgrey default if requested
  if (!identical(add, NA)) {
    col2rgb(add)
    palCols <- c(palCols, add)
  }
  return(palCols)
}

palBrewerPlus <- function() {
  c(
    # first 12 colours generated with:
    # RColorBrewer::brewer.pal(n = 12, name = "Paired")
    "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
    "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928",
    # vivid interlude
    "#1ff8ff", # a bright blue
    # "#FDFF00", # lemon (clashes with #FFFF99 on some screens)
    # "#00FF00", # lime (indistinguishable from bright blue on some screens)
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
    "#ff69b4", # hotpink
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
}

palKelly <- function() {
  c(
    # "#f2f3f4", "#222222", # white and black removed
    "#f3c300", "#875692", "#f38400", "#a1caf1", "#be0032", "#c2b280",
    "#848482", "#008856", "#e68fac", "#0067a5", "#f99379", "#604e97",
    "#f6a600", "#b3446c", "#dcd300", "#882d17", "#8db600", "#654522",
    "#e25822", "#2b3d26"
  )
}

palGreenArmytage <- function() {
  c(
    "#F0A3FF", "#0075DC", "#993F00", "#4C005C", # "#191919", # black removed
    "#005C31", "#2BCE48", "#FFCC99", "#808080", "#94FFB5", "#8F7C00",
    "#9DCC00", "#C20088", "#003380", "#19A405", "#FFA8BB", "#426600",
    "#FF0010", "#5EF1F2", "#00998F", "#E0FF66", "#100AFF", "#990000",
    "#FFFF80", "#FFE100", "#FF5000"
  )
}
