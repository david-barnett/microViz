#' Replace unknown, NA, or short tax_table values
#'
#' Identifies phyloseq tax_table values as unknown or uninformative and replaces them with the first informative value from a higher taxonomic rank.
#' - Short values in phyloseq tax_table are typically empty strings or " ", or "g__" etc. so it is helpful to replace them. Set `min_length` = 0 to avoid filtering on length.
#' - Values in `unknowns` are also removed, even if longer than `min_length`. It is up to the user to specify sensible values in `unknowns` if their dataset has other unwanted values.
#' - NA values are also replaced.
#'
#' @param x phyloseq or tax_table (taxonomyTable)
#' @param min_length replace strings shorter than this
#' @param unknowns also replace strings matching any in this vector
#' @param levels names of taxonomic levels to modify, defaults to all
#'
#' @return object same class as x
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' library(phyloseq)
#' library(microbiome)
#'
#' data(dietswap)
#' ps <- dietswap
#'
#' tt <- data.frame(tax_table(ps))
#'
#' # create unknowns to test filling
#' ntax <- ntaxa(ps)
#' set.seed(123)
#' tt[sample(1:ntax, 30), 3] <- "g__"
#' tt[sample(1:ntax, 20), 2] <- "f__"
#' tt[sample(1:ntax, 10), 1] <- "p__"
#' tt[sample(1:ntax, 10), 3] <- "unknown"
#'
#'
#' tt <- tax_table(as.matrix.data.frame(tt))
#' tax_table(ps) <- tt
#'
#' # works on phyloseq objects, and the defaults should solve most problems
#' ps
#' tax_fill_unknowns(ps)
#'
#' tt %>% head(50)
#' # this will replace "unknown"s as well as short values including "g__" and "f__"
#' tax_fill_unknowns(tt) %>% head(50)
#' # this will only replace values in Genus column, and will not replace "unknown"s
#' tax_fill_unknowns(tt, unknowns = NULL, levels = "Genus") %>% head(50)
#'
#' # larger example tax_table shows 1000s rows still fast, from microbiomeutilities package
#' # library(microbiomeutilities)
#' # data("hmp2")
#' # tax_fill_unknowns(hmp2)
#'
tax_fill_unknowns <- function(
                              x,
                              min_length = 4,
                              unknowns = c("unknown", paste0(c("p", "c", "o", "f", "g", "s"), "__")),
                              levels = phyloseq::rank_names(x)) {
  if (inherits(x, "phyloseq")) {
    tt <- unclass(phyloseq::tax_table(x))
  } else if (inherits(x, "taxonomyTable")) {
    tt <- unclass(x)
  } else {
    stop("x must be phyloseq or taxonomyTable class, it is class: ", paste(class(x), collapse = " "))
  }
  # get rownames to ensure order doesn't change
  original_rownames <- rownames(tt)
  ranknames <- colnames(tt)
  tt[is.na(tt)] <- "NA"

  # split the matrix tt (splits as a vector: see https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r/6823557)
  tt_list <- split(tt, row(tt))
  # repair lost names
  names(tt_list) <- original_rownames

  # message(names(tt_list))
  # replace unknowns
  tt_out <- vapply(
    X = tt_list,
    FUN.VALUE = unlist(tt_list[[1]]),
    FUN = function(vec) {
      vec <- unlist(vec)
      is_unknown <- (nchar(vec) < min_length) | (vec %in% c("NA", unknowns))
      # replace unknowns with nearest known if required and possible
      if (any(is_unknown)) {
        if (all(is_unknown)) {
          warning("This row contains no non-unknown values, returning unchanged: ", paste(vec, collapse = "; "))
        } else {
          # edit each unknown value in this row
          vec <- vapply(
            X = seq_along(vec),
            FUN.VALUE = vec[1],
            FUN = function(i) {
              if (is_unknown[i]) {
                known_above <- !is_unknown[1:(i - 1)]
                if (!any(known_above)) {
                  if (ranknames[[i]] %in% levels) {
                    warning("No non-unknown values to the left of 1+ entries in this row, returning those unchanged: ", paste(vec, collapse = "; "))
                  }
                  return(vec[i])
                }
                nearest_known <- max(which(known_above))
                tax <- vec[nearest_known]
                level <- ranknames[[i]]
                tax <- paste(tax, level, sep = ".")
                return(tax)
              } else {
                return(vec[i])
              }
            }
          )
        }
      }
      return(vec)
    }
  )
  # transpose result to match original tt and ensure original row order
  tt_out <- t(tt_out)[original_rownames, ]
  # repair colnames
  colnames(tt_out) <- ranknames

  # preserve any columns not listed in levels
  preserved <- !ranknames %in% levels
  if (any(preserved)) {
    tt_out[, preserved] <- tt[, preserved]
  }

  # return phyloseq or tax table (same as input)
  if (inherits(x, "phyloseq")) {
    phyloseq::tax_table(x) <- tt_out
  } else {
    x <- phyloseq::tax_table(tt_out)
  }
  return(x)
}
