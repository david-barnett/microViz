#' Replace unknown, NA, or short tax_table values
#'
#' @description
#' Identifies phyloseq tax_table values as unknown or uninformative and
#' replaces them with the first informative value from a higher taxonomic rank.
#' - Short values in phyloseq tax_table are typically empty strings or " ", or "g__" etc.
#' so it is helpful to replace them. (If this is unwanted: set `min_length` = 0 to avoid filtering on length.)
#' - Values in `unknowns` are also removed, even if longer than `min_length`.
#' It is up to the user to specify sensible values in `unknowns` if their dataset has other unwanted values.
#' - NA values are also replaced.
#'
#' See this article for an extended discussion of tax_table fixing.
#' \url{https://david-barnett.github.io/microViz/articles/web-only/tax-fixing.html}
#'
#' @details
#' By default (unknowns = NA), unknowns is set to a vector containing:
#'
#' 's__' 'g__' 'f__' 'o__' 'c__' 'p__' 'k__' 'S__' 'G__' 'F__' 'O__' 'C__' 'P__' 'K__' 'NA' 'NaN' ' ' ''
#' 'unknown' 'Unknown' 's__unknown' 's__Unknown' 's__NA' 'g__unknown' 'g__Unknown' 'g__NA'
#' 'f__unknown' 'f__Unknown' 'f__NA' 'o__unknown' 'o__Unknown' 'o__NA' 'c__unknown' 'c__Unknown' 'c__NA'
#' 'p__unknown' 'p__Unknown' 'p__NA' 'k__unknown' 'k__Unknown' 'k__NA' 'S__unknown' 'S__Unknown' 'S__NA'
#' 'G__unknown' 'G__Unknown' 'G__NA' 'F__unknown' 'F__Unknown' 'F__NA' 'O__unknown' 'O__Unknown' 'O__NA'
#' 'C__unknown' 'C__Unknown' 'C__NA' 'P__unknown' 'P__Unknown' 'P__NA' 'K__unknown' 'K__Unknown' 'K__NA'
#'
#'
#' @param ps
#' phyloseq or tax_table (taxonomyTable)
#' @param min_length
#' replace strings shorter than this, must be integer > 0
#' @param unknowns
#' also replace strings matching any in this vector,
#' NA default vector shown in details!
#' @param suffix_rank
#' "classified" (default) or "current", when replacing an entry,
#' should the suffix be taken from the lowest classified rank
#' for that taxon, "classified", or the "current" unclassified rank?
#' @param sep
#' character(s) separating new name and taxonomic rank level suffix (see suffix_rank)
#' @param anon_unique
#' make anonymous taxa unique by replacing unknowns with taxa_name?
#' otherwise they are replaced with paste("unknown", first_rank_name),
#' which is therefore the same for every anonymous taxon, meaning they will be merged if tax_agg is used.
#' (anonymous taxa are taxa with all unknown values in their tax_table row,
#' i.e. cannot be classified even at highest rank available)
#' @param verbose
#' emit warnings when cannot replace with informative name?
#'
#' @return object same class as ps
#' @export
#'
#' @seealso \code{\link{tax_fix_interactive}} for interactive tax_fix help
#'
#' @examples
#' library(dplyr)
#' library(phyloseq)
#'
#' data(dietswap, package = "microbiome")
#' ps <- dietswap
#'
#' # create unknowns to test filling
#' tt <- tax_table(ps)
#' ntax <- ntaxa(ps)
#' set.seed(123)
#' g <- sample(1:ntax, 30)
#' f <- sample(g, 10)
#' p <- sample(f, 3)
#' tt[g, 3] <- "g__"
#' tt[f, 2] <- "f__"
#' tt[p, 1] <- "p__"
#' tt[sample(1:ntax, 10), 3] <- "unknown"
#' # create a row with only NAs
#' tt[1, ] <- NA
#'
#' tax_table(ps) <- tax_table(tt)
#'
#' ps
#' # tax_fix with defaults should solve most problems
#' tax_table(ps) %>% head(50)
#'
#' # this will replace "unknown"s as well as short values including "g__" and "f__"
#' tax_fix(ps) %>%
#'   tax_table() %>%
#'   head(50)
#'
#' # This will only replace short entries, and so won't replace literal "unknown" values
#' ps %>%
#'   tax_fix(unknowns = NULL) %>%
#'   tax_table() %>%
#'   head(50)
#'
#' # Change rank suffix and separator settings
#' tax_fix(ps, suffix_rank = "current", sep = " - ") %>%
#'   tax_table() %>%
#'   head(50)
#'
#' # by default, completely unclassified (anonymous) taxa are named by their
#' # taxa_names / rownames at all ranks.
#' # This makes anonymous taxa distinct from each other,
#' # and so they won't be merged on aggregation with tax_agg.
#' # If you think your anonymous taxa should merge on tax_agg,
#' # or you just want them to be named the all same for another reason,
#' # set anon_unique = FALSE (compare the warning messages)
#' tax_fix(ps, anon_unique = FALSE)
#' tax_fix(ps, anon_unique = TRUE)
#'
#' # here's a larger example tax_table shows its still fast with 1000s rows,
#' # from microbiomeutilities package
#' # library(microbiomeutilities)
#' # data("hmp2")
#' # system.time(tax_fix(hmp2, min_length = 1))
tax_fix <- function(ps,
                    min_length = 4,
                    unknowns = NA,
                    suffix_rank = "classified", # or current
                    sep = " ",
                    anon_unique = TRUE,
                    verbose = TRUE) {
  if (!rlang::is_scalar_integerish(min_length) || min_length < 1L) {
    stop("min_length must be an integer greater than 0")
  }
  if (is(ps, "psExtra")) stop("ps is a psExtra, run tax_fix BEFORE tax_agg etc")
  if (methods::is(ps, "phyloseq")) {
    tt <- unclass(phyloseq::tax_table(ps))
  } else if (inherits(ps, "taxonomyTable")) {
    tt <- unclass(ps)
  } else {
    stop("ps must be phyloseq or taxonomyTable class, it is class: ", paste(class(ps), collapse = " "))
  }

  if (identical(unknowns, NA)) {
    unknowns <- tax_common_unknowns(min_length = min_length)
  }

  # get rownames to ensure order doesn't change
  original_rownames <- rownames(tt)
  ranknames <- colnames(tt)
  levels <- ranknames # TODO either fix levels functionality or remove completely
  tt[is.na(tt) | nchar(tt) < min_length | tt %in% unknowns] <- ""
  rowLengthOut <- ncol(tt) # save number of cols before adding .rownames.
  tt_extra <- cbind(tt, .rownames. = original_rownames)

  # split the matrix tt (splits as a vector: see https://stackoverflow.com/questions/6819804/how-to-convert-a-matrix-to-a-list-of-column-vectors-in-r/6823557)
  tt_list <- split(tt_extra, row(tt_extra))
  # repair lost names
  names(tt_list) <- original_rownames

  # replace tax_table unknowns
  tt_out <- vapply(
    X = tt_list,
    FUN.VALUE = character(length = rowLengthOut),
    FUN = function(vec) {
      vec <- unlist(vec)
      is_unknown <- vec == "" # all unknowns now replaced first!
      # replace unknowns with nearest known if required and possible
      if (any(is_unknown[1:rowLengthOut])) {
        if (all(is_unknown[1:rowLengthOut])) {
          if (isTRUE(anon_unique)) {
            out <- vec[rowLengthOut + 1]
          } else {
            out <- paste("unclassified", ranknames[[1]])
          }
          if (isTRUE(verbose)) {
            message(
              "Row named: ", vec[[rowLengthOut + 1]],
              "\ncontains no non-unknown values, returning:\n'",
              out, "' for all replaced levels.\n",
              "Consider editing this tax_table entry manually."
            )
          }
          vec <- rep(out, times = rowLengthOut)
        } else {
          # edit each unknown value in this row
          vec <- tax_fix_value(
            vec = vec, is_unknown = is_unknown, ranknames = ranknames,
            levels = levels, sep = sep, suffix_rank = suffix_rank,
            rowLengthOut = rowLengthOut
          )
        }
      }
      return(vec[1:rowLengthOut])
    }
  )

  # make match original tt format (taxa as rows, ranks as cols)
  if (inherits(tt_out, "matrix")) {
    tt_out <- t(tt_out)
  } else {
    # vapply returns vector if tt had only 1 rank
    tt_out <- as.matrix(tt_out) # returns 1-column matrix
  }

  # ensure original row order
  tt_out <- tt_out[original_rownames, , drop = FALSE]
  # repair colnames
  colnames(tt_out) <- ranknames

  # preserve any columns not listed in levels
  preserved <- !ranknames %in% levels
  if (any(preserved)) tt_out[, preserved] <- tt[, preserved]

  # return phyloseq or tax table (same as input)
  if (inherits(ps, "phyloseq")) {
    phyloseq::tax_table(ps) <- tt_out
    return(ps)
  } else {
    return(phyloseq::tax_table(tt_out))
  }
}

#' internal helper for tax_fix
#'
#' @param vec row over which iteration is occuring
#' @param is_unknown logical vector of length length(row)
#' @param ranknames character vector of length length(row)
#' @param rowLengthOut number of ranks, originally
#'
#' @inheritParams tax_fix
#' @return fixed value
#' @noRd
tax_fix_value <- function(vec,
                          is_unknown,
                          ranknames,
                          levels,
                          rowLengthOut,
                          suffix_rank,
                          sep) {
  vec_out <- vapply(
    X = seq_along(vec),
    FUN.VALUE = vec[1],
    FUN = function(i) {
      if (is_unknown[i]) {
        known_above <- !is_unknown[1:(i - 1)]
        if (!any(known_above)) {
          if (ranknames[[i]] %in% levels) {
            stop(
              "Unknown values detected to the left of known values\n",
              "in row named: ", vec[[rowLengthOut + 1]],
              "\nThis should not happen. Check/fix this row:\n",
              paste(vec[1:rowLengthOut], collapse = "; ")
            )
          }
          return(vec[i])
        }
        nearest_known <- max(which(known_above))
        tax <- vec[nearest_known]
        if (identical(suffix_rank, "classified")) {
          level <- ranknames[[nearest_known]]
        } else {
          level <- ranknames[[i]]
        }
        tax <- paste(tax, level, sep = sep)
        return(tax)
      } else {
        return(vec[i])
      }
    }
  )
  return(vec_out)
}


#' Helper function returns vector of common unknown/uninformative tax table entries
#'
#' @description
#'  Returns values often found in tax_tables (as assigned by e.g. DECIPHER or NGTAX2)
#'  This function is used inside tax_fix (these values are replaced)
#'  This function is used inside phyloseq_validate (values are reported only)
#'
#' @param min_length
#' minimum length taxa allowed to be to not be flagged or replaced on grounds of nchar!
#' (set for purposes of enclosing function, not this function)
#' @return character vector
#'
#' @noRd
tax_common_unknowns <- function(min_length) {
  # check tax_table for uninformative entries
  unknowns <- c("unknown", "Unknown")
  rank_letters <- c("s", "g", "f", "o", "c", "p", "k")
  rank_letters <- c(rank_letters, toupper(rank_letters))
  rank_stubs <- paste0(rank_letters, "__")
  rank_junk <- sapply(X = rank_stubs, paste0, c(unknowns, "NA"))
  unknowns <- c(unknowns, as.vector(rank_junk))

  # include shorter names if min_tax_length means nchar won't catch them
  if (min_length <= 1) unknowns <- c(" ", "", unknowns)
  if (min_length <= 2) unknowns <- c("NA", unknowns)
  if (min_length <= 3) unknowns <- c(rank_stubs, unknowns, "NaN")

  return(unknowns)
}
