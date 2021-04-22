#' Join a dataframe to phyloseq sample data
#'
#' You can use most types of join from the dplyr::*_join function family, including e.g. "inner", "left", "semi", "anti" (see details below).
#' Defaults to type = "left" which calls left_join(), this supports x as a phyloseq and y as a dataframe.
#' Most of the time you'll want "left" (adds variables with no sample filtering), or "inner" (adds variables and filters samples).
#' This function simply:
#'  1. extracts the sample_data from the phyloseq as a dataframe
#'  2. performs the chosen type of join (with the given arguments)
#'  3. filters the phyloseq if type = inner, semi or anti
#'  4. reattaches the modified sample_data to the phyloseq and returns the phyloseq
#'
#' __Mutating joins__, which will add columns from a dataframe to phyloseq sample data, matching rows based on the key columns named in the `by` argument:
#'
#' - "inner": includes all rows in present in both x and y.
#' - "left": includes all rows in x. (so x must be the phyloseq)
#' - "right": includes all rows in y. (so y must be the phyloseq)
#' - "full": includes all rows present in x or y. (will likely NOT work, as additional rows cannot be added to sample_data!)
#'
#' If a row in x matches multiple rows in y (based on variables named in the `by` argument),
#' all the rows in y will be added once for each matching row in x.
#' This will cause this function to fail, as additional rows cannot be added to the phyloseq sample_data!
#'
#' __Filtering joins__ filter rows from x based on the presence or absence of matches in y:
#'
#' - "semi": return all rows from x with a match in y.
#' - "anti": return all rows from x without a match in y.
#'
#' @param x phyloseq (or dataframe)
#' @param y dataframe (or phyloseq for e.g. type = "right")
#' @param by A character vector of variables to join by (col must be present in both x and y or paired via a named vector like c("xname" = "yname", etc.))
#' @param match_sample_names match against the phyloseq sample_names by naming a variable in the additional dataframe (this is in addition to any variables named in by)
#' @param keep_sample_name_col should the column named in match_sample_names be kept in the returned phyloseq's sample_data? (only relevant if match_sample_names is not NULL)
#' @param sample_name_natural_join if TRUE, use sample_name AND all shared colnames to match rows (only relevant if match_sample_names is not NULL, this arg takes precedence over anything also entered in `by` arg)
#' @param type name of type of join e.g. "left", "right", "inner", "semi" (see dplyr help pages)
#' @param .keep_all_taxa if FALSE (the default), remove taxa which are no longer present in the dataset after filtering
#'
#' @return phyloseq with modified sample_data (and possibly filtered)
#' @export
#'
#' @seealso \code{\link{ps_mutate}} for computing new variables from existing sample data
#' @seealso \code{\link{ps_select}} for selecting only some sample_data variables
#' @seealso \url{https://www.garrickadenbuie.com/project/tidyexplain/} for an animated introduction to joining dataframes
#'
#' @examples
#' library(phyloseq)
#' data("enterotype", package = "phyloseq")
#'
#' x <- enterotype
#' y <- data.frame(
#'   ID_var = sample_names(enterotype)[c(1:50, 101:150)],
#'   SeqTech = sample_data(enterotype)[c(1:50, 101:150), "SeqTech"],
#'   arbitrary_info = rep(c("A", "B"), 50)
#' )
#'
#' # simply match the new data to samples that exist in x, as default is a left_join
#' # where some sample names of x are expected to match variable ID_var in dataframe y
#' out1A <- ps_join(x = x, y = y, match_sample_names = "ID_var")
#' out1A
#' sample_data(out1A)[1:6, ]
#'
#'
#' # use sample_name and all shared variables to join
#' # (a natural join is not a type of join per se,
#' # but it indicates that all shared variables should be used for matching)
#' out1B <- ps_join(
#'   x = x, y = y, match_sample_names = "ID_var",
#'   sample_name_natural_join = TRUE, keep_sample_name_col = FALSE
#' )
#' out1B
#' sample_data(out1B)[1:6, ]
#'
#' # if you only want to keep phyloseq samples that exist in the new data, try an inner join
#' # this will add the new variables AND filter the phyloseq
#' # this example matches sample names to ID_var and by matching the shared SeqTech variable
#' out1C <- ps_join(x = x, y = y, type = "inner", by = "SeqTech", match_sample_names = "ID_var")
#' out1C
#' sample_data(out1C)[1:6, ]
#'
#' # the id variable is named Sample_ID in x and ID_var in y
#' # semi_join is only a filtering join (doesn't add new variables but just filters samples in x)
#' out2A <- ps_join(x = x, y = y, by = c("Sample_ID" = "ID_var"), type = "semi")
#' out2A
#' sample_data(out2A)[1:6, ]
#'
#' # anti_join is another type of filtering join
#' out2B <- ps_join(x = x, y = y, by = c("Sample_ID" = "ID_var"), type = "anti")
#' out2B
#' sample_data(out2B)[1:6, ]
#'
#' # semi and anti joins keep opposite sets of samples
#' intersect(sample_names(out2A), sample_names(out2B))
#'
#' # you can mix and match named and unnamed values in the `by` vector
#' # inner is like a combination of left join and semi join
#' out3 <- ps_join(x = x, y = y, by = c("Sample_ID" = "ID_var", "SeqTech"), type = "inner")
#' out3
#' sample_data(out3)[1:6, ]
ps_join <- function(x,
                    y,
                    by = NULL,
                    match_sample_names = NULL,
                    keep_sample_name_col = TRUE,
                    sample_name_natural_join = FALSE,
                    type = "left",
                    .keep_all_taxa = FALSE) {
  data_list <- list(x, y)
  classes <- sapply(data_list, class)

  # error message helper function
  message_xy_classes <- function() {
    message("class(x) = ", classes[[1]])
    message("class(y) = ", classes[[2]])
  }

  if (any(classes[[1]] %in% classes[[2]])) {
    message_xy_classes()
    stop("x should be a phyloseq and y should be a dataframe, or vice versa")
  }

  if (any(classes[[2]] == "phyloseq") && identical(type, "left")) {
    message_xy_classes()
    stop("For join type = 'left', x must be the phyloseq, and y a dataframe")
  }

  if (any(classes[[1]] == "phyloseq") && identical(type, "right")) {
    message_xy_classes()
    stop("For join type = 'right', x must be a dataframe, and y the phyloseq.")
  }

  # handle matching by phyloseq sample_names / rownames
  if (!identical(match_sample_names, NULL)) {
    rownames_col <- match_sample_names
    by <- c(rownames_col, by)
  } else {
    rownames_col <- ".SAMPLE_ID_ROWNAMES"
  }

  # handle phyloseq - whether in x or y
  if (inherits(x, "phyloseq")) {
    ps <- x
    x <- data.frame(phyloseq::sample_data(x), check.names = FALSE)
    x <- tibble::rownames_to_column(x, var = rownames_col)
  } else if (inherits(y, "phyloseq")) {
    ps <- y
    y <- data.frame(phyloseq::sample_data(y), check.names = FALSE)
    y <- tibble::rownames_to_column(y, var = rownames_col)
  }

  if (isTRUE(sample_name_natural_join) && identical(rownames_col, match_sample_names)) {
    by <- union(rownames_col, intersect(colnames(x), colnames(y)))
  }

  # get specified join function
  join_fun <- utils::getFromNamespace(x = paste0(type, "_join"), ns = "dplyr")

  new_df <- do.call(
    what = join_fun,
    args = list(x = x, y = y, by = by)
  )

  # check for non-unique matches leading to row gain
  n_gained_rows <- nrow(new_df) - phyloseq::nsamples(ps)
  if (n_gained_rows > 0) {
    stop(
      "Matching rows led to ", n_gained_rows, " more rows than in the original phyloseq.",
      "\n This is likely due to non-unique matches by: ", paste(by, collapse = " / ")
    )
  }

  # restore rownames from temporary column
  if (isTRUE(keep_sample_name_col)) {
    rownames(new_df) <- new_df[[rownames_col]]
  } else {
    new_df <- tibble::column_to_rownames(new_df, var = rownames_col)
  }
  new_df[[".SAMPLE_ID_ROWNAMES"]] <- NULL

  # prune_samples in phyloseq in case of a filter or inner join use
  if (type %in% c("inner", "semi", "anti")) {
    ps <- phyloseq::prune_samples(samples = rownames(new_df), x = ps)
  }
  # remove taxa that now have zero counts (or relative abundance) across all remaining samples
  if (isFALSE(.keep_all_taxa)) ps <- tax_filter_zeros(ps) # internal helper function in ps_filter.R

  phyloseq::sample_data(ps) <- new_df

  return(ps)
}
