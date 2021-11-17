#' De-duplicate phyloseq samples
#'
#' @description
#' Use 1 or more variables in the sample_data to identify and remove duplicate samples (leaving 1 per category).
#'
#' __methods:__
#' - method = "readcount" keeps the one sample in each duplicate group with the highest total number of reads according to sample_sums
#' - method = "first" keeps the first sample in each duplicate group encountered in the row order of the sample_data
#' - method = "last" keeps the last sample in each duplicate group encountered in the row order of the sample_data
#'
#' @details
#' What happens when duplicated samples have exactly equal readcounts in method = "readcount"?
#' The first encountered maximum is kept (in sample_data row order, like method = "first")
#'
#' @param ps phyloseq with sample data
#' @param vars names of variables, whose (combined) levels identify groups from which only 1 sample is desired
#' @param method
#' keep 1 sample with max "readcount" or the "first" or "last" samples encountered in given sample_data order for each dupe group
#' @param verbose message names of samples removed if TRUE (or "debug" for more info, when method = "readcount")
#'
#' @return phyloseq object
#' @export
#'
#' @seealso \code{\link{ps_filter}} for filtering samples by sample_data variables
#'
#' @examples
#' data("dietswap", package = "microbiome")
#'
#' dietswap
#' # let's pretend the dietswap data contains technical replicates from each subject
#' # we want to keep only one of them
#' ps_dedupe(dietswap, vars = "subject", method = "readcount", verbose = TRUE)
#'
#' # contrived example to show identifying "duplicates" via the interaction of multiple columns
#' ps1 <- ps_dedupe(
#'   ps = dietswap, method = "readcount", verbose = TRUE,
#'   vars = c("timepoint", "group", "bmi_group")
#' )
#' phyloseq::sample_data(ps1)
#'
#' ps2 <- ps_dedupe(
#'   ps = dietswap, method = "first", verbose = TRUE,
#'   vars = c("timepoint", "group", "bmi_group")
#' )
#' phyloseq::sample_data(ps2)
ps_dedupe <- function(ps,
                      vars,
                      method = "readcount",
                      verbose = TRUE) {
  ps_df <- samdatAsDataframe(ps)

  ps_df[[".temp_grouping_var"]] <- interaction(ps_df[, vars])

  dupe_names <- unique(
    ps_df[[".temp_grouping_var"]][duplicated(ps_df[, ".temp_grouping_var"])]
  )
  no_dupe_samples <-
    rownames(ps_df)[!ps_df[[".temp_grouping_var"]] %in% dupe_names]

  rejects_message <- function(rejects) {
    message(
      length(rejects), " samples being removed:\n",
      paste0(rejects, collapse = "; ")
    )
  }

  keepers <- switch(
    EXPR = method,
    "readcount" = {
      ps_df[[".temp_readcount_var"]] <- phyloseq::sample_sums(ps)
      ps_df[[".temp_sample_id_var"]] <- rownames(ps_df)
      ps_df <-
        dplyr::filter(ps_df, .data[[".temp_grouping_var"]] %in% dupe_names)
      ps_df <- dplyr::group_by(ps_df, .data[[".temp_grouping_var"]])
      df_sum <-
        dplyr::summarise(ps_df, max = max(.data[[".temp_readcount_var"]]))

      keepers <- unlist(
        sapply(dupe_names, function(dn) {
          dupe_group <- ps_df[ps_df$.temp_grouping_var == dn, ]
          max_reads <- df_sum$max[df_sum$.temp_grouping_var == dn][[1]]
          keepers <- dupe_group$.temp_sample_id_var[
            dupe_group$.temp_readcount_var == max_reads
          ]
          if (identical(verbose, "debug")) {
            to_drop <- dupe_group$.temp_sample_id_var != keepers[[1]]
            message(
              "\ngroup = ", dn, "\n",
              nrow(dupe_group), " dupes", "\nmax_reads = ", max_reads,
              "\nmatches = ",
              paste(keepers, collapse = "; "), "\nkeeping: ", keepers[[1]],
              "\ndropping: ",
              paste(dupe_group$.temp_sample_id_var[to_drop], collapse = "; ")
            )
          }
          keepers[[1]]
        })
      )

      if (!isFALSE(verbose)) {
        rejects <-
          ps_df$.temp_sample_id_var[!ps_df$.temp_sample_id_var %in% keepers]
        rejects_message(rejects)
      }
      union(keepers, no_dupe_samples)
    },
    "first" = {
      keepers <- !duplicated(ps_df[[".temp_grouping_var"]])
      if (!isFALSE(verbose)) {
        rejects_message(rownames(ps_df)[!keepers])
      }
      keepers
    },
    "last" = {
      keepers <- !duplicated(ps_df[[".temp_grouping_var"]], fromLast = TRUE)
      if (!isFALSE(verbose)) {
        rejects_message(rownames(ps_df)[!keepers])
      }
      keepers
    }
  )

  # prune phyloseq object samples
  phyloseq::prune_samples(x = ps, samples = keepers)
}
