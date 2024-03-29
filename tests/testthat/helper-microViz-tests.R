# internal helper functions only used for testing with `testthat`
# ref:
# https://testthat.r-lib.org/articles/custom-expectation.html
# https://testthat.r-lib.org/reference/expect_snapshot_file.html
#

# save csv and return path to it
save_csv <- function(object) {
  path <- base::tempfile(fileext = ".csv")
  utils::write.csv(x = object, file = path)
  return(path)
}

# custom testthat expectation
#' @param name name of object
#' @param object dataframe or similar object to save as csv
#' @noRd
expect_snapshot_csv <- function(name, object) {
  path <- save_csv(object)
  testthat::expect_snapshot_file(path = path, name = paste0(name, ".csv"))
}
