#' Deprecated, use tax_fix
#'
#' tax_fill_unknowns is deprecated, use tax_fix instead (with same arguments)
#' or try out tax_fix_interactive()
#'
#' @param ps deprecated, use tax_fix
#' @param min_length deprecated, use tax_fix
#' @param unknowns deprecated, use tax_fix
#' @param levels deprecated, use tax_fix
#' @param suffix_rank deprecated, use tax_fix
#' @param sep deprecated, use tax_fix
#' @param anon_unique deprecated, use tax_fix
#' @param verbose deprecated, use tax_fix
#'
#' @return nothing
#' @export
tax_fill_unknowns <- function(ps = NULL,
                              min_length = NULL,
                              unknowns = NULL,
                              levels = NULL,
                              suffix_rank = NULL,
                              sep = NULL,
                              anon_unique = NULL,
                              verbose = NULL) {
  stop(
    "tax_fill_unknowns is deprecated.\n",
    "Use tax_fix() instead (with same arguments)\n",
    "or try out tax_fix_interactive() ! "
  )
}
