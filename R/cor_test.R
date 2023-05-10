#' Simple wrapper around cor.test for y ~ x style formula input
#'
#' Intended for use within the function tax_model
#'
#' @param data dataframe
#' @param formula a formula in form y ~ x
#' @param ... passed to cor.test
#'
#' @export
#'
#' @examples
#' data("shao19")
#' ps <- shao19 %>%
#'   ps_filter(family_role == "mother") %>%
#'   tax_filter(min_prevalence = 20) %>%
#'   tax_agg("family")
#'
#' cors <- ps %>% tax_model(
#'   rank = "family", variables = list("age", "number_reads"), type = cor_test
#' )
#'
#' tax_models_get(cors)
#'
cor_test <- function(formula,
                     data,
                     ...) {
  rlang::check_dots_used()
  fstring <- formula2string(formula)
  fstring_lhs <- gsub(" ?~.+$", "", x = fstring)
  fstring_rhs <- gsub("^.+?~", "", x = fstring)
  f <- stats::formula(paste("~", fstring_lhs, "+", fstring_rhs))
  stats::cor.test(formula = f, data = data, ...)
}
