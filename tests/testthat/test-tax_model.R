
test_that("formulaMakerRHSstring handles lists of variables and formulae", {
  expect_identical(
    expected = "~a + b + c",
    object = microViz:::formulaMakerRHSstring(variables = letters[1:3])
  )
  expect_identical(
    expected = c("~a", "~b", "~c"),
    object = microViz:::formulaMakerRHSstring(variables = list("a", "b", "c"))
  )
  expect_identical(
    expected = c("~a + b", "~a"),
    object = microViz:::formulaMakerRHSstring(formula = c("~a + b", "~ a"))
  )
  expect_identical(
    expected = c("~a + b", "~a"),
    object = microViz:::formulaMakerRHSstring(formula = list("~a + b", "~ a"))
  )
  expect_identical(
    expected = c("~a + b", "~a"),
    object = microViz:::formulaMakerRHSstring(formula = list(~a + b, ~ a))
  )
  expect_identical(
    expected = c("~a + b", "~a"),
    object = microViz:::formulaMakerRHSstring(formula = list(~a + b, "~a"))
  )
})

test_that("formula and variable arg alternatives can be equivalent", {
  library(dplyr)
  data("dietswap", package = "microbiome")
  ps <- dietswap
  # create some binary variables for easy visualization
  ps <- ps %>%
    ps_mutate(
      female = if_else(sex == "female", 1, 0, NaN),
      overweight = if_else(bmi_group == "overweight", 1, 0, NaN),
      obese = if_else(bmi_group == "obese", 1, 0, NaN)
    )

  ps <- ps %>% tax_filter(min_prevalence = 0.1, min_total_abundance = 10000)

  models <- tax_model(
    ps = ps, type = "lm", rank = "Genus", taxa = 1:3,
    variables = c("female", "overweight", "obese")
  )

  expect_equal(
    object = names(models),
    expected = c("Akkermansia", "Allistipes et rel.", "Anaerostipes caccae et rel.")
  )

  # Alternative method using formula arg instead of vars for identical results
  models2 <- tax_model(
    ps = ps, type = "lm", rank = "Genus", taxa = 1:3,
    formula = ~ female + overweight + obese
  )
  # Alternative method using formula arg as string to produce identical results
  models3 <- tax_model(
    ps = ps, type = "lm", rank = "Genus", taxa = 1:3,
    formula = "~ female + overweight + obese"
  )

  expect_equal(models, models2)
  expect_equal(models, models3)

  # try some univariable models
  models4 <- tax_model(
    ps = ps, type = "lm", rank = "Genus", taxa = 1:3,
    formula = c(~ female, "~ overweight", "~ obese")
  )
  models5 <- tax_model(
    ps = ps, type = "lm", rank = "Genus", taxa = 1:3,
    variables = list("female", "overweight", "obese")
  )
  expect_equal(models5, models4)
})

