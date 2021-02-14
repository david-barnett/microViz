data("dietswap", package = "microbiome")

# compute distance
testDist <- dietswap %>%
  tax_agg("Genus") %>%
  dist_calc("bray")

test_that("permanova detects variables not in sample data", {
  expect_error(
    testDist %>%
      dist_permanova(
        seed = 1,
        variables = "nationality + sex * bmi_group + fake_Var",
        n_perms = 9
      ),
    regexp = "variables are not found"
  )
})

test_that("different ways of specifying the same permanova model are equivalent", {
  # try permanova with interaction terms
  PERM2 <- testDist %>%
    dist_permanova(
      seed = 1,
      variables = "nationality + sex * bmi_group",
      n_perms = 9
    )

  # specify the same model in alternative way
  PERM3 <- testDist %>%
    dist_permanova(
      seed = 1,
      variables = c("nationality", "sex", "bmi_group"),
      interactions = "sex * bmi_group",
      n_perms = 9
    )

  expect_equal(PERM3, PERM2)
})
