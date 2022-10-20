
test_that("(constrained) ordination gives correct warnings etc", {
  data("dietswap", package = "microbiome")

  # create a couple of numerical variables to use as constraints
  dietswap <- ps_mutate(
    dietswap,
    female = dplyr::if_else(sex == "female", true = 1, false = 0),
    weight = dplyr::recode(bmi_group, obese = 3, overweight = 2, lean = 1)
  )

  # add a couple of missing values to demo automated dropping of observations with missings
  phyloseq::sample_data(dietswap)$female[c(3, 4)] <- NA

  # compute distances
  tmpDist <- dietswap %>%
    tax_transform(rank = "Genus", trans = "identity") %>%
    dist_calc("bray")

  tmpNoDist <- dietswap %>%
    tax_transform(rank = "Genus", trans = "identity")

  ## Start of tests
  expect_warning(
    ord_calc(dietswap, method = "PCA"),
    "data provided to ord_calc is a phyloseq object, not a ps_extra"
  )
  expect_warning(
    ord_calc(ord_calc(tmpNoDist, method = "PCA")),
    "You already calculated an ordination"
  )
  expect_error(
    ord_calc(mtcars),
    "data should be ps_extra list output of dist_calc or tax_transform"
  )
  expect_error(
    ord_calc(tmpDist, conditions = 3:4),
    "conditions must be a character vector, or NULL"
  )
  expect_message(
    ord_calc(tmpDist, constraints = c("weight", "female"), scale_cc = FALSE),
    regexp = "Dropping samples with missings: 2"
  )
  expect_message(
    ord_calc(tmpDist, conditions = "weight", scale_cc = TRUE),
    regexp = "Centering \\(mean\\) and scaling \\(sd\\) the constraints and/or conditions"
  )
  expect_warning(
    object = ord_calc(data = tmpDist, method = "PCA"),
    regexp = "Distance matrix is not used for PCA"
  )
  expect_error(ord_calc(tmpNoDist, method = "PCoA"), "Distance matrix missing!")
  expect_error(ord_calc(tmpNoDist, method = "NMDS"), "Distance matrix missing!")
  expect_error(ord_calc(tmpNoDist, method = "PCOA"), "Did you mean \"PCoA\"?")
  expect_warning(
    object = ord_calc(data = tmpNoDist, method = "DCA"),
    regexp = "Neither DCA nor DPCoA have been tested"
  )
  expect_error(
    object = ord_calc(tmpNoDist, method = "PCA", constraints = "etc"),
    regexp = "PCA cannot use constraints\n"
  )
  expect_error(
    object = suppressMessages(ord_calc(tmpNoDist, constraints = "subject")),
    regexp = "Constraints and conditions must be numeric, logical, or integer"
  )
})
