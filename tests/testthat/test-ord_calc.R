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
  tax_transform(rank = "Genus", transformation = "identity") %>%
  dist_calc("bray")

tmpNoDist <- dietswap %>%
  tax_transform(rank = "Genus", transformation = "identity")

test_that("constrained ordination gives correct warnings etc", {
  expect_message(
    ord_calc(tmpDist, constraints = c("weight", "female"), scale_cc = FALSE),
    regexp = "Dropping samples with missings: 2"
  )
  expect_message(
    ord_calc(tmpDist, conditions = "weight", scale_cc = TRUE),
    regexp = "Centering"
  )
  expect_warning(
    object = ord_calc(data = tmpDist, method = "PCA"),
    regexp = "Distance matrix is not used for PCA"
  )
  expect_error(
    object = ord_calc(data = tmpNoDist, method = "PCoA"),
    regexp =
      "Use dist_calc\\(\\) before using ord_calc\\(\\) with this method: PCoA"
  )
  expect_error(
    object = ord_calc(data = tmpNoDist, method = "PCOA"),
    regexp =
      "PCOA is not a valid `ord_calc` method, must be one of"
  )
  expect_warning(
    object = ord_calc(data = tmpNoDist, method = "DCA"),
    regexp =
      "Neither DCA nor DPCoA have been tested"
  )
  expect_error(
    object = ord_calc(tmpNoDist, method = "PCA", constraints = "etc"),
    regexp =
      "PCA cannot use constraints, did you mean RDA, CAP or CCA"
  )

})

