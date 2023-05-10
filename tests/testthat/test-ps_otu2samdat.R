test_that("ps_otu2samdat works as expected", {
  data("shao19")
  taxnames <- phyloseq::taxa_names(shao19)
  varnames <- phyloseq::sample_variables(shao19)
  # quickly check nothing relevant changed before next checks
  expect_equal(taxnames[1:2], c("Escherichia coli", "Bacteroides caccae"))
  expect_identical(
    tail(phyloseq::sample_variables(ps_otu2samdat(shao19, taxa = 1:2)), n = 2),
    expected = taxnames[1:2]
  )
  expect_identical(
    tail(phyloseq::sample_variables(ps_otu2samdat(shao19, taxa = 1:2)), n = 2),
    expected = taxnames[1:2]
  )
  expect_identical(
    ps_otu2samdat(shao19, taxa = 1:2), ps_otu2samdat(shao19, taxa = taxnames[1:2])
  )
  expect_error(
    object = ps_otu2samdat(shao19, taxa = 5000), regexp = "Invalid taxa selection"
  )
  expect_error(
    object = ps_otu2samdat(shao19, taxa = "why"),
    regexp = "The following taxa were not found in the otu table"
  )
  # repeated selection of same
  tmp <- expect_warning(
    object = ps_otu2samdat(shao19, taxa = 1:2) %>% ps_otu2samdat(1:3),
    regexp = "Overwriting the following sample_data variables:
Escherichia coli / Bacteroides caccae"
  )
  expect_equal(object = ps_otu2samdat(shao19, taxa = 1:3), expected = tmp)
})
