
test_that("sample data returned without mangling names", {
  data("dietswap", package = "microbiome")
  phyloseq::sample_data(dietswap)[["b a d"]] <- 1:phyloseq::nsamples(dietswap)
  phyloseq::sample_names(dietswap) <- paste(
    phyloseq::sample_names(dietswap), 1:phyloseq::nsamples(dietswap)
  )
  df <- microViz:::samdatAsDataframe(dietswap)
  expect_s3_class(df, "data.frame")
  expect_equal(
    object = colnames(df),
    expected = phyloseq::sample_variables(dietswap)
  )
  expect_equal(
    object = rownames(df),
    expected = phyloseq::sample_names(dietswap)
  )
})

test_that("otu_get subsetting works as expected", {
  data("shao19")
  expect_identical(
    object = otu_get(shao19, samples = "B01042_mo", taxa = "Roseburia hominis"),
    expected = otu_get(shao19)["B01042_mo", "Roseburia hominis"]
  )
  expect_identical(
    object = otu_get(shao19, samples = "B01042_mo", taxa = NA),
    expected = otu_get(shao19)["B01042_mo", ]
  )
  expect_identical(
    object = otu_get(shao19, taxa = "Roseburia hominis"),
    expected = otu_get(shao19)[, "Roseburia hominis"]
  )
  expect_error(
    object = otu_get(shao19, taxa = c("Roseburia homini", "bugs")),
    regexp = "The following taxa were not found in the otu table:"
  )
  expect_error(
    object = otu_get(shao19, taxa = c("Roseburia homini", "bugs", NA, NA)),
    regexp = "Roseburia homini / bugs / NA"
  )
  expect_error(
    object = otu_get(shao19, samples = c("Roseburia hominis", "bugs")),
    regexp = "The following samples were not found in the otu table:"
  )
  expect_error(otu_get(shao19, samples = 1:5000), "Invalid sample selection")
  expect_error(otu_get(shao19, taxa = 1:50000), "Invalid taxa selection")
  expect_error(otu_get(shao19, samples = 1:5000), "Invalid sample selection")
})

