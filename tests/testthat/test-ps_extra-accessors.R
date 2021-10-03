
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
