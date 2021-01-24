library(phyloseq)
library(microbiome)
data("dietswap")

test_that("dietswap sample_data .target arranging remains the same", {
  expect_identical(
    phyloseq::sample_names(ps_arrange(ps = dietswap, subject, timepoint))[1:8],
    c("Sample-32", "Sample-109", "Sample-119", "Sample-130", "Sample-140", "Sample-41",  "Sample-61" , "Sample-93")
  )
})

test_that("dietswap otu_table .target arranging remains the same", {
  expect_identical(
    phyloseq::sample_names(ps_arrange(dietswap, dplyr::desc(Akkermansia), .target = "otu_table"))[1:8],
    c("Sample-140", "Sample-130", "Sample-167", "Sample-119", "Sample-100", "Sample-121", "Sample-41" , "Sample-35")
  )
})


