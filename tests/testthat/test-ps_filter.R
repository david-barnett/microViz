library(phyloseq)
library(microbiome)
data("enterotype")


# warning if you want to .keep_all_taxa = FALSE and some of the otu_values are negative
test_that("negative taxa gives warning", {
  expect_warning(
    object = enterotype %>%
      microbiome::transform(transform = "clr", target = "OTU") %>%
      ps_filter(SeqTech == "Sanger"),
    regexp = "some values in the otu_table are negative"
  )
})
