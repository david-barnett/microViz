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

test_that(desc = "ps_filter can do equivalent to ps_drop_incomplete", {
  expect_identical(
    object = enterotype %>% ps_filter(dplyr::across(dplyr::everything(), ~ !is.na(.)), .keep_all_taxa = TRUE),
    expected = enterotype %>% ps_drop_incomplete()
  )
})

test_that(desc = "ps_filter can do equivalent to dropping samples with incomplete sample_variables and tax_filtering 0s", {
  ps2 <- enterotype %>% ps_filter(dplyr::across(dplyr::everything(), ~ !is.na(.)))
  ps3 <- enterotype %>%
    ps_drop_incomplete() %>%
    tax_filter(prev_detection_threshold = 1e-20, is_counts = FALSE)
  expect_identical(object = ps2, expected = ps3)
})
