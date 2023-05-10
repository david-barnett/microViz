test_that("ps_dedupe errors appropriately'", {
  library(phyloseq)
  data("dietswap", package = "microbiome")
  v <- c("timepoint", "group", "bmi_group")
  expect_error(
    ps_dedupe(dietswap, method = "frst", verbose = FALSE, vars = v),
    regexp = '`method` must be one of \"readcount\"'
  )
  expect_error(
    ps_dedupe(dietswap, vars = v, n = 0),
    regexp = "n must be a positive number"
  )
  expect_error(ps_dedupe(dietswap, vars = NULL), "vars must be character")
  expect_error(
    ps_dedupe(dietswap, vars = "why"),
    "why is not a variable in phyloseq sample_data"
  )
  expect_error(
    ps_dedupe(dietswap, vars = v, verbose = "true"),
    regexp = "verbose must be TRUE or FALSE"
  )
  expect_error(
    ps_dedupe(dietswap, vars = v, .keep_group_var = "true"),
    regexp = ".keep_group_var must be TRUE or FALSE"
  )
  expect_error(
    ps_dedupe(dietswap, vars = v, .keep_readcount = "true"),
    regexp = ".keep_readcount must be TRUE or FALSE"
  )
  expect_error(
    ps_dedupe(dietswap, vars = v, .message_IDs = "true"),
    regexp = ".message_IDs must be TRUE or FALSE"
  )
  expect_error(
    ps_dedupe(dietswap, vars = v, .label_only = "true"),
    regexp = ".label_only must be TRUE or FALSE"
  )
  expect_error(
    ps_dedupe(dietswap, vars = v, .keep_all_taxa = "true"),
    regexp = ".keep_all_taxa must be TRUE or FALSE"
  )
})


test_that("ps_dedupe methods all gives same nsamples", {
  local_edition(3)
  library(phyloseq)
  data("dietswap", package = "microbiome")
  v <- c("timepoint", "group", "bmi_group")
  ps1 <- ps_dedupe(dietswap, method = "first", verbose = FALSE, vars = v)
  ps2 <- ps_dedupe(dietswap, method = "readcount", verbose = FALSE, vars = v)
  ps3 <- ps_dedupe(dietswap, method = "last", verbose = FALSE, vars = v)
  ps4 <- ps_dedupe(dietswap, method = "random", verbose = FALSE, vars = v)
  ps5 <- ps_dedupe(dietswap, verbose = FALSE, vars = v, n = 2)

  expect_equal(nsamples(ps1), nsamples(ps2))
  expect_equal(nsamples(ps1), 18)
  expect_equal(nsamples(ps3), 18)
  expect_equal(nsamples(ps4), 18)
  expect_equal(nsamples(ps5), 36)

  # check messages
  suppressMessages(
    expect_message(
      ps_dedupe(dietswap, verbose = TRUE, vars = v),
      regexp = "18 groups: with 8 to 15 samples"
    )
  )
  suppressMessages(
    expect_message(
      ps_dedupe(dietswap, verbose = TRUE, vars = v),
      regexp = "Dropped 204 samples."
    )
  )
  suppressMessages(
    expect_message(
      ps_dedupe(dietswap, verbose = TRUE, vars = c("subject", "timepoint")),
      regexp = "Dropped 0 samples."
    )
  )
  expect_message(
    ps_dedupe(
      ps = dietswap, vars = "timepoint", n = 37,
      .message_IDs = TRUE, verbose = FALSE
    ),
    regexp = "Sample-56, Sample-164"
  )

  # label only works, including keeping vars
  psLab <- ps_dedupe(
    ps = dietswap, verbose = FALSE, vars = v, .label_only = TRUE,
    .keep_group_var = TRUE, .keep_readcount = TRUE
  )
  expect_identical(phyloseq::nsamples(psLab), 222L)
  expect_identical(sample_names(psLab), sample_names(dietswap))
  expect_identical(psLab@sam_data$subject, dietswap@sam_data$subject)
  expect_identical(psLab@sam_data$.KEEP_SAMPLE. %>% sum(), 18L)
  expect_snapshot_csv("dedupe_group", psLab@sam_data$.GROUP.)
  expect_snapshot_csv("dedupe_readcount", psLab@sam_data$.READCOUNT.)
  expect_identical(psLab@sam_data$.READCOUNT., unname(sample_sums(dietswap)))
})
