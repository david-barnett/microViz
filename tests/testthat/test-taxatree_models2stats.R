
test_that("tax_models2stats works", {
  local_edition(3)

  data("shao19")

  psx <- shao19 %>%
    ps_filter(family_role == "child", infant_age == 4) %>%
    ps_filter(dplyr::row_number() < 50) %>%
    tax_transform("compositional", rank = "genus")

  expect_s4_class(psx, "psExtra")
  expect_error(
    psx %>% tax_models2stats(),
    "psExtra must have tax_models list attached, did you use tax_model"
  )

  # simple linear model
  lmBirthweight_psx <- psx %>%
    tax_model(rank = "class", variables = "birth_weight", verbose = FALSE)  %>%
    tax_models2stats(.keep_models = TRUE)

  expect_s4_class(lmBirthweight_psx, "psExtra")
  expect_snapshot(lmBirthweight_psx)
  expect_equal(names(tax_models_get(lmBirthweight_psx)), "class")
  expect_s3_class(tax_models_get(lmBirthweight_psx)[[1]][[1]], "lm")
  expect_snapshot_csv(
    name = "lmBirthweight_psx",
    object = lmBirthweight_psx %>%
      tax_stats_get() %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., digits = 8)))
  )

  # multivariable wilcox fail
  expect_error(
    psx %>% tax_model(
      rank = "phylum", variables = c("birth_mode", "sex"), verbose = FALSE,
      type = wilcox.test, exact = FALSE # exact passed to wilcox.test
    ),
    regexp = "'formula' missing or incorrect"
  )

  # multiple wilcox tests
  wilcox2_psx <- psx %>%
    tax_model(
      rank = "phylum", variables = list("birth_mode", "sex"), verbose = FALSE,
      type = wilcox.test, exact = FALSE # exact passed to wilcox.test
    )  %>%
    tax_models2stats(.keep_models = TRUE)

  expect_s4_class(wilcox2_psx, "psExtra")
  expect_snapshot(wilcox2_psx)
  expect_equal(names(tax_models_get(wilcox2_psx)), "phylum")
  # is nested list (single rank -> variables -> taxa)
  expect_equal(names(tax_models_get(wilcox2_psx)[["phylum"]]), c("birth_mode", "sex"))
  expect_type(tax_models_get(wilcox2_psx)[[1]][[1]], "list")
  expect_s3_class(tax_models_get(wilcox2_psx)[[1]][[1]][[1]], "htest")
  expect_snapshot_csv(
    name = "wilcox2_psx",
    object = wilcox2_psx %>%
      tax_stats_get() %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., digits = 8)))
  )

})


