
test_that("taxatree_plotkey works as expected", {
  local_edition(3)
  data("shao19")

  ps <- shao19 %>%
    ps_filter(dplyr::row_number() <= 15, .keep_all_taxa = TRUE) %>%
    tax_filter(min_prevalence = 6)

  # plot non-circular ggrepel labels without warnings
  expect_silent(
    tax_prepend_ranks(ps) %>% taxatree_plotkey(circular = FALSE, rank != "species")
  )
  # plot circular format with ggrepel labels without warnings (e.g. dplyr::across dataframe result warning)
  expect_silent(
    tax_prepend_ranks(ps) %>% taxatree_plotkey(rank != "species")
  )

  # check node sorting works
  expect_setequal(
    taxatree_plotkey(tax_prepend_ranks(ps), node_sort = "decreasing")$data$taxon,
    taxatree_plotkey(tax_prepend_ranks(ps))$data$taxon
  )
  expect_failure(expect_equal(
    taxatree_plotkey(tax_prepend_ranks(ps), node_sort = "decreasing")$data$taxon,
    taxatree_plotkey(tax_prepend_ranks(ps))$data$taxon,
    ignore_attr = TRUE
  ))

  # draw circular key without labels (calculate labels though)
  unlabeledKey <- tax_prepend_ranks(ps) %>%
    taxatree_plotkey(rank != "species", .draw_label = FALSE)

  # draw rectangular key without labels
  unlabeledKey_rect <- tax_prepend_ranks(ps) %>%
    taxatree_plotkey(circular = FALSE, rank != "species", .draw_label = FALSE)

  # check rectangular plot data
  expect_s3_class(unlabeledKey_rect$layers[[1]]$geom, "GeomEdgePath") # asserts no circles drawn behind

  # plot non-circular geom_text labels without warnings
  expect_silent(
    labeledKey_rect_geomtext <- unlabeledKey_rect %>%
      taxatree_plot_labels(circular = FALSE, fun = ggplot2::geom_text)
  )

  # plot circular geom_text labels without warnings (unknown args)
  expect_silent(
    labeledKey_geomtext <- unlabeledKey %>%
      taxatree_plot_labels(fun = ggplot2::geom_text, x_nudge = 0.01)
  )

  # warning on use of color = whatever, in ellipses (british spelling only)
  expect_warning(
    tmp <- unlabeledKey %>% taxatree_plot_labels(color = "red"),
    regexp = "'color' argument is ignored, please use 'colour'"
  )

  # get circular plot data
  keyDat <- unlabeledKey$data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 6))

  # get rectangular plot data
  keyDatRect <- unlabeledKey_rect$data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 6))

  expect_snapshot_csv(name = "unlabeledKeyData", keyDat)
  expect_snapshot_csv(name = "unlabeledKey_rectData", keyDatRect)

  skip_if(R.version$major != "4") # printing of x and ... different
  expect_snapshot(attr(unlabeledKey$data, "graph"))
  expect_snapshot(attr(unlabeledKey_rect$data, "graph"))
})

test_that("taxatree_plotkey can sort nodes", {
  data("shao19")

  ps <- shao19 %>%
    ps_filter(dplyr::row_number() <= 15, .keep_all_taxa = TRUE) %>%
    tax_filter(min_prevalence = 6)

  psX <- ps %>%
    taxatree_models(
      ranks = c("class", "genus"), variables = "birth_mode", verbose = FALSE
    ) %>%
    taxatree_models2stats()

  expect_s3_class(k <- taxatree_plotkey(psX, drop_ranks = TRUE), "ggplot")
  expect_s3_class(p <- taxatree_plots(psX, drop_ranks = TRUE)[[1]], "ggplot")
  expect_equal(p$data$taxon, k$data$taxon)
  expect_equal(p$data$x, k$data$x)
  expect_equal(p$data$y, k$data$y)

  ps1 <- psX %>%
    taxatree_models(ranks = "genus", variables = "birth_mode", verbose = FALSE) %>%
    taxatree_models2stats()

  expect_s3_class(k1 <- taxatree_plotkey(ps1, circular = FALSE), "ggplot")
  expect_s3_class(p1 <- taxatree_plots(ps1, circular = FALSE)[[1]], "ggplot")
  expect_equal(p1$data$taxon, k1$data$taxon)
  expect_equal(p1$data$x, k1$data$x)
  expect_equal(p1$data$y, k1$data$y)
})


test_that("taxatree_plotkey can drop ranks", {
  local_edition(3)
  data("shao19")

  ps <- shao19 %>%
    ps_filter(dplyr::row_number() <= 15, .keep_all_taxa = TRUE) %>%
    tax_filter(min_prevalence = 6)

  psX <- ps %>%
    taxatree_models(
      ranks = c("class", "genus"), variables = "birth_mode", verbose = FALSE
    ) %>%
    taxatree_models2stats()

  expect_s3_class(k <- taxatree_plotkey(psX, drop_ranks = TRUE), "ggplot")
  expect_s3_class(p <- taxatree_plots(psX, drop_ranks = TRUE)[[1]], "ggplot")
  expect_equal(p$data$taxon, k$data$taxon)
  expect_equal(p$data$x, k$data$x)
  expect_equal(p$data$y, k$data$y)

  ps1 <- psX %>%
    taxatree_models(ranks = "genus", variables = "birth_mode", verbose = FALSE) %>%
    taxatree_models2stats()

  expect_s3_class(k1 <- taxatree_plotkey(ps1, circular = FALSE), "ggplot")
  expect_s3_class(p1 <- taxatree_plots(ps1, circular = FALSE)[[1]], "ggplot")
  expect_equal(p1$data$taxon, k1$data$taxon)
  expect_equal(p1$data$x, k1$data$x)
  expect_equal(p1$data$y, k1$data$y)
})

test_that("taxatree_label and plot_labels allows multiple rounds of custom labels", {
  local_edition(3)
  data("shao19")

  ps <- shao19 %>%
    ps_filter(dplyr::row_number() <= 15, .keep_all_taxa = TRUE) %>%
    tax_filter(min_prevalence = 6)

  expect_silent(
    pMulti <- ps %>%
      tax_prepend_ranks() %>%
      taxatree_label(.label_var = "phylum_label", rank == "phylum") %>%
      taxatree_label(.label_var = "class_label", rank == "class") %>%
      taxatree_plotkey(.calc_label = FALSE, .draw_label = FALSE) %>%
      taxatree_plot_labels(label_var = "phylum_label", colour = "red") %>%
      taxatree_plot_labels(label_var = "class_label", size = 2, rotate = -5)
  )
  expect_s3_class(pMulti, "ggplot")
  pMultiDat <- pMulti$data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 6))
  expect_snapshot_csv("taxatreekey-multiplelabels", pMultiDat)
})

