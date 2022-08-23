
test_that("tax_palette throws appropriate errors", {
  ps <- tax_filter(corncob::ibd_phylo, min_prevalence = 10)
  ps <- tax_fix(ps)

  expect_error(
    tax_palette(ps, rank = function() {}, n = 10),
    regexp = "`rank` must be the name of a valid rank:
Kingdom / Phylum / Class / Order / Family / Genus / Species
or: unique / NA"
  )
  expect_error(
    tax_palette(ps, rank = "Genus", n = 42),
    regexp = "Palette 'brewerPlus' has 41 colors, not 42"
  )
  expect_error(
    tax_palette(data = ps, add = "why", rank = "Genus"),
    regexp = "`add` must be NA or named vector of colours"
  )
  expect_error(
    tax_palette(data = ps, n = 0, rank = "Genus"),
    regexp = "`n` must be a single number, greater than zero"
  )
  expect_error(
    tax_palette(
      data = ps, rank = "Genus", n = 10,
      add = c(other = "red", fail = "redd")
    ),
    regexp = "invalid color name 'redd'"
  )
  er <- expect_error(tax_palette(ps, rank = "Genus", n = 10, pal = "b"))
  expect_match(
    er$message, 'of \"brewerPlus\", \"kelly\", or \"greenArmytage\", not \"b\"'
  )
})

test_that("tax_palette works", {
  ps <- tax_filter(corncob::ibd_phylo, min_prevalence = 10)
  ps <- tax_fix(ps)

  expect_equal(
    object = tax_palette(ps, rank = "Genus", n = 4),
    expected = c(
      "Bacteroides" = "#A6CEE3", "Faecalibacterium" = "#1F78B4",
      "Escherichia/Shigella" = "#B2DF8A", "Blautia" = "#33A02C",
      "other" = "lightgrey"
    )
  )
  expect_equal(
    object = tax_palette(ps, rank = "Genus", n = 4, pal = "kelly"),
    expected = c(
      "Bacteroides" = "#f3c300", "Faecalibacterium" = "#875692",
      "Escherichia/Shigella" = "#f38400", "Blautia" = "#a1caf1",
      "other" = "lightgrey"
    )
  )
  expect_equal(
    tax_palette(ps, rank = "Genus", n = 1, add = NA),
    c("Bacteroides" = "#A6CEE3")
  )
  expect_equal(
    tax_palette(ps, rank = "Genus", n = 1, add = c("OTHER" = "white")),
    expected = c("Bacteroides" = "#A6CEE3", "OTHER" = "white")
  )
})

test_that("tax_palette_plot works", {
  local_edition(3)
  # announcing (with standardised names) avoids deletion on skip
  announce_snapshot_file(name = "tax-palette-plot.svg")
  skip_on_os(c("windows", "mac"))
  ps <- tax_filter(corncob::ibd_phylo, min_prevalence = 10)
  ps <- tax_fix(ps)
  vdiffr::expect_doppelganger(
    "tax_palette_plot",
    tax_palette_plot(
      tax_palette(ps, rank = "Genus", n = 35)
    ) +
      ggplot2::theme_test()
  )
})
