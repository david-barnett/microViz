data("esophagus", package = "phyloseq")
data("dietswap", package = "microbiome")
options(width = 80)

# ord_explore_init -----------------------------------------------------------
test_that("ord_explore_init stays the same", {
  local_edition(3)
  transformed <- dietswap %>%
    ps_mutate(
      weight = dplyr::recode(bmi_group, obese = 3, overweight = 2, lean = 1),
      female = dplyr::if_else(sex == "female", true = 1, false = 0)
    ) %>%
    tax_transform(trans = "clr", rank = "Genus")

  suppressMessages(expect_message( # additional messages, one per constraint
    ord <- ord_calc(transformed, constraints = c("weight", "female"))
  ))
  expect_snapshot(ord_explore_init(dietswap))
  expect_snapshot(ord_explore_init(ord))
  expect_snapshot(ord_explore_init(esophagus))
})

# dist_choices ---------------------------------------------------------------
test_that("dist_choices helper works", {
  local_edition(3)
  # phyloseq without phy_tree
  expect_snapshot(dist_choices(dietswap, type = "tree"))
  expect_snapshot_csv(
    dist_choices(dietswap, type = "noTree"),
    name = "dietDistsNoTree"
  )
  expect_snapshot_csv(
    dist_choices(dietswap, type = "all"),
    name = "dietDistsAll"
  )
  # phyloseq with phy_tree
  expect_snapshot(dist_choices(esophagus, type = "tree"))
  expect_snapshot_csv(
    dist_choices(esophagus, type = "noTree"),
    name = "esoDistsNoTree"
  )
  expect_snapshot_csv(
    dist_choices(esophagus, type = "all"),
    name = "esoDistsAll"
  )
})

# ord_choices ----------------------------------------------------------------
test_that("ord_choices helper works", {
  local_edition(3)
  cons <- list("constrained", "unconstrained")
  dists <- list("dist", "noDist")
  for (type in c(
    "all", cons, dists,
    lapply(cons, c, "dist"),
    lapply(cons, c, "noDist")
  )
  ) {
    expect_snapshot({
      cat(type)
      cat(ord_choices(type))
    })
  }
})

# ord_code --------------------------------------------------------------------
test_that("ord_code helper works", {
  local_edition(3)

  # Iterate through different values for plot_taxa
  for (p in list(FALSE, 1:6)) {
    # Iterate through different values for alpha
    for (a in list(0.5, "aVariable")) {
      # Iterate through different values for const
      for (c in list(NULL, c("test1", "test2"))) {
        # Capture snapshots of ord_code() outputs
        expect_snapshot({
          # Print the current combination of p, a, and c values
          for (x in list(p, a, c)) cat(x, "\t")

          # Call ord_code() with the current combination of p, a, and c values
          ord_code(
            rank = "Genus", trans = "identity", dist = "none",
            ord = "RDA", const = c, conds = NULL, x = 1, y = 2,
            colour = "v", fill = "v", shape = "var",
            alpha = a, size = 1, plot_taxa = p,
            ellipses = FALSE, chulls = FALSE, paths = NULL
          )
        })
      }
    }
  }
})


test_that("ord_code_dist helper works", {
  local_edition(3)
  expect_snapshot(cat(ord_code_dist("aitchison")))
  expect_snapshot(cat(ord_code_dist("none")))
})

test_that("Testing ord_code_stat() different combinations of ellipses and chulls", {
  local_edition(3)
  expect_snapshot(cat(ord_code_stat(ellipses = TRUE, chulls = FALSE, colour = "aVar")))
  expect_snapshot(cat(ord_code_stat(ellipses = FALSE, chulls = FALSE, colour = "aVar")))
  expect_snapshot(cat(ord_code_stat(ellipses = FALSE, chulls = TRUE, colour = "aVar")))
  expect_snapshot(cat(ord_code_stat(ellipses = TRUE, chulls = TRUE, colour = "aVar")))
})

test_that("Testing ord_code_paths() with different all_vars options (string & vec)", {
  local_edition(3)
  expect_snapshot(cat(
    ord_code_paths(paths = list(
      colour = "aVar", id_var = "bVar", id_values = letters[1:4],
      all_vars = "aVar"
    ))
  ))
  expect_snapshot(cat(
    ord_code_paths(paths = list(
      colour = "aVar", id_var = "bVar", id_values = letters[1:4],
      all_vars = c("otherVar", "anotherVar")
    ))
  ))
})

# ord_build ------------------------------------------------------------------
test_that("ord_build works", {
  local_edition(3)
  expect_snapshot(ord_build(
    data = dietswap, rank = "Genus", trans = "identity", dist = "bray",
    method = "PCoA", constraints = NULL, conditions = NULL
  ))
  expect_snapshot(ord_build(
    data = dietswap, rank = "Genus", trans = "clr", dist = NA,
    method = "auto", constraints = NULL, conditions = NULL
  ))
})

# palet_fun ------------------------------------------------------------------
test_that("ord_explore_palet_fun works", {
  local_edition(3)
  expect_snapshot(ord_explore_palet_fun(dietswap, "Genus"))
  expect_snapshot(ord_explore_palet_fun(
    ps = dietswap, tax_level = "Family", top_by = median, other = "colourz"
  ))
})
