test_that("ord_plot arrow style helpers work", {
  local_edition(3)
  expect_error(vec_tax_sel(arrow = "none"), "arrow must be")
  expect_error(vec_tax_all(arrow = NA), "arrow must be")
  expect_error(vec_constraint(arrow = grid::arrow), "arrow must be")

  skip_if(packageVersion("ggplot2") < "3.4.0")
  expect_equal(object = vec_tax_sel(), expected = list(
    linewidth = 0.5, alpha = 1,
    arrow = grid::arrow(length = grid::unit(0.005, "npc"), type = "closed"),
    colour = "black", lineend = "round", linejoin = "mitre"
  ))
  expect_snapshot(vec_tax_all())
  expect_snapshot(vec_constraint())

  skip_if(interactive()) # frequency setting of warnings requires fresh session
  expect_warning(
    vec_tax_sel(size = 1),
    "Since ggplot2 v3.4.0, you should use 'linewidth' instead of 'size'"
  )
  # 2nd vec_tax_sel call won't throw a warning because frequency is every 8 hrs
  expect_equal(object = vec_tax_sel(size = 0.1), vec_tax_sel(linewidth = 0.1))
})
