library(dplyr)
data("dietswap", package = "microbiome")

# create a couple of numerical variables to use
psq <- dietswap %>%
  ps_mutate(
    weight = recode(bmi_group, obese = 3, overweight = 2, lean = 1),
    female = if_else(sex == "female", true = 1, false = 0),
    african = if_else(nationality == "AFR", true = 1, false = 0)
  )
psq <- tax_agg(psq, "Genus")

# randomly select 30 taxa from the 50 most abundant taxa
set.seed(123)
taxa <- sample(microbiome::top_taxa(ps_get(psq))[1:50], size = 30)

column_tax_anno <- tax_anno(undetected = 50, which = "column")

test_that("default taxa_side ('right') is not compatible with taxa column annotations", {
  expect_error(
    object = cor_heatmap(psq, taxa, anno_tax = column_tax_anno),
    regexp = "This is not compatible with the taxa_side argument you specified"
  )
})

test_that("cor_heatmap doesn't change: ", {
  local_edition(3)
  # make simple correlation heatmap with all numeric-like variables
  p <- cor_heatmap(
    data = psq, taxa = taxa, anno_tax = tax_anno(undetected = 50)
  )
  expect_snapshot_csv(
    name = "cor_heatmap_dietswap",
    object = p@matrix
  )
  expect_snapshot(p@matrix_param[names(p@matrix_param) != "cell_fun"])
  # expect_equal(
  #   ignore_function_env = TRUE,
  #   object = p@matrix_param$cell_fun,
  #   expected = function(j, i, x, y, width, height, fill) {
  #     val <- numbers_mat[i, j]
  #     if (!is.na(val)) grid::grid.text(label = sprintf(numbers[["fmt"]], val), x = x, y = y, gp = numbers[["gp"]])
  #   }
  # )
  expect_snapshot(p@matrix_color_mapping)
  expect_snapshot(p@right_annotation@anno_list)
  expect_snapshot(str(p@column_dend_param$obj))
  expect_snapshot(str(p@row_dend_param$obj))
  expect_snapshot_csv(name = "cor_row_order", object = p@row_order)
  expect_snapshot_csv(name = "cor_col_order", object = p@column_order)
})

test_that("comp_heatmap doesn't change: ", {
  local_edition(3)
  # make simple correlation heatmap with all numeric-like variables
  p <- comp_heatmap(
    data = psq, taxa = taxa, anno_tax = tax_anno(undetected = 50)
  )

  expect_snapshot_csv(
    name = "comp_heatmap_dietswap",
    object = p@matrix
  )
  expect_snapshot(p@matrix_param)
  expect_snapshot(p@matrix_color_mapping)
  expect_snapshot(p@right_annotation@anno_list)
  expect_snapshot(str(p@column_dend_param$obj))
  expect_snapshot(str(p@row_dend_param$obj))
  expect_snapshot_csv(name = "comp_row_order", object = p@row_order)
  expect_snapshot_csv(name = "comp_col_order", object = p@column_order)
})
