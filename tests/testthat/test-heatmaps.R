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

# cor_heatmap --------------------------------------------------------------
test_that("default taxa_side ('right') is not compatible with taxa column annotations", {
  column_tax_anno <- tax_anno(undetected = 50, which = "column")
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
  # note: cell fun environment name always changes, hence excluded
  expect_snapshot(p@matrix_param[names(p@matrix_param) != "cell_fun"])
  expect_snapshot(p@matrix_color_mapping)
  expect_snapshot(p@right_annotation@anno_list)
  expect_snapshot(str(p@column_dend_param$obj))
  expect_snapshot(str(p@row_dend_param$obj))
  expect_snapshot_csv(name = "cor_row_order", object = p@row_order)
  expect_snapshot_csv(name = "cor_col_order", object = p@column_order)
})

test_that("cor_heatmap with var_anno doesn't change: ", {
  local_edition(3)

  v <- cor_heatmap(
    data = psq, taxa = taxa,
    anno_tax = tax_anno(undetected = 50),
    anno_vars = var_anno(
      annos = c("var_hist", "var_box"),
      funs = list("identity", function(x) log10(x + 1)),
      names = c("x", "log10(x+1)"), rel_sizes = c(1, 2)
    )
  )

  expect_snapshot_csv(
    name = "cor_heatmap_dietswap",
    object = v@matrix
  )
  # note: cell fun environment name always changes, hence excluded
  expect_snapshot(v@matrix_param[names(v@matrix_param) != "cell_fun"])
  expect_snapshot(v@matrix_color_mapping)
  expect_snapshot(v@right_annotation@anno_list)
  expect_snapshot(v@top_annotation@anno_list)
  expect_snapshot(str(v@column_dend_param$obj))
  expect_snapshot(str(v@row_dend_param$obj))
  expect_snapshot_csv(name = "cor_row_order_v", object = v@row_order)
  expect_snapshot_csv(name = "cor_col_order_v", object = v@column_order)
})



# comp_heatmap ------------------------------------------------------------

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
