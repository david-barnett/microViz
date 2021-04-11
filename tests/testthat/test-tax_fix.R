library(dplyr)
library(phyloseq)
data(dietswap, package = "microbiome")
ps <- dietswap

# create unknowns to test filling
tt <- tax_table(ps)
ntax <- ntaxa(ps)
set.seed(123)
g <- sample(1:ntax, 30)
f <- sample(g, 10)
p <- sample(f, 3)
tt[g, 3] <- "g__"
tt[f, 2] <- "f__"
tt[p, 1] <- "p__"
tt[sample(1:ntax, 10), 3] <- "unknown"
# create a row with only NAs
tt[1, ] <- NA

tax_table(ps) <- tax_table(tt)

data("enterotype", package = "phyloseq")
data("soilrep", package = "phyloseq")
data("atlas1006", package = "microbiome")

datasets <- list(
  enterotype = enterotype, modified_dietswap = ps,
  ibd_phylo = corncob::ibd_phylo
)

for (pseq in names(datasets)) {
  test_that(paste("dataset stays same:", pseq), {
    local_edition(3)
    expect_snapshot_csv(
      name = pseq,
      object = head(tax_table(datasets[[pseq]]), 500)
    )
  })

  fixed <- suppressWarnings(tax_fix(datasets[[pseq]]))

  test_that(paste("tax_fix dataset stays same:", pseq), {
    local_edition(3)
    expect_snapshot_csv(
      name = paste0("fixed_", pseq),
      object = head(tax_table(fixed), 500)
    )
  })

  for (r in rank_names(datasets[[pseq]])) {
    test_that(
      desc = paste("tax_fix defaults allow agg:", pseq, r),
      code = {
        local_edition(3)
        expect_snapshot(
          tax_agg(ps = fixed, rank = r)
        )
      }
    )
  }
}

# tax_common_unknowns ==========================
test_that("tax_common_unknowns doesn't change", {
  local_edition(3)
  for (i in 0:5){
    expect_snapshot_csv(
      name = paste0("tax_common_unknowns-", i),
      object = tax_common_unknowns(min_length = i)
    )
  }
})

