library(microViz)
library(phyloseq)
data("dietswap", package = "microbiome")

# simple example #
ps <- dietswap %>%
  tax_fix()

ord1 <- ps %>%
  dist_calc("bray") %>%
  ord_calc("PCoA")

ord_explore(data = ord1)
