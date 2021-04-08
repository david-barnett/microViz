library(microViz)
library(phyloseq)
data("dietswap", package = "microbiome")

# simple example #
ps <- dietswap %>%
  tax_fix_interactive()
