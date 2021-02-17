library(microViz)
library(phyloseq)
data("enterotype")

# simple example #
ps <- enterotype

ord1 <- ps %>%
 dist_calc("bray") %>%
 ord_calc("PCoA")

ord_explore(ord = ord1)
