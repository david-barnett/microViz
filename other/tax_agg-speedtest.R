library(phyloseq)
library(microbiome)
data("dietswap")

agg_level_test <- c("Phylum", "Family", "Genus")

for (i in 1:100){
  for (level in agg_level_test) {
    biome <- microbiome::aggregate_taxa(x = dietswap, level = level)
    viz <- ps_get(tax_agg(ps = dietswap, level))
  }
}
