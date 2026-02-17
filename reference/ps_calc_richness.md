# Calculate richness estimate and add to phyloseq sample data

Wrapper around microbiome::richness() function. Takes and returns a
phyloseq object. Calculates a richness estimate at a given taxonomic
rank. Returns phyloseq unaggregated, with an additional variable.
Variable name is by default created by pasting the index and rank.

## Usage

``` r
ps_calc_richness(
  ps,
  rank,
  index = "observed",
  detection = 0,
  varname = paste0(index, "_", rank)
)
```

## Arguments

- ps:

  phyloseq

- rank:

  taxonomic rank name, or "unique"

- index:

  "observed" or "chao1" - name of richness estimate from
  microbiome::richness()

- detection:

  Detection threshold. Used for the "observed" index.

- varname:

  name of the variable to be added to phyloseq sample data

## Value

phyloseq

## Details

Don't filter taxa before calculating richness.

These richness indices are estimates. For a discussion of the
uncertainty and bias of these estimates see e.g. work by Amy Willis
https://doi.org/10.3389/fmicb.2019.02407

## See also

[`ps_calc_diversity`](https://david-barnett.github.io/microViz/reference/ps_calc_diversity.md)

`microbiome::`[`richness`](https://rdrr.io/pkg/microbiome/man/richness.html)

## Examples

``` r
data(ibd, package = "microViz")
ibd %>%
  ps_filter(abx == "abx") %>%
  tax_fix() %>%
  ps_calc_richness("Genus", index = "observed") %>%
  ps_calc_richness("Family", index = "chao1") %>%
  tax_transform(rank = "Genus", transform = "clr") %>%
  ord_calc("PCA") %>%
  ord_plot(
    colour = "observed_Genus", size = "chao1_Family"
  ) +
  ggplot2::scale_colour_viridis_c()
#> Warning: `transformation` argument deprecated, use `trans` instead.

```
