# Adjust p values in taxatree_stats dataframe

Apply a p value adjustment method from
[`stats::p.adjust.methods`](https://rdrr.io/r/stats/p.adjust.html), such
as false-discovery rate adjustment with "BH", or more conservative
family-wise error rate controlling methods such as "holm" or
"bonferroni".

## Usage

``` r
taxatree_stats_p_adjust(
  data,
  method,
  grouping = "rank",
  p = "p.value",
  new_var = NULL
)
```

## Arguments

- data:

  psExtra with taxatree_stats dataframe, or just the dataframe

- method:

  any method from
  [`stats::p.adjust.methods`](https://rdrr.io/r/stats/p.adjust.html)

- grouping:

  defines grouping of p-values into families for adjustment, see
  details.

- p:

  name of variable containing p values for adjustment

- new_var:

  name of new variable created for adjusted p values (automatically
  inferred by default)

## Value

psExtra with dataframe of statistics, or just the data.frame

## Details

Define how to group the p values for adjustment with the `grouping`
argument. The default is to adjust the p values in groups at each
taxonomic rank, but you could also adjust per "taxon" or per "term". Or
even group by a combination of rank and term with c("rank", "term"). You
should specify the name of the new variable containing the adjusted p
values in the new_var argument. If left as NULL the new variable name
will be created by pasting together p.adj, the method, and the grouping
variable(s) separated by ".".

## See also

[`taxatree_models2stats`](https://david-barnett.github.io/microViz/reference/models2stats.md)

[`taxatree_models`](https://david-barnett.github.io/microViz/reference/taxatree_models.md)

`stats::`[`p.adjust`](https://rdrr.io/r/stats/p.adjust.html)

## Examples

``` r
# This example is an abbreviated excerpt from article on taxon modelling on
# the microViz documentation website

library(corncob)
library(dplyr)
data("ibd", package = "microViz")

# We'll keep only the Ulcerative Colitis and Healthy Control samples, to
# simplify the analyses for this example. We'll also remove the Species
# rank information, as most OTUs in this dataset are not assigned to a
# species. We'll also use `tax_fix` to fill any gaps where the Genus is
# unknown, with the family name or whatever higher rank classification is
# known.

phylo <- ibd %>%
  ps_filter(DiseaseState %in% c("UC", "nonIBD")) %>%
  tax_mutate(Species = NULL) %>%
  tax_fix()

# Let's make some sample data variables that are easier to use and compare
# in the statistical modelling ahead. We will convert dichotomous
# categorical variables into similar binary variables (values: 1 for true,
# or 0 for false). We will also scale and center the numeric variable for
# age.

phylo <- phylo %>%
  ps_mutate(
    UC = ifelse(DiseaseState == "UC", yes = 1, no = 0),
    female = ifelse(gender == "female", yes = 1, no = 0),
    antibiotics = ifelse(abx == "abx", yes = 1, no = 0),
    steroids = ifelse(steroids == "steroids", yes = 1, no = 0),
    age_scaled = scale(age, center = TRUE, scale = TRUE)
  )

bb_models <- phylo %>%
  tax_fix() %>%
  tax_prepend_ranks() %>%
  tax_filter(min_prevalence = 0.3) %>%
  taxatree_models(
    type = corncob::bbdml,
    ranks = c("Phylum", "Class", "Order"),
    variables = c("UC", "female", "antibiotics", "steroids", "age_scaled")
  )
#> Proportional min_prevalence given: 0.3 --> min 21/67 samples.
#> 2026-02-17 09:18:24.834646 - modelling at rank: Phylum
#> 2026-02-17 09:18:24.948831 - modelling at rank: Class
#> 2026-02-17 09:18:25.112386 - modelling at rank: Order

bb_stats <- bb_models %>%
  taxatree_models2stats(param = "mu") %>%
  taxatree_stats_p_adjust(method = "BH", grouping = "rank")

bb_stats
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 105 taxa and 67 samples ]
#> sample_data() Sample Data:       [ 67 samples by 19 sample variables ]
#> tax_table()   Taxonomy Table:    [ 105 taxa by 6 taxonomic ranks ]
#> 
#> 
#> taxatree_stats dataframe:
#> 19 taxa at 3 ranks: Phylum, Class, Order 
#> 5 terms: UC, female, antibiotics, steroids, age_scaled

bb_stats %>% taxatree_stats_get()
#> # A tibble: 95 × 10
#> # Groups:   rank [3]
#>    term     taxon rank  formula parameter estimate std.error t.statistic p.value
#>    <fct>    <chr> <fct> <chr>   <chr>        <dbl>     <dbl>       <dbl>   <dbl>
#>  1 UC       P: F… Phyl… `P: Fi… mu         -0.375      0.434     -0.864    0.392
#>  2 female   P: F… Phyl… `P: Fi… mu          0.0227     0.309      0.0734   0.942
#>  3 antibio… P: F… Phyl… `P: Fi… mu         -0.781      0.478     -1.63     0.108
#>  4 steroids P: F… Phyl… `P: Fi… mu         -0.162      0.426     -0.380    0.705
#>  5 age_sca… P: F… Phyl… `P: Fi… mu          0.294      0.188      1.56     0.124
#>  6 UC       P: B… Phyl… `P: Ba… mu         -0.814      0.558     -1.46     0.150
#>  7 female   P: B… Phyl… `P: Ba… mu         -0.559      0.349     -1.60     0.115
#>  8 antibio… P: B… Phyl… `P: Ba… mu         -0.116      0.706     -0.164    0.871
#>  9 steroids P: B… Phyl… `P: Ba… mu         -0.130      0.656     -0.197    0.844
#> 10 age_sca… P: B… Phyl… `P: Ba… mu         -0.0967     0.264     -0.366    0.716
#> # ℹ 85 more rows
#> # ℹ 1 more variable: p.adj.BH.rank <dbl>

# you can also directly modify the dataframe,
# and choose a different variable name
bb_stats %>%
  taxatree_stats_get() %>%
  taxatree_stats_p_adjust(
    method = "holm", grouping = "taxon", new_var = "p_adj_holm"
  )
#> # A tibble: 95 × 11
#> # Groups:   taxon [19]
#>    term     taxon rank  formula parameter estimate std.error t.statistic p.value
#>    <fct>    <chr> <fct> <chr>   <chr>        <dbl>     <dbl>       <dbl>   <dbl>
#>  1 UC       P: F… Phyl… `P: Fi… mu         -0.375      0.434     -0.864    0.392
#>  2 female   P: F… Phyl… `P: Fi… mu          0.0227     0.309      0.0734   0.942
#>  3 antibio… P: F… Phyl… `P: Fi… mu         -0.781      0.478     -1.63     0.108
#>  4 steroids P: F… Phyl… `P: Fi… mu         -0.162      0.426     -0.380    0.705
#>  5 age_sca… P: F… Phyl… `P: Fi… mu          0.294      0.188      1.56     0.124
#>  6 UC       P: B… Phyl… `P: Ba… mu         -0.814      0.558     -1.46     0.150
#>  7 female   P: B… Phyl… `P: Ba… mu         -0.559      0.349     -1.60     0.115
#>  8 antibio… P: B… Phyl… `P: Ba… mu         -0.116      0.706     -0.164    0.871
#>  9 steroids P: B… Phyl… `P: Ba… mu         -0.130      0.656     -0.197    0.844
#> 10 age_sca… P: B… Phyl… `P: Ba… mu         -0.0967     0.264     -0.366    0.716
#> # ℹ 85 more rows
#> # ℹ 2 more variables: p.adj.BH.rank <dbl>, p_adj_holm <dbl>

# see all available adjustment methods
stats::p.adjust.methods
#> [1] "holm"       "hochberg"   "hommel"     "bonferroni" "BH"        
#> [6] "BY"         "fdr"        "none"      
```
