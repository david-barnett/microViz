# Shiny app to help you use tax_fix

Try this app if you get errors with
[`tax_fix()`](https://david-barnett.github.io/microViz/reference/tax_fix.md)
that are tricky to work past, or suggestions to use
[`tax_fix()`](https://david-barnett.github.io/microViz/reference/tax_fix.md)
that you don't understand.

The app shows you the tax_table of your data (searchable) with unknown
values highlighted.

It allows you to interactively modify minimum allowed length and to
select further values to be defined as unknown.

It will show you the correct
[`tax_fix()`](https://david-barnett.github.io/microViz/reference/tax_fix.md)
code to copy paste into your script to reproduce the interactive
filtering.

## Usage

``` r
tax_fix_interactive(data, app_options = list(launch.browser = TRUE))
```

## Arguments

- data:

  a phyloseq object

- app_options:

  options list passed to shinyApp()

## Value

nothing

## See also

[`tax_fix`](https://david-barnett.github.io/microViz/reference/tax_fix.md)
for the non-interactive function to use in your scripts

## Examples

``` r
library(dplyr)
library(phyloseq)

# create some problem-filled example tax_table data
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

# function takes a phyloseq and shows code for how to fix the tax_table
# tax_fix_interactive(data = ps)
```
