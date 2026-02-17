# Join a dataframe to phyloseq sample data

You can use most types of join from the dplyr::\*\_join function family,
including e.g. "inner", "left", "semi", "anti" (see details below).
Defaults to type = "left" which calls left_join(), this supports x as a
phyloseq and y as a dataframe. Most of the time you'll want "left" (adds
variables with no sample filtering), or "inner" (adds variables and
filters samples). This function simply:

1.  extracts the sample_data from the phyloseq as a dataframe

2.  performs the chosen type of join (with the given arguments)

3.  filters the phyloseq if type = inner, semi or anti

4.  reattaches the modified sample_data to the phyloseq and returns the
    phyloseq

## Usage

``` r
ps_join(
  x,
  y,
  by = NULL,
  match_sample_names = NULL,
  keep_sample_name_col = TRUE,
  sample_name_natural_join = FALSE,
  type = "left",
  .keep_all_taxa = FALSE
)
```

## Arguments

- x:

  phyloseq (or dataframe)

- y:

  dataframe (or phyloseq for e.g. type = "right")

- by:

  A character vector of variables to join by (col must be present in
  both x and y or paired via a named vector like c("xname" = "yname",
  etc.))

- match_sample_names:

  match against the phyloseq sample_names by naming a variable in the
  additional dataframe (this is in addition to any variables named in
  by)

- keep_sample_name_col:

  should the column named in match_sample_names be kept in the returned
  phyloseq's sample_data? (only relevant if match_sample_names is not
  NULL)

- sample_name_natural_join:

  if TRUE, use sample_name AND all shared colnames to match rows (only
  relevant if match_sample_names is not NULL, this arg takes precedence
  over anything also entered in `by` arg)

- type:

  name of type of join e.g. "left", "right", "inner", "semi" (see dplyr
  help pages)

- .keep_all_taxa:

  if FALSE (the default), remove taxa which are no longer present in the
  dataset after filtering

## Value

phyloseq with modified sample_data (and possibly filtered)

## Details

**Mutating joins**, which will add columns from a dataframe to phyloseq
sample data, matching rows based on the key columns named in the `by`
argument:

- "inner": includes all rows in present in both x and y.

- "left": includes all rows in x. (so x must be the phyloseq)

- "right": includes all rows in y. (so y must be the phyloseq)

- "full": includes all rows present in x or y. (will likely NOT work, as
  additional rows cannot be added to sample_data!)

If a row in x matches multiple rows in y (based on variables named in
the `by` argument), all the rows in y will be added once for each
matching row in x. This will cause this function to fail, as additional
rows cannot be added to the phyloseq sample_data!

**Filtering joins** filter rows from x based on the presence or absence
of matches in y:

- "semi": return all rows from x with a match in y.

- "anti": return all rows from x without a match in y.

## See also

[`ps_mutate`](https://david-barnett.github.io/microViz/reference/ps_mutate.md)
for computing new variables from existing sample data

[`ps_select`](https://david-barnett.github.io/microViz/reference/ps_select.md)
for selecting only some sample_data variables

<https://www.garrickadenbuie.com/project/tidyexplain/> for an animated
introduction to joining dataframes

## Examples

``` r
library(phyloseq)
data("enterotype", package = "phyloseq")

x <- enterotype
y <- data.frame(
  ID_var = sample_names(enterotype)[c(1:50, 101:150)],
  SeqTech = sample_data(enterotype)[c(1:50, 101:150), "SeqTech"],
  arbitrary_info = rep(c("A", "B"), 50)
)

# simply match the new data to samples that exist in x, as default is a left_join
# where some sample names of x are expected to match variable ID_var in dataframe y
out1A <- ps_join(x = x, y = y, match_sample_names = "ID_var")
out1A
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 553 taxa and 280 samples ]
#> sample_data() Sample Data:       [ 280 samples by 12 sample variables ]
#> tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]
sample_data(out1A)[1:6, ]
#>              ID_var Enterotype Sample_ID SeqTech.x  SampleID     Project
#> AM.AD.1     AM.AD.1       <NA>   AM.AD.1    Sanger   AM.AD.1      gill06
#> AM.AD.2     AM.AD.2       <NA>   AM.AD.2    Sanger   AM.AD.2      gill06
#> AM.F10.T1 AM.F10.T1       <NA> AM.F10.T1    Sanger AM.F10.T1 turnbaugh09
#> AM.F10.T2 AM.F10.T2          3 AM.F10.T2    Sanger AM.F10.T2 turnbaugh09
#> DA.AD.1     DA.AD.1          2   DA.AD.1    Sanger   DA.AD.1     MetaHIT
#> DA.AD.1T   DA.AD.1T       <NA>  DA.AD.1T    Sanger      <NA>        <NA>
#>           Nationality Gender Age ClinicalStatus SeqTech.y arbitrary_info
#> AM.AD.1      american      F  28        healthy    Sanger              A
#> AM.AD.2      american      M  37        healthy    Sanger              B
#> AM.F10.T1    american      F  NA          obese    Sanger              A
#> AM.F10.T2    american      F  NA          obese    Sanger              B
#> DA.AD.1        danish      F  59        healthy    Sanger              A
#> DA.AD.1T         <NA>   <NA>  NA           <NA>    Sanger              B


# use sample_name and all shared variables to join
# (a natural join is not a type of join per se,
# but it indicates that all shared variables should be used for matching)
out1B <- ps_join(
  x = x, y = y, match_sample_names = "ID_var",
  sample_name_natural_join = TRUE, keep_sample_name_col = FALSE
)
out1B
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 553 taxa and 280 samples ]
#> sample_data() Sample Data:       [ 280 samples by 10 sample variables ]
#> tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]
sample_data(out1B)[1:6, ]
#>           Enterotype Sample_ID SeqTech  SampleID     Project Nationality Gender
#> AM.AD.1         <NA>   AM.AD.1  Sanger   AM.AD.1      gill06    american      F
#> AM.AD.2         <NA>   AM.AD.2  Sanger   AM.AD.2      gill06    american      M
#> AM.F10.T1       <NA> AM.F10.T1  Sanger AM.F10.T1 turnbaugh09    american      F
#> AM.F10.T2          3 AM.F10.T2  Sanger AM.F10.T2 turnbaugh09    american      F
#> DA.AD.1            2   DA.AD.1  Sanger   DA.AD.1     MetaHIT      danish      F
#> DA.AD.1T        <NA>  DA.AD.1T  Sanger      <NA>        <NA>        <NA>   <NA>
#>           Age ClinicalStatus arbitrary_info
#> AM.AD.1    28        healthy              A
#> AM.AD.2    37        healthy              B
#> AM.F10.T1  NA          obese              A
#> AM.F10.T2  NA          obese              B
#> DA.AD.1    59        healthy              A
#> DA.AD.1T   NA           <NA>              B

# if you only want to keep phyloseq samples that exist in the new data, try an inner join
# this will add the new variables AND filter the phyloseq
# this example matches sample names to ID_var and by matching the shared SeqTech variable
out1C <- ps_join(x = x, y = y, type = "inner", by = "SeqTech", match_sample_names = "ID_var")
out1C
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 533 taxa and 100 samples ]
#> sample_data() Sample Data:       [ 100 samples by 11 sample variables ]
#> tax_table()   Taxonomy Table:    [ 533 taxa by 1 taxonomic ranks ]
sample_data(out1C)[1:6, ]
#>              ID_var Enterotype Sample_ID SeqTech  SampleID     Project
#> AM.AD.1     AM.AD.1       <NA>   AM.AD.1  Sanger   AM.AD.1      gill06
#> AM.AD.2     AM.AD.2       <NA>   AM.AD.2  Sanger   AM.AD.2      gill06
#> AM.F10.T1 AM.F10.T1       <NA> AM.F10.T1  Sanger AM.F10.T1 turnbaugh09
#> AM.F10.T2 AM.F10.T2          3 AM.F10.T2  Sanger AM.F10.T2 turnbaugh09
#> DA.AD.1     DA.AD.1          2   DA.AD.1  Sanger   DA.AD.1     MetaHIT
#> DA.AD.1T   DA.AD.1T       <NA>  DA.AD.1T  Sanger      <NA>        <NA>
#>           Nationality Gender Age ClinicalStatus arbitrary_info
#> AM.AD.1      american      F  28        healthy              A
#> AM.AD.2      american      M  37        healthy              B
#> AM.F10.T1    american      F  NA          obese              A
#> AM.F10.T2    american      F  NA          obese              B
#> DA.AD.1        danish      F  59        healthy              A
#> DA.AD.1T         <NA>   <NA>  NA           <NA>              B

# the id variable is named Sample_ID in x and ID_var in y
# semi_join is only a filtering join (doesn't add new variables but just filters samples in x)
out2A <- ps_join(x = x, y = y, by = c("Sample_ID" = "ID_var"), type = "semi")
out2A
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 533 taxa and 100 samples ]
#> sample_data() Sample Data:       [ 100 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 533 taxa by 1 taxonomic ranks ]
sample_data(out2A)[1:6, ]
#>           Enterotype Sample_ID SeqTech  SampleID     Project Nationality Gender
#> AM.AD.1         <NA>   AM.AD.1  Sanger   AM.AD.1      gill06    american      F
#> AM.AD.2         <NA>   AM.AD.2  Sanger   AM.AD.2      gill06    american      M
#> AM.F10.T1       <NA> AM.F10.T1  Sanger AM.F10.T1 turnbaugh09    american      F
#> AM.F10.T2          3 AM.F10.T2  Sanger AM.F10.T2 turnbaugh09    american      F
#> DA.AD.1            2   DA.AD.1  Sanger   DA.AD.1     MetaHIT      danish      F
#> DA.AD.1T        <NA>  DA.AD.1T  Sanger      <NA>        <NA>        <NA>   <NA>
#>           Age ClinicalStatus
#> AM.AD.1    28        healthy
#> AM.AD.2    37        healthy
#> AM.F10.T1  NA          obese
#> AM.F10.T2  NA          obese
#> DA.AD.1    59        healthy
#> DA.AD.1T   NA           <NA>

# anti_join is another type of filtering join
out2B <- ps_join(x = x, y = y, by = c("Sample_ID" = "ID_var"), type = "anti")
out2B
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 549 taxa and 180 samples ]
#> sample_data() Sample Data:       [ 180 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 549 taxa by 1 taxonomic ranks ]
sample_data(out2B)[1:6, ]
#>        Enterotype Sample_ID  SeqTech SampleID Project Nationality Gender Age
#> MH0010          1    MH0010 Illumina     <NA>    <NA>        <NA>   <NA>  NA
#> MH0011          1    MH0011 Illumina     <NA>    <NA>        <NA>   <NA>  NA
#> MH0012          1    MH0012 Illumina     <NA>    <NA>        <NA>   <NA>  NA
#> MH0013          1    MH0013 Illumina     <NA>    <NA>        <NA>   <NA>  NA
#> MH0014          1    MH0014 Illumina     <NA>    <NA>        <NA>   <NA>  NA
#> MH0015          1    MH0015 Illumina     <NA>    <NA>        <NA>   <NA>  NA
#>        ClinicalStatus
#> MH0010           <NA>
#> MH0011           <NA>
#> MH0012           <NA>
#> MH0013           <NA>
#> MH0014           <NA>
#> MH0015           <NA>

# semi and anti joins keep opposite sets of samples
intersect(sample_names(out2A), sample_names(out2B))
#> character(0)

# you can mix and match named and unnamed values in the `by` vector
# inner is like a combination of left join and semi join
out3 <- ps_join(x = x, y = y, by = c("Sample_ID" = "ID_var", "SeqTech"), type = "inner")
out3
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 533 taxa and 100 samples ]
#> sample_data() Sample Data:       [ 100 samples by 10 sample variables ]
#> tax_table()   Taxonomy Table:    [ 533 taxa by 1 taxonomic ranks ]
sample_data(out3)[1:6, ]
#>           Enterotype Sample_ID SeqTech  SampleID     Project Nationality Gender
#> AM.AD.1         <NA>   AM.AD.1  Sanger   AM.AD.1      gill06    american      F
#> AM.AD.2         <NA>   AM.AD.2  Sanger   AM.AD.2      gill06    american      M
#> AM.F10.T1       <NA> AM.F10.T1  Sanger AM.F10.T1 turnbaugh09    american      F
#> AM.F10.T2          3 AM.F10.T2  Sanger AM.F10.T2 turnbaugh09    american      F
#> DA.AD.1            2   DA.AD.1  Sanger   DA.AD.1     MetaHIT      danish      F
#> DA.AD.1T        <NA>  DA.AD.1T  Sanger      <NA>        <NA>        <NA>   <NA>
#>           Age ClinicalStatus arbitrary_info
#> AM.AD.1    28        healthy              A
#> AM.AD.2    37        healthy              B
#> AM.F10.T1  NA          obese              A
#> AM.F10.T2  NA          obese              B
#> DA.AD.1    59        healthy              A
#> DA.AD.1T   NA           <NA>              B
```
