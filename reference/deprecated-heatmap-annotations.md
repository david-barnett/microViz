# DEPRECATED Heatmap annotations helpers

Functions to easily define ComplexHeatmap annotations for taxa and/or
variables

- tax_anno creates list describing taxa annotation (for cor_heatmap or
  comp_heatmap)

- var_anno creates list describing variable annotation (for cor_heatmap)

## Usage

``` r
tax_anno(
  undetected = 0,
  which = NA,
  prev = 1,
  abund = 2,
  size = 30,
  gap = 2,
  rel_sizes = NA,
  args = NULL,
  ...
)

anno_prev(
  data,
  taxa,
  undetected = 0,
  which = "row",
  size = 15,
  bar_width = 0.6,
  gp = grid::gpar(fill = "grey85"),
  ...
)

anno_abund(
  data,
  taxa,
  undetected = 0,
  which = "row",
  size = 15,
  point_size = 0.75,
  box_width = 0.6,
  gp = grid::gpar(fill = "grey85"),
  ...
)

var_anno(
  annos = "var_box",
  funs = "identity",
  names = NA,
  which = "column",
  size = 15 * length(annos),
  gap = 2,
  rel_sizes = NA,
  args = NULL,
  ...
)

old_anno_var_hist(data, vars = NA, which = "column", size = 15, ...)

old_anno_var_box(data, vars = NA, which = "column", size = 15, ...)
```

## Arguments

- undetected:

  value above which taxa are considered present/detected in a sample

- which:

  "row" or "column" annnotation

- prev:

  order in which prevalence annotation shown (number, or NA to not show)

- abund:

  order in which abundance annotation shown (number, or NA to not show)

- size:

  total size (mm) of annotations (width/height depending on which)

- gap:

  gap in mm between annotations

- rel_sizes:

  relative sizes of annotations (NA for equal sizes, or same length as
  annos)

- args:

  extra args passed to each annotation: give as list of lists (one inner
  list per arg, named, e.g. list(prev = list(whatever = whatever))

- ...:

  further named args to be passed on (to list)

- data:

  phyloseq or ps-extra (or a data.frame or matrix for anno_var\_\*
  functions)

- taxa:

  names of taxa to plot

- bar_width:

  relative width of barchart bars

- gp:

  a grid::gpar() object for graphics parameter settings like fill or lwd

- point_size:

  size of outlier points in mm

- box_width:

  relative width of boxplot boxes

- annos:

  name(s) of annotation(s) to show, in order (e.g. 'var_box',
  'var_hist')

- funs:

  function(s) to transform matrix of variable values before plotting
  (length must be 1 or same length as annos)

- names:

  names to use for each annotation in annos

- vars:

  names of variables to plot
