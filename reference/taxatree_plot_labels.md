# Add labels to taxatree plots/key

Finer control over label drawing for `taxatree_plotkey` (with
.draw_label = FALSE), and label drawing for `taxatree_plots` output too.

## Usage

``` r
taxatree_plot_labels(
  p,
  circular = TRUE,
  taxon_renamer = identity,
  fun = ggrepel::geom_text_repel,
  label_var = "label",
  x_nudge = 0.1,
  y_nudge = 0.025,
  rotate = 0,
  fontface = "bold",
  size = 2.5,
  colour = "grey15",
  max.overlaps = Inf,
  min.segment.length = 0,
  segment.size = 0.15,
  segment.color = "grey15",
  point.padding = 0.05,
  box.padding = 0.1,
  seed = NA,
  ...
)
```

## Arguments

- p:

  taxatree_plotkey or taxatree_plots output plot

- circular:

  is the plot layout circular? labels are drawn differently for circular
  trees

- taxon_renamer:

  function that takes taxon names and returns modified names for labels

- fun:

  ggrepel labelling function: geom_text_repel or geom_label_repel

- label_var:

  name of variable in taxatree_stats that indicates which taxa to label

- x_nudge:

  absolute amount by which the initial position of taxon labels is
  nudged (relevant only for circular layouts, use nudge_x for other
  layouts)

- y_nudge:

  absolute amount by which the initial position of taxon labels is
  nudged (relevant only for circular layouts, use nudge_y for other
  layouts)

- rotate:

  angle to rotate labels' outer edges away from horizontal (relevant
  only for circular layouts, use angle for other layouts)

- fontface:

  fontface of label text

- size:

  size of labels

- colour:

  colour of label outlines and text

- max.overlaps:

  max number of overlapping labels tolerated

- min.segment.length:

  min length of label line segment to bother drawing

- segment.size:

  thickness of line segment

- segment.color:

  colour of line segment

- point.padding:

  padding around node points (for label positioning)

- box.padding:

  padding around labels/text (for label positioning)

- seed:

  set this for reproducible label positions

- ...:

  Arguments passed on to
  [`ggrepel::geom_text_repel`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)

  `arrow`

  :   specification for arrow heads, as created by
      [`arrow`](https://rdrr.io/r/grid/arrow.html)

  `force`

  :   Force of repulsion between overlapping text labels. Defaults to 1.

  `force_pull`

  :   Force of attraction between a text label and its corresponding
      data point. Defaults to 1.

  `max.time`

  :   Maximum number of seconds to try to resolve overlaps. Defaults to
      0.5.

  `max.iter`

  :   Maximum number of iterations to try to resolve overlaps. Defaults
      to 10000.

  `xlim,ylim`

  :   Limits for the x and y axes. Text labels will be constrained to
      these limits. By default, text labels are constrained to the
      entire plot area.

  `direction`

  :   "both", "x", or "y" – direction in which to adjust position of
      labels

  `verbose`

  :   If `TRUE`, some diagnostics of the repel algorithm are printed
