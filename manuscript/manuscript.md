Summary
=======

microViz is an R package for the statistical analysis and visualization
of microbiota data. This package extends the functionality of popular
microbial ecosystem data analysis R packages, including
phyloseq(McMurdie and Holmes 2013), vegan(Oksanen et al. 2020) and
microbiome(Lahti and Shetty 2012). microViz provides a selection of
powerful additions to the toolbox of researchers already familiar with
phyloseq and microbiome, as well as assisting researchers with less R
programming experience to independently explore and analyse their data
and to generate publication-ready figures.

The tools offered by microViz include:

-   A Shiny app for interactive exploration of microbiota data within R,
    pairing ordination plots with abundance bar charts (Chang et
    al. 2021)
-   Easy to use functions for generating publication-ready ordination
    plots with ggplot2 (Wickham 2016), accommodating constrained and
    partial ordination, bi-plots and tri-plots, and automatic captioning
    designed to promote methodological transparency and reproducibility
-   A novel visualization approach pairing ordination plots with
    circular bar charts (iris plots) for comprehensive, intuitive and
    compact visualization of the similarity and composition of hundreds
    of microbial ecosystems
-   Correlation and composition heatmaps for microbiome data annotated
    with plots showing each taxon’s prevalence and/or abundance
-   A compact cladogram visualization approach for intuitive comparison
    of numerous microbe-metadata associations derived from
    (multivariable) statistical models (taxonomic association trees)

Statement of need
=================

Modern microbiome research typically involves the use of
next-generation-sequencing methods to profile the relative abundance of
hundreds of microbial taxa across tens, hundreds or thousands of
samples. Alongside increasing sample sizes, the amount of relevant
metadata collected is growing, particularly in human cohort studies.
These trends all increase the size and complexity of the resulting
dataset, which makes its exploration, statistical analysis, and
presentation increasingly challenging.

Ordination methods like Principle Coordinates/Components Analyses
(PCoA/PCA) are a staple method in microbiome research. The vegan R
package implements the majority of distance and ordination calculation
methods, and the phyloseq and microbiome R packages provide easier
interfaces for using these methods to produce ordination plots. microViz
provides an extended set of distance measures (including
e.g. Generalized UniFrac and Aitchison), and makes the use of further
ordination calculation and visualization methods more accessible, such
as (distance-based) redundancy analysis (RDA) and canonical
correspondence analysis (CCA), as well as bi-plots and tri-plots
generated with the ggplot2 R package. These figures are captioned
automatically, by default. The captions are intended to promote better
reporting of ordination methods in published research, where too often
insufficient information is given to reproduce the ordination plot. To
provide the automated captioning, microViz implements a simple S3 list
class, ps\_extra, for provenance tracking, by storing distance matrices
and ordination objects alongside the phyloseq object they were created
from, as well as relevant taxonomic aggregation and transformation
information. Additionally, microViz provides an interactive exploration
function that allows the user to interact with any ordination plot
without leaving R, via a Shiny app(Chang et al. 2021). The user can
click and drag to select samples on the ordination plot and directly
examine their taxonomic compositions on a customizable stacked bar chart
with a clear colour scheme. Alternatively, for a comprehensive and
intuitive static presentation of both sample variation patterns and
underlying microbial composition, microViz provides an easy approach to
pair ordination plots with attractive circular bar charts (iris plots)
by ordering the bar chart in accordance with the rotational position of
samples around the origin point on the ordination plot, e.g. . Bar
charts do have limitations when visualizing highly diverse samples, such
as the adult gut microbiome, at a detailed taxonomic level. This is why
microViz also offers an enhanced heatmap visualization approach, pairing
an ordered heatmap of (transformed and/or scaled) microbial abundances
with compact plots showing each taxon’s overall prevalence and/or
abundance distribution. The same annotation can easily be added to
metadata-to-microbe correlation heatmaps.

microViz provides a flexible wrapper around methods for the statistical
modelling of microbial abundances, including e.g. beta-binomial
regression models from the corncob R package (Martin, Witten, and Willis
2020), and compositional linear regression. To visualize
metadata-to-microbiome associations derived from more complicated
statistical models, microViz offers a visualization approach that
combines multiple annotated cladograms to comprehensively and compactly
display patterns of microbial associations with multiple covariates from
the same multivariable statistical model. These “taxonomic association
trees” facilitate direct comparison of the direction, strength and
significance of microbial associations between covariates and across
multiple taxonomic ranks. This visualisation also provides an intuitive
reminder of the balancing act inherent in compositional data analysis:
if one clade/branch goes up, others must go down. Other packages in R,
such as ggtree (Yu et al. 2017) or metacoder (Foster, Sharpton, and
Grünwald 2017), can be used to make annotated cladograms similar to the
microViz taxonomic association tree visualizations, but the microViz
style has a few advantages for the purpose of reporting multivariable
model results: Firstly, microViz cladogram generating functions are
directly paired with functions to compute the statistical model results
for all taxa in a phyloseq. Secondly, the tree layouts are more compact,
by default, for displaying multiple trees for easy comparison.

Finally, beyond the main visualization functionality, microViz provides
a suite of tools for working easily with phyloseq objects including
wrapper functions that bring approaches from the popular dplyr package
to phyloseq, to help the researcher easily filter, select, join, mutate
and arrange phyloseq sample data. All microViz functions are designed to
work with magrittr’s pipe operator (%&gt;%), to chain successive
functions together and improve code readability (Bache and Wickham
2020). Lastly, for user convenience, microViz documentation and
tutorials are hosted online via a pkgdown (Wickham and Hesselberth 2020)
website on Github Pages, with extensive examples of code and output
generated with example datasets.

![Figure 1 Example of microViz figure pairing an ordination plot of
microbial samples (left) with an “iris plot” (right): a circular stacked
barchart showing the microbial compositions. This figure is created with
a subset of the “dietswap” dataset available within the microbiome R
package. The ordination plot is a PCA bi-plot created using
centered-log-ratio transformed species-like microbial features. The dark
grey filled points on both plots indicate samples where the
participant’s nationality is AFR. AFR = African; AAM = African American.
](fig1.svg)

References
==========

Bache, Stefan Milton, and Hadley Wickham. 2020. *Magrittr: A
Forward-Pipe Operator for R*.
<https://CRAN.R-project.org/package=magrittr>.

Chang, Winston, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke,
Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert, and Barbara
Borges. 2021. *Shiny: Web Application Framework for R*.
<https://CRAN.R-project.org/package=shiny>.

Foster, Zachary, Thomas Sharpton, and Niklaus Grünwald. 2017.
“Metacoder: An R Package for Visualization and Manipulation of Community
Taxonomic Diversity Data.” *PLOS Computational Biology* 13 (2): 1–15.
<https://doi.org/10.1371/journal.pcbi.1005404>.

Lahti, Leo, and Sudarshan Shetty. 2012. *Microbiome R Package*.

Martin, Bryan D., Daniela Witten, and Amy D. Willis. 2020. “Modeling
Microbial Abundances and Dysbiosis with Beta-Binomial Regression.” *The
Annals of Applied Statistics* 14 (1): 94–115.
<https://doi.org/10.1214/19-AOAS1283>.

McMurdie, Paul J., and Susan Holmes. 2013. “Phyloseq: An R Package for
Reproducible Interactive Analysis and Graphics of Microbiome Census
Data.” *PLoS ONE* 8 (4): e61217.
<http://dx.plos.org/10.1371/journal.pone.0061217>.

Oksanen, Jari, F. Guillaume Blanchet, Michael Friendly, Roeland Kindt,
Pierre Legendre, Dan McGlinn, Peter R. Minchin, et al. 2020. *Vegan:
Community Ecology Package*. <https://CRAN.R-project.org/package=vegan>.

Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. <https://ggplot2.tidyverse.org>.

Wickham, Hadley, and Jay Hesselberth. 2020. *Pkgdown: Make Static Html
Documentation for a Package*.
<https://CRAN.R-project.org/package=pkgdown>.

Yu, Guangchuang, David K. Smith, Huachen Zhu, Yi Guan, and Tommy
Tsan-Yuk Lam. 2017. “Ggtree: An R Package for Visualization and
Annotation of Phylogenetic Trees with Their Covariates and Other
Associated Data.” *Methods in Ecology and Evolution* 8: 28–36.
<https://doi.org/https://doi.org/10.1111/2041-210X.12628>.
