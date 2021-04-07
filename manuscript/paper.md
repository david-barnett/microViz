---
title: 'microViz: an R package for microbiome data visualization and statistics'
authors:
- name: David J.M. Barnett
  orcid: 0000-0003-1961-7206
  affiliation: "1, 2"
- name: Ilja C.W. Arts
  orcid: 0000-0001-6462-6692
  affiliation: 1
- name: John Penders
  orcid: 0000-0001-9146-5919
  affiliation: "2, 3, 4"
date: "7th April 2021"
affiliations:
- name: Maastricht Centre for Systems Biology (MaCSBio)
  index: 1
- name: Maastricht University Medical Centre+, Department of Medical Microbiology
  index: 2
- name: Public Health Research Institute (CAPHRI)
  index: 3
- name: School of Nutrition and Translational Research in Metabolism (NUTRIM)
  index: 4
bibliography: microViz.bib
tags:
- R
- microbiome
- visualization
---

# Summary 

microViz is an R package for the statistical analysis and visualization of microbiota data. This package extends the functionality of popular microbial ecosystem data analysis R packages, including phyloseq[@mcmurdie_phyloseq_2013], vegan[@oksanen_vegan_2020] and microbiome[@lahti_microbiome_2012]. 
microViz provides a selection of powerful additions to the toolbox of researchers already familiar with phyloseq and microbiome, as well as assisting researchers with less R programming experience to independently explore and analyse their data and to generate publication-ready figures.

The tools offered by microViz include:

- A Shiny app for interactive exploration of microbiota data within R, pairing ordination plots with abundance bar charts [@shiny]
- Easy to use functions for generating publication-ready ordination plots with ggplot2 [@wickham_ggplot2_2016], accommodating constrained and partial ordination, bi-plots and tri-plots, and automatic captioning designed to promote methodological transparency and reproducibility
- A novel visualization approach pairing ordination plots with circular bar charts (iris plots) for comprehensive, intuitive and compact visualization of the similarity and composition of hundreds of microbial ecosystems
- Correlation and composition heatmaps for microbiome data annotated with plots showing each taxon's prevalence and/or abundance
- A compact cladogram visualization approach for intuitive comparison of numerous microbe-metadata associations derived from (multivariable) statistical models (taxonomic association trees)

# Statement of need

Modern microbiome research typically involves the use of next-generation-sequencing methods to profile the relative abundance of hundreds of microbial taxa across tens, hundreds or thousands of samples.
Alongside increasing sample sizes, the amount of relevant metadata collected is growing, particularly in human cohort studies. 
These trends all increase the size and complexity of the resulting dataset, which makes its exploration, statistical analysis, and presentation increasingly challenging.

Ordination methods like Principle Coordinates/Components Analyses (PCoA/PCA) are a staple method in microbiome research. 
The vegan R package implements the majority of distance and ordination calculation methods, and the phyloseq and microbiome R packages provide easier interfaces for using these methods to produce ordination plots. 
microViz provides an extended set of distance measures (including e.g. Generalized UniFrac and Aitchison), and makes the use of further ordination calculation and visualization methods more accessible, such as (distance-based) redundancy analysis (RDA) and canonical correspondence analysis (CCA), as well as bi-plots and tri-plots generated with the ggplot2 R package. 
These figures are captioned automatically, by default. The captions are intended to promote better reporting of ordination methods in published research, where too often insufficient information is given to reproduce the ordination plot. 
To provide the automated captioning, microViz implements a simple S3 list class, ps_extra, for provenance tracking, by storing distance matrices and ordination objects alongside the phyloseq object they were created from, as well as relevant taxonomic aggregation and transformation information. 
Additionally, microViz provides an interactive exploration function that allows the user to interact with any ordination plot without leaving R, via a Shiny app[@shiny]. 
The user can click and drag to select samples on the ordination plot and directly examine their taxonomic compositions on a customizable stacked bar chart with a clear colour scheme. 
Alternatively, for a comprehensive and intuitive static presentation of both sample variation patterns and underlying microbial composition, microViz provides an easy approach to pair ordination plots with attractive circular bar charts (iris plots) by ordering the bar chart in accordance with the rotational position of samples around the origin point on the ordination plot, e.g. \autoref{fig:one}. 
Bar charts do have limitations when visualizing highly diverse samples, such as the adult gut microbiome, at a detailed taxonomic level. 
This is why microViz also offers an enhanced heatmap visualization approach, pairing an ordered heatmap of (transformed and/or scaled) microbial abundances with compact plots showing each taxon’s overall prevalence and/or abundance distribution. 
The same annotation can easily be added to metadata-to-microbe correlation heatmaps.

microViz provides a flexible wrapper around methods for the statistical modelling of microbial abundances, including e.g. beta-binomial regression models from the corncob R package [@martin_modeling_2020], and compositional linear regression. 
To visualize metadata-to-microbiome associations derived from more complicated statistical models, microViz offers a visualization approach that combines multiple annotated cladograms to comprehensively and compactly display patterns of microbial associations with multiple covariates from the same multivariable statistical model. 
These “taxonomic association trees” facilitate direct comparison of the direction, strength and significance of microbial associations between covariates and across multiple taxonomic ranks. This visualisation also provides an intuitive reminder of the balancing act inherent in compositional data analysis: if one clade/branch goes up, others must go down. 
Other packages in R, such as ggtree [@yu_ggtree_2017] or metacoder [@foster_metacoder_2017], can be used to make annotated cladograms similar to the microViz taxonomic association tree visualizations, but the microViz style has a few advantages for the purpose of reporting multivariable model results: Firstly, microViz cladogram generating functions are directly paired with functions to compute the statistical model results for all taxa in a phyloseq. Secondly, the tree layouts are more compact, by default, for displaying multiple trees for easy comparison. 

Finally, beyond the main visualization functionality, microViz provides a suite of tools for working easily with phyloseq objects including wrapper functions that bring approaches from the popular dplyr package to phyloseq, to help the researcher easily filter, select, join, mutate and arrange phyloseq sample data. All microViz functions are designed to work with magrittr's pipe operator (%>%), to chain successive functions together and improve code readability [@magrittr]. Lastly, for user convenience, microViz documentation and tutorials are hosted online via a pkgdown [@pkgdown] website on Github Pages, with extensive examples of code and output generated with example datasets. 

![Simple example of microViz figure pairing an ordination plot of microbial samples (left) with an “iris plot” (right): a circular stacked barchart showing the microbial compositions. This figure is created with a subset of the “dietswap” dataset available within the microbiome R package. The ordination plot is a PCA bi-plot created using centered-log-ratio transformed species-like HITChip microbial features. The dark grey filled points on both plots indicate samples where the participant’s nationality is AFR. AFR = African; AAM = African American. \label{fig:one}](fig1.jpg)

# References

