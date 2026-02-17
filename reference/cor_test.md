# Simple wrapper around cor.test for y ~ x style formula input

Intended for use within the function tax_model

## Usage

``` r
cor_test(formula, data, ...)
```

## Arguments

- formula:

  a formula in form y ~ x

- data:

  dataframe

- ...:

  passed to cor.test

## Examples

``` r
data("shao19")
ps <- shao19 %>%
  ps_filter(family_role == "mother") %>%
  tax_filter(min_prevalence = 20) %>%
  tax_agg("family")

cors <- ps %>% tax_model(
  rank = "family", variables = list("age", "number_reads"), type = cor_test
)
#> Warning: 25 / 174 values are NA in age
#> Modelling: Enterobacteriaceae
#> Modelling: Bacteroidaceae
#> Modelling: Oscillospiraceae
#> Modelling: Clostridia class
#> Modelling: Bifidobacteriaceae
#> Modelling: Enterococcaceae
#> Modelling: Coriobacteriaceae
#> Modelling: Rikenellaceae
#> Modelling: Lachnospiraceae
#> Modelling: Firmicutes phylum
#> Modelling: Barnesiellaceae
#> Modelling: Clostridiaceae
#> Modelling: Peptostreptococcaceae
#> Modelling: Tannerellaceae
#> Modelling: Victivallaceae
#> Modelling: Eubacteriaceae
#> Modelling: Streptococcaceae
#> Modelling: Turicibacteraceae
#> Modelling: Eggerthellaceae
#> Modelling: Veillonellaceae
#> Modelling: Odoribacteraceae
#> Modelling: Eubacteriales order
#> Modelling: Desulfovibrionaceae
#> Modelling: Erysipelotrichaceae
#> Modelling: Micrococcaceae
#> Modelling: Pasteurellaceae
#> Modelling: Acidaminococcaceae
#> Modelling: Akkermansiaceae
#> Modelling: Sutterellaceae
#> Modelling: Actinomycetaceae
#> Modelling: Atopobiaceae
#> Modelling: Prevotellaceae
#> Modelling: Lactobacillaceae
#> Modelling: Proteobacteria phylum
#> Modelling: Coprobacillaceae
#> Modelling: Methanobacteriaceae

tax_models_get(cors)
#> $family
#> $family$age
#> $family$age$Enterobacteriaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Enterobacteriaceae and age
#> t = 1.8356, df = 147, p-value = 0.06844
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.01138293  0.30319302
#> sample estimates:
#>      cor 
#> 0.149691 
#> 
#> 
#> $family$age$Bacteroidaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Bacteroidaceae and age
#> t = -0.52811, df = 147, p-value = 0.5982
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.2028969  0.1181097
#> sample estimates:
#>         cor 
#> -0.04351673 
#> 
#> 
#> $family$age$Oscillospiraceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Oscillospiraceae and age
#> t = 0.7232, df = 147, p-value = 0.4707
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1022361  0.2182530
#> sample estimates:
#>        cor 
#> 0.05954271 
#> 
#> 
#> $family$age$`Clostridia class`
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Clostridia class and age
#> t = 1.5391, df = 147, p-value = 0.1259
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.03559243  0.28103740
#> sample estimates:
#>       cor 
#> 0.1259282 
#> 
#> 
#> $family$age$Bifidobacteriaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Bifidobacteriaceae and age
#> t = -1.9549, df = 147, p-value = 0.05249
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.31199519  0.00166131
#> sample estimates:
#>        cor 
#> -0.1591812 
#> 
#> 
#> $family$age$Enterococcaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Enterococcaceae and age
#> t = -0.65662, df = 147, p-value = 0.5125
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.2130252  0.1076585
#> sample estimates:
#>        cor 
#> -0.0540776 
#> 
#> 
#> $family$age$Coriobacteriaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Coriobacteriaceae and age
#> t = -0.20105, df = 147, p-value = 0.8409
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1769087  0.1446051
#> sample estimates:
#>        cor 
#> -0.0165804 
#> 
#> 
#> $family$age$Rikenellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Rikenellaceae and age
#> t = -0.96076, df = 147, p-value = 0.3383
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.23678670  0.08285795
#> sample estimates:
#>         cor 
#> -0.07899449 
#> 
#> 
#> $family$age$Lachnospiraceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Lachnospiraceae and age
#> t = -0.00041091, df = 147, p-value = 0.9997
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1608330  0.1607669
#> sample estimates:
#>           cor 
#> -3.389166e-05 
#> 
#> 
#> $family$age$`Firmicutes phylum`
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Firmicutes phylum and age
#> t = 0.79264, df = 147, p-value = 0.4293
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.09657627  0.22369020
#> sample estimates:
#>        cor 
#> 0.06523678 
#> 
#> 
#> $family$age$Barnesiellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Barnesiellaceae and age
#> t = -1.2593, df = 147, p-value = 0.2099
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.25979317  0.05846221
#> sample estimates:
#>        cor 
#> -0.1033089 
#> 
#> 
#> $family$age$Clostridiaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Clostridiaceae and age
#> t = -0.64966, df = 147, p-value = 0.5169
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.2124778  0.1082251
#> sample estimates:
#>         cor 
#> -0.05350595 
#> 
#> 
#> $family$age$Peptostreptococcaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Peptostreptococcaceae and age
#> t = -0.53057, df = 147, p-value = 0.5965
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.2030909  0.1179102
#> sample estimates:
#>         cor 
#> -0.04371863 
#> 
#> 
#> $family$age$Tannerellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Tannerellaceae and age
#> t = 0.29411, df = 147, p-value = 0.7691
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1370841  0.1843315
#> sample estimates:
#>        cor 
#> 0.02425037 
#> 
#> 
#> $family$age$Victivallaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Victivallaceae and age
#> t = -0.01424, df = 147, p-value = 0.9887
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1619439  0.1596556
#> sample estimates:
#>          cor 
#> -0.001174489 
#> 
#> 
#> $family$age$Eubacteriaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Eubacteriaceae and age
#> t = 0.051362, df = 147, p-value = 0.9591
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1566705  0.1649238
#> sample estimates:
#>         cor 
#> 0.004236223 
#> 
#> 
#> $family$age$Streptococcaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Streptococcaceae and age
#> t = 0.66048, df = 147, p-value = 0.51
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1073443  0.2133286
#> sample estimates:
#>        cor 
#> 0.05439454 
#> 
#> 
#> $family$age$Turicibacteraceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Turicibacteraceae and age
#> t = -1.2697, df = 147, p-value = 0.2062
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.26059201  0.05760825
#> sample estimates:
#>        cor 
#> -0.1041565 
#> 
#> 
#> $family$age$Eggerthellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Eggerthellaceae and age
#> t = -0.29017, df = 147, p-value = 0.7721
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1840180  0.1374025
#> sample estimates:
#>         cor 
#> -0.02392605 
#> 
#> 
#> $family$age$Veillonellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Veillonellaceae and age
#> t = -0.70296, df = 147, p-value = 0.4832
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.2166657  0.1038845
#> sample estimates:
#>         cor 
#> -0.05788232 
#> 
#> 
#> $family$age$Odoribacteraceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Odoribacteraceae and age
#> t = 1.3805, df = 147, p-value = 0.1695
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.04855327  0.26903578
#> sample estimates:
#>       cor 
#> 0.1131299 
#> 
#> 
#> $family$age$`Eubacteriales order`
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Eubacteriales order and age
#> t = -0.73374, df = 147, p-value = 0.4643
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.2190789  0.1013777
#> sample estimates:
#>         cor 
#> -0.06040694 
#> 
#> 
#> $family$age$Desulfovibrionaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Desulfovibrionaceae and age
#> t = -1.3366, df = 147, p-value = 0.1834
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.26569560  0.05214148
#> sample estimates:
#>        cor 
#> -0.1095772 
#> 
#> 
#> $family$age$Erysipelotrichaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Erysipelotrichaceae and age
#> t = -1.1646, df = 147, p-value = 0.2461
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.25252969  0.06620559
#> sample estimates:
#>         cor 
#> -0.09561224 
#> 
#> 
#> $family$age$Micrococcaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Micrococcaceae and age
#> t = -0.37709, df = 147, p-value = 0.7067
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1909320  0.1303651
#> sample estimates:
#>         cor 
#> -0.03108649 
#> 
#> 
#> $family$age$Pasteurellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Pasteurellaceae and age
#> t = -0.31094, df = 147, p-value = 0.7563
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.185672  0.135722
#> sample estimates:
#>         cor 
#> -0.02563745 
#> 
#> 
#> $family$age$Acidaminococcaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Acidaminococcaceae and age
#> t = -0.035408, df = 147, p-value = 0.9718
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1636435  0.1579537
#> sample estimates:
#>          cor 
#> -0.002920404 
#> 
#> 
#> $family$age$Akkermansiaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Akkermansiaceae and age
#> t = 1.4961, df = 147, p-value = 0.1368
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.0391025  0.2777969
#> sample estimates:
#>       cor 
#> 0.1224675 
#> 
#> 
#> $family$age$Sutterellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Sutterellaceae and age
#> t = 1.2978, df = 147, p-value = 0.1964
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.05531393  0.26273607
#> sample estimates:
#>       cor 
#> 0.1064327 
#> 
#> 
#> $family$age$Actinomycetaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Actinomycetaceae and age
#> t = -0.50115, df = 147, p-value = 0.617
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.2007659  0.1202997
#> sample estimates:
#>         cor 
#> -0.04129917 
#> 
#> 
#> $family$age$Atopobiaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Atopobiaceae and age
#> t = 0.53467, df = 147, p-value = 0.5937
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1175765  0.2034153
#> sample estimates:
#>        cor 
#> 0.04405642 
#> 
#> 
#> $family$age$Prevotellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Prevotellaceae and age
#> t = -0.2941, df = 147, p-value = 0.7691
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1843309  0.1370847
#> sample estimates:
#>         cor 
#> -0.02424975 
#> 
#> 
#> $family$age$Lactobacillaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Lactobacillaceae and age
#> t = -0.90507, df = 147, p-value = 0.3669
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.23245882  0.08740476
#> sample estimates:
#>         cor 
#> -0.07444144 
#> 
#> 
#> $family$age$`Proteobacteria phylum`
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Proteobacteria phylum and age
#> t = -1.8453, df = 147, p-value = 0.06701
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.30390957  0.01059375
#> sample estimates:
#>        cor 
#> -0.1504625 
#> 
#> 
#> $family$age$Coprobacillaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Coprobacillaceae and age
#> t = -0.96553, df = 147, p-value = 0.3359
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.23715727  0.08246802
#> sample estimates:
#>         cor 
#> -0.07938464 
#> 
#> 
#> $family$age$Methanobacteriaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Methanobacteriaceae and age
#> t = 0.077097, df = 147, p-value = 0.9387
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1545993  0.1669879
#> sample estimates:
#>         cor 
#> 0.006358728 
#> 
#> 
#> 
#> $family$number_reads
#> $family$number_reads$Enterobacteriaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Enterobacteriaceae and number_reads
#> t = -0.74985, df = 172, p-value = 0.4544
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.20411874  0.09247282
#> sample estimates:
#>         cor 
#> -0.05708231 
#> 
#> 
#> $family$number_reads$Bacteroidaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Bacteroidaceae and number_reads
#> t = 12.47, df = 172, p-value < 2.2e-16
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.6020160 0.7599377
#> sample estimates:
#>       cor 
#> 0.6890713 
#> 
#> 
#> $family$number_reads$Oscillospiraceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Oscillospiraceae and number_reads
#> t = 9.3364, df = 172, p-value < 2.2e-16
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.4718928 0.6708389
#> sample estimates:
#>       cor 
#> 0.5799483 
#> 
#> 
#> $family$number_reads$`Clostridia class`
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Clostridia class and number_reads
#> t = 3.7383, df = 172, p-value = 0.000252
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.1306848 0.4063244
#> sample estimates:
#>       cor 
#> 0.2741251 
#> 
#> 
#> $family$number_reads$Bifidobacteriaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Bifidobacteriaceae and number_reads
#> t = 3.6366, df = 172, p-value = 0.0003649
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.1233364 0.4000697
#> sample estimates:
#>       cor 
#> 0.2672034 
#> 
#> 
#> $family$number_reads$Enterococcaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Enterococcaceae and number_reads
#> t = -0.70111, df = 172, p-value = 0.4842
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.20056021  0.09615026
#> sample estimates:
#>         cor 
#> -0.05338318 
#> 
#> 
#> $family$number_reads$Coriobacteriaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Coriobacteriaceae and number_reads
#> t = 1.8851, df = 172, p-value = 0.0611
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.006634549  0.285012872
#> sample estimates:
#>       cor 
#> 0.1422757 
#> 
#> 
#> $family$number_reads$Rikenellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Rikenellaceae and number_reads
#> t = 8.2469, df = 172, p-value = 4.044e-14
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.4165395 0.6311120
#> sample estimates:
#>       cor 
#> 0.5323222 
#> 
#> 
#> $family$number_reads$Lachnospiraceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Lachnospiraceae and number_reads
#> t = 6.3964, df = 172, p-value = 1.451e-09
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.3097988 0.5511887
#> sample estimates:
#>       cor 
#> 0.4383649 
#> 
#> 
#> $family$number_reads$`Firmicutes phylum`
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Firmicutes phylum and number_reads
#> t = 1.8893, df = 172, p-value = 0.06054
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.006321532  0.285300448
#> sample estimates:
#>       cor 
#> 0.1425824 
#> 
#> 
#> $family$number_reads$Barnesiellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Barnesiellaceae and number_reads
#> t = 3.5789, df = 172, p-value = 0.0004485
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.1191617 0.3965049
#> sample estimates:
#>       cor 
#> 0.2632645 
#> 
#> 
#> $family$number_reads$Clostridiaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Clostridiaceae and number_reads
#> t = 3.2392, df = 172, p-value = 0.001439
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.09437574 0.37516597
#> sample estimates:
#>      cor 
#> 0.239779 
#> 
#> 
#> $family$number_reads$Peptostreptococcaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Peptostreptococcaceae and number_reads
#> t = 1.7873, df = 172, p-value = 0.07565
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.01401751  0.27821471
#> sample estimates:
#>       cor 
#> 0.1350339 
#> 
#> 
#> $family$number_reads$Tannerellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Tannerellaceae and number_reads
#> t = 2.2396, df = 172, p-value = 0.0264
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.02006385 0.30935403
#> sample estimates:
#>       cor 
#> 0.1683312 
#> 
#> 
#> $family$number_reads$Victivallaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Victivallaceae and number_reads
#> t = 2.0114, df = 172, p-value = 0.04584
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.002890989 0.293740766
#> sample estimates:
#>       cor 
#> 0.1515956 
#> 
#> 
#> $family$number_reads$Eubacteriaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Eubacteriaceae and number_reads
#> t = 3.6418, df = 172, p-value = 0.000358
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.1237170 0.4003943
#> sample estimates:
#>       cor 
#> 0.2675623 
#> 
#> 
#> $family$number_reads$Streptococcaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Streptococcaceae and number_reads
#> t = 0.44949, df = 172, p-value = 0.6536
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1151034  0.1820949
#> sample estimates:
#>        cor 
#> 0.03425301 
#> 
#> 
#> $family$number_reads$Turicibacteraceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Turicibacteraceae and number_reads
#> t = -0.40283, df = 172, p-value = 0.6876
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1786551  0.1186103
#> sample estimates:
#>         cor 
#> -0.03070127 
#> 
#> 
#> $family$number_reads$Eggerthellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Eggerthellaceae and number_reads
#> t = 2.6811, df = 172, p-value = 0.008052
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.05310387 0.33896106
#> sample estimates:
#>       cor 
#> 0.2002914 
#> 
#> 
#> $family$number_reads$Veillonellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Veillonellaceae and number_reads
#> t = -0.027574, df = 172, p-value = 0.978
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1508252  0.1467132
#> sample estimates:
#>          cor 
#> -0.002102528 
#> 
#> 
#> $family$number_reads$Odoribacteraceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Odoribacteraceae and number_reads
#> t = 7.6079, df = 172, p-value = 1.747e-12
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.3814894 0.6053608
#> sample estimates:
#>       cor 
#> 0.5017811 
#> 
#> 
#> $family$number_reads$`Eubacteriales order`
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Eubacteriales order and number_reads
#> t = 1.527, df = 172, p-value = 0.1286
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.03370026  0.25994681
#> sample estimates:
#>       cor 
#> 0.1156494 
#> 
#> 
#> $family$number_reads$Desulfovibrionaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Desulfovibrionaceae and number_reads
#> t = 2.8757, df = 172, p-value = 0.004542
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.06756108 0.35174043
#> sample estimates:
#>       cor 
#> 0.2141782 
#> 
#> 
#> $family$number_reads$Erysipelotrichaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Erysipelotrichaceae and number_reads
#> t = 0.16333, df = 172, p-value = 0.8705
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1365703  0.1609242
#> sample estimates:
#>       cor 
#> 0.0124525 
#> 
#> 
#> $family$number_reads$Micrococcaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Micrococcaceae and number_reads
#> t = 4.2731, df = 172, p-value = 3.186e-05
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.1688003 0.4383575
#> sample estimates:
#>       cor 
#> 0.3097905 
#> 
#> 
#> $family$number_reads$Pasteurellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Pasteurellaceae and number_reads
#> t = 2.2194, df = 172, p-value = 0.02777
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.01854693 0.30798113
#> sample estimates:
#>       cor 
#> 0.1668564 
#> 
#> 
#> $family$number_reads$Acidaminococcaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Acidaminococcaceae and number_reads
#> t = 4.6875, df = 172, p-value = 5.605e-06
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.1976940 0.4621927
#> sample estimates:
#>       cor 
#> 0.3365652 
#> 
#> 
#> $family$number_reads$Akkermansiaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Akkermansiaceae and number_reads
#> t = 1.6533, df = 172, p-value = 0.1001
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.02414786  0.26883889
#> sample estimates:
#>       cor 
#> 0.1250713 
#> 
#> 
#> $family$number_reads$Sutterellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Sutterellaceae and number_reads
#> t = 5.9824, df = 172, p-value = 1.242e-08
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.2837629 0.5309981
#> sample estimates:
#>       cor 
#> 0.4150128 
#> 
#> 
#> $family$number_reads$Actinomycetaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Actinomycetaceae and number_reads
#> t = 1.2116, df = 172, p-value = 0.2273
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.05756938  0.23750782
#> sample estimates:
#>       cor 
#> 0.0919883 
#> 
#> 
#> $family$number_reads$Atopobiaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Atopobiaceae and number_reads
#> t = 0.11847, df = 172, p-value = 0.9058
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1399253  0.1575907
#> sample estimates:
#>         cor 
#> 0.009032607 
#> 
#> 
#> $family$number_reads$Prevotellaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Prevotellaceae and number_reads
#> t = 0.45974, df = 172, p-value = 0.6463
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1143326  0.1828499
#> sample estimates:
#>        cor 
#> 0.03503307 
#> 
#> 
#> $family$number_reads$Lactobacillaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Lactobacillaceae and number_reads
#> t = 1.6286, df = 172, p-value = 0.1052
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.02601643  0.26710340
#> sample estimates:
#>       cor 
#> 0.1232304 
#> 
#> 
#> $family$number_reads$`Proteobacteria phylum`
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Proteobacteria phylum and number_reads
#> t = -0.14429, df = 172, p-value = 0.8854
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1595100  0.1379945
#> sample estimates:
#>         cor 
#> -0.01100118 
#> 
#> 
#> $family$number_reads$Coprobacillaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Coprobacillaceae and number_reads
#> t = 0.16346, df = 172, p-value = 0.8703
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.1365604  0.1609340
#> sample estimates:
#>       cor 
#> 0.0124626 
#> 
#> 
#> $family$number_reads$Methanobacteriaceae
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  Methanobacteriaceae and number_reads
#> t = 3.231, df = 172, p-value = 0.001478
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.09377651 0.37464637
#> sample estimates:
#>       cor 
#> 0.2392091 
#> 
#> 
#> 
#> 
```
