# tax_model bbdml results stay the same: Allistipes et rel.

    Code
      names(bb_models[[t]]$b.mu)
    Output
      [1] "mu.(Intercept)" "mu.female"      "mu.overweight"  "mu.obese"      

---

    Code
      bb_models[[t]]$b.mu
    Output
      mu.(Intercept)      mu.female  mu.overweight       mu.obese 
         -4.34836689     0.01476891     0.29261151    -0.74690500 

---

    Code
      bb_models[[t]]$b.phi
    Output
      phi.(Intercept)      phi.female  phi.overweight       phi.obese 
           -3.7595480       0.2952961       0.2538415      -1.3059506 

# tax_model bbdml results stay the same: Akkermansia

    Code
      names(bb_models[[t]]$b.mu)
    Output
      [1] "mu.(Intercept)" "mu.female"      "mu.overweight"  "mu.obese"      

---

    Code
      bb_models[[t]]$b.mu
    Output
      mu.(Intercept)      mu.female  mu.overweight       mu.obese 
          -4.3189151      0.2074677      0.4353846      0.7802854 

---

    Code
      bb_models[[t]]$b.phi
    Output
      phi.(Intercept)      phi.female  phi.overweight       phi.obese 
          -4.20598274      0.82873708     -0.06036421      0.69103042 

# tax_model bbdml results stay the same: Bifidobacterium

    Code
      names(bb_models[[t]]$b.mu)
    Output
      [1] "mu.(Intercept)" "mu.female"      "mu.overweight"  "mu.obese"      

---

    Code
      bb_models[[t]]$b.mu
    Output
      mu.(Intercept)      mu.female  mu.overweight       mu.obese 
        -5.385095217    0.002008981    0.515254866    0.370213065 

---

    Code
      bb_models[[t]]$b.phi
    Output
      phi.(Intercept)      phi.female  phi.overweight       phi.obese 
         -5.410498670    -0.005639129     0.671960731     0.565395000 

# tax_model compositional lm results stay the same: Allistipes et rel.

    Code
      names(lm_models[[t]]$coefficients)
    Output
      [1] "(Intercept)" "female"      "overweight"  "obese"      

---

    Code
      lm_models[[t]]$coefficients
    Output
       (Intercept)       female   overweight        obese 
       0.012570870  0.006664491  0.001375442 -0.011149041 

---

    Code
      lm_models[[t]]$qr$qraux
    Output
      [1] 1.067116 1.057986 1.106926 1.015180

# tax_model compositional lm results stay the same: Akkermansia

    Code
      names(lm_models[[t]]$coefficients)
    Output
      [1] "(Intercept)" "female"      "overweight"  "obese"      

---

    Code
      lm_models[[t]]$coefficients
    Output
       (Intercept)       female   overweight        obese 
      0.0130508219 0.0009329216 0.0111684050 0.0151289619 

---

    Code
      lm_models[[t]]$qr$qraux
    Output
      [1] 1.067116 1.057986 1.106926 1.015180

# tax_model compositional lm results stay the same: Bifidobacterium

    Code
      names(lm_models[[t]]$coefficients)
    Output
      [1] "(Intercept)" "female"      "overweight"  "obese"      

---

    Code
      lm_models[[t]]$coefficients
    Output
       (Intercept)       female   overweight        obese 
      0.0045857411 0.0001690498 0.0029079072 0.0018860486 

---

    Code
      lm_models[[t]]$qr$qraux
    Output
      [1] 1.067116 1.057986 1.106926 1.015180

# taxatree plot scale limits are harmonised

    Code
      plots$african$scales$scales[[3]]$limits
    Output
      [1] -0.2793325  0.2793325

# taxatree lm plots don't change: african

    Code
      plots[[v]]$data$y
    Output
       [1]  0.0000000  0.4698463  0.2500000 -0.5000000  0.2500000  0.4698463
       [7]  0.9396926  0.5000000 -0.1736482 -0.7660444 -1.0000000 -0.7660444
      [13] -0.1736482  0.5000000  0.9396926

---

    Code
      plots[[v]]$data$x
    Output
       [1]  0.000000e+00  1.710101e-01  4.330127e-01  3.061617e-17 -4.330127e-01
       [6] -1.710101e-01  3.420201e-01  8.660254e-01  9.848078e-01  6.427876e-01
      [11]  6.123234e-17 -6.427876e-01 -9.848078e-01 -8.660254e-01 -3.420201e-01

---

    Code
      plots[[v]]$data$taxon_mean
    Output
       [1] 12466.46396    72.74775  6645.07658  5553.39640    80.10811   115.13514
       [7]    72.74775  6645.07658    76.36486  3806.77477   139.80631  1366.73423
      [13]   163.71622    80.10811   115.13514

---

    Code
      plots[[v]]$data$p.value
    Output
       [1]           NA 2.350569e-02 9.768289e-01 6.893840e-01 7.859753e-01
       [6] 3.795889e-02 2.350569e-02 9.768289e-01 2.126659e-01 1.218328e-02
      [11] 6.835953e-02 5.065192e-06 1.273698e-02 7.859753e-01 3.795889e-02

---

    Code
      plots[[v]]$data$estimate
    Output
       [1]            NA -0.0032645958 -0.0009113403  0.0119455674  0.0009256134
       [6] -0.0086952447 -0.0032645958 -0.0009113403 -0.0028467015  0.0624213152
      [11] -0.0094834013 -0.0457759216  0.0076302766  0.0009256134 -0.0086952447

---

    Code
      plots[[v]]$data$taxon_from
    Output
       [1] "root"               "root"               "root"              
       [4] "root"               "root"               "root"              
       [7] "P: Actinobacteria"  "P: Bacteroidetes"   "P: Firmicutes"     
      [10] "P: Firmicutes"      "P: Firmicutes"      "P: Firmicutes"     
      [13] "P: Firmicutes"      "P: Proteobacteria"  "P: Verrucomicrobia"

# taxatree lm plots don't change: female

    Code
      plots[[v]]$data$y
    Output
       [1]  0.0000000  0.4698463  0.2500000 -0.5000000  0.2500000  0.4698463
       [7]  0.9396926  0.5000000 -0.1736482 -0.7660444 -1.0000000 -0.7660444
      [13] -0.1736482  0.5000000  0.9396926

---

    Code
      plots[[v]]$data$x
    Output
       [1]  0.000000e+00  1.710101e-01  4.330127e-01  3.061617e-17 -4.330127e-01
       [6] -1.710101e-01  3.420201e-01  8.660254e-01  9.848078e-01  6.427876e-01
      [11]  6.123234e-17 -6.427876e-01 -9.848078e-01 -8.660254e-01 -3.420201e-01

---

    Code
      plots[[v]]$data$taxon_mean
    Output
       [1] 12466.46396    72.74775  6645.07658  5553.39640    80.10811   115.13514
       [7]    72.74775  6645.07658    76.36486  3806.77477   139.80631  1366.73423
      [13]   163.71622    80.10811   115.13514

---

    Code
      plots[[v]]$data$p.value
    Output
       [1]           NA 3.527742e-01 9.829214e-01 6.432107e-01 4.766281e-03
       [6] 4.048784e-01 3.527742e-01 9.829214e-01 9.392069e-02 7.511192e-01
      [11] 6.090656e-01 6.150046e-01 9.126698e-05 4.766281e-03 4.048784e-01

---

    Code
      plots[[v]]$data$estimate
    Output
       [1]            NA  0.0013292717 -0.0006699403 -0.0138092805  0.0096834065
       [6]  0.0034665427  0.0013292717 -0.0006699403  0.0038217808 -0.0078216650
      [11] -0.0026447837  0.0049156278 -0.0120802404  0.0096834065  0.0034665427

---

    Code
      plots[[v]]$data$taxon_from
    Output
       [1] "root"               "root"               "root"              
       [4] "root"               "root"               "root"              
       [7] "P: Actinobacteria"  "P: Bacteroidetes"   "P: Firmicutes"     
      [10] "P: Firmicutes"      "P: Firmicutes"      "P: Firmicutes"     
      [13] "P: Firmicutes"      "P: Proteobacteria"  "P: Verrucomicrobia"

