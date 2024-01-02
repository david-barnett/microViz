# unifrac distances work

    Code
      dist_get(dist_calc(esophagus, dist = "gunifrac"))
    Output
                B         C
      C 0.4404284          
      D 0.4332325 0.4969773

---

    Code
      dist_get(dist_calc(esophagus, dist = "unifrac"))
    Output
                B         C
      C 0.5175550          
      D 0.5182284 0.5422394

---

    Code
      dist_get(dist_calc(esophagus, dist = "wunifrac"))
    Output
                B         C
      C 0.2035424          
      D 0.2603371 0.2477016

