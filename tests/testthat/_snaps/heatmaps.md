# cor_heatmap doesn't change: 

    Code
      p@matrix_param[names(p@matrix_param) != "cell_fun"]
    Output
      $row_km
      [1] 1
      
      $row_km_repeats
      [1] 1
      
      $row_gap
      [1] 1mm
      
      $column_km
      [1] 1
      
      $column_km_repeats
      [1] 1
      
      $column_gap
      [1] 1mm
      
      $jitter
      [1] FALSE
      
      $gp
      $col
      [1] "white"
      
      $alpha
      [1] 1
      
      $lty
      [1] 1
      
      $lwd
      [1] 0.5
      
      $lex
      [1] 1
      
      $lineend
      [1] "round"
      
      $linejoin
      [1] "round"
      
      $lineheight
      [1] 0.9
      
      
      $border
      [1] NA
      
      $border_gp
      $col
      [1] "black"
      
      
      $width
      [1] 5null
      
      $height
      [1] 30null
      

---

    Code
      p@matrix_color_mapping
    Output
      Continuous color mapping:
      name: pearson 
      default breaks:
      [1] -1.0 -0.5  0.0  0.5  1.0
      
      colors:
      [1] "#11C638FF" "#54CC5CFF" "#E2E2E2FF" "#F2A346FF" "#EF9708FF"
      

---

    Code
      p@right_annotation@anno_list
    Output
      $prev
      A single annotation with anno_barplot() function
        name: prev 
        position: row 
        no legend
        items: 30 
        width: 15mm 
        height: 1npc 
        this object is  subsetable
        8.94924444444444mm extension on the bottom 
      
      $abund
      A single annotation with anno_boxplot() function
        name: abund 
        position: row 
        no legend
        items: 30 
        width: 15mm 
        height: 1npc 
        this object is  subsetable
        8.94924444444444mm extension on the bottom 
      

---

    Code
      str(p@column_dend_param$obj)
    Output
      --[dendrogram w/ 2 branches and 5 members at h = 2.23]
        |--leaf "african" 
        `--[dendrogram w/ 2 branches and 4 members at h = 0.946]
           |--[dendrogram w/ 2 branches and 2 members at h = 0.415]
           |  |--leaf "timepoint" 
           |  `--leaf "timepoint.within.group" 
           `--[dendrogram w/ 2 branches and 2 members at h = 0.738]
              |--leaf "weight" 
              `--leaf "female" 

---

    Code
      str(p@row_dend_param$obj)
    Output
      --[dendrogram w/ 2 branches and 30 members at h = 1.62]
        |--[dendrogram w/ 2 branches and 6 members at h = 0.301]
        |  |--[dendrogram w/ 2 branches and 2 members at h = 0.0866]
        |  |  |--leaf "Allistipes et rel." 
        |  |  `--leaf "Bacteroides vulgatus et rel." 
        |  `--[dendrogram w/ 2 branches and 4 members at h = 0.223]
        |     |--[dendrogram w/ 2 branches and 3 members at h = 0.139]
        |     |  |--[dendrogram w/ 2 branches and 2 members at h = 0.119]
        |     |  |  |--leaf "Bacteroides plebeius et rel." 
        |     |  |  `--leaf "Tannerella et rel." 
        |     |  `--leaf "Parabacteroides distasonis et rel." 
        |     `--leaf "Bacteroides uniformis et rel." 
        `--[dendrogram w/ 2 branches and 24 members at h = 1.11]
           |--[dendrogram w/ 2 branches and 17 members at h = 0.716]
           |  |--[dendrogram w/ 2 branches and 11 members at h = 0.547]
           |  |  |--[dendrogram w/ 2 branches and 7 members at h = 0.39]
           |  |  |  |--[dendrogram w/ 2 branches and 3 members at h = 0.309]
           |  |  |  |  |--[dendrogram w/ 2 branches and 2 members at h = 0.165]
           |  |  |  |  |  |--leaf "Prevotella tannerae et rel." 
           |  |  |  |  |  `--leaf "Anaerostipes caccae et rel." 
           |  |  |  |  `--leaf "Bryantella formatexigens et rel." 
           |  |  |  `--[dendrogram w/ 2 branches and 4 members at h = 0.213]
           |  |  |     |--[dendrogram w/ 2 branches and 2 members at h = 0.0816]
           |  |  |     |  |--leaf "Subdoligranulum variable at rel." 
           |  |  |     |  `--leaf "Bifidobacterium" 
           |  |  |     `--[dendrogram w/ 2 branches and 2 members at h = 0.132]
           |  |  |        |--leaf "Clostridium symbiosum et rel." 
           |  |  |        `--leaf "Clostridium sphenoides et rel." 
           |  |  `--[dendrogram w/ 2 branches and 4 members at h = 0.352]
           |  |     |--leaf "Bacteroides fragilis et rel." 
           |  |     `--[dendrogram w/ 2 branches and 3 members at h = 0.249]
           |  |        |--[dendrogram w/ 2 branches and 2 members at h = 0.214]
           |  |        |  |--leaf "Lachnospira pectinoschiza et rel." 
           |  |        |  `--leaf "Lachnobacillus bovis et rel." 
           |  |        `--leaf "Akkermansia" 
           |  `--[dendrogram w/ 2 branches and 6 members at h = 0.295]
           |     |--[dendrogram w/ 2 branches and 2 members at h = 0.119]
           |     |  |--leaf "Escherichia coli et rel." 
           |     |  `--leaf "Clostridium cellulosi et rel." 
           |     `--[dendrogram w/ 2 branches and 4 members at h = 0.23]
           |        |--[dendrogram w/ 2 branches and 3 members at h = 0.119]
           |        |  |--leaf "Phascolarctobacterium faecium et rel." 
           |        |  `--[dendrogram w/ 2 branches and 2 members at h = 0.0523]
           |        |     |--leaf "Streptococcus mitis et rel." 
           |        |     `--leaf "Streptococcus bovis et rel." 
           |        `--leaf "Ruminococcus obeum et rel." 
           `--[dendrogram w/ 2 branches and 7 members at h = 0.422]
              |--[dendrogram w/ 2 branches and 6 members at h = 0.402]
              |  |--[dendrogram w/ 2 branches and 4 members at h = 0.278]
              |  |  |--[dendrogram w/ 2 branches and 3 members at h = 0.228]
              |  |  |  |--[dendrogram w/ 2 branches and 2 members at h = 0.109]
              |  |  |  |  |--leaf "Clostridium nexile et rel." 
              |  |  |  |  `--leaf "Outgrouping clostridium cluster XIVa" 
              |  |  |  `--leaf "Clostridium orbiscindens et rel." 
              |  |  `--leaf "Uncultured Mollicutes" 
              |  `--[dendrogram w/ 2 branches and 2 members at h = 0.221]
              |     |--leaf "Sporobacter termitidis et rel." 
              |     `--leaf "Uncultured Clostridiales II" 
              `--leaf "Prevotella oralis et rel." 

# comp_heatmap doesn't change: 

    Code
      p@matrix_param
    Output
      $row_km
      [1] 1
      
      $row_km_repeats
      [1] 1
      
      $row_gap
      [1] 1mm
      
      $column_km
      [1] 1
      
      $column_km_repeats
      [1] 1
      
      $column_gap
      [1] 1mm
      
      $jitter
      [1] FALSE
      
      $gp
      $col
      [1] "black"
      
      $alpha
      [1] 1
      
      $lty
      [1] 1
      
      $lwd
      [1] 0.1
      
      $lex
      [1] 1
      
      $lineend
      [1] "round"
      
      $linejoin
      [1] "round"
      
      $lineheight
      [1] 0.9
      
      
      $border
      [1] NA
      
      $border_gp
      $col
      [1] "black"
      
      
      $width
      [1] 222null
      
      $height
      [1] 30null
      

---

    Code
      p@matrix_color_mapping
    Output
      Continuous color mapping:
      name: mat 
      default breaks:
      [1] -2  0  2  4  6  8
      
      colors:
      [1] "#F6FBF4FF" "#D4ECCCFF" "#99CD91FF" "#56A156FF" "#1B6A2AFF" "#004616FF"
      

---

    Code
      p@right_annotation@anno_list
    Output
      $prev
      A single annotation with anno_barplot() function
        name: prev 
        position: row 
        no legend
        items: 30 
        width: 15mm 
        height: 1npc 
        this object is  subsetable
        8.94924444444444mm extension on the bottom 
      
      $abund
      A single annotation with anno_boxplot() function
        name: abund 
        position: row 
        no legend
        items: 30 
        width: 15mm 
        height: 1npc 
        this object is  subsetable
        8.94924444444444mm extension on the bottom 
      

---

    Code
      str(p@column_dend_param$obj)
    Output
      --[dendrogram w/ 2 branches and 222 members at h = 61]
        |--[dendrogram w/ 2 branches and 94 members at h = 22.7]
        |  |--[dendrogram w/ 2 branches and 50 members at h = 17.1]
        |  |  |--[dendrogram w/ 2 branches and 32 members at h = 11.8]
        |  |  |  |--[dendrogram w/ 2 branches and 14 members at h = 9.27]
        |  |  |  |  |--[dendrogram w/ 2 branches and 5 members at h = 6.76]
        |  |  |  |  |  |--[dendrogram w/ 2 branches and 2 members at h = 5.37]
        |  |  |  |  |  |  |--leaf "Sample-35" 
        |  |  |  |  |  |  `--leaf "Sample-132" 
        |  |  |  |  |  `--[dendrogram w/ 2 branches and 3 members at h = 4.54]
        |  |  |  |  |     |--leaf "Sample-112" 
        |  |  |  |  |     `--[dendrogram w/ 2 branches and 2 members at h = 3.12]
        |  |  |  |  |        |--leaf "Sample-118" 
        |  |  |  |  |        `--leaf "Sample-69" 
        |  |  |  |  `--[dendrogram w/ 2 branches and 9 members at h = 7.26]
        |  |  |  |     |--[dendrogram w/ 2 branches and 6 members at h = 4.96]
        |  |  |  |     |  |--[dendrogram w/ 2 branches and 3 members at h = 3.18]
        |  |  |  |     |  |  |--[dendrogram w/ 2 branches and 2 members at h = 2.72]
        |  |  |  |     |  |  |  |--leaf "Sample-220" 
        |  |  |  |     |  |  |  `--leaf "Sample-31" 
        |  |  |  |     |  |  `--leaf "Sample-15" 
        |  |  |  |     |  `--[dendrogram w/ 2 branches and 3 members at h = 4.02]
        |  |  |  |     |     |--[dendrogram w/ 2 branches and 2 members at h = 2.98]
        |  |  |  |     |     |  |--leaf "Sample-5" 
        |  |  |  |     |     |  `--leaf "Sample-208" 
        |  |  |  |     |     `--leaf "Sample-54" 
        |  |  |  |     `--[dendrogram w/ 2 branches and 3 members at h = 3.59]
        |  |  |  |        |--[dendrogram w/ 2 branches and 2 members at h = 1.92]
        |  |  |  |        |  |--leaf "Sample-53" 
        |  |  |  |        |  `--leaf "Sample-102" 
        |  |  |  |        `--leaf "Sample-79" 
        |  |  |  `--[dendrogram w/ 2 branches and 18 members at h = 7.47]
        |  |  |     |--[dendrogram w/ 2 branches and 11 members at h = 6.28]
        |  |  |     |  |--[dendrogram w/ 2 branches and 2 members at h = 2]
        |  |  |     |  |  |--leaf "Sample-193" 
        |  |  |     |  |  `--leaf "Sample-190" 
        |  |  |     |  `--[dendrogram w/ 2 branches and 9 members at h = 5.39]
        |  |  |     |     |--[dendrogram w/ 2 branches and 7 members at h = 4.69]
        |  |  |     |     |  |--[dendrogram w/ 2 branches and 5 members at h = 3.79]
        |  |  |     |     |  |  |--[dendrogram w/ 2 branches and 3 members at h = 3.27]
        |  |  |     |     |  |  |  |--[dendrogram w/ 2 branches and 2 members at h = 2.77]
        |  |  |     |     |  |  |  |  |--leaf "Sample-187" 
        |  |  |     |     |  |  |  |  `--leaf "Sample-107" 
        |  |  |     |     |  |  |  `--leaf "Sample-94" 
        |  |  |     |     |  |  `--[dendrogram w/ 2 branches and 2 members at h = 2.53]
        |  |  |     |     |  |     |--leaf "Sample-168" 
        |  |  |     |     |  |     `--leaf "Sample-182" 
        |  |  |     |     |  `--[dendrogram w/ 2 branches and 2 members at h = 3.19]
        |  |  |     |     |     |--leaf "Sample-161" 
        |  |  |     |     |     `--leaf "Sample-212" 
        |  |  |     |     `--[dendrogram w/ 2 branches and 2 members at h = 3.22]
        |  |  |     |        |--leaf "Sample-207" 
        |  |  |     |        `--leaf "Sample-61" 
        |  |  |     `--[dendrogram w/ 2 branches and 7 members at h = 6.61]
        |  |  |        |--[dendrogram w/ 2 branches and 3 members at h = 3.89]
        |  |  |        |  |--[dendrogram w/ 2 branches and 2 members at h = 2.87]
        |  |  |        |  |  |--leaf "Sample-209" 
        |  |  |        |  |  `--leaf "Sample-210" 
        |  |  |        |  `--leaf "Sample-213" 
        |  |  |        `--[dendrogram w/ 2 branches and 4 members at h = 4.65]
        |  |  |           |--[dendrogram w/ 2 branches and 3 members at h = 3.33]
        |  |  |           |  |--leaf "Sample-50" 
        |  |  |           |  `--[dendrogram w/ 2 branches and 2 members at h = 1.91]
        |  |  |           |     |--leaf "Sample-89" 
        |  |  |           |     `--leaf "Sample-104" 
        |  |  |           `--leaf "Sample-56" 
        |  |  `--[dendrogram w/ 2 branches and 18 members at h = 11.5]
        |  |     |--[dendrogram w/ 2 branches and 8 members at h = 6.82]
        |  |     |  |--[dendrogram w/ 2 branches and 5 members at h = 4.69]
        |  |     |  |  |--[dendrogram w/ 2 branches and 2 members at h = 4.2]
        |  |     |  |  |  |--leaf "Sample-55" 
        |  |     |  |  |  `--leaf "Sample-95" 
        |  |     |  |  `--[dendrogram w/ 2 branches and 3 members at h = 3.34]
        |  |     |  |     |--leaf "Sample-81" 
        |  |     |  |     `--[dendrogram w/ 2 branches and 2 members at h = 2.83]
        |  |     |  |        |--leaf "Sample-88" 
        |  |     |  |        `--leaf "Sample-101" 
        |  |     |  `--[dendrogram w/ 2 branches and 3 members at h = 4.18]
        |  |     |     |--[dendrogram w/ 2 branches and 2 members at h = 2.6]
        |  |     |     |  |--leaf "Sample-60" 
        |  |     |     |  `--leaf "Sample-77" 
        |  |     |     `--leaf "Sample-66" 
        |  |     `--[dendrogram w/ 2 branches and 10 members at h = 8.16]
        |  |        |--[dendrogram w/ 2 branches and 8 members at h = 7.88]
        |  |        |  |--[dendrogram w/ 2 branches and 5 members at h = 5.95]
        |  |        |  |  |--[dendrogram w/ 2 branches and 2 members at h = 3.64]
        |  |        |  |  |  |--leaf "Sample-67" 
        |  |        |  |  |  `--leaf "Sample-86" 
        |  |        |  |  `--[dendrogram w/ 2 branches and 3 members at h = 4.61]
        |  |        |  |     |--[dendrogram w/ 2 branches and 2 members at h = 4.26]
        |  |        |  |     |  |--leaf "Sample-6" 
        |  |        |  |     |  `--leaf "Sample-59" 
        |  |        |  |     `--leaf "Sample-93" 
        |  |        |  `--[dendrogram w/ 2 branches and 3 members at h = 4.85]
        |  |        |     |--leaf "Sample-52" 
        |  |        |     `--[dendrogram w/ 2 branches and 2 members at h = 2.52]
        |  |        |        |--leaf "Sample-103" 
        |  |        |        `--leaf "Sample-99" 
        |  |        `--[dendrogram w/ 2 branches and 2 members at h = 3.7]
        |  |           |--leaf "Sample-105" 
        |  |           `--leaf "Sample-9" 
        |  `--[dendrogram w/ 2 branches and 44 members at h = 18.2]
        |     |--[dendrogram w/ 2 branches and 10 members at h = 13.9]
        |     |  |--[dendrogram w/ 2 branches and 5 members at h = 8.2]
        |     |  |  |--[dendrogram w/ 2 branches and 3 members at h = 5.28]
        |     |  |  |  |--leaf "Sample-14" 
        |     |  |  |  `--[dendrogram w/ 2 branches and 2 members at h = 3.57]
        |     |  |  |     |--leaf "Sample-30" 
        |     |  |  |     `--leaf "Sample-22" 
        |     |  |  `--[dendrogram w/ 2 branches and 2 members at h = 4.73]
        |     |  |     |--leaf "Sample-4" 
        |     |  |     `--leaf "Sample-40" 
        |     |  `--[dendrogram w/ 2 branches and 5 members at h = 6.67]
        |     |     |--[dendrogram w/ 2 branches and 2 members at h = 2.06]
        |     |     |  |--leaf "Sample-121" 
        |     |     |  `--leaf "Sample-126" 
        |     |     `--[dendrogram w/ 2 branches and 3 members at h = 4.37]
        |     |        |--[dendrogram w/ 2 branches and 2 members at h = 2.83]
        |     |        |  |--leaf "Sample-137" 
        |     |        |  `--leaf "Sample-147" 
        |     |        `--leaf "Sample-49" 
        |     `--[dendrogram w/ 2 branches and 34 members at h = 15.4]
        |        |--[dendrogram w/ 2 branches and 19 members at h = 8.97]
        |        |  |--[dendrogram w/ 2 branches and 6 members at h = 6.78]
        |        |  |  |--[dendrogram w/ 2 branches and 4 members at h = 4.22]
        |        |  |  |  |--leaf "Sample-16" 
        |        |  |  |  `--[dendrogram w/ 2 branches and 3 members at h = 2.56]
        |        |  |  |     |--leaf "Sample-218" 
        |        |  |  |     `--[dendrogram w/ 2 branches and 2 members at h = 2.23]
        |        |  |  |        |--leaf "Sample-29" 
        |        |  |  |        `--leaf "Sample-21" 
        |        |  |  `--[dendrogram w/ 2 branches and 2 members at h = 3.84]
        |        |  |     |--leaf "Sample-7" 
        |        |  |     `--leaf "Sample-2" 
        |        |  `--[dendrogram w/ 2 branches and 13 members at h = 8.36]
        |        |     |--[dendrogram w/ 2 branches and 7 members at h = 6.49]
        |        |     |  |--[dendrogram w/ 2 branches and 4 members at h = 5.35]
        |        |     |  |  |--[dendrogram w/ 2 branches and 2 members at h = 3.82]
        |        |     |  |  |  |--leaf "Sample-25" 
        |        |     |  |  |  `--leaf "Sample-222" 
        |        |     |  |  `--[dendrogram w/ 2 branches and 2 members at h = 3.61]
        |        |     |  |     |--leaf "Sample-3" 
        |        |     |  |     `--leaf "Sample-214" 
        |        |     |  `--[dendrogram w/ 2 branches and 3 members at h = 3.87]
        |        |     |     |--[dendrogram w/ 2 branches and 2 members at h = 2.85]
        |        |     |     |  |--leaf "Sample-8" 
        |        |     |     |  `--leaf "Sample-20" 
        |        |     |     `--leaf "Sample-23" 
        |        |     `--[dendrogram w/ 2 branches and 6 members at h = 5.93]
        |        |        |--[dendrogram w/ 2 branches and 4 members at h = 4.12]
        |        |        |  |--[dendrogram w/ 2 branches and 3 members at h = 3.08]
        |        |        |  |  |--[dendrogram w/ 2 branches and 2 members at h = 2.7]
        |        |        |  |  |  |--leaf "Sample-217" 
        |        |        |  |  |  `--leaf "Sample-12" 
        |        |        |  |  `--leaf "Sample-28" 
        |        |        |  `--leaf "Sample-13" 
        |        |        `--[dendrogram w/ 2 branches and 2 members at h = 4.71]
        |        |           |--leaf "Sample-11" 
        |        |           `--leaf "Sample-10" 
        |        `--[dendrogram w/ 2 branches and 15 members at h = 9.37]
        |           |--[dendrogram w/ 2 branches and 11 members at h = 7.06]
        |           |  |--[dendrogram w/ 2 branches and 6 members at h = 5.67]
        |           |  |  |--[dendrogram w/ 2 branches and 2 members at h = 4.95]
        |           |  |  |  |--leaf "Sample-76" 
        |           |  |  |  `--leaf "Sample-83" 
        |           |  |  `--[dendrogram w/ 2 branches and 4 members at h = 3.53]
        |           |  |     |--[dendrogram w/ 2 branches and 2 members at h = 3.15]
        |           |  |     |  |--leaf "Sample-63" 
        |           |  |     |  `--leaf "Sample-64" 
        |           |  |     `--[dendrogram w/ 2 branches and 2 members at h = 2.66]
        |           |  |        |--leaf "Sample-90" 
        |           |  |        `--leaf "Sample-57" 
        |           |  `--[dendrogram w/ 2 branches and 5 members at h = 4.2]
        |           |     |--[dendrogram w/ 2 branches and 2 members at h = 2.3]
        |           |     |  |--leaf "Sample-58" 
        |           |     |  `--leaf "Sample-91" 
        |           |     `--[dendrogram w/ 2 branches and 3 members at h = 3.69]
        |           |        |--leaf "Sample-24" 
        |           |        `--[dendrogram w/ 2 branches and 2 members at h = 3.22]
        |           |           |--leaf "Sample-65" 
        |           |           `--leaf "Sample-85" 
        |           `--[dendrogram w/ 2 branches and 4 members at h = 6.22]
        |              |--[dendrogram w/ 2 branches and 2 members at h = 2.31]
        |              |  |--leaf "Sample-92" 
        |              |  `--leaf "Sample-98" 
        |              `--[dendrogram w/ 2 branches and 2 members at h = 3.18]
        |                 |--leaf "Sample-96" 
        |                 `--leaf "Sample-97" 
        `--[dendrogram w/ 2 branches and 128 members at h = 33.3]
           |--[dendrogram w/ 2 branches and 63 members at h = 24]
           |  |--[dendrogram w/ 2 branches and 38 members at h = 17.1]
           |  |  |--[dendrogram w/ 2 branches and 9 members at h = 7.82]
           |  |  |  |--[dendrogram w/ 2 branches and 6 members at h = 5.62]
           |  |  |  |  |--[dendrogram w/ 2 branches and 3 members at h = 3.55]
           |  |  |  |  |  |--[dendrogram w/ 2 branches and 2 members at h = 2.17]
           |  |  |  |  |  |  |--leaf "Sample-75" 
           |  |  |  |  |  |  `--leaf "Sample-84" 
           |  |  |  |  |  `--leaf "Sample-100" 
           |  |  |  |  `--[dendrogram w/ 2 branches and 3 members at h = 2]
           |  |  |  |     |--leaf "Sample-18" 
           |  |  |  |     `--[dendrogram w/ 2 branches and 2 members at h = 1.63]
           |  |  |  |        |--leaf "Sample-26" 
           |  |  |  |        `--leaf "Sample-215" 
           |  |  |  `--[dendrogram w/ 2 branches and 3 members at h = 4.21]
           |  |  |     |--leaf "Sample-221" 
           |  |  |     `--[dendrogram w/ 2 branches and 2 members at h = 1.86]
           |  |  |        |--leaf "Sample-216" 
           |  |  |        `--leaf "Sample-27" 
           |  |  `--[dendrogram w/ 2 branches and 29 members at h = 13.9]
           |  |     |--[dendrogram w/ 2 branches and 11 members at h = 8.61]
           |  |     |  |--[dendrogram w/ 2 branches and 7 members at h = 7.38]
           |  |     |  |  |--[dendrogram w/ 2 branches and 4 members at h = 5.27]
           |  |     |  |  |  |--[dendrogram w/ 2 branches and 2 members at h = 3.15]
           |  |     |  |  |  |  |--leaf "Sample-80" 
           |  |     |  |  |  |  `--leaf "Sample-87" 
           |  |     |  |  |  `--[dendrogram w/ 2 branches and 2 members at h = 3.87]
           |  |     |  |  |     |--leaf "Sample-51" 
           |  |     |  |  |     `--leaf "Sample-108" 
           |  |     |  |  `--[dendrogram w/ 2 branches and 3 members at h = 3.48]
           |  |     |  |     |--leaf "Sample-133" 
           |  |     |  |     `--[dendrogram w/ 2 branches and 2 members at h = 2.55]
           |  |     |  |        |--leaf "Sample-143" 
           |  |     |  |        `--leaf "Sample-44" 
           |  |     |  `--[dendrogram w/ 2 branches and 4 members at h = 4.56]
           |  |     |     |--[dendrogram w/ 2 branches and 2 members at h = 4.07]
           |  |     |     |  |--leaf "Sample-34" 
           |  |     |     |  `--leaf "Sample-111" 
           |  |     |     `--[dendrogram w/ 2 branches and 2 members at h = 2.99]
           |  |     |        |--leaf "Sample-142" 
           |  |     |        `--leaf "Sample-43" 
           |  |     `--[dendrogram w/ 2 branches and 18 members at h = 10.5]
           |  |        |--[dendrogram w/ 2 branches and 8 members at h = 8.35]
           |  |        |  |--[dendrogram w/ 2 branches and 3 members at h = 4.4]
           |  |        |  |  |--leaf "Sample-149" 
           |  |        |  |  `--[dendrogram w/ 2 branches and 2 members at h = 3.29]
           |  |        |  |     |--leaf "Sample-129" 
           |  |        |  |     `--leaf "Sample-123" 
           |  |        |  `--[dendrogram w/ 2 branches and 5 members at h = 6.68]
           |  |        |     |--[dendrogram w/ 2 branches and 3 members at h = 5.33]
           |  |        |     |  |--[dendrogram w/ 2 branches and 2 members at h = 4.23]
           |  |        |     |  |  |--leaf "Sample-163" 
           |  |        |     |  |  `--leaf "Sample-17" 
           |  |        |     |  `--leaf "Sample-165" 
           |  |        |     `--[dendrogram w/ 2 branches and 2 members at h = 4.04]
           |  |        |        |--leaf "Sample-115" 
           |  |        |        `--leaf "Sample-136" 
           |  |        `--[dendrogram w/ 2 branches and 10 members at h = 8.68]
           |  |           |--[dendrogram w/ 2 branches and 4 members at h = 5.97]
           |  |           |  |--[dendrogram w/ 2 branches and 2 members at h = 4.39]
           |  |           |  |  |--leaf "Sample-146" 
           |  |           |  |  `--leaf "Sample-162" 
           |  |           |  `--[dendrogram w/ 2 branches and 2 members at h = 5.26]
           |  |           |     |--leaf "Sample-19" 
           |  |           |     `--leaf "Sample-156" 
           |  |           `--[dendrogram w/ 2 branches and 6 members at h = 6.07]
           |  |              |--[dendrogram w/ 2 branches and 2 members at h = 3.86]
           |  |              |  |--leaf "Sample-211" 
           |  |              |  `--leaf "Sample-219" 
           |  |              `--[dendrogram w/ 2 branches and 4 members at h = 4.63]
           |  |                 |--leaf "Sample-169" 
           |  |                 `--[dendrogram w/ 2 branches and 3 members at h = 2.77]
           |  |                    |--leaf "Sample-174" 
           |  |                    `--[dendrogram w/ 2 branches and 2 members at h = 2.53]
           |  |                       |--leaf "Sample-178" 
           |  |                       `--leaf "Sample-184" 
           |  `--[dendrogram w/ 2 branches and 25 members at h = 21.3]
           |     |--[dendrogram w/ 2 branches and 17 members at h = 11.5]
           |     |  |--[dendrogram w/ 2 branches and 8 members at h = 9.71]
           |     |  |  |--[dendrogram w/ 2 branches and 5 members at h = 6.95]
           |     |  |  |  |--[dendrogram w/ 2 branches and 2 members at h = 4.13]
           |     |  |  |  |  |--leaf "Sample-78" 
           |     |  |  |  |  `--leaf "Sample-106" 
           |     |  |  |  `--[dendrogram w/ 2 branches and 3 members at h = 4.16]
           |     |  |  |     |--leaf "Sample-62" 
           |     |  |  |     `--[dendrogram w/ 2 branches and 2 members at h = 3.62]
           |     |  |  |        |--leaf "Sample-82" 
           |     |  |  |        `--leaf "Sample-74" 
           |     |  |  `--[dendrogram w/ 2 branches and 3 members at h = 5.25]
           |     |  |     |--[dendrogram w/ 2 branches and 2 members at h = 3.38]
           |     |  |     |  |--leaf "Sample-196" 
           |     |  |     |  `--leaf "Sample-198" 
           |     |  |     `--leaf "Sample-204" 
           |     |  `--[dendrogram w/ 2 branches and 9 members at h = 7.6]
           |     |     |--[dendrogram w/ 2 branches and 5 members at h = 5.8]
           |     |     |  |--[dendrogram w/ 2 branches and 3 members at h = 3.88]
           |     |     |  |  |--[dendrogram w/ 2 branches and 2 members at h = 2.06]
           |     |     |  |  |  |--leaf "Sample-202" 
           |     |     |  |  |  `--leaf "Sample-200" 
           |     |     |  |  `--leaf "Sample-206" 
           |     |     |  `--[dendrogram w/ 2 branches and 2 members at h = 4.53]
           |     |     |     |--leaf "Sample-125" 
           |     |     |     `--leaf "Sample-189" 
           |     |     `--[dendrogram w/ 2 branches and 4 members at h = 5.26]
           |     |        |--[dendrogram w/ 2 branches and 3 members at h = 4.13]
           |     |        |  |--[dendrogram w/ 2 branches and 2 members at h = 3.12]
           |     |        |  |  |--leaf "Sample-192" 
           |     |        |  |  `--leaf "Sample-159" 
           |     |        |  `--leaf "Sample-181" 
           |     |        `--leaf "Sample-38" 
           |     `--[dendrogram w/ 2 branches and 8 members at h = 8.76]
           |        |--[dendrogram w/ 2 branches and 2 members at h = 2.78]
           |        |  |--leaf "Sample-47" 
           |        |  `--leaf "Sample-46" 
           |        `--[dendrogram w/ 2 branches and 6 members at h = 6.91]
           |           |--[dendrogram w/ 2 branches and 4 members at h = 5.32]
           |           |  |--[dendrogram w/ 2 branches and 2 members at h = 3.39]
           |           |  |  |--leaf "Sample-145" 
           |           |  |  `--leaf "Sample-135" 
           |           |  `--[dendrogram w/ 2 branches and 2 members at h = 4.2]
           |           |     |--leaf "Sample-114" 
           |           |     `--leaf "Sample-37" 
           |           `--[dendrogram w/ 2 branches and 2 members at h = 1.5]
           |              |--leaf "Sample-124" 
           |              `--leaf "Sample-120" 
           `--[dendrogram w/ 2 branches and 65 members at h = 18.8]
              |--[dendrogram w/ 2 branches and 51 members at h = 16.7]
              |  |--[dendrogram w/ 2 branches and 13 members at h = 11.6]
              |  |  |--[dendrogram w/ 2 branches and 7 members at h = 11.4]
              |  |  |  |--[dendrogram w/ 2 branches and 4 members at h = 5.66]
              |  |  |  |  |--[dendrogram w/ 2 branches and 2 members at h = 4.24]
              |  |  |  |  |  |--leaf "Sample-127" 
              |  |  |  |  |  `--leaf "Sample-45" 
              |  |  |  |  `--[dendrogram w/ 2 branches and 2 members at h = 3.79]
              |  |  |  |     |--leaf "Sample-134" 
              |  |  |  |     `--leaf "Sample-144" 
              |  |  |  `--[dendrogram w/ 2 branches and 3 members at h = 2.83]
              |  |  |     |--leaf "Sample-140" 
              |  |  |     `--[dendrogram w/ 2 branches and 2 members at h = 2.18]
              |  |  |        |--leaf "Sample-41" 
              |  |  |        `--leaf "Sample-130" 
              |  |  `--[dendrogram w/ 2 branches and 6 members at h = 5.41]
              |  |     |--[dendrogram w/ 2 branches and 3 members at h = 3.55]
              |  |     |  |--leaf "Sample-119" 
              |  |     |  `--[dendrogram w/ 2 branches and 2 members at h = 1.68]
              |  |     |     |--leaf "Sample-131" 
              |  |     |     `--leaf "Sample-141" 
              |  |     `--[dendrogram w/ 2 branches and 3 members at h = 4.24]
              |  |        |--[dendrogram w/ 2 branches and 2 members at h = 2.89]
              |  |        |  |--leaf "Sample-33" 
              |  |        |  `--leaf "Sample-110" 
              |  |        `--leaf "Sample-42" 
              |  `--[dendrogram w/ 2 branches and 38 members at h = 13.8]
              |     |--[dendrogram w/ 2 branches and 18 members at h = 12.5]
              |     |  |--[dendrogram w/ 2 branches and 12 members at h = 9.53]
              |     |  |  |--[dendrogram w/ 2 branches and 8 members at h = 7.75]
              |     |  |  |  |--[dendrogram w/ 2 branches and 3 members at h = 4.43]
              |     |  |  |  |  |--[dendrogram w/ 2 branches and 2 members at h = 2.9]
              |     |  |  |  |  |  |--leaf "Sample-48" 
              |     |  |  |  |  |  `--leaf "Sample-148" 
              |     |  |  |  |  `--leaf "Sample-1" 
              |     |  |  |  `--[dendrogram w/ 2 branches and 5 members at h = 6.59]
              |     |  |  |     |--[dendrogram w/ 2 branches and 3 members at h = 5.02]
              |     |  |  |     |  |--leaf "Sample-39" 
              |     |  |  |     |  `--[dendrogram w/ 2 branches and 2 members at h = 0.84]
              |     |  |  |     |     |--leaf "Sample-154" 
              |     |  |  |     |     `--leaf "Sample-116" 
              |     |  |  |     `--[dendrogram w/ 2 branches and 2 members at h = 3.48]
              |     |  |  |        |--leaf "Sample-36" 
              |     |  |  |        `--leaf "Sample-113" 
              |     |  |  `--[dendrogram w/ 2 branches and 4 members at h = 5.04]
              |     |  |     |--[dendrogram w/ 2 branches and 2 members at h = 2.61]
              |     |  |     |  |--leaf "Sample-177" 
              |     |  |     |  `--leaf "Sample-173" 
              |     |  |     `--[dendrogram w/ 2 branches and 2 members at h = 2.78]
              |     |  |        |--leaf "Sample-73" 
              |     |  |        `--leaf "Sample-155" 
              |     |  `--[dendrogram w/ 2 branches and 6 members at h = 4.78]
              |     |     |--[dendrogram w/ 2 branches and 3 members at h = 3.48]
              |     |     |  |--[dendrogram w/ 2 branches and 2 members at h = 2.64]
              |     |     |  |  |--leaf "Sample-157" 
              |     |     |  |  `--leaf "Sample-164" 
              |     |     |  `--leaf "Sample-179" 
              |     |     `--[dendrogram w/ 2 branches and 3 members at h = 3.03]
              |     |        |--[dendrogram w/ 2 branches and 2 members at h = 1.74]
              |     |        |  |--leaf "Sample-171" 
              |     |        |  `--leaf "Sample-185" 
              |     |        `--leaf "Sample-175" 
              |     `--[dendrogram w/ 2 branches and 20 members at h = 9.49]
              |        |--[dendrogram w/ 2 branches and 14 members at h = 7.23]
              |        |  |--[dendrogram w/ 2 branches and 10 members at h = 6.85]
              |        |  |  |--[dendrogram w/ 2 branches and 4 members at h = 6.57]
              |        |  |  |  |--[dendrogram w/ 2 branches and 2 members at h = 5.1]
              |        |  |  |  |  |--leaf "Sample-183" 
              |        |  |  |  |  `--leaf "Sample-167" 
              |        |  |  |  `--[dendrogram w/ 2 branches and 2 members at h = 4.44]
              |        |  |  |     |--leaf "Sample-32" 
              |        |  |  |     `--leaf "Sample-109" 
              |        |  |  `--[dendrogram w/ 2 branches and 6 members at h = 5.64]
              |        |  |     |--[dendrogram w/ 2 branches and 3 members at h = 3.15]
              |        |  |     |  |--[dendrogram w/ 2 branches and 2 members at h = 2.53]
              |        |  |     |  |  |--leaf "Sample-195" 
              |        |  |     |  |  `--leaf "Sample-197" 
              |        |  |     |  `--leaf "Sample-203" 
              |        |  |     `--[dendrogram w/ 2 branches and 3 members at h = 1.71]
              |        |  |        |--[dendrogram w/ 2 branches and 2 members at h = 1.4]
              |        |  |        |  |--leaf "Sample-199" 
              |        |  |        |  `--leaf "Sample-205" 
              |        |  |        `--leaf "Sample-201" 
              |        |  `--[dendrogram w/ 2 branches and 4 members at h = 3.85]
              |        |     |--leaf "Sample-160" 
              |        |     `--[dendrogram w/ 2 branches and 3 members at h = 2.41]
              |        |        |--leaf "Sample-188" 
              |        |        `--[dendrogram w/ 2 branches and 2 members at h = 1.1]
              |        |           |--leaf "Sample-191" 
              |        |           `--leaf "Sample-194" 
              |        `--[dendrogram w/ 2 branches and 6 members at h = 5.37]
              |           |--[dendrogram w/ 2 branches and 3 members at h = 2.5]
              |           |  |--leaf "Sample-72" 
              |           |  `--[dendrogram w/ 2 branches and 2 members at h = 1.38]
              |           |     |--leaf "Sample-153" 
              |           |     `--leaf "Sample-152" 
              |           `--[dendrogram w/ 2 branches and 3 members at h = 4.28]
              |              |--[dendrogram w/ 2 branches and 2 members at h = 2.61]
              |              |  |--leaf "Sample-150" 
              |              |  `--leaf "Sample-71" 
              |              `--leaf "Sample-151" 
              `--[dendrogram w/ 2 branches and 14 members at h = 12.5]
                 |--[dendrogram w/ 2 branches and 8 members at h = 7.08]
                 |  |--[dendrogram w/ 2 branches and 6 members at h = 5.35]
                 |  |  |--[dendrogram w/ 2 branches and 2 members at h = 1.41]
                 |  |  |  |--leaf "Sample-128" 
                 |  |  |  `--leaf "Sample-122" 
                 |  |  `--[dendrogram w/ 2 branches and 4 members at h = 3.09]
                 |  |     |--[dendrogram w/ 2 branches and 3 members at h = 2.79]
                 |  |     |  |--[dendrogram w/ 2 branches and 2 members at h = 1.94]
                 |  |     |  |  |--leaf "Sample-138" 
                 |  |     |  |  `--leaf "Sample-139" 
                 |  |     |  `--leaf "Sample-70" 
                 |  |     `--leaf "Sample-68" 
                 |  `--[dendrogram w/ 2 branches and 2 members at h = 5.12]
                 |     |--leaf "Sample-117" 
                 |     `--leaf "Sample-166" 
                 `--[dendrogram w/ 2 branches and 6 members at h = 6.27]
                    |--[dendrogram w/ 2 branches and 4 members at h = 4.93]
                    |  |--leaf "Sample-186" 
                    |  `--[dendrogram w/ 2 branches and 3 members at h = 3.18]
                    |     |--[dendrogram w/ 2 branches and 2 members at h = 2.18]
                    |     |  |--leaf "Sample-172" 
                    |     |  `--leaf "Sample-176" 
                    |     `--leaf "Sample-170" 
                    `--[dendrogram w/ 2 branches and 2 members at h = 3.85]
                       |--leaf "Sample-158" 
                       `--leaf "Sample-180" 

---

    Code
      str(p@row_dend_param$obj)
    Output
      --[dendrogram w/ 2 branches and 30 members at h = 91.4]
        |--[dendrogram w/ 2 branches and 8 members at h = 40.2]
        |  |--[dendrogram w/ 2 branches and 7 members at h = 38.2]
        |  |  |--leaf "Prevotella oralis et rel." 
        |  |  `--[dendrogram w/ 2 branches and 6 members at h = 27.8]
        |  |     |--leaf "Clostridium cellulosi et rel." 
        |  |     `--[dendrogram w/ 2 branches and 5 members at h = 16.8]
        |  |        |--leaf "Sporobacter termitidis et rel." 
        |  |        `--[dendrogram w/ 2 branches and 4 members at h = 14]
        |  |           |--[dendrogram w/ 2 branches and 2 members at h = 11.6]
        |  |           |  |--leaf "Clostridium orbiscindens et rel." 
        |  |           |  `--leaf "Ruminococcus obeum et rel." 
        |  |           `--[dendrogram w/ 2 branches and 2 members at h = 13.7]
        |  |              |--leaf "Subdoligranulum variable at rel." 
        |  |              `--leaf "Clostridium symbiosum et rel." 
        |  `--leaf "Bacteroides vulgatus et rel." 
        `--[dendrogram w/ 2 branches and 22 members at h = 49.2]
           |--[dendrogram w/ 2 branches and 7 members at h = 31.6]
           |  |--[dendrogram w/ 2 branches and 3 members at h = 15.6]
           |  |  |--[dendrogram w/ 2 branches and 2 members at h = 14.8]
           |  |  |  |--leaf "Allistipes et rel." 
           |  |  |  `--leaf "Parabacteroides distasonis et rel." 
           |  |  `--leaf "Bacteroides fragilis et rel." 
           |  `--[dendrogram w/ 2 branches and 4 members at h = 22.8]
           |     |--leaf "Bacteroides uniformis et rel." 
           |     `--[dendrogram w/ 2 branches and 3 members at h = 12.2]
           |        |--leaf "Prevotella tannerae et rel." 
           |        `--[dendrogram w/ 2 branches and 2 members at h = 8.81]
           |           |--leaf "Tannerella et rel." 
           |           `--leaf "Bacteroides plebeius et rel." 
           `--[dendrogram w/ 2 branches and 15 members at h = 39.7]
              |--[dendrogram w/ 2 branches and 5 members at h = 29.5]
              |  |--[dendrogram w/ 2 branches and 2 members at h = 10.2]
              |  |  |--leaf "Streptococcus bovis et rel." 
              |  |  `--leaf "Streptococcus mitis et rel." 
              |  `--[dendrogram w/ 2 branches and 3 members at h = 24.6]
              |     |--[dendrogram w/ 2 branches and 2 members at h = 20.7]
              |     |  |--leaf "Phascolarctobacterium faecium et rel." 
              |     |  `--leaf "Escherichia coli et rel." 
              |     `--leaf "Uncultured Mollicutes" 
              `--[dendrogram w/ 2 branches and 10 members at h = 27.4]
                 |--[dendrogram w/ 2 branches and 7 members at h = 19.2]
                 |  |--[dendrogram w/ 2 branches and 4 members at h = 15.7]
                 |  |  |--leaf "Lachnobacillus bovis et rel." 
                 |  |  `--[dendrogram w/ 2 branches and 3 members at h = 12.3]
                 |  |     |--[dendrogram w/ 2 branches and 2 members at h = 9.93]
                 |  |     |  |--leaf "Clostridium nexile et rel." 
                 |  |     |  `--leaf "Outgrouping clostridium cluster XIVa" 
                 |  |     `--leaf "Lachnospira pectinoschiza et rel." 
                 |  `--[dendrogram w/ 2 branches and 3 members at h = 14.7]
                 |     |--[dendrogram w/ 2 branches and 2 members at h = 10.8]
                 |     |  |--leaf "Clostridium sphenoides et rel." 
                 |     |  `--leaf "Bryantella formatexigens et rel." 
                 |     `--leaf "Anaerostipes caccae et rel." 
                 `--[dendrogram w/ 2 branches and 3 members at h = 20.1]
                    |--[dendrogram w/ 2 branches and 2 members at h = 17.6]
                    |  |--leaf "Bifidobacterium" 
                    |  `--leaf "Uncultured Clostridiales II" 
                    `--leaf "Akkermansia" 

