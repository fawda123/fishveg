---
output:
  html_document:
    keep_md: yes
    toc: no
    self_contained: no
---

## README



#### Files

All data created in `R\dat_proc.R`.  Source data in the ignore folder were created elsewhere.

* `country.RData` SpatialPolygonsDataFrame of US conterminous state borders

* `fish_dat.RData` Same as `fish_all.RData` but data are converted to CPUE, surveys are Jul/Aug/Sep, and 'standard population assessments' and 'resurveys'.  CPUE is estimated as total fish weight (kg) divided by effort, unique to species, date, lake, and gear type.  CPUE was estimated separately for trapnet, gillnet. Length to weight equations were from the Handbook of Freshwater Fishery Biology. The arguments to `cpue_fun` show the species and gear type combos, including parameters for length/weight conversions.  Bullhead are black and yellow bullhead combined, and crappie are white and black crappie combined. Species are not separated by adult or yoy. 

* `fishveg_dat.RData` combined fisheries and veg data, veg data summarized by total rich and subm rich for each lake.  Fish and veg data combined if the survey was in the same year. Covariates for each lake include UTM coordinates, ecoregion, watershed area, lake depth, lake area, percent human development in watershed, SDI, and secchi depth.   

* `map_dat.RData` Several R objects for creating plots. 

* `mnstate.RData` SpatialPolygonsDataFrame of MN state boundaries

* `veg_dat.RData` DNR veg transect data from 1992 to present. Format is dow, date, transect, species, and abundance category.  NULL abundance entries are not removed, these are species in the survey but not observed on a transect.  Note that there were no lakes in the dataset that had zero veg.  

#### Summary table


Ecoregion   Variable          Ave. (Med.)    Var.      Min./Max.    
----------  ----------------  -------------  --------  -------------
ETF         SpeciesRichness   10.3 (10)      27.6      0 / 24       
            Carp              2.1 (0.4)      23        0 / 45.1     
            Bullhead          1.4 (0.1)      42.6      0 / 87.4     
            Bluegill          1.9 (1.1)      6.7       0 / 18       
            Area              2.5 (1.4)      12.3      0.1 / 22.5   
            Depth             11.2 (9.7)     50.5      1.8 / 34.2   
            Human             0.6 (0.6)      0         0.1 / 1      
            SDI               1.7 (1.5)      0.4       1.1 / 4.4    
            Secchi            1.6 (1.4)      0.7       0.3 / 5.4    
            ShedArea          112.5 (28.9)   77601.3   0.2 / 2808.1 
GP          SpeciesRichness   3.6 (3)        13.6      0 / 16       
            Carp              6.1 (3.7)      47.8      0 / 36       
            Bullhead          5.8 (1.9)      95        0 / 48.4     
            Bluegill          0.5 (0.1)      1.8       0 / 10.8     
            Area              2.3 (1.5)      6.7       0.2 / 10.9   
            Depth             4.7 (3.4)      13.7      1.5 / 25.3   
            Human             0.7 (0.8)      0         0.4 / 0.9    
            SDI               1.7 (1.5)      0.3       1 / 3.5      
            Secchi            0.8 (0.6)      0.4       0.2 / 3.5    
            ShedArea          34.1 (10.8)    2134.9    1.2 / 249.4  


```
##  var      vif             
##  Carp     1.40087938822791
##  Bullhead 1.32609969244421
##  Bluegill 1.2967275170074 
##  Secchi   2.31599398230004
##  SDI      1.34263804704789
##  Human    1.22289758460126
##  Area     1.65132535880825
##  ShedArea 1.61081633407663
##  Depth    2.36527586392161
## 
## All variables have VIF < 10, max VIF 2.37
```

```
## [1] "Carp"     "Bullhead" "Bluegill" "Secchi"   "SDI"      "Human"   
## [7] "Area"     "ShedArea" "Depth"
```

#### Figures

<img src="README_files/figure-html/Fig1.png" width="60%" style="display: block; margin: auto;" />

<img src="README_files/figure-html/Fig2.png" width="60%" style="display: block; margin: auto;" />

<img src="README_files/figure-html/Fig3.png" width="60%" style="display: block; margin: auto;" />

#### Between group differences

Using exploratory factor analysis... 


```r
library(vegan)
library(tidyverse)

# prep data
merged_2 <- read.csv("ignore/merged_2.csv")

d <- merged_2[, c(
  "S_rich", "common.carp_GN", "black.bullhead_TN", "bluegill_TN",
  "secchim", "sdi", "phuman", "aream2", "shedaream2", "ecoreg", "depthm"
)]
names(d) <- c(
  "SpeciesRichness", "Carp", "Bullhead", "Bluegill", "Secchi", "SI",
  "Human", "Area", "ShedArea", "Ecoregion", "Depth"
)
levels(d$Ecoregion) <- c("Forest", "Plain")

d <- within(d, {
  Bullhead.cut <- cut(Bullhead, c(-Inf, 0, 0.5, 1, 2, 5, 10, 20, Inf))
  Carp.cut <- cut(Carp, c(-Inf, 0, 0.5, 1, 2, 5, 10, 20, Inf))
  Bullhead.plusMin <- Bullhead + min(Bullhead[Bullhead > 0])
  Carp.plusMin <- Carp + min(Carp[Carp > 0])
  Bluegill.plusMin <- Bluegill + min(Bluegill[Bluegill > 0])
})
d$AnyCarp <- ifelse(d$Carp > 0, "C", "NC") ## 1 is a value for Carp and bullhead when impact becomes visible
d$AnyBullhead <- ifelse(d$Bullhead > 0, "B", "NB")
d$Group <- factor(paste0(d$AnyCarp, "-", d$AnyBullhead))
d$Group <- relevel(d$Group, "NC-NB")
d1 <- subset(d, ShedArea < 100000000 & Area < 10000000)

# lake groups
grps <- d1$Group

# standardized variables with lake groups
vargrp <-  d1 %>% 
  select(Depth, Secchi, Area, SI, ShedArea, Human, Bluegill) %>% 
  decostand(method = 'standardize') %>% 
  mutate(Group = grps)

# factor analysis
efagrp <- vargrp %>% 
  group_by(Group) %>% 
  nest %>% 
  mutate(
    efa = map(data, function(x){
  
      p <- 0; f <- 1
      while(p < 0.05 & f < 4){
        
        out <- factanal(x, factors = f, rotation = 'varimax')
        p <- out$PVAL
        f <- f + 1
        
      }
        
      return(out)
      
    })
  ) %>% 
  select(-data) %>% 
  deframe
efagrp
```

```
## $`C-B`
## 
## Call:
## factanal(x = x, factors = f, rotation = "varimax")
## 
## Uniquenesses:
##    Depth   Secchi     Area       SI ShedArea    Human Bluegill 
##    0.366    0.229    0.265    0.517    0.491    0.005    0.841 
## 
## Loadings:
##          Factor1 Factor2 Factor3
## Depth     0.149   0.781         
## Secchi            0.841  -0.248 
## Area      0.853                 
## SI        0.694                 
## ShedArea  0.616           0.357 
## Human            -0.159   0.983 
## Bluegill          0.394         
## 
##                Factor1 Factor2 Factor3
## SS loadings      1.620   1.508   1.159
## Proportion Var   0.231   0.215   0.166
## Cumulative Var   0.231   0.447   0.612
## 
## Test of the hypothesis that 3 factors are sufficient.
## The chi square statistic is 14.97 on 3 degrees of freedom.
## The p-value is 0.00184 
## 
## $`NC-B`
## 
## Call:
## factanal(x = x, factors = f, rotation = "varimax")
## 
## Uniquenesses:
##    Depth   Secchi     Area       SI ShedArea    Human Bluegill 
##    0.621    0.005    0.277    0.570    0.722    0.822    0.979 
## 
## Loadings:
##          Factor1 Factor2
## Depth     0.231   0.571 
## Secchi            0.997 
## Area      0.844   0.104 
## SI        0.620   0.213 
## ShedArea  0.527         
## Human    -0.339  -0.251 
## Bluegill         -0.143 
## 
##                Factor1 Factor2
## SS loadings      1.544   1.460
## Proportion Var   0.221   0.209
## Cumulative Var   0.221   0.429
## 
## Test of the hypothesis that 2 factors are sufficient.
## The chi square statistic is 7.65 on 8 degrees of freedom.
## The p-value is 0.468 
## 
## $`C-NB`
## 
## Call:
## factanal(x = x, factors = f, rotation = "varimax")
## 
## Uniquenesses:
##    Depth   Secchi     Area       SI ShedArea    Human Bluegill 
##    1.000    0.959    0.253    0.522    0.239    0.982    0.970 
## 
## Loadings:
##          Factor1
## Depth           
## Secchi   -0.203 
## Area      0.864 
## SI        0.691 
## ShedArea  0.873 
## Human     0.135 
## Bluegill -0.173 
## 
##                Factor1
## SS loadings      2.076
## Proportion Var   0.297
## 
## Test of the hypothesis that 1 factor is sufficient.
## The chi square statistic is 16.41 on 14 degrees of freedom.
## The p-value is 0.289 
## 
## $`NC-NB`
## 
## Call:
## factanal(x = x, factors = f, rotation = "varimax")
## 
## Uniquenesses:
##    Depth   Secchi     Area       SI ShedArea    Human Bluegill 
##    0.565    0.861    0.391    0.558    0.976    0.958    0.533 
## 
## Loadings:
##          Factor1
## Depth     0.660 
## Secchi    0.373 
## Area      0.781 
## SI        0.665 
## ShedArea  0.154 
## Human    -0.206 
## Bluegill  0.683 
## 
##                Factor1
## SS loadings      2.159
## Proportion Var   0.308
## 
## Test of the hypothesis that 1 factor is sufficient.
## The chi square statistic is 22.34 on 14 degrees of freedom.
## The p-value is 0.0719
```

