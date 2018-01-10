---
title: "README"
output:
  html_document:
    keep_md: yes
    toc: no
    self_contained: no
---



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


```r
library(vegan)
library(tidyverse)
library(mvabund)

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
d$AnyCarp <- ifelse(d$Carp > 1, "C", "NC") ## 1 is a value for Carp and bullhead when impact becomes visible
d$AnyBullhead <- ifelse(d$Bullhead > 1, "B", "NB")
d$Group <- factor(paste0(d$AnyCarp, "-", d$AnyBullhead))
d$Group <- relevel(d$Group, "NC-NB")
d1 <- subset(d, ShedArea < 100000000 & Area < 10000000)

# get distance matrix of fish abundance
abudist <- d1 %>% 
  select(Carp, Bullhead) %>% 
  vegdist

# lake groups
grps <- d1$Group

# exploratory plot
toplo <- d1 %>% 
  select(Carp, Bullhead, Group) %>% 
  gather('spp', 'abu', -Group) %>% 
  group_by(Group, spp) %>% 
  summarise(
    ave = mean(abu, na.rm = T), 
    vrc = var(abu, na.rm = T)
  )

ggplot(toplo, aes(x = ave, y = vrc, colour = spp, shape = Group)) + 
  geom_point(size = 6) + 
  theme_bw() + 
  scale_x_log10('Log-mean abundance') + 
  scale_y_log10('Log-variance abundance')
```

<img src="README_files/figure-html/unnamed-chunk-3.png" width="60%" style="display: block; margin: auto;" />

```r
# get multivariate homogeneity of groups
bdisper <- betadisper(abudist, grps)

# test if one or more groups is more variable than the others
# variance between group is not homogenous
permutest(bdisper)
```

```
## 
## Permutation test for homogeneity of multivariate dispersions
## Permutation: free
## Number of permutations: 999
## 
## Response: Distances
##            Df Sum Sq Mean Sq      F N.Perm Pr(>F)    
## Groups      3 2.0485 0.68283 36.375    999  0.001 ***
## Residuals 235 4.4114 0.01877                         
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# multivariate abundance
abund <- d1 %>% 
  select(Carp, Bullhead) %>% 
  mvabund

# test for group (location) effecgt
mod <- manyglm(abund ~ grps, family = 'negative.binomial')
anova(mod)
```

```
## Time elapsed: 0 hr 0 min 7 sec
```

```
## Analysis of Deviance Table
## 
## Model: manyglm(formula = abund ~ grps, family = "negative.binomial")
## 
## Multivariate test:
##             Res.Df Df.diff   Dev Pr(>Dev)    
## (Intercept)    258                           
## grps           255       3 613.3    0.001 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## Arguments:
##  Test statistics calculated assuming uncorrelated response (for faster computation) 
##  P-value calculated using 999 resampling iterations via PIT-trap resampling (to account for correlation in testing).
```

