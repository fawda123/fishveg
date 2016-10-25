# README



#### Files

All data created in `R\dat_proc.R`.  Source data in the ignore folder were created elsewhere.

* `fish_all.RData` DNR trapnet/gillnet fish data for all years, months, and survey types.  Dat are one unique record per fish.  Carp, black bullhead, yellow bullhead, bluegill, black crappie, white crappie, yellow perch, northern pike, and walleye are identified, all other species labelled as 'other'. Total fish length is reported in mm. Data are only for July, August, September, all survey types. Includes true zeroes. 

* `fish_dat.RData` Same as `fish_all.RData` but data are converted to CPUE, surveys are Jul/Aug/Sep, and 'standard population assessments' and 'resurveys'.  CPUE is estimated as total fish weight (kg) divided by effort, unique to species, date, lake, and gear type.  CPUE was estimated separately for trapnet, gillnet. Length to weight equations were from the Handbook of Freshwater Fishery Biology. The arguments to `cpue_fun` show the species and gear type combos, including parameters for length/weight conversions.  Bullhead are black and yellow bullhead combined, and crappie are white and black crappie combined. Species are not separated by adult or yoy. 

* `fishveg_dat.RData` combined fisheries and veg data, veg data summarized by total rich and subm rich for each lake.  Fish and veg data combined if the survey was in the same year. Covariates for each lake include UTM coordinates, ecoregion, watershed area, lake depth, lake area, percent human development in watershed, SDI, and secchi depth.   

* `map_dat.RData` Several R objects for creating plots. 

* `veg_dat.RData` DNR veg transect data from 1992 to present. Format is dow, date, transect, species, and abundance category.  NULL abundance entries are not removed, these are species in the survey but not observed on a transect.  Note that there were no lakes in the dataset that had zero veg.  

#### Exploratory analysis

![](README_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
  
![](README_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

![](README_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
data(map_dat)

dat <- select(fishdat, 
  S_rich, common.carp_GN, black.bullhead_TN, bluegill_TN, secchim, sdi, phuman, aream2, shedaream2
  ) %>% 
  mutate(
    CAP = as.character(1 * (common.carp_GN > 0)),
    BHD = as.character(1 * (black.bullhead_TN > 0))
  ) %>% 
  unite(fish_cat, BHD, CAP) %>% 
  mutate(
    fish_cat = factor(fish_cat, 
      levels = c('1_1', '1_0', '0_1', '0_0'),
      labels = c('BHD_CAP', 'BHD_only', 'CAP_only', 'none')
      )
    ) %>% 
  select(-common.carp_GN, -black.bullhead_TN)
lks <- dat$fish_cat
dat <- select(dat, -fish_cat) %>%
  decostand(dat, method = 'log', logbase = 10)

# pca  
pcamod <- prcomp(dat)

# tiff('fig4.tif', height = 6, width = 8, units = 'in', compression = 'lzw', res = 300, family = 'serif')
ggord(pcamod, lks, vec_ext = 3, size = 3, alpha = 0.8)
```

![](README_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
dev.off()
```

```
## null device 
##           1
```

```r
# nms
nmsmod <- metaMDS(dat, distance = 'bray', trace = 0, autotransform = F, k = 2)

tiff('fig5.tif', height = 6, width = 8, units = 'in', compression = 'lzw', res = 300, family = 'serif')
ggord(nmsmod, lks, size = 3, alpha = 0.8)
# dev.off()
```
