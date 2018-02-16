---
output:
  html_document:
    keep_md: yes
    toc: yes
    self_contained: no
    code_folding: hide
---

# README


```r
library(tidyr)
library(ggplot2)
library(purrr)
library(dplyr)
library(stargazer)
library(vegan)
library(maptools)
library(ggsn)
library(knitr)
library(ggord)
library(gridExtra)
library(FactoMineR)
source('R/funcs.R')

opts_chunk$set(fig.align = 'center', message = F, echo = T, cache = F, dev = 'png', dev.args = list(family = 'serif'), dpi = 400, fig.pos = '!h', warning = F, background = 'white', out.width = '60%',
  fig.process = function(x) {
  x2 = sub('-\\d+([.][a-z]+)$', '\\1', x)
  if (file.rename(x, x2)) x2 else x
  })
```

## Files

All data created in `R\dat_proc.R`.  Source data in the ignore folder were created elsewhere.

* `country.RData` SpatialPolygonsDataFrame of US conterminous state borders

* `fish_dat.RData` Same as `fish_all.RData` but data are converted to CPUE, surveys are Jul/Aug/Sep, and 'standard population assessments' and 'resurveys'.  CPUE is estimated as total fish weight (kg) divided by effort, unique to species, date, lake, and gear type.  CPUE was estimated separately for trapnet, gillnet. Length to weight equations were from the Handbook of Freshwater Fishery Biology. The arguments to `cpue_fun` show the species and gear type combos, including parameters for length/weight conversions.  Bullhead are black and yellow bullhead combined, and crappie are white and black crappie combined. Species are not separated by adult or yoy. 

* `fishveg_dat.RData` combined fisheries and veg data, veg data summarized by total rich and subm rich for each lake.  Fish and veg data combined if the survey was in the same year. Covariates for each lake include UTM coordinates, ecoregion, watershed area, lake depth, lake area, percent human development in watershed, SDI, and secchi depth.   

* `map_dat.RData` Several R objects for creating plots. 

* `mnstate.RData` SpatialPolygonsDataFrame of MN state boundaries

* `veg_dat.RData` DNR veg transect data from 1992 to present. Format is dow, date, transect, species, and abundance category.  NULL abundance entries are not removed, these are species in the survey but not observed on a transect.  Note that there were no lakes in the dataset that had zero veg.  

## Summary table


```r
# diagnostic plots
data(map_dat)

fishdat <- 

# format data
totab <- select(fishdat, 
                S_rich, common.carp_GN, black.bullhead_TN, bluegill_TN, secchim, sdi, phuman, aream2, shedaream2, ecoreg, depthm
) %>% 
  mutate(
    ecoreg = factor(ecoreg, levels = c('eastern temperate forests', 'great plains'), labels = c('ETF', 'GP'))
  ) %>% 
  rename(
    SpeciesRichness = S_rich, 
    Carp = common.carp_GN,
    Bullhead = black.bullhead_TN, 
    Bluegill = bluegill_TN,
    Secchi = secchim, 
    SDI = sdi, 
    Human = phuman, 
    Area = aream2,
    ShedArea = shedaream2,
    Depth = depthm, 
    Ecoregion = ecoreg
  ) %>% 
  mutate(
    Area = Area * 1e-6,
    ShedArea = ShedArea * 1e-6
  ) %>% 
  gather('Variable', 'val', -Ecoregion) %>% 
  mutate(Variable = factor(Variable, 
                      levels = c('SpeciesRichness', 'Carp', 'Bullhead', 'Bluegill', 'Area', 'Depth', 'Human', 'SDI', 'Secchi', 'ShedArea'))
         ) %>% 
  group_by(Ecoregion, Variable) %>% 
  summarise(
    Average = round(mean(val, na.rm = T), 1), 
    Median  = round(median(val, na.rm = T), 1),
    Var. = round(var(val, na.rm = T), 1), 
    Min. = round(min(val, na.rm = T), 1),
    Max. = round(max(val, na.rm = T), 1)
  ) %>% 
  unite('Min./Max.', Min., Max., sep = ' / ') %>% 
  ungroup %>% 
  mutate(
    Median = paste0('(', Median, ')'), 
    Ecoregion = ifelse(duplicated(Ecoregion), '', as.character(Ecoregion)),
    Var. = as.character(Var.)
  ) %>% 
  unite('Ave. (Med.)', Average, Median, sep = ' ')

write.csv(totab, 'ignore/summtab.csv', quote = F, row.names = F)
kable(totab)
```



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


```r
merged_2<-read.csv("ignore/merged_2.csv")
d <- merged_2[,c("common.carp_GN", "black.bullhead_TN", "bluegill_TN", 
                 "secchim", "sdi", "phuman", "aream2", "shedaream2", "depthm")]
names(d) <- c("Carp", "Bullhead", "Bluegill", "Secchi", "SDI", 
              "Human", "Area", "ShedArea", "Depth")
d <- log(1 + d)
vif_func(d)
```

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

## Fig 1

```r
library(tidyverse)
library(maptools)
library(gridExtra)
library(grid)
library(RColorBrewer)
load(file = 'data/map_dat.RData')
load(file = 'data/country.RData')
load(file = 'data/mnstate.RData')

######
# maps

# get legend from an existing ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# format data for ggplot
state <- fortify(state)
ecoregs <- fortify(ecoregs)
counties <- fortify(counties)
ecoregs <- ecoregs[ecoregs$id %in% c(3, 4), ]
ecoregs$Ecoregion <- factor(ecoregs$id, levels = c(3, 4), 
  labels = c("ETF", "GP"))

# format fish data to same scale
brks <- c(-Inf, 0, 1, 2, 5, 10, Inf)
labs <- c('0', '< 1',  '< 2', '< 5', '< 10', '> 10')
toplo <- select(fishdat, utmx, utmy, common.carp_GN, black.bullhead_TN) %>% 
  mutate(
    common.carp_GN = cut(common.carp_GN, breaks = brks, labels = labs),
    black.bullhead_TN = cut(black.bullhead_TN, breaks = brks, labels = labs)
    )

# legend labels, color palette
leglabs <- list(expression(0), expression(phantom('')<=1), expression(phantom('')<=2), expression(phantom('')<=5), expression(phantom('')<=10), expression(phantom('')>10))
cols <- colorRampPalette(c('darkgrey', 'black'))(length(leglabs))

# carp cpue GN
p1 <- ggplot(state, aes(x = long, y = lat)) + 
  geom_polygon(data = counties, aes(x = long, y = lat, group = group), 
    fill = NA, colour = 'grey') +
  geom_polygon(data = ecoregs, aes(x = long, y = lat, group = group, fill= Ecoregion),  
    alpha = 0.5) +
  geom_polygon(fill = NA, colour = 'black') +
  geom_point(data = toplo, aes(x = utmx, y = utmy, size = common.carp_GN, color = common.carp_GN), alpha = 0.8) +
  scale_size_discrete('Fish kg/net', range = c(2, 12), labels = leglabs) + 
  scale_colour_manual('Fish kg/net', values = cols, labels = leglabs) +
  theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(), 
          legend.box.just = 'right',
        legend.position = c(0.9, 0.37),
        plot.title = element_text(size = 18)
    ) +
  coord_equal() + 
  ggtitle('Common Carp') + 
  scalebar(location = 'topright', y.min = 5372428, y.max = 5472428, x.min = 761655.1, x.max = 761655.1, 
    dist=100, st.dist=.1, st.size = 4, height = 0.1)

# black bullhead CPUE TN
p2 <- ggplot(state, aes(x = long, y = lat)) + 
  geom_polygon(data = counties, aes(x = long, y = lat, group = group), 
    fill = NA, colour = 'grey') +
  geom_polygon(data = ecoregs, aes(x = long, y = lat, group = group, fill= Ecoregion),  
    alpha = 0.5) +
  geom_polygon(fill = NA, colour = 'black') +
  geom_point(data = toplo, aes(x = utmx, y = utmy, size = black.bullhead_TN, color = black.bullhead_TN), alpha = 0.8) +
  scale_size_discrete('Bull kg/net', range = c(2, 12), labels = leglabs) + 
  scale_colour_manual('Bull kg/net', values = cols, labels = leglabs) +
  theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(), 
          legend.box.just = 'right', 
        legend.position = 'none', 
        plot.title = element_text(size = 18)
    ) +
  coord_equal() +
  ggtitle('Black Bullhead')

# inset
# inset map
country <- thinnedSpatialPoly(country, tolerance = 10000, topologyPreserve = TRUE, 
                              avoidGEOS = FALSE)
pinset <- ggplot(country, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = NA, colour = 'grey') +
  geom_polygon(data = mnstate, aes(x = long, y = lat, group = group), 
    fill = 'black') + 
  theme_classic() + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.background=element_blank(),panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),plot.background=element_blank()#, 
          # legend.position = 'none'
    ) +
  coord_equal()

# save
# tiff('map.tif', height = 7, width = 12, units = 'in', compression = 'lzw', res = 300, family = 'serif')
grid.newpage()
v1 <- viewport(width = 1, height = 1, x = 0.25, y = 0.5) 
v2 <- viewport(width = 1, height = 1, x = 0.75, y = 0.5) 
v3 <- viewport(width = 0.28, height = 0.28, x = 0.89, y = 0.87)
print(p1, vp = v1) 
print(p2, vp = v2)
print(pinset, vp = v3)
```

<img src="README_files/figure-html/Fig1.png" width="60%" style="display: block; margin: auto;" />

```r
# dev.off()
```

## Fig 2

```r
######
# diagnostic plots
data(map_dat)

rev_fishdat <- read.csv("ignore/d2.csv")

# format data
toplo <- select(rev_fishdat, 
  SpeciesRichness, Carp, Bullhead, Bluegill, Secchi, SDI, Human, Area, ShedArea, Ecoregion, Depth
  ) %>% 
  rename(SI = SDI) %>% 
  mutate(
    Carp = 1 + Carp, 
    Bullhead = 1 + Bullhead, 
    Bluegill = 1 + Bluegill,
    Ecoregion = factor(Ecoregion, levels = c('Forest', 'Plain'), labels = c('Eastern Temperate Forests', 'Great Plains'))
  ) %>% 
  gather('var', 'val', -SpeciesRichness, -Ecoregion)
  
# create plot
p3 <- ggplot(toplo, aes(x = val, y = SpeciesRichness, fill = Ecoregion, colour = Ecoregion)) + 
  geom_point(pch = 21, alpha = 0.8) + 
  scale_x_log10('Value, log10') +
  scale_y_continuous('Plant richness', expand = c(0, 0)) + 
  stat_smooth(method ='lm', colour = 'black', se = TRUE) + 
  facet_wrap( ~ var, ncol = 9, scales = 'free_x') +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    strip.background = element_rect(fill = "white")
    )

# save
# tiff('fig3.tif', height = 5, width = 12, units = 'in', compression = 'lzw', res = 300, family = 'serif')
p3
```

<img src="README_files/figure-html/Fig2.png" width="60%" style="display: block; margin: auto;" />

```r
# dev.off()
```

## Fig 3

```r
rev_fishdat <- read.csv("ignore/d2.csv")

# format data
d<- select(rev_fishdat, 
  SpeciesRichness, Carp, Bullhead, Bluegill, Secchi, SDI, Human, Area, ShedArea, Ecoregion, Depth
  ) %>% 
  rename(SI = SDI) %>% 
  mutate(
    Bluegill = 1 + Bluegill,
    Ecoregion = factor(Ecoregion, levels = c('Forest', 'Plain'), labels = c('Eastern Temperate Forests', 'Great Plains'))
  )
  
d <- d %>% 
  mutate(
    carp_cat = ifelse(Carp > 0, 'C_prs', 'C_abs'),
    bull_cat = ifelse(Bullhead > 0 , 'B_prs', 'B_abs'),
    abuall = Carp + Bullhead
    ) %>% 
  unite('Group', carp_cat, bull_cat, sep = ', ')

tomod <- d %>% 
  select(Bluegill, Depth, Secchi, Area, SI, ShedArea, Human) %>% 
  decostand(method = 'standardize')
ppp <- PCA(tomod, scale.unit = F, graph = F)
dimdesc(ppp, axes = c(1:3))
```

```
## $Dim.1
## $Dim.1$quanti
##          correlation      p.value
## Area       0.7003128 3.807019e-37
## SI         0.6913952 7.004454e-36
## Depth      0.6681614 8.605820e-33
## Secchi     0.5682525 3.508253e-22
## ShedArea   0.5148838 7.483030e-18
## Bluegill   0.3366858 7.480955e-08
## Human     -0.3085629 9.321755e-07
## 
## 
## $Dim.2
## $Dim.2$quanti
##          correlation      p.value
## ShedArea   0.6014935 2.666295e-25
## Area       0.5103664 1.609032e-17
## Human      0.4193134 9.127571e-12
## SI         0.4108897 2.576117e-11
## Bluegill  -0.4404832 5.903931e-13
## Depth     -0.4946689 2.110547e-16
## Secchi    -0.6035959 1.645328e-25
## 
## 
## $Dim.3
## $Dim.3$quanti
##          correlation      p.value
## Human      0.8010119 1.271334e-55
## Depth      0.3189660 3.777135e-07
## ShedArea   0.2441587 1.206597e-04
## Secchi     0.1748791 6.273497e-03
## Bluegill   0.1491421 2.002227e-02
## SI        -0.2379305 1.812935e-04
```

```r
mythm <- theme_bw(base_family = 'serif') +
  theme(
  strip.background = element_blank(), 
  plot.margin = margin(0, 2, 0, 2, "pt")
) 

##
# carp + bullhead and none lakes, separate points for carp/bullhead, jittered, no jitter
rngs <- c(1, 7)
    
brks <- c(-Inf, 0, 1, 5, 10, Inf)
labs <- c('0', '< 1', '< 5', '< 10', '> 10')

locs <- ppp %>% 
  .$ind %>% 
  .$coord %>% 
  as.data.frame %>% 
  select(Dim.1, Dim.2) %>% 
  rename(
    one = Dim.1, 
    two = Dim.2
  ) %>% 
  rownames_to_column('id')

toplo <- d %>% 
  select(Carp, Bullhead) %>% 
  rownames_to_column('id') %>% 
  gather('var', 'val', -id) %>% 
  group_by(id) %>% 
  nest %>% 
  mutate(var = map(data, function(x){
  
    if(sum(x$val) == 0)
      rep('None', 2)
    else 
      x$var
    
    } 
  )) %>% 
  unnest %>% 
  mutate(valcut = cut(val, breaks = brks, labels = labs)) %>% 
  select(id, var, val, valcut) %>% 
  left_join(locs, by = 'id') %>% 
  rename(Group = var)

# format fish data to same scale

# legend labels, color palette
leglabs <- list(expression(0), expression(phantom('')<=1), expression(phantom('')<=5), expression(phantom('')<=10), expression(phantom('')>10))

p <- ggord(ppp, vec_ext = 7, size = -1, txt = 4, arrow = 0.3, 
      repel = T, ellipse = F) + 
  geom_point(data = toplo, aes(size = valcut, fill = Group), pch = 21, alpha = 1) +
  scale_size_manual('Fish kg/net', values = c(1.5, 2.25, 3, 3.75, 5.5), labels = leglabs) +
  scale_fill_manual(values = c('lightblue', 'tomato1', 'yellow2')) + 
  mythm +
  guides(fill = guide_legend(override.aes = list(size = 3)))
# tiff('fig3.tif', height = 5, width = 4, units = 'in', compression = 'lzw', res = 300, family = 'serif')
p
```

<img src="README_files/figure-html/Fig3.png" width="60%" style="display: block; margin: auto;" />

```r
# dev.off()
```

## MRRP all groups

```r
# MRPP to test differences among groups (by mean and/or variance of dissimilarities)
# significant values means that groupings were moree different than would be expected by chance
# delta statistics for each group indicates relative dissimilarity within groups
# higher delta is more dissimilar within group
mrpp(tomod, grouping = d$Group)
```

```
## 
## Call:
## mrpp(dat = tomod, grouping = d$Group) 
## 
## Dissimilarity index: euclidean 
## Weights for groups:  n 
## 
## Class means and counts:
## 
##       C_abs, B_abs C_abs, B_prs C_prs, B_abs C_prs, B_prs
## delta 3.103        3.904         3.34        3.161       
## n     20           64           28           131         
## 
## Chance corrected within-group agreement A: 0.03475 
## Based on observed delta 3.372 and expected delta 3.494 
## 
## Significance of delta: 0.001 
## Permutation: free
## Number of permutations: 999
```

## MRPP multiple comparisons

```r
# pairwise comparisons of groups with MRPP
grps <- d$Group %>% unique
grps <- combn(grps, 2)
pval <- rep(NA, ncol(grps))
aval <- rep(NA, ncol(grps))
for(col in 1:ncol(grps)){
  grp <- d$Group %in% grps[, col, drop = TRUE]
  res <- mrpp(tomod[grp, ], d$Group[grp])
  pval[col] <- res$Pvalue
  aval[col] <- res$A
}

# adjust p-values using holm sequential bonferroni
pval <- p.adjust(pval, method = 'holm')

# pval as t/f using bonferroni correction
vecs <- rep(FALSE, ncol(grps))
vecs[pval < 0.05] <- TRUE
names(vecs) <- paste(grps[1, ], grps[2, ], sep = '-')

# group membership based on multiple comparisons
multcompView::multcompLetters(vecs)$Letters
```

```
## C_prs, B_prs C_abs, B_prs C_prs, B_abs C_abs, B_abs 
##          "a"          "b"          "b"          "b"
```

```r
rbind(grps, round(aval, 4))
```

```
##      [,1]           [,2]           [,3]           [,4]          
## [1,] "C_prs, B_prs" "C_prs, B_prs" "C_prs, B_prs" "C_abs, B_prs"
## [2,] "C_abs, B_prs" "C_prs, B_abs" "C_abs, B_abs" "C_prs, B_abs"
## [3,] "0.0206"       "0.0295"       "0.0216"       "0.0099"      
##      [,5]           [,6]          
## [1,] "C_abs, B_prs" "C_prs, B_abs"
## [2,] "C_abs, B_abs" "C_abs, B_abs"
## [3,] "0.0034"       "0.0016"
```

