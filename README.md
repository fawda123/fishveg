# README



#### Files

All data created in `R\dat_proc.R`.  Source data in the ignore folder were created elsewhere.

* `fish_all.RData` DNR trapnet/gillnet fish data for all years, one unique record per fish.  Only Carp and bullhead species are identified, all other species labelled as 'other'. Total fish length is reported in mm. Data are only for July, August, September.  Includes true zeroes. 

* `fish_dat.RData` Same as `fish_all.RData` but data are converted to CPUE.  CPUE estimated as total fish weight divided by effort, unique to species, date, lake, and gear type.  CPUE was esimated separately for trapnet, gillnet, then both were summed.  Length to weight equations were from the Handbook of Freshwater Fishery Biology.

For carp, weight in kg from length in mm:
$$
log10(weight) = -4.44245 + 2.83840 * log10(length)
$$

For bullhead (yellow and black), weight in kg from length in mm:
$$
log10(weight) = -4.60512 + 2.88495 * log10(length)
$$

* `fishveg_dat.RData` combined fisheries and veg data, veg data summarized by total rich and subm rich for each lake.  Fish and veg data combined if the survey was in the same year. Covariates for each lake include UTM coordinates, ecoregion, watershed area, lake depth, lake area, percent human development in watershed, SDI, and secchi depth.   

* `veg_dat.RData` DNR veg transect data from 1992 to present. Format is dow, date, transect, species, and abundance category.  NULL abundance entries are not removed, these are species in the survey but not observed on a transect.  Note that there were no lakes in the dataset that had zero veg.  

#### Exploratory analysis

![CPUE (kg per unit effort) of adult bullhead (> 100 mm) and carp (> 150 mm) in lakes surveyed by Minnesota DNR, 1992 to through 2012.  Lakes shown are those with a vegetation survey in the same year.](README_files/figure-html/unnamed-chunk-2-1.png)


![Lake counts in DNR fisheries database with presence/absence of adult bullhead (BHD) and adult carp (CAP).](README_files/figure-html/unnamed-chunk-3-1.png)

![Mean vegetation richness for lakes categorized by presence/absence of adult bullhead (BHD) and adult carp (CAP).  Vegetation richness is summarized for all and submersed species.  95% confidence intervals for each category are also shown.](README_files/figure-html/unnamed-chunk-4-1.png)





<table style="text-align:center"><caption><strong>Categorical regression of vegetation richness for lakes with and without benthivorous fish. The first model is total vegetation richness and the second is submersed vegetation richness. Model intercepts are lakes with neither adult carp (CAP) nor adult bullhead (BHD).</strong></caption>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>Total</td><td>Submersed</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">BHD_adult1</td><td>0.153<sup>***</sup> (0.038)</td><td>0.337<sup>***</sup> (0.045)</td></tr>
<tr><td style="text-align:left">CAP_adult1</td><td>-0.576<sup>***</sup> (0.198)</td><td>-0.345 (0.234)</td></tr>
<tr><td style="text-align:left">BHD_adult1:CAP_adult1</td><td>0.070 (0.203)</td><td>-0.105 (0.239)</td></tr>
<tr><td style="text-align:left">Constant</td><td>3.163<sup>***</sup> (0.029)</td><td>2.205<sup>***</sup> (0.034)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>1,009</td><td>1,009</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.135</td><td>0.096</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.133</td><td>0.093</td></tr>
<tr><td style="text-align:left">Residual Std. Error (df = 1005)</td><td>0.519</td><td>0.612</td></tr>
<tr><td style="text-align:left">F Statistic (df = 3; 1005)</td><td>52.354<sup>***</sup></td><td>35.422<sup>***</sup></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


<table style="text-align:center"><caption><strong>Linear regression of vegetation richness versus adult bullhead and carp CPUE. The first model is total vegetation richness and the second is submersed vegetation richness.</strong></caption>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>Total</td><td>Submersed</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">log(1 + BHD_adult)</td><td>-0.016 (0.022)</td><td>0.091<sup>***</sup> (0.027)</td></tr>
<tr><td style="text-align:left">log(1 + CAP_adult)</td><td>-0.347<sup>***</sup> (0.040)</td><td>-0.228<sup>***</sup> (0.049)</td></tr>
<tr><td style="text-align:left">log(1 + BHD_adult):log(1 + CAP_adult)</td><td>0.019 (0.025)</td><td>-0.072<sup>**</sup> (0.031)</td></tr>
<tr><td style="text-align:left">Constant</td><td>3.263<sup>***</sup> (0.022)</td><td>2.369<sup>***</sup> (0.026)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>1,009</td><td>1,009</td></tr>
<tr><td style="text-align:left">R<sup>2</sup></td><td>0.203</td><td>0.139</td></tr>
<tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.200</td><td>0.137</td></tr>
<tr><td style="text-align:left">Residual Std. Error (df = 1005)</td><td>0.499</td><td>0.598</td></tr>
<tr><td style="text-align:left">F Statistic (df = 3; 1005)</td><td>85.175<sup>***</sup></td><td>54.150<sup>***</sup></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>
