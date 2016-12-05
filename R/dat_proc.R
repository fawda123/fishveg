######
# processing and combining fish/veg data

library(tidyverse)
library(forcats)
library(foreign)
library(maptools)
source('R/funcs.R')

######
# DNR fish data, in the format of CPUE all species
# from DNR data request to Al Stevens al.stevens@state.mn.us, Maggie Gorsuch maggie.gorsuch@state.mn.us
# see email 12/2/16

# import raw file
fish_all <- read_excel('ignore/Beck_Marcus_GLOBAL_CATCH_FILE.XLSX')
names(fish_all) <- tolower(names(fish_all))

# import fish names
fish_nms <- read_excel('ignore/Fishname.xlsx') %>% 
  select(CODE, `COMMON NAME`) %>% 
  rename(
    fish = CODE, 
    species_code = `COMMON NAME`
  )

# surveys to keep
grs <- c('Standard gill nets', 'Standard trap nets', '15-ft beach seine', '50-ft beach seine', 'Backpack electrofishing')

# months to keep
mos <- c(6, 7, 8, 9)

# survey types to keep
typ <- c('Population Assessment', 'Re-Survey')

# bullhead are combined here because the lenght/weight parameters are same for black and yellow
# crappie are not combined here because the length/weight parameters are different
fish_dat <- mutate(fish_all,
    month = month(survey_date)
  ) %>% 
  filter( 
    month %in% mos &
    survey_type %in% typ &
    gear %in% grs
  ) %>% 
  select(id, survey_date, species_code, gear, catch_cpue) %>% 
  rename(
    dow = id, 
    date = survey_date
  ) %>% 
  mutate(
    date = as.Date(date),
    dow = as.numeric(dow), 
    gear = factor(gear),
    gear = fct_collapse(gear,
      TN = 'Standard trap nets', 
      GN = 'Standard gill nets', 
      NS = c('15-ft beach seine', '50-ft beach seine', 'Backpack electrofishing')
    )
  ) %>% 
  filter(!is.na(dow)) %>%  # one or tow lake entries had weird DOW numbers, removed
  left_join(fish_nms, by = 'species_code') %>% 
  filter(!is.na(species_code)) %>% 
  select(-species_code) %>% 
  rename(species_code = fish)

# # check number of lakes/dates with all survey types
# chk <- group_by(fish_dat, dow, date) %>% 
#   summarise(
#     chkgrs = length(unique(gear))
#     )
# table(chk$chkgrs)
  
save(fish_dat, file = 'data/fish_dat.RData', compress = 'xz')

######
# veg transect data
# 
# load(file = 'ignore/trans_dat_nrri.RData')
# load(file = 'ignore/vegcodes_nrri.RData')
# load(file = 'ignore/trans_dat_current.RData')
# 
# ###
# # nrri processing
# 
# # processing veg codes to remove redundancies
# vegcodes_nrri <- select(vegcodes_nrri, NRRILUMP, COMMONNAME, SCIEN_NAME, SUB_VEG) %>% 
#   unique %>% 
#   filter(!NRRILUMP %in% c('ALGAE', 'not used', 'unknown', 'empty')) %>% 
#   nest(COMMONNAME, SCIEN_NAME) %>% 
#   mutate(nms = map(data, function(x){
#     if(nrow(x) > 1) {
#       
#       sp <- x[grep(' sp\\.', x$SCIEN_NAME), ]
#       if(nrow(sp) == 0 ) x
#       else sp
#       
#     } else {
#       
#       x
#       
#     }
#   })) %>% 
#   unnest(nms)
#   
# # format nrri data and combine with codes to get common/speices names
# nrri_tmp <- select(trans_dat_nrri, LAKENUM, NRRILUMP, END_DATE, matches('^TRSECT')) %>% 
#   gather('transect', 'val', TRSECT01:TRSECT52) %>% 
#   na.omit %>% 
#   mutate(
#     date = as.Date(as.character(END_DATE), format = '%Y-%m-%d'),
#     dow = as.numeric(LAKENUM), 
#     transect = as.numeric(gsub('^TRSECT', '', transect)), 
#     abundance = factor(val, levels = c(0, 1, 3, 5), labels = c('NULL', 'Rare', 'Common', 'Abundant'))
#   ) %>% 
#   select(dow, date, transect, NRRILUMP, abundance) %>% 
#   left_join(., vegcodes_nrri, by = 'NRRILUMP') %>% 
#   filter(NRRILUMP != 'ALGAE') %>% 
#   select(-NRRILUMP) %>% 
#   rename(
#     common_name = COMMONNAME, 
#     scientific_name = SCIEN_NAME,
#     growth_form = SUB_VEG
#     )
# 
# ###
# # 2006 to present transect data
# 
# # info on veg growth forms
# veg_growth <- read.csv('ignore/veg_growth.csv')
# veg_growth <- select(veg_growth, -SCIENTIFIC_NAME)
# 
# curr_tmp <- select(trans_dat, DOW, SURVEY_ID_DATE, SAMP_STATION_NBR, ABUNDANCE_NAME, COMMON_NAME, SCIENTIFIC_NAME) %>% 
#   left_join(., veg_growth, by = 'COMMON_NAME') %>%  
#   rename(
#     dow = DOW, 
#     date = SURVEY_ID_DATE, 
#     transect = SAMP_STATION_NBR, 
#     abundance = ABUNDANCE_NAME,
#     growth_form = growth,
#     common_name = COMMON_NAME, 
#     scientific_name = SCIENTIFIC_NAME
#   ) %>% 
#   mutate(
#     dow = as.numeric(dow), 
#     date = as.Date(as.character(date), format = '%m/%d/%y'), 
#     transect = as.numeric(transect), 
#     common_name = as.character(common_name), 
#     scientific_name = as.character(scientific_name), 
#     abundance = factor(abundance, levels = c('NULL', 'Rare', 'Common', 'Abundant'))
#   )
# 
# ###
# # combine nrri and current transect data
# 
# veg_dat <- rbind(curr_tmp, nrri_tmp) %>% 
#   arrange(dow, date)
# 
# save(veg_dat, file = 'data/veg_dat.RData', compress = 'xz')
# 
# ###
# # add missing years (2004 and 2005) to veg_dat
# 
# rm(list = ls())
# 
# # veg codes to combine with transect data
# vegcod <- read_excel('ignore/VEGECODE.xlsx') %>% 
#   select(VEGECODE, COMMONNAME, SCIEN_NAME, SUB_VEG) %>% 
#   rename(
#     VEG_CODE = VEGECODE, 
#     common_name = COMMONNAME, 
#     scientific_name = SCIEN_NAME, 
#     growth_form = SUB_VEG
#     )
# 
# # format transect data to match existing format for veg_dat
# # only for years 2004 and 2005
# mis_dat <- read.dbf('ignore/VEGEFIL.dbf') %>% 
#   select(-COMP_DATE, -START_DATE, -VERIFY_COD) %>% 
#   unite('dow', DOW_NUM, SUB_BASIN, sep = '') %>% 
#   gather( 'transect', 'abundance', TRANSECT1:TRANSECT10) %>% 
#   mutate(
#     date = as.Date(as.character(END_DATE), format = '%Y-%m-%d'), 
#     transect = gsub('TRANSECT', '', transect),
#     transect = as.numeric(TRANSNUM) + as.numeric(transect) - 1
#     ) %>% 
#   select(-END_DATE, -TRANSNUM) %>% 
#   filter(!is.na(abundance) & !abundance %in% c('1', 'B')) %>% 
#   mutate(
#     abundance = factor(abundance, 
#       levels = c('A', 'C', 'R', 'N'), 
#       labels = c('Abundant', 'Common', 'Rare', 'NULL')
#     )) %>% 
#   filter(date >= as.Date('2004-01-01') & date <= as.Date('2005-12-31')) %>% 
#   mutate(VEG_CODE = as.character(VEG_CODE)) %>% 
#   left_join(., vegcod, by = 'VEG_CODE') %>% 
#   select(dow, date, transect, abundance, common_name, scientific_name, growth_form)
# 
# # combine missing data with veg dat
# data(veg_dat)
# veg_dat <- rbind(veg_dat, mis_dat) %>% 
#   arrange(dow, date)
# 
# save(veg_dat, file = 'data/veg_dat.RData', compress = 'xz')

######
# combine fish and veg data
# veg data are summarized as total rich and total submsersed species rich
# includes zero veg lakes

rm(list = ls())

data(veg_dat)
data(fish_dat)

# ecoregion shapefile
ecoreg <- maptools::readShapeSpatial('ignore/MN_eco_keep.shp')

# covariates
covdat <- read.table('ignore/MNDNRwatersheds.txt', sep = ',', header = T)

# first summarize veg transect data
# total rich and total subm rich
# raw transect data
veg_summ <- select(veg_dat, dow, date, abundance, common_name, growth_form) %>% 
  group_by(dow, date) %>% 
  nest %>% 
  mutate(
    richests = map(data, function(x){
      
      x <- select(x, -abundance) %>% 
        unique
      veg_rich <- length(x$common_name)
      S_rich <- sum(x$growth_form == 'S', na.rm = T)

      data.frame(veg_rich, S_rich)
            
    })) %>% 
  select(-data) %>% 
  unnest %>%
  rename(veg_date = date) %>% 
  ungroup(.) %>% 
  mutate(
    dow = as.character(dow), 
    yr = year(veg_date)
  )

# summarise fish data
# get those that only have all three survey types
# richness estimtes only
fish_summ <- group_by(fish_dat, dow, date) %>% 
  mutate(
    chkgrs = length(unique(gear))
    ) %>%
  filter(chkgrs == 3) %>%
  select(-chkgrs) %>%
  summarise(fish_rich = length(unique(species_code))) %>% 
  ungroup %>% 
  mutate(yr = year(date))

# all lake utm coordinates in MN, zone 15N
lk_locs <- foreign::read.dbf('ignore/10k_pts.dbf') %>% 
  select(MAIN_DOW, UTM_X, UTM_Y) %>% 
  mutate(dow = as.character(as.numeric(as.character(MAIN_DOW)))) %>% 
  select(-MAIN_DOW) %>% 
  group_by(dow) %>% 
  summarize(
    utmx = mean(UTM_X, na.rm = TRUE),
    utmy = mean(UTM_Y, na.rm = TRUE)
  )

# merge fish data with veg data, each row is a unique lake
# add UTM coords
fishveg_dat <- mutate(fish_summ, 
    dow = as.character(dow)
  ) %>%  
  inner_join(., veg_summ, by = c('dow', 'yr')) %>% 
  mutate(diff_dt = abs(date - veg_date)) %>% 
  group_by(dow) %>% 
  filter(yr == max(yr) & diff_dt == min(diff_dt)) %>% 
  arrange(dow) %>% 
  select(-yr, -diff_dt) %>% 
  rename(fish_date = date) %>% 
  left_join(., lk_locs, by = 'dow') %>% 
  na.omit # st. louis river estuary not a lake, no coords in database

# get dnr ecoregions
# lakes as spatialpointsdataframe for overlay
fishveg_pts <- select(fishveg_dat, dow, utmx, utmy) %>% 
  data.frame
dow <- fishveg_pts$dow
fishveg_pts <- with(fishveg_pts, cbind(utmx, utmy))
rownames(fishveg_pts) <- dow
fishveg_pts <- na.omit(fishveg_pts)
fishveg_pts <- SpatialPointsDataFrame(fishveg_pts, data = data.frame(dow = rownames(fishveg_pts)))

# overlay lake locations with ecore shapefile  
ecoreg <- over(fishveg_pts, ecoreg) %>% 
  data.frame(
    dow = rownames(.), 
    ecoreg = tolower(.$NA_L1NAME)
  ) %>% 
  select(dow, ecoreg) %>% 
  mutate(dow = as.character(dow)) %>% 
  na.omit

# add ecoregion to fishveg_dat
fishveg_dat <- left_join(fishveg_dat, ecoreg, by = 'dow')

# organize addl covariate data
covdat <- select(covdat, DOWLKNUM, Sorderout, depthft, LKACRES, shedaream2, SDI, strmlgth, PDEVL, PAG, secchi) %>% 
  mutate(
    depthm = depthft * 0.3048, 
    aream2 = LKACRES * 4046.86,
    phuman = PDEVL + PAG, 
    sdi = SDI, 
    strmlgthm = strmlgth,
    secchim = secchi, 
    dow = DOWLKNUM
    ) %>% 
  select(-depthft, -LKACRES, -PDEVL, -PAG, -SDI, -secchi, -strmlgth, -DOWLKNUM)

# combine fishveg_dat with covariates
fishveg_dat <- ungroup(fishveg_dat) %>% 
  mutate(dow = as.numeric(dow)) %>% 
  left_join(., covdat, by = 'dow') %>% 
  na.omit %>% 
  data.frame %>% 
  arrange(dow, fish_date)
  
# save
# save(fishveg_dat, file = 'data/fishveg_dat.RData')
# write.csv(fishveg_dat, 'ignore/fishveg_dat.csv', quote = F, row.names = F)
