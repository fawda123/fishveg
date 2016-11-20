######
# processing and combining fish/veg data

library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(foreign)
library(lubridate)
library(maptools)
source('R/funcs.R')

######
# DNR fish data, in the format of CPUE all species
# adapted from Przemek's code

###
# import dnr fisheries csv files on Google docs
# newer data
# save as rdata objects for faster import
#
# library(dplyr)
# 
# fls <- list.files('C:/Users/mbeck/Desktop/', pattern = '\\.csv', full.names = T)
# for(fl in fls){
#   
#   # import file, get name, assign to object
#   cat(fl, '\n')
#   tmp <- read.csv(fl)
#   nm <- basename(fl) %>% 
#     gsub('_.*$', '', .) %>% 
#     paste0('fish_', .)
#   assign(nm, tmp)
#   
#   # save clean up workspace
#   save(list = nm, file = paste0('ignore/', nm, '.RData'), compress = 'xz')
#   rm(tmp)
#   rm(list = nm)
#   
# }
# 
# ######
# # import dnr fisheries txt files on Google docs
# # older data
# # save as rdata objects for faster import
# 
# library(dplyr)
# 
# fls <- list.files('C:/Users/mbeck/Desktop/fish_txt/', pattern = '\\.txt', full.names = T)
# for(fl in fls){
#   
#   cat(fl, '\n')
#   
#   # import file, get name, assign to object
#   tmp <- readLines(fl) %>%
#     strsplit(., split = '|', fixed = TRUE) %>%
#     do.call('rbind', .)
#   nms <- tmp[1, ]
#   tmp <- data.frame(tmp[-1, ], stringsAsFactors = FALSE)
#   names(tmp) <- nms
#   
#   # name of object for saving, make assignment
#   nm <- basename(fl) %>% 
#     gsub('_.*$', '', .) %>% 
#     paste0('fish_', .)
#   assign(nm, tmp)
#   
#   # save, clean up workspace
#   save(list = nm, file = paste0('ignore/', nm, '.RData'), compress = 'xz')
#   rm(tmp)
#   rm(list = nm)
#   
# }
# 
# ##
# # create a master file of fish survey data
# 
# rm(list = ls())
# 
# # files to import and names
# fls <- list.files('ignore', '^fish_.*\\.RData$', full.names = TRUE)
# nms <- basename(fls) %>% 
#   gsub('\\.RData', '', .)
# 
# # empty list for output
# fish_ls <- vector('list', length = length(nms))
# names(fish_ls) <- nms
# 
# # columns to keep
# cols <- c('DOW_NBR_PRIMARY', 'SURVEY_DATE', 'SURVEY_DATE_MONTH_NAME_DV', 'SURVEY_TYPE_NAME', 'SURVEY_COMPONENT_CLASS_NAME', 'SAMP_STA_TYPE_TOTAL_SETS_DV', 'FISH_SPECIES_ABBREV', 'TOTAL_LENGTH_MM')
# 
# # iterate through files
# # import, minor format, append to output
# for(fl in seq_along(fls)){
#     
#   cat(fl, 'of', length(fls), '\n')
#   
#   # import the file
#   load(file = fls[fl])
#   dat <- get(nms[fl])
# 
#   # filter for above
#   dat <- dat[, cols] %>% 
#     mutate(
#       TOTAL_LENGTH_MM = as.numeric(TOTAL_LENGTH_MM)
#       ) %>% 
#     rename(
#       dow = DOW_NBR_PRIMARY, 
#       date = SURVEY_DATE, 
#       month = SURVEY_DATE_MONTH_NAME_DV,
#       gear = SURVEY_COMPONENT_CLASS_NAME, 
#       type = SURVEY_TYPE_NAME,
#       effort = SAMP_STA_TYPE_TOTAL_SETS_DV, 
#       sp_abb = FISH_SPECIES_ABBREV, 
#       tl_mm = TOTAL_LENGTH_MM
#     )
#   
#   # add to list
#   fish_ls[[nms[fl]]] <- dat
#   
# }
# 
# fish_all <- do.call('rbind', fish_ls) 
# row.names(fish_all) <- 1:nrow(fish_all)
# save(fish_all, file = 'data/fish_all.RData', compress = 'xz')

##
# get fish cpue by lake for fish_all
  
rm(list = ls())

source('R/funcs.R')

data(fish_all)

# addl processing for fish_all

# surveys to keep
srv <- c('Gill Netting', 'Trap Netting')

# months to keep
mos <- c('July', 'August', 'September')

# species to keep - carp, black bullhead, yellowbullhead, bluegill, black crappie, white crappie, perch, pike, walleye
spp <- c('CAP', 'BLB', 'YEB', 'BLG', 'BLC', 'WHC', 'YEP', 'NOP', 'WAE')

# survey types to keep
typ <- c('Population Assessment', 'Re-Survey')

# other species to label as 'other'
# put in regex format
other <- unique(fish_all$sp_abb)
other <- other[!other %in% spp] %>% 
  paste0('^', ., '$') %>% 
  paste(., collapse = '|') 

# bullhead are combined here because the lenght/weight parameters are same for black and yellow
# crappie are not combined here because the length/weight parameters are different
fish_dat <- filter(fish_all, 
    month %in% mos &
    type %in% typ &
    gear %in% srv
  ) %>% 
  select(-month, -type) %>% 
  mutate(
    date = as.Date(date, '%m/%d/%Y'),
    dow = as.numeric(paste0(dow, '00'))
    ) %>% 
  group_by(date, dow) %>% 
  summarise(fish_rich = length(unique(sp_abb)))

save(fish_dat, file = 'data/fish_dat.RData', compress = 'xz')

######
# veg transect data

load(file = 'ignore/trans_dat_nrri.RData')
load(file = 'ignore/vegcodes_nrri.RData')
load(file = 'ignore/trans_dat_current.RData')

###
# nrri processing

# processing veg codes to remove redundancies
vegcodes_nrri <- select(vegcodes_nrri, NRRILUMP, COMMONNAME, SCIEN_NAME, SUB_VEG) %>% 
  unique %>% 
  filter(!NRRILUMP %in% c('ALGAE', 'not used', 'unknown', 'empty')) %>% 
  nest(COMMONNAME, SCIEN_NAME) %>% 
  mutate(nms = map(data, function(x){
    if(nrow(x) > 1) {
      
      sp <- x[grep(' sp\\.', x$SCIEN_NAME), ]
      if(nrow(sp) == 0 ) x
      else sp
      
    } else {
      
      x
      
    }
  })) %>% 
  unnest(nms)
  
# format nrri data and combine with codes to get common/speices names
nrri_tmp <- select(trans_dat_nrri, LAKENUM, NRRILUMP, END_DATE, matches('^TRSECT')) %>% 
  gather('transect', 'val', TRSECT01:TRSECT52) %>% 
  na.omit %>% 
  mutate(
    date = as.Date(as.character(END_DATE), format = '%Y-%m-%d'),
    dow = as.numeric(LAKENUM), 
    transect = as.numeric(gsub('^TRSECT', '', transect)), 
    abundance = factor(val, levels = c(0, 1, 3, 5), labels = c('NULL', 'Rare', 'Common', 'Abundant'))
  ) %>% 
  select(dow, date, transect, NRRILUMP, abundance) %>% 
  left_join(., vegcodes_nrri, by = 'NRRILUMP') %>% 
  filter(NRRILUMP != 'ALGAE') %>% 
  select(-NRRILUMP) %>% 
  rename(
    common_name = COMMONNAME, 
    scientific_name = SCIEN_NAME,
    growth_form = SUB_VEG
    )

###
# 2006 to present transect data

# info on veg growth forms
veg_growth <- read.csv('ignore/veg_growth.csv')
veg_growth <- select(veg_growth, -SCIENTIFIC_NAME)

curr_tmp <- select(trans_dat, DOW, SURVEY_ID_DATE, SAMP_STATION_NBR, ABUNDANCE_NAME, COMMON_NAME, SCIENTIFIC_NAME) %>% 
  left_join(., veg_growth, by = 'COMMON_NAME') %>%  
  rename(
    dow = DOW, 
    date = SURVEY_ID_DATE, 
    transect = SAMP_STATION_NBR, 
    abundance = ABUNDANCE_NAME,
    growth_form = growth,
    common_name = COMMON_NAME, 
    scientific_name = SCIENTIFIC_NAME
  ) %>% 
  mutate(
    dow = as.numeric(dow), 
    date = as.Date(as.character(date), format = '%m/%d/%y'), 
    transect = as.numeric(transect), 
    common_name = as.character(common_name), 
    scientific_name = as.character(scientific_name), 
    abundance = factor(abundance, levels = c('NULL', 'Rare', 'Common', 'Abundant'))
  )

###
# combine nrri and current transect data

veg_dat <- rbind(curr_tmp, nrri_tmp) %>% 
  arrange(dow, date)

save(veg_dat, file = 'data/veg_dat.RData', compress = 'xz')

###
# add missing years (2004 and 2005) to veg_dat

rm(list = ls())

# veg codes to combine with transect data
vegcod <- read_excel('ignore/VEGECODE.xlsx') %>% 
  select(VEGECODE, COMMONNAME, SCIEN_NAME, SUB_VEG) %>% 
  rename(
    VEG_CODE = VEGECODE, 
    common_name = COMMONNAME, 
    scientific_name = SCIEN_NAME, 
    growth_form = SUB_VEG
    )

# format transect data to match existing format for veg_dat
# only for years 2004 and 2005
mis_dat <- read.dbf('ignore/VEGEFIL.dbf') %>% 
  select(-COMP_DATE, -START_DATE, -VERIFY_COD) %>% 
  unite('dow', DOW_NUM, SUB_BASIN, sep = '') %>% 
  gather( 'transect', 'abundance', TRANSECT1:TRANSECT10) %>% 
  mutate(
    date = as.Date(as.character(END_DATE), format = '%Y-%m-%d'), 
    transect = gsub('TRANSECT', '', transect),
    transect = as.numeric(TRANSNUM) + as.numeric(transect) - 1
    ) %>% 
  select(-END_DATE, -TRANSNUM) %>% 
  filter(!is.na(abundance) & !abundance %in% c('1', 'B')) %>% 
  mutate(
    abundance = factor(abundance, 
      levels = c('A', 'C', 'R', 'N'), 
      labels = c('Abundant', 'Common', 'Rare', 'NULL')
    )) %>% 
  filter(date >= as.Date('2004-01-01') & date <= as.Date('2005-12-31')) %>% 
  mutate(VEG_CODE = as.character(VEG_CODE)) %>% 
  left_join(., vegcod, by = 'VEG_CODE') %>% 
  select(dow, date, transect, abundance, common_name, scientific_name, growth_form)

# combine missing data with veg dat
data(veg_dat)
veg_dat <- rbind(veg_dat, mis_dat) %>% 
  arrange(dow, date)

save(veg_dat, file = 'data/veg_dat.RData', compress = 'xz')

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
fishveg_dat <- mutate(fish_dat, 
    dow = as.character(dow), 
    yr = year(date)
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
save(fishveg_dat, file = 'data/fishveg_dat.RData')
write.csv(fishveg_dat, 'ignore/fishveg_dat.csv', quote = F, row.names = F)
