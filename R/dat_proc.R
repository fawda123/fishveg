######
# processing and combining fish/veg data

library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)

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

# files to import and names
fls <- list.files('ignore', '^fish_.*\\.RData$', full.names = TRUE)
nms <- basename(fls) %>% 
  gsub('\\.RData', '', .)

# empty list for output
fish_ls <- vector('list', length = length(nms))
names(fish_ls) <- nms

# columns to keep
cols <- c('REGION_NAME', 'LAKE_CLASS_ID_PRIMARY', 'DOW_NBR_PRIMARY', 'LAKE_AREA_GIS_ACRES', 'MAX_DEPTH_FEET', 'MEAN_DEPTH_FEET', 'SURVEY_DATE', 'SURVEY_DATE_MONTH_NAME_DV', 'SURVEY_DATE_CALENDAR_YEAR_DV', 'SURVEY_COMPONENT_CLASS_NAME', 'SAMP_STA_TYPE_TOTAL_SETS_DV', 'FISH_SPECIES_ABBREV', 'TOTAL_LENGTH_MM')

# surveys to keep
srv <- c('Gill Netting', 'Trap Netting')

# months to keep
mos <- c('July', 'August', 'September')

# species to keep
spp <- c('CAP', 'BLG', 'BLC', 'WHC', 'GSF', 'BLB', 'YEB', 'YEP', 'NOP', 'LMB', 'WAE')

# iterate through files
# import, format, append to output
for(fl in seq_along(fls)){
    
  cat(fl, 'of', length(fls), '\n')
  
  # import the file
  load(file = fls[fl])
  dat <- get(nms[fl])

  # filter for above
  dat <- dat[, cols] %>% 
    filter(
      SURVEY_COMPONENT_CLASS_NAME %in% srv &
      SURVEY_DATE_MONTH_NAME_DV %in% mos &
      FISH_SPECIES_ABBREV %in% spp
    ) %>% 
    mutate(
      TOTAL_LENGTH_MM = as.numeric(TOTAL_LENGTH_MM),
      Species = NA
      ) 
  
  # create length classes
  dat$Species[dat$FISH_SPECIES_ABBREV=="CAP" & dat$SURVEY_COMPONENT_CLASS_NAME=="Gill Netting"& dat$TOTAL_LENGTH_MM>300]<-"COC_300_GN"
  dat$Species[dat$FISH_SPECIES_ABBREV=="CAP" & dat$SURVEY_COMPONENT_CLASS_NAME=="Trap Netting"& dat$TOTAL_LENGTH_MM>300]<-"COC_300_TN"
  dat$Species[dat$FISH_SPECIES_ABBREV=="CAP" & dat$TOTAL_LENGTH_MM<150& dat$SURVEY_COMPONENT_CLASS_NAME=="Trap Netting"]<-"COC_150_TN"
  dat$Species[dat$FISH_SPECIES_ABBREV=="CAP" & dat$TOTAL_LENGTH_MM>=150 & dat$TOTAL_LENGTH_MM<=300& dat$SURVEY_COMPONENT_CLASS_NAME=="Gill Netting"]<-"COC_150_300_GN"
  dat$Species[dat$FISH_SPECIES_ABBREV=="CAP" & dat$TOTAL_LENGTH_MM>=150 & dat$TOTAL_LENGTH_MM<=300& dat$SURVEY_COMPONENT_CLASS_NAME=="Trap Netting"]<-"COC_150_300_TN"
  dat$Species[dat$FISH_SPECIES_ABBREV=="BLG"& dat$SURVEY_COMPONENT_CLASS_NAME=="Trap Netting"]<-"BLG"
  dat$Species[dat$FISH_SPECIES_ABBREV%in% c("BLC","WHC")& dat$SURVEY_COMPONENT_CLASS_NAME=="Trap Netting"]<-"CRP"
  dat$Species[dat$FISH_SPECIES_ABBREV%in% c("BLB","YEB") & dat$TOTAL_LENGTH_MM>100& dat$SURVEY_COMPONENT_CLASS_NAME=="Trap Netting"]<-"BHD_adult"
  dat$Species[dat$FISH_SPECIES_ABBREV%in% c("BLB","YEB") & dat$TOTAL_LENGTH_MM<=100& dat$SURVEY_COMPONENT_CLASS_NAME=="Trap Netting"]<-"BHD_YOY"
  dat$Species[dat$FISH_SPECIES_ABBREV=="YEP"& dat$SURVEY_COMPONENT_CLASS_NAME=="Gill Netting"]<-"YEP"
  dat$Species[dat$FISH_SPECIES_ABBREV=="NOP"& dat$SURVEY_COMPONENT_CLASS_NAME=="Gill Netting"]<-"NOP"
  dat$Species[dat$FISH_SPECIES_ABBREV=="WAE"& dat$SURVEY_COMPONENT_CLASS_NAME=="Gill Netting"]<-"WAE"
  dat$Species[dat$FISH_SPECIES_ABBREV=="LMB"& dat$SURVEY_COMPONENT_CLASS_NAME=="Gill Netting"]<-"LMB"
  
  dat <- melt(dat,
    id.var=c("REGION_NAME","LAKE_CLASS_ID_PRIMARY","DOW_NBR_PRIMARY","LAKE_AREA_GIS_ACRES",   
    "MAX_DEPTH_FEET","MEAN_DEPTH_FEET","SURVEY_DATE","SURVEY_DATE_MONTH_NAME_DV","SURVEY_DATE_CALENDAR_YEAR_DV",
    "SURVEY_COMPONENT_CLASS_NAME","SAMP_STA_TYPE_TOTAL_SETS_DV","Species"),
    measure.var="TOTAL_LENGTH_MM",value.name="TOTAL_LENGTH_MM")
  
  dat <- ddply(dat,c("REGION_NAME","LAKE_CLASS_ID_PRIMARY","DOW_NBR_PRIMARY","LAKE_AREA_GIS_ACRES",   
    "MAX_DEPTH_FEET","MEAN_DEPTH_FEET","SURVEY_DATE","SURVEY_DATE_MONTH_NAME_DV","SURVEY_DATE_CALENDAR_YEAR_DV",
    "SURVEY_COMPONENT_CLASS_NAME","SAMP_STA_TYPE_TOTAL_SETS_DV","Species"), summarize,
    N=length(TOTAL_LENGTH_MM))
  dat$CPUE <- dat$N/as.numeric(dat$SAMP_STA_TYPE_TOTAL_SETS_DV)
  
  dat <- melt(dat,
    id.var=c("REGION_NAME","LAKE_CLASS_ID_PRIMARY","DOW_NBR_PRIMARY","LAKE_AREA_GIS_ACRES",   
    "MAX_DEPTH_FEET","MEAN_DEPTH_FEET","SURVEY_DATE","SURVEY_DATE_MONTH_NAME_DV","SURVEY_DATE_CALENDAR_YEAR_DV",
    "SURVEY_COMPONENT_CLASS_NAME","SAMP_STA_TYPE_TOTAL_SETS_DV","Species"),
    measure.var="CPUE",variable.name="CPUE")
  
  dat$value[is.na(dat$value)]<-0  # replacew the NAs with 0s 
  
  dat <- dcast(dat, SURVEY_DATE_CALENDAR_YEAR_DV+SURVEY_DATE+SURVEY_DATE_MONTH_NAME_DV+REGION_NAME+LAKE_CLASS_ID_PRIMARY+
    DOW_NBR_PRIMARY+MAX_DEPTH_FEET+LAKE_AREA_GIS_ACRES~CPUE+Species,sum)
  
  dat$CPUE_NOP <- NULL

  # add to list
  fish_ls[[nms[fl]]] <- dat
  
}

# data from Pre-1993 did not have bull head YOY, CPUE_BHD_YOY

######
# combining the list
# addl formatting
fish_all <- do.call('rbind', fish_ls)
row.names(fish_all) <- 1:nrow(fish_all)
fish_all <- rename(fish_all, 
  year = SURVEY_DATE_CALENDAR_YEAR_DV,
  date = SURVEY_DATE,
  month =SURVEY_DATE_MONTH_NAME_DV,
  region = REGION_NAME,
  class = LAKE_CLASS_ID_PRIMARY,
  dow = DOW_NBR_PRIMARY,
  maxd_ft = MAX_DEPTH_FEET,
  area_ac =LAKE_AREA_GIS_ACRES
)
names(fish_all) <- tolower(names(fish_all))
fish_all <- mutate(fish_all, 
  date = as.Date(date, format = '%m/%d/%Y'), 
  month = as.character(month), 
  region = as.character(region), 
  dow = paste0(dow, '000'),
  dow = substr(dow, 1, 8)
  ) %>% 
  arrange(date, dow)

fish_dat <- fish_all
fish_dat[order(fish_dat$date), ] <- fish_dat
save(fish_dat, file = 'data/fish_dat.RData', compress = 'xz')

write.csv(fish_dat, file = 'ignore/fish_dat.csv', quote = F, row.names = F)

######
# veg transect data

load(file = 'ignore/trans_dat_nrri.RData')
load(file = 'ignore/vegcodes_nrri.RData')
load(file = 'ignore/trans_dat_current.RData')

###
# nrri processing

# processing veg codes to remove redundancies
vegcodes_nrri <- select(vegcodes_nrri, NRRILUMP, COMMONNAME, SCIEN_NAME) %>% 
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
  filter(!is.na(val)) %>% 
  filter(!val %in% 0) %>% 
  mutate(
    date = as.Date(as.character(END_DATE), format = '%Y-%m-%d'),
    dow = as.numeric(LAKENUM), 
    transect = as.numeric(gsub('^TRSECT', '', transect)), 
    abundance = factor(val, levels = c(1, 3, 5), labels = c('Rare', 'Common', 'Abundant'))
  ) %>% 
  select(dow, date, transect, NRRILUMP, abundance) %>% 
  left_join(., vegcodes_nrri, by = 'NRRILUMP') %>% 
  filter(NRRILUMP != 'ALGAE') %>% 
  select(-NRRILUMP) %>% 
  rename(
    common_name = COMMONNAME, 
    scientific_name = SCIEN_NAME
    )


###
# 2006 to present transect data

curr_tmp <- select(trans_curr, DOW, SURVEY_ID_DATE, SAMP_STATION_NBR, ABUNDANCE_NAME, COMMON_NAME, SCIENTIFIC_NAME) %>% 
  rename(
    dow = DOW, 
    date = SURVEY_ID_DATE, 
    transect = SAMP_STATION_NBR, 
    abundance = ABUNDANCE_NAME,
    common_name = COMMON_NAME, 
    scientific_name = SCIENTIFIC_NAME
  ) %>% 
  mutate(
    dow = as.numeric(dow), 
    date = as.Date(as.character(date), format = '%m/%d/%y'), 
    transect = as.numeric(transect), 
    common_name = as.character(common_name), 
    scientific_name = as.character(scientific_name)
  )

###
# combine nrri and current transect data

veg_dat <- rbind(curr_tmp, nrri_tmp) %>% 
  arrange(dow, date)

save(veg_dat, file = 'data/veg_dat.RData', compress = 'xz')










# info on veg growth forms
veg_growth <- read.csv('ignore/veg_growth.csv')

# macro rich by transect, includes richness by growth form from veg_growth
# rich based on unique dow, survey id, and date
veg_growth <- select(veg_growth, -SCIENTIFIC_NAME)
veg_dat <- mutate(trans_curr, COMMON_NAME = as.character(COMMON_NAME)) %>% 
  left_join(veg_growth, by = 'COMMON_NAME') %>% 
  select(DOW, SURVEY_ID, SURVEY_ID_DATE, COMMON_NAME, growth) %>% 
  unique %>% 
  group_by(DOW, SURVEY_ID, SURVEY_ID_DATE) %>% 
  summarise(
    veg_rich = length(COMMON_NAME),
    S_rich = sum(growth == 'S'), 
    E_rich = sum(growth == 'E'), 
    F_rich = sum(growth == 'F'), 
    T_rich = sum(growth == 'T')
    ) %>%
  mutate(SURVEY_ID_DATE = as.Date(SURVEY_ID_DATE, '%m/%d/%y')) %>% 
  rename(veg_date = SURVEY_ID_DATE) %>% 
  ungroup(.) %>% 
  mutate(DOW = as.numeric(DOW))
