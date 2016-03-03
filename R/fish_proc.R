# # import the csv files, save as rdata objects for faster import
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

# libraries
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)

# files to import and names
fls <- list.files('ignore', '^fish_', full.names = TRUE)
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
    mutate(Species = NA) 
  
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
  dat$CPUE <- dat$N/dat$SAMP_STA_TYPE_TOTAL_SETS_DV
  
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

save(fish_all, file = 'data/fish_all.RData')
  
  

