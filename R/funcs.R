######
# summarise bullhead, carp dataset by cpue of adult/yoy
#  
# dat_in fish_all dataset created in dat_proc.r
# bhd_yoy upper length limit (mm) for bullhead yoy
# cap_yoy upper length limit (mm) for carp yoy
cpue_fun <- function(dat_in, bhd_yoy = 100, cap_yoy = 150){
  
  library(tidyr)
  library(dplyr)
  
  dat <- dat_in
  dat$age <- 'adult'
  
  # create length classes
  dat$age[dat$sp_abb == 'CAP' & dat$tl_mm < cap_yoy] <- 'yoy'
  dat$age[dat$sp_abb == 'BHD' & dat$tl_mm < bhd_yoy] <- 'yoy'
  
  # summarize by cpue
  dat <- group_by(dat, dow, date, type, sp_abb, age, effort) %>% 
    summarise(
      n = length(tl_mm)
    ) %>% 
    ungroup %>% 
    mutate(cpue = as.numeric(n)/as.numeric(effort)) %>% 
    group_by(dow, date, sp_abb, age) %>% 
    summarise(cpue = sum(cpue)) %>% 
    ungroup %>% 
    unite(var, sp_abb, age, sep = '_') %>% 
    spread(var, cpue, fill = 0)
  
  names(dat)[names(dat) %in% 'other_adult'] <- 'other'
  
  return(dat)
    
}