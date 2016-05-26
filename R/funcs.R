######
# summarise bullhead, carp dataset by cpue of adult/yoy
#  
# dat_in fish_all dataset created in dat_proc.r
# bhd_yoy upper length limit (mm) for bullhead yoy
# cap_yoy upper length limit (mm) for carp yoy
# bywt logical indicating if CPUE is biomass/effort if TRUE, otherwise count/effort
cpue_fun <- function(dat_in, bhd_yoy = 100, cap_yoy = 150, bywt = TRUE){
  
  library(tidyr)
  library(dplyr)
  
  dat <- dat_in
  dat$age <- 'adult'
  
  # create length classes
  dat$age[dat$sp_abb == 'CAP' & dat$tl_mm < cap_yoy] <- 'yoy'
  dat$age[dat$sp_abb == 'BHD' & dat$tl_mm < bhd_yoy] <- 'yoy'
  
  # add weight column 
  # coeffs from Handbook of Freshwater Fishery Biology
  dat$wt_g <- NA
  dat[dat$sp_abb %in% 'CAP', 'wt_g'] <- 10^(-4.44245 + 2.83840 * log10(dat[dat$sp_abb %in% 'CAP', 'tl_mm']))
  dat[dat$sp_abb %in% 'BHD', 'wt_g'] <- 10^(-4.60512 + 2.88495 * log10(dat[dat$sp_abb %in% 'BHD', 'tl_mm']))
  
  # wt to kg
  dat <- rename(dat, wt_kg = wt_g) %>% 
    mutate(wt_kg = wt_kg * 0.001)
  
  # sum weights for species if T  
  if(bywt){

    dat <- group_by(dat, dow, date, type, sp_abb, age, effort) %>% 
      summarise(
        ctch = sum(wt_kg, na.rm = TRUE)
      ) %>% 
      ungroup
    
  # otherwise get counts  
  } else {
    
    dat <- group_by(dat, dow, date, type, sp_abb, age, effort) %>% 
      summarise(
        ctch = length(tl_mm)
      )
      
  }

  # get CPUE as weight (or count) divided by effort
  # sum by gear
  # remove species other than carp, bullhead
  dat <- ungroup(dat) %>% 
    mutate(cpue = as.numeric(ctch)/as.numeric(effort)) %>% 
    group_by(dow, date, sp_abb, age) %>% 
    summarise(cpue = sum(cpue)) %>% 
    ungroup %>% 
    unite(var, sp_abb, age, sep = '_') %>% 
    spread(var, cpue, fill = 0) %>% 
    select(-other_adult)
  
  return(dat)
    
}