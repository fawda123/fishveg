######
# summarise fish data from fish_all.RData
#  
# dat_in fish_all dataset created in dat_proc.r
# spp chr string of species to keep from raw data
# gear chr string of gear types to keep for each species
# yoylim numeric indicating length (mm) for separating adult and YOY, creates two columns for the species, set to 0 combines all
# slopes numeric indicating slope for lenght/weight conversion, from the Handbook of Freshwater Fishery Biology
# intercepts numeric indicating intercept for length/weight conversion, from the Handbook of Freshwater Fishery Biology
# bywt logical indicating if CPUE is biomass/effort if TRUE, otherwise count/effort
cpue_fun <- function(dat_in, 
  spp = c('CAP', 'BHD', 'BLG', 'BLC', 'WHC', 'YEP', 'NOP', 'WAE'), 
  gear = c('GN', 'TN', 'TN', 'TN', 'TN', 'GN', 'GN', 'GN'),
  yoylim = c(0, 0, 0, 0, 0, 0, 0, 0),
  slopes = c(2.83840, 2.88495, 3.17266, 3.17980, 3.3835, 3.17285, 3.14178, 3.03606),
  intercepts = c(-4.44245, -4.60512, -5.10377, -5.24330, -5.8236, -5.33475, -5.61083, -5.14176),
  bywt = TRUE){
  
  library(tidyr)
  library(dplyr)

  # comb args
  combs <- list(spp = spp, gear = gear, yoylim = yoylim, slopes = slopes, intercepts = intercepts)
  
  ## sanity checks
  
  # check species
  sppchk <- !spp %in% dat_in$sp_abb
  if(any(sppchk))
    stop('Check species: ', paste0(spp[sppchk], collapse = ', '), ' not found')

  # check spp, gear, yoylim are all same length
  lenchk <- lapply(combs, length) %>% 
    unique
  if(length(lenchk) > 1)
    stop('arguments for spp, gear, yoylim must be same length')

  ##
  # process

  dat <- dat_in
  dat$age <- ''
  dat$wt_kg <- NA

  # combs as data.frame, used for weight ests and yoylims
  combs <- data.frame(combs)

  # get weights for each species
  # add yoy 
  for(i in 1:nrow(combs)){
    
    # subsets
    spp <- combs[i, 'spp']
    yoylim <- combs[i, 'yoylim']
    slope <- combs[i, 'slopes']
    intercept <- combs[i, 'intercepts']
    
    # add the weight
    dat[dat$sp_abb %in% spp, 'wt_kg'] <- (10^(intercept + slope * log10(dat[dat$sp_abb %in% spp, 'tl_mm']))) * 0.001
    
    # add the yoy
    dat$age[dat$sp_abb == spp & dat$tl_mm < yoylim] <- 'yoy'
    
  }

  # combine black and white crappie
  # unite species abb and age class
  dat <- mutate(dat,
      sp_abb = gsub('BLC|WHC', 'CRP', sp_abb)
    ) %>% 
    unite('sp', sp_abb, age, sep ='')

  # sum weights for species if T  
  if(bywt){

    dat <- group_by(dat, dow, date, gear, sp, effort) %>% 
      summarise(
        ctch = sum(wt_kg, na.rm = TRUE)
      ) %>% 
      ungroup
    
  # otherwise get counts  
  } else {
    
    dat <- group_by(dat, dow, date, gear, sp, effort) %>% 
      summarise(
        ctch = length(tl_mm)
      )
      
  }

  # get CPUE as weight (or count) divided by effort
  # sum by gear
  # remove 'other' species
  dat <- ungroup(dat) %>% 
    unite('sp', sp, gear, sep = '_') %>% 
    mutate(cpue = as.numeric(ctch)/as.numeric(effort)) %>% 
    group_by(dow, date, sp) %>% 
    summarise(cpue = sum(cpue)) %>% 
    ungroup %>% 
    spread(sp, cpue, fill = 0) %>% 
    select(-matches('^other'))
  
  # finally, remove columns for spp, gear combo that we don't care about 
  # doing this last will still return lakes that were surveyed but do not contain relevant fish/gear combos
  nms_kp <- with(combs, c(paste0(spp, 'yoy_', gear), paste0(spp, '_', gear))) %>% 
    c('dow', 'date', .) %>% 
    gsub('BLC|WHC', 'CRP', .) %>% 
    unique
  dat <- dat[, names(dat) %in% nms_kp]
  
  return(dat)
    
}

##
# vif function
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}