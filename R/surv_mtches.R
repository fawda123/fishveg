library(tidyr)
library(purrr)
library(dplyr)
library(ggplot2)

load(file = 'data/fish_dat.RData')
load(file = 'ignore/veg_dat.RData')

# format trans_dat just to get unique dates, dows
veg_dts <- select(veg_dat, dow, date) %>% 
  unique %>% 
  mutate(dow = as.character(dow)) %>% 
  filter(dow = grepl('00$', dow))
  
fish_dts <- select(fish_dat, dow, date, region) %>% 
  filter(region != '')

comp_dts <- inner_join(veg_dts, fish_dts, by = 'dow') %>% 
  mutate(diff_dt = abs(date.x - date.y)) %>% 
  group_by(dow, region) %>% 
  filter(diff_dt == min(diff_dt)[1]) %>% 
  arrange(dow)

# # survey within +/- 365 days
# sum(comp_dts$diff_dt < 365)

# all lakes
toplo <- cumsum(table(comp_dts$diff_dt))
toplo <- data.frame(
  days = as.numeric(as.character(names(toplo))), 
  counts = toplo
  )

p1 <- ggplot(toplo, aes(x = days, y = counts)) + 
  scale_x_continuous("Days between veg/fish surveys") + 
  scale_y_continuous("Number of lakes") + 
  geom_step() + 
  theme_minimal()

# by regions
toplo <- group_by(comp_dts, region) %>% 
  mutate(regref = region) %>% 
  nest %>% 
  mutate(cumlks = map(data, 
    function(x) {

      tots <- cumsum(table(x$diff_dt))
      tots <- data.frame(
        days = as.numeric(as.character(names(tots))), 
        counts = tots
        )
      
      return(tots)
      
    })) %>% 
  select(region, cumlks) %>% 
  unnest

p2 <- ggplot(toplo, aes(x = days, y = counts, colour = region)) + 
  scale_x_continuous("Days between veg/fish surveys") + 
  scale_y_continuous("Number of lakes") + 
  geom_step() + 
  theme_minimal() + 
  theme(legend.position = 'top')
 
pdf('mtches.pdf', height = 6, width = 6, family = 'serif')
print(p1)
print(p2)
dev.off()