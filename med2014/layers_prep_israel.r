# layers_prep_israel.r
# this converts global data with cntry_key==ISR to rgn_ids 1:6. 
# once this code is run and the files exist, it won't need to be rerun. 

library(dplyr)
library(stringr)

f_in  = 'le_gdp_pc_ppp_global2013.csv' # change the select() call based on column names
f_out = str_replace(f_in, 'global2013', 'israel2014')

a = read.csv(file.path('layers', f_in)); head(a)

b = rbind(a %>% mutate(rgn_id = 1), 
          a %>% mutate(rgn_id = 2),
          a %>% mutate(rgn_id = 3),
          a %>% mutate(rgn_id = 4),
          a %>% mutate(rgn_id = 5),
          a %>% mutate(rgn_id = 6)) %>%
  select(rgn_id, year, usd)  # change this select() call 


write.csv(b, file.path('layers', f_out), row.names = F)