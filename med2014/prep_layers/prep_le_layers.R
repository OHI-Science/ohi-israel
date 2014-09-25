# prep_le_layers.r

# setup
library(dplyr)
library(tidyr) # install.packages('tidyr')
dir_prep = 'prep_layers' # this folder is within ohi-israel/med2014


# run prep_le_hotels.r
source('prep_layers/prep_le_hotels.r')

# run prep_le_aqua.r
source('prep_layers/prep_le_aqua.r')



## save jobs and wages layers

f_corrected = read.csv(file.path(dir_prep,'hotel_jobs_right.csv')) # ensure this is the appropriate file. 

# write.csv(f_corrected %>%
#             select(rgn_id, year, count = hotel_jobs_n), 
#           file.path('layers', 'le_





# 
