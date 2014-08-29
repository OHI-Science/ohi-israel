# prep_le_hotels.r

# prep hotel jobs + wages data:
# 'Coastal hotel jobs.csv' is created from Anat's 'Coastal hotel jobs.xlsx'


# setup
file_in  = file.path(dir_prep, 'Coastal hotel jobs.csv')


# read in file
f = read.csv(file_in); head(f); sapply(f, class)

## combine coastal cities:
# sum jobs and average wages across coastal cities into districts (Herzlia + Netanya --> Center)

hotels_by_2rgn = f %>%
  group_by(rgn_id, year) %>%
  summarize(hotel_jobs_n               = sum(Number.of.hotel.Jobs),
            wages_tourism_monthly_mean = mean(Mean.monthly.wages.Tourism))
head(hotels_by_2rgn); summary(hotels_by_2rgn)

## combine regions:
# sum jobs and average wages across regions into Israel (Tel aviv + Center --> Israel)

hotels_by_israel = hotels_by_2rgn %>%
  group_by(year) %>%
  summarize(hotel_jobs_n               = sum(hotel_jobs_n),
            wages_tourism_monthly_mean = mean(wages_tourism_monthly_mean)) %>%
    mutate(rgn_id = 0) %>%
  select(rgn_id, year, hotel_jobs_n, wages_tourism_monthly_mean)
head(hotels_by_israel); summary(hotels_by_israel)

# Option 1. 
write.csv(hotels_by_israel, file.path(dir_prep,'hotel_jobs_wrong_tmp.csv'), row.names=F, na='')
# by hand save to hotel_jobs_wrong.csv after repeating for rgns 1:6. 
# hotel_jobs_wrong.csv will still be incomplete because you will now have 6x total jobs in Israel, and wages for rgns that you know have no tourism. So will either need to apply a modifier (coastal population?) to this file, or, take the following approach and then gapfill 3 regions:

# Option 2. 
write.csv(hotels_by_2rgn, file.path(dir_prep,'hotel_jobs_right_tmp.csv'), row.names=F, na='')
# by hand save hotel_jobs_right.csv, which includes real data for Tel Aviv[rgn_id=6], Center[4], and Haifa Bay[3], and no data for the other 3 regions. Will have to think of a good way to modify these 3 regions, but it is a better starting point than option 1. 




