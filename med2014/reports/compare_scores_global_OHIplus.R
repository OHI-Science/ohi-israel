## Global OHI vs. Local OHI+ Scores Comparison
# Run this script to produce a table of differences

## setup ---
# libraries
library(dplyr)
library(RColorBrewer)  # install.packages('RColorBrewer')
library(hwriter)       # install.packages('hwriter')

# working directory filepath
wd  = '~/github/chn/province2015'


## read in global/eez2014 scores ----
# from final score release: github.com/OHI-Science/ohi-global/releases/tag/v2014.3
v2014.3_url = 'https://raw.githubusercontent.com/OHI-Science/ohi-global/4da6b4a1d69d694264ea68456359a939b0c03f9c/eez2014/scores.csv'
scores_v2014.3 = read.csv(v2014.3_url)

# china's region_id in the global assessment is 209
scores_global_china = scores_v2014.3 %>%            
  filter(region_id == 209) %>%
  mutate(goal = as.character(goal))


## read in chn/province2015 scores         
local_csv = file.path(wd, 'scores.csv')      
scores_local = read.csv(local_csv) %>%
  mutate(goal = as.character(goal))


## join OHI+ and global scores ----
scores_compare = scores_global_china %>%   
  select(goal, 
         dimension,
         score_global = score) %>%         
  left_join(scores_local %>%
              filter(region_id == 0) %>%
              select(goal, 
                     dimension, 
                     score_local = score), 
            by = c('goal', 'dimension')) %>%
  filter(dimension %in% c('future', 'score', 'status')) %>%
  mutate(score_global = round(score_global, 1),
         score_local  = round(score_local, 1),
         score_diff = round(score_global - score_local, 1))


## make an html file that is color coded ----
pal = brewer.pal(10, 'RdYlBu')
pal = pal[1:9]

# cols = data.frame(goal = scores_compare$goal)
cols = scores_compare[, 'score_diff']
cols = assign('score_diff', cut(cols, 
                                 breaks=c(-100, -50, -30, -20, -10, 0, 10, 20, 50, 100), 
                                 include.lowest = TRUE, labels=pal))
 
hwrite(scores_compare, 
       file.path(wd, 'reports', 'compare_scores_OHIplus_global.html'), br=TRUE, center=TRUE, border=0, 
       row.style=list(goal='text-align:center'), table.style='padding: 10px; margin:20px;', 
       col.bgcolor=list(scenario='#fff',dimension='#fff',country='#fff', region_id='#fff', score_diff=cols))
      
