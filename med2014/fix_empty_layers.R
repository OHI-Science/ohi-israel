
library(dplyr)

# assume you are already in ohi-israel/med2014 folder

n_rgn = 6 # number of regions within ohi-israel

dir_layers_empty = 'tmp/layers_empty'
dir_layers = 'layers'

for (f in list.files(path = file.path(dir_layers_empty), full.names=T)) { # f = "tmp/layers_empty/np_cyanide_global2013.csv"
  d = read.csv(f); head(d)
  n = names(d) 
  
  if ('rgn_id' %in% names(d)){
    d_new = data.frame('rgn_id' = 1:n_rgn, 'tmp_name' = rep(NA, n_rgn))
  }         
  
  if ('cntry_key' %in% names(d)){
    d_new = data.frame('cntry_key' = 'ISR', 'sector' = 'ph', 'tmp_name' = NA) 
    if ('year' %in% names(d)) {
      d_new = cbind(d_new, 'year' = 2000)
    }
  } 
  
  names(d_new)[names(d_new) == 'tmp_name'] = n[2]
  write.csv(d_new, file.path(dir_layers, basename(f)), na = '', row.names = F)
}
