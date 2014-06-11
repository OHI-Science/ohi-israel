# create initial Baltic layers based on Global2013.www2013 layers
# Requirements:
#  1. spatial prep
#  1. use RStudio to setup project
#  1. install ohicore: http://ohi-science.org/pages/install.html
#  1. clone of https://github.com/ohi-science/ohibaltic in ../ohicore

# get paths based on host machine
dirs = list(
  neptune_data  = '/Volumes/data_edit', 
  neptune_local = '/Volumes/local_edit',
  ohiprep       = '../ohiprep',
  ohicore       = '../ohicore')

# load ohicore, development mode
library(devtools)
load_all(dirs$ohicore)

# other vars
f_spatial    = c(sprintf('%s/Israel/Hamaarag-Regions_v2014a/data/regions_gcs.js', dirs$ohiprep))
dir_conf_in  = sprintf('%s/inst/extdata/conf.Global2013.www2013'  , dirs$ohicore)
dir_lyrs_in  = sprintf('%s/inst/extdata/layers.Global2013.www2013', dirs$ohicore)

# create dirs
for (dir in c('tmp','layers','conf','spatial')) dir.create(dir, showWarnings=F)

# load Google spreadsheet for 2013 layers
g.url = 'https://docs.google.com/spreadsheet/pub?key=0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE&output=csv'
g0 = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings='')
write.csv(g0, 'tmp/layers_navigation_2012a_2013a.csv', na='', row.names=F)

# modify
lyrs = g0 %.%
  filter(ingest==T) %.%
  select(
    targets, layer, layer_old, name, description, 
    citation_nature2012, citation_plos2013, 
    fld_value, units,
    dir_2013a, fn_2013a) %.%
  mutate(
    filename = sprintf('%s_global2013.csv', layer)) %.%
  arrange(targets, layer)
write.csv(lyrs, 'tmp/layers.csv', na='', row.names=F)

# csvs for regions and countries
rgn_new_csv   = '../ohiprep/Israel/Hamaarag-Regions_v2014a/data/rgn_data.csv'
rgn_old_csv   = sprintf('%s/%s', dir_lyrs_in, subset(lyrs, layer=='rgn_labels', fn_2013a, drop=T))
cntry_old_csv = sprintf('%s/%s', dir_lyrs_in, subset(lyrs, layer=='cntry_rgn' , fn_2013a, drop=T))

# old to new regions
rgn_new = read.csv(rgn_new_csv) %.%
  filter(rgn_type=='eez') %.%
  select(rgn_id_new=rgn_id, rgn_name_new=rgn_name) %.%
  mutate(rgn_name_old = 'Israel') %.%
  merge(
    read.csv(rgn_old_csv, na.strings='') %.%
      select(rgn_name_old=label, rgn_id_old=rgn_id),
    by='rgn_name_old', all.x=T) %.%
  select(rgn_id_new, rgn_name_new, rgn_id_old, rgn_name_old) %.%
  arrange(rgn_name_new)

# old to new countries
cntry_new = read.csv(cntry_old_csv) %.%
  select(cntry_key, rgn_id_old=rgn_id) %.%
  merge(
    rgn_new,
    by='rgn_id_old') %.%
  group_by(cntry_key, rgn_id_new) %.%
  summarise(n=n()) %.%
  select(cntry_key, rgn_id_new) %.%
  as.data.frame()

for (i in 1:nrow(lyrs)){ # i=1
  csv_in  = sprintf('%s/%s', dir_lyrs_in, lyrs$fn_2013a[i])
  csv_out = sprintf('layers/%s', lyrs$filename[i])
  
  d = read.csv(csv_in, na.strings='')
  flds = names(d)
  
  if ('rgn_id' %in% names(d)){
    d = d %.%
      filter(rgn_id %in% rgn_new$rgn_id_old) %.%
      merge(rgn_new, by.x='rgn_id', by.y='rgn_id_old') %.%
      mutate(rgn_id=rgn_id_new) %.%
      subset(select=flds)
  }
  if ('cntry_key' %in% names(d)){
    d = d %.%
      filter(cntry_key %in% cntry_new$cntry_key)
  }
    
  if (lyrs$layer[i]=='rgn_labels'){
    csv_out = sprintf('layers/%s_israel2014.csv', lyrs$layer[i])
    lyrs$filename[i] = basename(csv_out)
    d = d %.%
      merge(rgn_new, by.x='rgn_id', by.y='rgn_id_new') %.%
      select(rgn_id, type, label=rgn_name_new)
  }
  write.csv(d, csv_out, row.names=F, na='')
  
  if (nrow(d)==0) {
    dir.create('tmp/layers_empty', showWarnings=F)
    file.copy(csv_in, file.path('tmp/layers_empty', lyrs$filename[i]))
  }  
}

# copy custom layers
for (f in list.files('tmp/layers_custom', full.names=T)){ # f = list.files('tmp/layers_custom', full.names=T)[1]
  file.copy(f, file.path('layers', basename(f)), overwrite=T)  
}

# layers registry
write.csv(select(lyrs, -dir_2013a, -fn_2013a, -layer_old), 'layers.csv', row.names=F, na='')

# run checks on layers
CheckLayers('layers.csv', 'layers', 
            flds_id=c('rgn_id','cntry_key','country_id','saup_id','fao_id','fao_saup_id'))

# order for layers for substitution old to new name in files
lyrs = lyrs %.%
  arrange(desc(nchar(as.character(layer_old))))

# copy configuration files
conf_files = c('config.R','functions.R','goals.csv','pressures_matrix.csv','resilience_matrix.csv','resilience_weights.csv')
for (f in conf_files){ # f = conf_files[2]
  
  f_in  = sprintf('%s/%s', dir_conf_in, f)
  f_out = sprintf('conf/%s', f)
  
  # substitute old layer names with new
  s = readLines(f_in, warn=F, encoding='UTF-8')
  for (i in 1:nrow(lyrs)){ # i=1
    s = gsub(lyrs$layer_old[i], lyrs$layer[i], s, fixed=T)
  }
  writeLines(s, f_out)
  
  # update confugration
  if (f=='config.R'){
    # set map center and zoom level
    s = gsub('map_lat=0; map_lon=0; map_zoom=3', 'map_lat=32.5; map_lon=34.5; map_zoom=8', s) # Israel specific
    # use just rgn_labels (not rgn_global)
    s = gsub('rgn_global', 'rgn_labels', s)
  }
  
  writeLines(s, f_out)
}

# append custom functions to overwrite others
cat(
  paste(c(
    '\n\n\n# CUSTOM FUNCTIONS ----\n',
    readLines('tmp/functions_custom.R', warn=F, encoding='UTF-8')), 
    collapse='\n'), 
  file='conf/functions.R', append=T)
    
# DEBUG: copy custom layers
for (f in list.files('tmp/layers_custom', full.names=T)){ # f = list.files('tmp/layers_custom', full.names=T)[1]
  file.copy(f, file.path('layers', basename(f)), overwrite=T)  
}

# calculate scores
layers = Layers('layers.csv', 'layers')
conf   = Conf('conf')
scores = CalculateAll(conf, layers, debug=T)
write.csv(scores, 'scores.csv', na='', row.names=F)

# spatial
for (f in f_spatial){ # f = f_spatial[1]
  file.copy(f, sprintf('spatial/%s', basename(f)))
}
 
# create shortcuts
WriteScenario(
  scenario = list(
    conf    = conf, 
    layers  = layers, 
    scores  = scores,
    spatial = 'spatial',
    dir     = '.'),
  make_all_launchApp=T)

# launch on Mac
system('open launchApp.command')