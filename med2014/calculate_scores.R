# presume that working directory in current scenario directory, eg: setwd('~/github/ohi-israel/med2014')

# load conf
conf = Conf('conf')

# run checks on layers
CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields) 

# load layers
layers = Layers('layers.csv', 'layers') 
# source(fix_empty_layers.r) # run if there are errors about empty files '...has no rows of data'

# calculate scores
scores = CalculateAll(conf, layers, debug=F)
write.csv(scores, 'scores.csv', na='', row.names=F)
