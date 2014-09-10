#library(devtools)
#load_all('~/github/ohicore')
#devtools::install('~/github/ohicore')
suppressWarnings(library(ohicore))

# presume that working directory in current scenario directory, eg:
setwd('~/github/ohi-israel/med2014')

# load conf
conf = Conf('conf')

#cat(sprintf('DEBUG calculate_scores.R:32, fxn environment summarize: %s\n', environmentName(environment(summarize)) ))

# run checks on layers
#CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

# load layers
layers = Layers('layers.csv', 'layers')

# calculate scores
scores = CalculateAll(conf, layers, debug=F)
write.csv(scores, 'scores.csv', na='', row.names=F)
