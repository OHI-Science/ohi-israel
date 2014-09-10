# Ocean Health Index Report
# 'Israel' is the name of the entire study area: it is all 6 regions combined

# options:
overwrite  = T
israel_only= F
do_flowers = T
do_tables  = T # tables not saved


# remove 'CS' from goals and scores
conf$goals = conf$goals %>%
  filter(goal != 'CS')
scores = scores %>% filter(goal != 'CS')

# create directories
dir_report  = 'reports'
dir.create(dir_report, recursive=T, showWarnings=F)

# set viz options
ohi.options <- function() {
    double.digits <- 15 # <- floor(log10(.Machine$double.base^.Machine$double.digits)) 
    options(digits=double.digits)
    options(stringsAsFactors=FALSE) # to prevent factors
    options(width=120) # for outputting wide columns
    options(rstudio.markdownToHTML = 
              function(inputFile, outputFile) {      
                # example: eg /var/data/ohi/model/GL-NCEAS-Pressures_Matrix/report9.Rmd
                # see: http://www.rstudio.com/ide/docs/authoring/markdown_custom_rendering
                # original: '/Applications/RStudio.app/Contents/Resources/resources/markdown.css'
                markdownToHTML(inputFile, options=getOption('markdown.HTML.options'), outputFile, stylesheet=ohi.markdown.css)   
              })
    options()
}
opt_old = options(ohi.options())

# get goals for flowers, all and specific to weights
goals.all = arrange(conf$goals, order_color)[['goal']]

# get colors for aster, based on 10 colors, but extended to all goals. subselect for goals.wts
cols.goals.all = colorRampPalette(RColorBrewer::brewer.pal(10, 'Spectral'), space='Lab')(length(goals.all))
names(cols.goals.all) = goals.all

# get subgoals and goals, not supragoals, for doing flower plot
goals_supra = na.omit(unique(conf$goals$parent))
wts = with(subset(conf$goals, !goal %in% goals_supra, c(goal, weight)), setNames(weight, goal))
goal_labels = gsub('\\n', '\n', with(conf$goals, setNames(name_flower, goal))[names(wts)], fixed=T)

# region names, ordered by GLOBAL and alphabetical
rgn_names = rename(SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T), 
                   c('id_num'='region_id', 'val_chr'='rgn_name'))[,c('region_id','rgn_name')]
rgn_names = rbind(data.frame(region_id=0, rgn_name='ISRAEL'), 
                  arrange(rgn_names, rgn_name))

# determine regions
if (israel_only){
  rgns = 0
} else {
  rgns = rgn_names$region_id
}

# directory to store figures
dir_fig = file.path(dir_report, 'figures')
dir.create(dir_fig, showWarnings=F)

# use factors to sort by goal and dimension in scores
conf$goals = arrange(conf$goals, order_hierarchy)
scores$goal_label = factor(
  scores$goal, 
  levels = c('Index', conf$goals$goal),
  labels = c('Index', ifelse(!is.na(conf$goals$parent),
                             sprintf('. %s', conf$goals$name),
                             conf$goals$name)),
  ordered=T)
scores$dimension_label = factor(
  scores$dimension,
  levels = names(conf$config$dimension_descriptions),
  ordered=T)

# loop through regions
for (rgn_id in rgns){ # rgn_id=0
   
  # header md
  rgn_name = subset(rgn_names, region_id==rgn_id, rgn_name, drop=T)
  cat(sprintf('\n## %s (%d)\n\n', rgn_name, rgn_id))
  
  # flower plot ----
  if (do_flowers){
  
    # region scores    
    x = with(subset(scores, dimension=='score' & region_id==rgn_id & goal %in% names(wts)),
             setNames(score, goal))[names(wts)]
    
    fig_pdf = sprintf('%s/flower_%s.pdf', dir_fig, gsub(' ','_', rgn_name))
    fig_png = sprintf('%s/flower_%s.png', dir_fig, gsub(' ','_', rgn_name))
    res=72
    if (overwrite | !file.exists(fig_png)){
      png(fig_png, width=res*7, height=res*7)
      PlotFlower(main = rgn_name,
                 lengths=x,
                 widths=wts,
                 fill.col=ifelse(is.na(x), 
                                 'grey80', 
                                 cols.goals.all[names(wts)]),
                 labels  =ifelse(is.na(x), 
                                 paste(goal_labels, '-', sep='\n'), 
                                 paste(goal_labels, round(x), sep='\n')),
                 center=round(weighted.mean(x, wts, na.rm=T)),
                 max.length = 100, disk=0.4, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
      dev.off()      
    }
    
    if (overwrite | !file.exists(fig_pdf)){
      pdf(fig_pdf, width=7, height=7)
      PlotFlower(main = rgn_name,
                 lengths=x,
                 widths=wts,
                 fill.col=ifelse(is.na(x), 
                                 'grey80', 
                                 cols.goals.all[names(wts)]),
                 labels  =ifelse(is.na(x), 
                                 paste(goal_labels, '-', sep='\n'), 
                                 paste(goal_labels, round(x), sep='\n')),
                 center=round(weighted.mean(x, wts, na.rm=T)),
                 max.length = 100, disk=0.4, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
      dev.off()      
    }
    
  }
  
  # table md
  if (do_tables){
    
    x = dcast(subset(scores, region_id==rgn_id), goal_label ~ dimension_label, value.var='score')
    row.names(x) = x$goal_label; x = x[, names(x)!='goal_label']
#     knitr::kable(x, format='markdown')  
  }
}

dir_scen = getwd()
cat(sprintf('\nfigures saved in %s\n', file.path(dir_scen, dir_fig)))
