# This script installs:
# - ohicore R library with core functions and interactive application
#   [https://github.com/ohi-science/ohicore]
# - ohi-israel files with all data specific to Israel 
#   and launch_app.* to launch the application populated with Israel data and scores
#   [https://github.com/ohi-science/ohi-israel]
# - all R libraries upon which ohicore depends

# check base R version
if (as.integer(R.Version()$major) < 3){
  stop('Please update your R software to the latest (requires version 3 or higher), such as at http://cran.rstudio.com.')
}

# remove old packages
for (p in c('ohicore','ohigui','rCharts','git2r','devtools')){  
  if (p %in% rownames(installed.packages())){
    lib = subset(as.data.frame(installed.packages()), Package==p, LibPath, drop=T)
    remove.packages(p, lib)  
  }
}

# install the latest devtools
install.packages('devtools')

# install github packages
library(devtools)
install_github('ohi-science/rCharts')
install_github('ohi-science/ohicore')

# variables for local assessment, downloaded from github repository, containing scenario(s)
github_repo  = 'ohi-science/ohi-israel'
dir_local    = '~/ohi-israel'
scenarios    = c('med2014')

# get scenarios and write shortcuts
suppressWarnings(library(ohicore))
get_scenarios(github_repo, dir_local, scenarios)

# launch app for the first scenario
launch_app(file.path(dir_local, scenarios[1]))
