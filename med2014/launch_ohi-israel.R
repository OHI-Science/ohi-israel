# launch_ohi-israel.R
# prepare the toolbox for ohi-israel assessment

# remove old packages
for (p in c('ohicore','ohigui','rCharts')){  
  if (p %in% rownames(installed.packages())){
    lib = subset(as.data.frame(installed.packages()), Package==p, LibPath, drop=T);
    remove.packages(p, lib)  
  }
}

# install dependencies
for (p in c('devtools')){
  if (!require(p, character.only=T)){
    install.packages(p)
    require(p, character.only=T)
  }
}

# install ohicore
install_github('ohi-science/rCharts')
load_all('~/github/ohicore') 
library(ohicore)

# set working directory
setwd("C:/Users/Lenovo/Google Drive/Maarag/Personal/Hila/ohi-israel/med2014") 
# setwd('~/github/ohi-israel/med2014') # if it's possible to save the folder in a shorter path that is preferred

# run toolbox calculations
source('calculate_scores.r') # runs local ohi-israel/med2014/calculate_scores.r
