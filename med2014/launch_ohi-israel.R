# launch_ohi-israel.R
# prepare the toolbox for ohi-israel assessment

# make sure R and RStudio are updated. 
# update.packages() # update all packages

# note: to run, also download the 'ohicore' repo and change the path below.

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

# install rCharts
install_github('ohi-science/rCharts')

# set ohicore directory and install ohicore
library(devtools)
load_all('~/github/ohicore') # !! set this path after downloading and saving repo from github.com/OHI-Science/ohicore
library(ohicore)
library(reshape2)

# set working directory
# setwd("C:/Users/Lenovo/Google Drive/Maarag/Personal/Hila/ohi-israel/med2014") 
setwd('~/github/ohi-israel/med2014') # if it's possible to save the folder in a shorter path that is preferred

# run toolbox calculations
source('calculate_scores.r') # runs local ohi-israel/med2014/calculate_scores.r

# flower plots and table
source('report.r')

# view results in the Toolbox App
source('launch_app_code.r')
