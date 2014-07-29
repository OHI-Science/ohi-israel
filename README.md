ohi-israel: Israel's Ocean Health Index
==========

## OHI Toolbox App with Israel Scenario

The OHI toolbox loaded with the Israel scenario layers initially looks like this in your web browser when launched:

![ohi-israel_tbx_screen](med2014/tmp/fig/ohi-israel_tbx_screen.png)

## Install

To get the latest Ocean Health Index toolbox `ohicore` and Israel scenario files `ohi-israel` into your home folder `~/github` and launch the application, run the following in R:

```r
# remove old packages
for (p in c('ohicore','ohigui','rCharts')){  
  if (p %in% rownames(installed.packages())){
    lib = subset(as.data.frame(installed.packages()), Package==p, LibPath, drop=T)
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

# install github packages
install_github('ohi-science/rCharts')
install_github('ohi-science/ohicore')

# define destination of ohi-israel scenario
scenario = '~/ohi-israel/med2014'
cat(sprintf('Writing https://github.com/ohi-science/ohi-israel to:\n  %s\n', 
  suppressWarnings(normalizePath(dirname(scenario)))))
    
# get scenario
library(ohicore)
get_scenarios('ohi-science/ohi-israel', dirname(scenario))

# write launch_app shortcuts specific to R path of operating system
write_shortcuts(scenario)
cat(sprintf('In future, you can launch app to scenario with:\n  %s\n', 
  suppressWarnings(normalizePath(file.path(scenario, 'launch_app.*')))))

# launch app (can use launch_app.* from inside scenario folder to launch in future)
launch_app(scenario)
```

After the install you can double click on the appropriate launch_app.* (*.bat for Windows, *.command for Mac) inside the '~/ohi-israel/med2014' folder to open in future.

As an alternative to `get_scenarios` in the install script above, you can [fork and clone](https://help.github.com/articles/fork-a-repo) this repository. (I recommend cloning to `~/github/ohi-israel`). This would allow you to commit changes to your local version, push commits up to github for offsite archiving, and eventually make a pull request to have those changes merged back to ohi-science/ohi-israel. See [Setup](https://github.com/OHI-Science/ohiprep/wiki/Setup) for installation of git, Github and RStudio. If you wish to modify the ohi-science/ohi-israel files directly (so no need to fork or send pull requests; simply clone from ohi-science/ohi-israel and push/pull from there), you can alternatively provide your Github username (easy to [signup](http://github.com)) to bbest@nceas.ucsb.edu for write access.

## Regions
The 6 regions and ancillary buffers (offshore, offshore1km, offshore3nm, inland, inland1km, inland25km) were created at: 

  https://github.com/OHI-Science/ohiprep/tree/master/Israel/Hamaarag-Regions_v2014a

![map](https://raw.githubusercontent.com/OHI-Science/ohiprep/master/Israel/Hamaarag-Regions_v2014a/fig/ISR-regions_v2-gadm.png)

## Layers
So far, this is just a straight copy of the OHI Global2013 layers extracting just for Israel and replicating per region (n=4). So there's no differentiation yet between regions since they all have the same value.

The easy next step to gain differentiation between regions is extracting the detailed pressure rasters (a la [Halpern et al 2008 Impacts](http://www.nceas.ucsb.edu/globalmarine/impacts)).

It's also worth noting that the following layers are empty (ie no data available for Israel in the original Global2013 data):
- pressures
  +  fp_art_hb
  +  fp_targetharvest
  +  hd_subtidal_hb
- resilience
  +  msi_gov
- LIV
  +  le_wage_cur_adj_value
  +  le_wage_cur_base_value
  +  le_wage_ref_adj_value
  +  le_wage_ref_base_value

## Functions
The following functions had to be customized from the Global2013:
- LIV_ECO: Israel has no wages data so LIV is just based on jobs. Removed missing regions addendum from 2013 global and georegional gapfilling.
- TR: skipped georegional gapfilling and assigning NA to uninhabitated islands

## More
Please visit t
