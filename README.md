ohi-israel: Israel's Ocean Health Index
==========

The scenario files can be found here:

https://github.com/bbest/ohi-israel

As long as the ohicore library is installed (see [Install](http://ohi-science.org/pages/install.html)), 2x-clicking on the appropriate launchApp (.bat for Windows, .command for Mac) should open the toolbox with Israel layers loaded to look like this:

![image](https://cloud.githubusercontent.com/assets/2837257/3241407/b243a7a4-f13c-11e3-9e0b-62a0a185d827.png)

## Regions
The 4 regions and ancillary buffers (eg offshore3nm, inland1km) were created at: 

https://github.com/ohi-science/ohiprep/tree/master/Israel/Hamaarag-Regions_v2014a

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