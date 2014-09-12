ohi-israel: Israel's Ocean Health Index
==========


## OHI Toolbox App with Israel Scenario  


The OHI toolbox loaded with the Israel scenario layers initially looks like this in your web browser when launched:

![ohi-israel_tbx_screen](med2014/tmp/fig/ohi-israel_tbx_screen.png)


## Updated ohi-israel, August 19
**ohi-israel** is now running properly, with updated Israel-specific information incorporated for AO, NP, and MAR.  

To troubleshoot the 'id_num not found' error we had (logged on the new [troubleshooting page](https://github.com/OHI-Science/ohimanual/blob/master/tutorials/toolbox_troubleshooting/toolbox_troubleshooting.md#error-in-matchx-table-nomatch--ol--object-id_num-not-found)), I began with a fresh **ohi-israel** repository and incorporated all of Hila's changes for AO, NP and MAR. I incorporated the changes for each goal sequentially (first NP, then AO, then MAR), and committed them to GitHub separately to track progress and identify when the error emerged. The updated changes from Hila's work were:

1. saved new layers in the [layers](https://github.com/OHI-Science/ohi-israel/tree/master/med2014/layers) folder (see timestamps)
2. registered the layers in [layers.csv](https://github.com/OHI-Science/ohi-israel/blob/master/med2014/layers.csv) ([history of changes](https://github.com/OHI-Science/ohi-israel/commits/master/med2014/layers.csv))
3. registered the layers in [goals.csv](https://github.com/OHI-Science/ohi-israel/blob/master/med2014/conf/goals.csv) ([history of changes](https://github.com/OHI-Science/ohi-israel/commits/master/med2014/conf/goals.csv))
4. updated goal models in [functions.r](https://github.com/OHI-Science/ohi-israel/blob/master/med2014/conf/functions.R) ([history of changes](https://github.com/OHI-Science/ohi-israel/commits/master/med2014/conf/functions.R))
5. checked (and updated when appropriate) [pressures_matrix.csv](https://github.com/OHI-Science/ohi-israel/blob/master/med2014/conf/pressures_matrix.csv) ([history of changes](https://github.com/OHI-Science/ohi-israel/commits/master/med2014/conf/pressures_matrix.csv)) and [resilience_matrix.csv](https://github.com/OHI-Science/ohi-israel/blob/master/med2014/conf/resilience_matrix.csv) ([history of changes](https://github.com/OHI-Science/ohi-israel/commits/master/med2014/conf/resilience_matrix.csv)). 

I found that the answer to the error was because of the NP entry in `resilience_matrix.csv`: it requires at least one input per goal (we had removed all of them: (see [old](https://github.com/OHI-Science/ohi-israel/blob/03e990f3bd3f25eae291fea46059f4f55769ddc6/med2014/conf/resilience_matrix.csv#L14) vs. [new](https://github.com/OHI-Science/ohi-israel/blob/master/med2014/conf/resilience_matrix.csv#L14)).  

**Please note:** This fix makes the Toolbox work correctly, however it will be important to review `pressures_matrix.csv` and `resilience_matrix.csv` and decide which pressures are appropriate for the modified NP goal. 
  
**Next Steps:**  

1. [Download](https://github.com/OHI-Science/ohi-israel#install-alternative) this updated version of **ohi-israel** from GitHub, and overwrite the old folder `C:/Users/Lenovo/Google Drive/Maarag/Personal/Hila/ohi-israel`. The updated **ohi-israel** runs correctly with Israeli data incorporated. 
2. [Download](https://github.com/OHI-Science/ohicore) the **ohicore** repository and save on your local machine in the same folder as ohi-israel.
3. Use [launch_ohi-israel.r](https://github.com/OHI-Science/ohi-israel/blob/master/med2014/launch_ohi-israel.R) to launch the Toolbox. Be sure to set the file paths correctly for [ohicore](https://github.com/OHI-Science/ohi-israel/blob/master/med2014/launch_ohi-israel.R#L26) and for the [working directory](https://github.com/OHI-Science/ohi-israel/blob/master/med2014/launch_ohi-israel.R#L30). **launch_ohi-israel.r** will execute the proper setup and call calculate_scores.r, which runs the Toolbox. 
  
Also note: the 12 warning messages that will appear after running the Toolbox are because of the LE data layers that are empty, which will need to be addressed:

  - le_wage_cur_adj_value
  - le_wage_cur_base_value
  - le_wage_ref_adj_value
  - le_wage_ref_base_value
  - le_wage_sector_year


Note: old install instructions have been deleted; see an [older version](https://github.com/OHI-Science/ohi-israel/blob/a9c76a3f489f0e4582e2e5d3478d8bdbaa88eb86/README.md#install-obsolete-now-just-here-as-reference) of this README if interested. 


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
