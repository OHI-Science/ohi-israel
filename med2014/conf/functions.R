Setup = function(){
  
#   extra.packages.required = c('zoo') # zoo for MAR(), NP()
#   
#   # install packages if needed
#   for (p in extra.packages.required){
#     if (!suppressWarnings(library(p, character.only=T, logical.return=T))){
#       cat(sprintf('\n\nInstalling %s...\n', p))
#       install.packages(p)
#       require(p, character.only=T)
#     }
#   }
  
  require(zoo)
  
}

FIS = function(layers, status_year=2010){
  
  # layers used: fis_meancatch, fis_b_bmsy, fis_proparea_saup2rgn
  
  # catch data
  c = SelectLayersData(layers, layers='fis_meancatch', narrow=T) %.%
    select(
      fao_saup_id    = id_chr,
      taxon_name_key = category,
      year,
      catch          = val_num)  
  
  # separate out the region ids:
  c$fao_id    <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[1]))
  c$saup_id   <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[2]))
  c$TaxonName <- sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[1])
  c$TaxonKey  <- as.numeric(sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[2]))
  c$catch     <- as.numeric(c$catch)
  c$year      <- as.numeric(as.character(c$year))
  #Create Identifier for linking assessed stocks with country-level catches
  c$stock_id <- paste(as.character(c$TaxonName),
                      as.character(c$fao_id), sep="_")
  
  # b_bmsy data
  b = SelectLayersData(layers, layer='fis_b_bmsy', narrow=T) %.%
    select(
      fao_id         = id_num,
      TaxonName      = category,
      year,
      bmsy           = val_num)
  # Identifier taxa/fao region:
  b$stock_id <- paste(b$TaxonName, b$fao_id, sep="_")
  b$bmsy     <- as.numeric(b$bmsy)
  b$fao_id   <- as.numeric(as.character(b$fao_id))
  b$year     <- as.numeric(as.character(b$year))
  
  # area data for saup to rgn conversion
  a = layers$data[['fis_proparea_saup2rgn']] %.%
    select(saup_id, rgn_id, prop_area)
  a$prop_area <- as.numeric(a$prop_area)
  a$saup_id   <- as.numeric(as.character(a$saup_id))
  a$rgn_id    <- as.numeric(as.character(a$rgn_id))
  
  # ------------------------------------------------------------------------
  # STEP 1. Merge the species status data with catch data
  #     AssessedCAtches: only taxa with catch status data
  # -----------------------------------------------------------------------
  AssessedCatches <- join(b, c, 
                          by=c("stock_id", "year"), type="inner")
  
  # b,c by stock_id
  
  # include only taxa with species-level data
  AssessedCatches <- AssessedCatches[as.numeric(AssessedCatches$TaxonKey)>=600000, ]
  AssessedCatches$penalty <- 1
  
  # ------------------------------------------------------------------------
  # STEP 2. Estimate status data for catch taxa without species status
  #     UnAssessedCatches: taxa with catch status data
  # -----------------------------------------------------------------------  
  UnAssessedCatches <- c[!(c$year %in% AssessedCatches$year &
                             c$stock_id %in% AssessedCatches$stock_id), ]
  
  # 2a.  Join UnAssessedCatches data to the b_bmsy summaries for each FAO/Year
  
  # Average status data for assessed stocks by FAO region for each year. 
  # This is used as the starting estimate for unassesed stocks
  # Here, the Median b_bmsy was chosen for TaxonKey >= 600000 
  # and Min b_bmsy for TaxonKey < 600000
  #  *************NOTE *****************************
  #  Using the minimum B/BMSY score as an starting point
  #  for the estimate of B/BMSY for unassessed taxa not
  #  identified to species level is very conservative.
  #  This is a parameter that can be changed.
  #  ***********************************************
  b_summary <- ddply(b, .(fao_id, year), summarize,
                     Medianb_bmsy=quantile(as.numeric(bmsy), probs=c(0.5)), 
                     Minb_bmsy=min(as.numeric(bmsy)))
  
  UnAssessedCatches <- join(UnAssessedCatches, b_summary, by=c("fao_id", "year"),
                            type="left", match="all")
  # 2b.  Create a penalty variable based on taxa level:
  UnAssessedCatches$TaxonPenaltyCode <- substring(UnAssessedCatches$TaxonKey,1,1)
  
  # 2c. Create a penalty table for taxa not identified to species level
  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the 
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  penaltyTable <- data.frame(TaxonPenaltyCode=1:6, 
                             penalty=c(0.01, 0.1, 0.25, 0.5, 0.75, 1))
  # 2d.Merge with data
  UnAssessedCatches <- join(UnAssessedCatches, penaltyTable, by="TaxonPenaltyCode")
  
  # ------------------------------------------------------------------------
  # STEP 3. Calculate score for all taxa based on status (b/bmsy) and taxa
  # -----------------------------------------------------------------------
  
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05
  
  ## Function to calculate score for different scenarios:
  score <- function(data, variable){
    #data <- AssessedCatches
    #variable <- "bmsy"
    ifelse(data[ ,variable]*data[, "penalty"]<lowerBuffer,
           data[ ,variable]*data[, "penalty"],
           ifelse(data[ ,variable]*data[, "penalty"]>upperBuffer,
                  ifelse(1-alpha*(data[ ,variable]*data[, "penalty"]
                                  -upperBuffer)>beta,
                         1-alpha*(data[ ,variable]*data[, "penalty"]-upperBuffer),beta),
                  1))
  }
  
  AssessedCatches$score <- score(data=AssessedCatches, variable="bmsy")
  
  # Median is used to calculate score for species with Taxon 6 coding
  UnAssessedCatchesT6 <- subset(UnAssessedCatches, penalty==1)
  UnAssessedCatchesT6$score <- score(UnAssessedCatchesT6, "Medianb_bmsy")
  
  UnAssessedCatches <- subset(UnAssessedCatches, penalty!=1)
  UnAssessedCatches$score <- score(UnAssessedCatches, "Minb_bmsy")
  
  AllScores <- rbind(AssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                     UnAssessedCatchesT6[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                     UnAssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")])
  
  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each saup_id region
  # -----------------------------------------------------------------------
  
  # 4a. To calculate the weight (i.e, the relative catch of each stock per saup_id),
  # the mean catch of taxon i is divided by the   
  # sum of mean catch of all species in region r, which is calculated as: 
  
  smc <- ddply(.data = AllScores, .(year, saup_id), summarize, 
               SumCatch = sum(catch))   
  AllScores<-join(AllScores,smc,by=c("year","saup_id"))  
  AllScores$wprop<-AllScores$catch/AllScores$SumCatch 
  
  
  #  4b. The "score" and "weight" values per taxon per SAUP region are used to  
  #    calculate a geometric weighted mean across taxa for each saup_id region
  geomMean <- ddply(.data = AllScores, .(saup_id, year), summarize, status_saup = prod(score^wprop)) 
  
  # ------------------------------------------------------------------------
  # STEP 5. Convert status from saup spatial scale to OHI spatial scale  
  # -----------------------------------------------------------------------
  # In many cases the ohi reporting regions are comprised of multiple saup regions.
  # To correct for this, the proportion of each saup area of the total area of the 
  # OHI region was calculated. This was used to calculate Status from the Status_saup.
  # This type of adjustment is omitted if the data were collected at the same spatial 
  # scale as the collecting region.
  
  # Join region names/ids to Geom data
  geomMean <- join(a, geomMean, type="inner", by="saup_id") # merge km2 of shelf area with status results
  
  # weighted mean scores
  #StatusData <- ddply(.data = geomMean, .(rgn_id, year), summarize, Status = round(sum(status_saup*prop_area)*100))
  StatusData <- ddply(.data = geomMean, .(rgn_id, year), summarize, Status = sum(status_saup*prop_area))
  
  # 2013 status is based on 2011 data (most recent data)
  status = StatusData %.%
    filter(year==status_year) %.%
    mutate(
      score     = round(Status*100),
      dimension = 'status') %.%
    select(region_id=rgn_id, dimension, score)
  
  # ------------------------------------------------------------------------
  # STEP 6. Calculate trend  
  # -----------------------------------------------------------------------
  # NOTE: Status is rounded to 2 digits before trend is 
  # calculated in order to match OHI 2013 results (is this what we want to do?)
  trend = ddply(StatusData, .(rgn_id), function(x){
    mdl = lm(Status ~ year, data=x)
    data.frame(
      score     = round(coef(mdl)[['year']] * 5, 2),
      dimension = 'trend')}) %.%
    select(region_id=rgn_id, dimension, score)
  # %.% semi_join(status, by='rgn_id')
  
  # assemble dimensions
  scores = rbind(status, trend) %.% mutate(goal='FIS')
  return(scores)  
}

MAR = function(layers){
  
  # status
  r.status = rename(SelectLayersData(layers, layers='rn_mar_status', narrow=T), c('id_num'='region_id','val_num'='score'))
  r.status$score = r.status$score * 100
  
  # trend
  r.trend = rename(SelectLayersData(layers, layers='rn_mar_trend', narrow=T), c('id_num'='region_id','val_num'='score'))
  
  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='MAR'))
  return(scores)  
}

FP = function(layers, scores){
  # weights
  w = rename(SelectLayersData(layers, layers='fp_wildcaught_weight', narrow=T),
             c('id_num'='region_id', 'val_num'='w_FIS')); head(w)
  
  # scores
  s = dcast(scores, region_id + dimension ~ goal, value.var='score', subset=.(goal %in% c('FIS','MAR') & !dimension %in% c('pressures','resilience'))); head(s)
  
  # combine
  d = merge(s, w)
  d$w_MAR = 1 - d$w_FIS
  d$score = apply(d[,c('FIS','MAR','w_FIS', 'w_MAR')], 1, function(x){ weighted.mean(x[1:2], x[3:4]) })
  d$goal = 'FP'
  
  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}


# Hila, 8.6.14 - Change the model to calculate status according to stocks for each species relative to max stock landings (see documentation)
# Hila, 6.8.14 - Calculate status for Israel and duplicate score for regions, because no data is available at region level
AO = function(layers,
              year_max=max(layers_data$year, na.rm=T), 
              year_min=max(min(layers_data$year, na.rm=T), max(layers_data$year, na.rm=T)-10)){
  
  # cast data
  layers_data = SelectLayersData(layers, targets='AO')
  
  rky = rename(dcast(layers_data, category + year ~ layer, value.var='val_num', 
                     subset = .(layer %in% c('rky_ao_stock'))),
               c('category'='sciname', 'rky_ao_stock'='stock')); head(rky); summary(rky)
  
  n=5 # use only the last n years to calculate trend
  
  # calcultae max stock rates and recent trend for each year (using only years >= year_min)
  rky2 = ddply(rky, .(sciname), function(x){
    data.frame(max_stock = max(x$stock),max_yr = max(x$year[which.max(x$stock)]),
               apply(embed(as.matrix(x[c('year','stock')]),n), 1, function(x){  
                 
                 # lm for stock rate ~ years
                 year = x[!(1:(n*2) %% 2) == 0]
                 stock = x[(1:(n*2) %% 2) == 0]
                 
                 data.frame(recent_trend = lm(stock ~  year)$coefficients[['year']],
                            year = year[1])
               }))
  })
  
  # reshape data frame from wide (year.1, year.2... ect) to long format (with 1 year, trend column)
  rky2 = reshape(rky2,varying=list(names(rky2)[grep('year',names(rky2))],names(rky2)[grep('recent_trend',names(rky2))]),
                 v.names=c("year","recent_trend"),direction="long")
  
  rky = merge(rky,rky2,all.x=T,by=c('sciname','year'))
  
  # Set status according to FAO weight from Sea Around Us project (see documentation)
  rky3 = ddply(subset(rky, year >= year_min), .(sciname, year), function(x){
    if (x$year < x$max_yr) # Stock landings have not reached a peak or peak occurs in the last year of the times series
    {
      # Exploitation Category = Developing (w=1)
      w=1
    }
    else
    {
      if (x$stock/x$max_stock > 0.5) # Stock landings are between 50-100% of peak
      {
        # Exploitation Category = Fully exploited (w=1)
        w=1
      }
      
      if (x$stock/x$max_stock < 0.1) # Stock landings are < 10% of peak 
      {
        # Exploitation Category = Collapsed (w=0)
        w=0
      }
      
      if (x$stock/x$max_stock > 0.1 & x$stock/x$max_stock < 0.5) # Stock landings are between 10-50% of peak
      {
        if (x$recent_trend >= 0)
        {
          # Exploitation Category = Overexploited (w=0.5)
          w=0.5
        }
        
        if (x$recent_trend < 0)
        {
          # Exploitation Category = Rebuilding (w=0.25)
          w=0.25
        }
      }
      
    }
    data.frame (weight = w)
  })
  
  rky = merge(rky,rky3,by=c('sciname','year'))
  
  # yearly status is set as mean weight across species, per year
  ry = ddply(rky, .(year), summarize, status = mean(weight)); head(ry)
  
  # Hila - duplicate national status for all regions
  ry = data.frame(region_id=rep(1:6,times=1,each=nrow(ry)),ry[rep(seq_len(nrow(ry)), 6), ]); summary(ry); dim(ry)
  
  # status
  r.status = subset(ry, year==year_max, c(region_id,status)); summary(r.status); dim(r.status)
  r.status$status = r.status$status * 100
  
  # trend: trend is the slope of
  # status before multiplied by 100 (status between 0-1)
  r.trend = ddply(ry, .(region_id), function(x){
    data.frame(trend = if(length(na.omit(x$status))>1) 
    {
      d = data.frame(status=x$status, year=x$year)[tail(which(!is.na(x$status)), 5),]
      lm(status ~ year, d)$coefficients[['year']]
      
    } else {
      NA
    }
    )})
  
  # return scores
  s.status = cbind(rename(r.status, c('status'='score')), data.frame('dimension'='status')); head(s.status)
  s.trend  = cbind(rename(r.trend , c('trend' ='score')), data.frame('dimension'='trend')); head(s.trend)
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='AO')); #dlply(scores, .(dimension), summary)
  return(scores)  
}

# Hila, 27/4/14, Changes to Natural products to use desalination only as product (see IsraelUpdate documentation)
NP = function(layers){
  
  # layers
  lyrs = list('rky' = c('rnky_np_desal_relative'    = 'w'), # Desalination score (relative to a target value)
              'rk'  = c('rnk_np_sustainability_score' = 'S')) # Sustainability score
  lyr_names = sub('^\\w*\\.', '', names(unlist(lyrs))) 
  
  # cast data
  D = SelectLayersData(layers, layers=lyr_names)
  year_min=max(min(D$year, na.rm=T), max(D$year, na.rm=T)-10) # min year to caculate 5 last years trend
  status_year=max(D$year, na.rm=T) # status year is max year with data
  
  rky = rename(dcast(D, id_num + category + year ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rky']]))),
               c('id_num'='region_id', 'category'='plant', lyrs[['rky']]))
  rk  = rename(dcast(D, id_num + category ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['rk']]))),
               c('id_num'='region_id', 'category'='plant', lyrs[['rk']]))
  
  # merge W with S
  rky = merge(rky, rk, all.x=T)
  
  # get status across stations, per region and year
  ry = ddply(rky, .(region_id, year), summarize,
             status = sum(w * S) * 100); head(ry)
  r.status = subset(ry, year==status_year, c(region_id,status))
  
  
  # trend: ternd is the slope of
  # status before multiplied by 100 (status between 0-1)
  r.trend = ddply(subset(
    ry, year >= year_min), .(region_id), function(x){      
      data.frame(trend = 
                   if(length(na.omit(x$status))>1) {
                     # use only last valid 5 years worth of status data since year_min
                     d = data.frame(status=x$status, year=x$year)[tail(which(!is.na(x$status)), 5),];
                     lm(status ~ year, d)$coefficients[['year']] / 100
                   } else {
                     NA
                   })});
  
  # return scores
  s.status = cbind(rename(r.status, c('status'='score')), data.frame('dimension'='status'))
  s.trend  = cbind(rename(r.trend , c('trend' ='score')), data.frame('dimension'='trend'))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='NP'))
  return(scores)  
}

CP = function(layers){
  # Israel: CP goal includes rocky reef and sand dunes
  # Hila: update 10.9.14 - CP will only only include sand dunes. Rocky reef removed (see email from Katie in documentation)
  
  # get layer data
  d = 
    join_all(
      list(
        layers$data[['hab_health']] %>%
          select(rgn_id, habitat, health),
        
        layers$data[['hab_trend']] %>%
          select(rgn_id, habitat, trend),
        
        layers$data[['hab_extent']] %>%
          select(rgn_id, habitat, km2)),
      
      by=c('rgn_id','habitat'), type='full') %>% 
    select(rgn_id, habitat, km2, health, trend)
  
  # set ranks for CP habitats 
  # habitat.rank = c('rocky_reef'  = 1, # Israel: updated habitat rank
  #                  'sand_dunes'  = 2)
  habitat.rank = c('sand_dunes'  = 1)
    
    d = d %>%
      filter(habitat %in% names(habitat.rank)) %>%
      mutate(
        rank = habitat.rank[habitat],
        extent = ifelse(km2==0, NA, km2))
    
    scores_CP = rbind_list(
      # status
      d %>% 
        filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
        group_by(rgn_id) %>%
        summarize(      
          score = pmin(1, sum(rank * health * extent) / (sum(extent) * max(rank)) ) * 100,
          dimension = 'status'),
      # trend
      d %>% 
        filter(!is.na(rank) & !is.na(trend) & !is.na(extent)) %>%
        group_by(rgn_id) %>%
        summarize(      
          score = sum(rank * trend * extent) / (sum(extent)* max(rank)),
          dimension = 'trend')) %>%
      mutate(
        goal = 'CP') %>%
      select(region_id=rgn_id, goal, dimension, score)
    
    # return scores
    return(scores_CP)
  d = data.frame(region_id=1:6, goal = 'CP', dimension='blank', score=1:6)
}


# Hila 24/8/14 - TR will be calculated according to Hotels occupancy scores
# Combined with Parks scores. Parks will be gived twice the weight as hotels.
# Final scores should be multiplied by sustainability. see TR documantaion.
TR = function(layers, year_max){
  
  # formula:
  #   H - Hotels scores
  #   P - Parks scores
  #   S - Sustainabiliy
  #   Xtr = (H * 1/3 + P * 2/3) * S
  
  # cast data
  D = SelectLayersData(layers, targets='TR')
  #year_max=max(D$year, na.rm=T) # status year is max year with data
  
  Hotels = rename(dcast(D, id_num + year ~ layer, value.var='val_num', 
                        subset = .(layer %in% c('tr_hotels'))),
                  c('id_num'='region_id', 'tr_hotels'='H')); head(Hotels); summary(Hotels)
  Parks = rename(dcast(D, id_num + year ~ layer, value.var='val_num', 
                       subset = .(layer %in% c('tr_parks'))),
                 c('id_num'='region_id', 'tr_parks'='P')); head(Parks); summary(Parks)
  S = rename(dcast(D, id_num + year ~ layer, value.var='val_num', 
                   subset = .(layer %in% c('tr_sustainability'))),
             c('id_num'='region_id', 'tr_sustainability'='S')); head(S); summary(S)
  
  # merge W with S
  ry = merge(Hotels, Parks, all=T)
  ry$H[is.na(ry$H)] = 0 # Treat na as zero
  ry$P[is.na(ry$P)] = 0
  ry = merge(ry, S, all=T)
  
  # get status per region and year
  ry = data.frame(ry, status = (ry$H * 0.33 * ry$S + ry$P * 0.66) * 100)
  r.status = subset(ry, year==year_max, c(region_id,status))
  
  
  # trend: ternd is the slope of
  # status before multiplied by 100 (status between 0-1)
  r.trend = ddply(ry, .(region_id), function(x){ 
    data.frame(trend = 
                 if(length(na.omit(x$status))>1) {
                   # use only last valid 5 years worth of status data since year_min
                   d = data.frame(status=x$status, year=x$year)[tail(which(!is.na(x$status)), 5),];
                   lm(status ~ year, d)$coefficients[['year']] / 100
                 } else {
                   NA
                 })});
  
  # return scores
  s.status = cbind(rename(r.status, c('status'='score')), data.frame('dimension'='status'))
  s.trend  = cbind(rename(r.trend , c('trend' ='score')), data.frame('dimension'='trend'))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='TR'))
  return(scores)  
  
}

LIV_ECO = function(layers, subgoal){
  
  #   # DEBUG inside
  #   #devtools::install('~/github/ohicore')
  #   suppressWarnings(library(ohicore))
  #   #devtools::load_all('~/github/ohicore')
  #   setwd('~/github/ohi-israel/med2014')
  #   conf   = Conf('conf')
  #   options(warn=0)
  #   #CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)
  #   layers = Layers('layers.csv', 'layers')
  #   scores = read.csv('scores.csv')  
  #   subgoal='LIV'
  
  ## read in all data:
  
  # gdp, wages, jobs and workforce_size data (israel: this is dummy data)
  le_gdp   = SelectLayersData(layers, layers='le_gdp')  %>%
    select(rgn_id = id_num, year, usd = val_num)
  le_wages = SelectLayersData(layers, layers='le_wages') %>%
    select(rgn_id = id_num, year, sector = category, nis = val_num)
  le_jobs  = SelectLayersData(layers, layers='le_jobs') %>%
    select(rgn_id = id_num, year, sector = category, count = val_num)
  le_workforce_size = SelectLayersData(layers, layers='le_workforcesize_size') %>%
    select(rgn_id = id_num, year, workforce_size = val_num)
  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    select(rgn_id = id_num, year, percent_unemployment = val_num)
  
  # debug
  #   le_gdp            = read.csv('layers/le_gdp_israel2014.csv')
  #   le_wages          = read.csv('layers/le_wages_israel2014.csv')
  #   le_jobs           = read.csv('layers/le_jobs_israel2014.csv')  
  #   le_workforce_size = read.csv('layers/le_workforcesize_adj_israel2014.csv')
  #   le_unemployment   = read.csv('layers/le_unemployment_israel2014.csv')
  
  # multipliers from Table S10 (Halpern et al 2012 SOM)
  multipliers_jobs = data.frame('sector' = c('mar', 'tour'), 'multiplier' = c(2.7, 1)) # no multiplers for tour (=1)
  # multipliers_rev  = data.frame('sector' = c('mar', 'tour'), 'multiplier' = c(1.59, 1)) # not used because GDP data is not by sector
  
  # account for inflation: Consumer Price Index convert to 2010 US Dollars (as in global assessment)
  cpi_2010 = read.csv('tmp/cpi_osu.csv') # to convert amounts to a common year based on global inflation
  
  # convert from NIS (aka ILS) to USD: http://www.xe.com/currencytables/
  nis_to_usd = read.csv('tmp/nis_to_usd_xedotcom.csv')
  
  # don't need to account for ppp (purchasing power parity) because not comparing across countries
  
  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - percent_unemployment) / 100, 
           employed            = workforce_size * proportion_employed)
  
  
  # Israel: do you have national [non-marine-only] average wages and GDP, respectively'? (see p. 29 of SOM). 
  # We have employment rates from the global data so we adjusted jobs but can't make the adjustments for wages or GDP
  
  
  # reworded from SOM p.26-27
  #reference point for wages is the reference region (r) with the highest average wages across all sectors. 
  #Reference points for jobs (j) and revenue (e) employ a moving baseline. The two metrics (j, e) are calculated
  #as relative values: the value in the current year (or most recent year), c, relative to the value in a recent 
  #moving reference period, r, defined as 5 years prior to c. This reflects an implicit goal of maintaining coastal
  #livelihoods and economies (L&E) on short time scales, allowing for decadal or generational shifts in what people 
  #want and expect for coastal L&E. The most recent year c must be 2000 or later in order for the data to be included.
  
  
  ## LIV israel
  liv = 
    # adjust jobs
    le_jobs %>%
    left_join(multipliers_jobs, by = 'sector') %>%
    mutate(jobs_mult = count * multiplier) %>%  # adjust jobs by multipliers
    left_join(le_employed, by= c('rgn_id', 'year')) %>%
    mutate(jobs_adj = jobs_mult / proportion_employed) %>% # adjust jobs by proportion employed
    select(rgn_id, year, sector, jobs_adj) %>%
    
    left_join(
      
      # adjust wages
      le_wages %>%
        left_join(nis_to_usd, by = 'year') %>%
        mutate(usd = nis * ils_to_usd) %>%  # convert from nis to usd
        left_join(cpi_2010, by = 'year') %>%
        mutate(wages_adj = usd * cf2010) %>%  # adjust for inflation: to 2010 usd
        select(rgn_id, year, sector, wages_adj),
      # adjust for national wages if data are available
      
      by = c('rgn_id', 'year', 'sector')) %>% # left_join le_jobs and le_wages
    arrange(year, sector, rgn_id)
  
  # debug
  write.csv(liv, 'temp/liv.csv', row.names=F, na='')
  
  ## LIV calculations
  
  # LIV status
  liv_status = liv %>%
    # debug: overwrite with random values differing across regions, years and sectors to see effect of group_by calculations
    # mutate(
    #   jobs_adj  = sample(seq(1,1000), nrow(liv)),
    #   wages_adj = sample(seq(1,1000), nrow(liv))) %>%
    filter(!is.na(jobs_adj) & !is.na(wages_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    arrange(rgn_id, year, sector) %>%
    # summarize across sectors
    group_by(rgn_id, year) %>%
    summarize(
      # across sectors, jobs are summed
      jobs_sum  = sum(jobs_adj, na.rm=T),
      # across sectors, wages are averaged
      wages_avg = mean(wages_adj, na.rm=T)) %>%
    group_by(rgn_id) %>%
    mutate(
      # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
      jobs_sum_first  = first(jobs_sum , order_by=year),
      # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
      # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
      wages_avg_first = first(wages_avg, order_by=year)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      x_jobs  = pmax(-1, pmin(1,  jobs_sum / jobs_sum_first)),
      x_wages = pmax(-1, pmin(1, wages_avg / wages_avg_first)),
      score   = mean(c(x_jobs, x_wages), na.rm=T) * 100) %>%
    # filter for most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    select(
      region_id = rgn_id,
      score) %>%
    mutate(
      goal      = 'LIV',
      dimension = 'status')
  
  # LIV trend
  # From SOM p. 29: trend was calculated as the slope in the individual sector values (not summed sectors)
  # over the most recent five years... 
  # with the average weighted by the number of jobs in each sector
  # ... averaging slopes across sectors weighted by the revenue in each sector
  
  # get trend across years as slope of individual sectors for jobs and wages
  liv_trend = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wages_adj)) %>%
    # TODO: consider "5 year time spans" as having 5 [(max(year)-4):max(year)] or 6 [(max(year)-5):max(year)] member years
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago 
    # get sector weight as total jobs across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(jobs_adj, na.rm=T)) %>%
    # reshape into jobs and wages columns into single metric to get slope of both with one do() call
    reshape2::melt(id=c('rgn_id','year','sector','weight'), variable='metric') %>%
    mutate(
      sector = as.character(sector),
      metric = as.character(metric)) %>%
    # get linear model coefficient per metric
    group_by(metric, rgn_id, sector, weight) %>%
    do(mdl = lm(value ~ year, data=.)) %>%
    summarize(
      metric = metric,
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 4))) %>%
    arrange(rgn_id, metric, sector) %>%
    # get weighted mean across sectors per region-metric
    group_by(metric, rgn_id) %>%
    summarize(
      metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # get mean trend across metrics (jobs, wages) per region
    group_by(rgn_id) %>%
    summarize(
      score = mean(metric_trend, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'LIV',
      dimension = 'trend') %>%
    select(
      goal, dimension,
      region_id = rgn_id,
      score)
  
  # ECO calculations ----
  eco = le_gdp %>%
    mutate(
      rev_adj = usd,
      sector = 'gdp') %>% 
    # adjust rev with national GDP rates if available. Example: (rev_adj = usd / ntl_gdp) 
    select(rgn_id, year, sector, rev_adj)
  
  # ECO status
  eco_status = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # across sectors, revenue is summed
    group_by(rgn_id, year) %>%
    summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]  
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    select(
      goal, dimension,
      region_id = rgn_id,
      score)
  
  # ECO trend
  eco_trend = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    # get sector weight as total revenue across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(rgn_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    summarize(
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 4))) %>%
    # get weighted mean across sectors per region
    group_by(rgn_id) %>%
    summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    select(
      goal, dimension,
      region_id = rgn_id,
      score)
  
  # report LIV and ECO scores separately
  if (subgoal=='LIV'){
    d = rbind(liv_status, liv_trend)
  } else if (subgoal=='ECO'){
    d = rbind(eco_status, eco_trend)
  } else {
    stop('LIV_ECO function only handles subgoal of "LIV" or "ECO"')
  }
  return(d)
  
}


LE = function(scores, layers){
  
  # calculate LE scores
  scores.LE = scores %.% 
    filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %.%
    dcast(region_id + dimension ~ goal, value.var='score') %.%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm=T)) %.%
    select(region_id, dimension, score) %.%
    mutate(goal  = 'LE')
  
  # rbind to all scores
  scores = scores %.%
    rbind(scores.LE)  
  
  # return scores
  return(scores)  
}

ICO = function(layers){
  
  # layers
  lyrs = c('ico_spp_extinction_status' = 'risk_category',
           'ico_spp_popn_trend'        = 'popn_trend')
  
  # cast data ----
  layers_data = SelectLayersData(layers, layers=names(lyrs))  
  rk = rename(dcast(layers_data, id_num + category ~ layer, value.var='val_chr'),
              c('id_num'='region_id', 'category'='sciname', lyrs))
  
  # lookup for weights status
  w.risk_category = c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)
  
  # lookup for population trend
  w.popn_trend = c('Decreasing' = -0.5,
                   'Stable'     =  0,                                           
                   'Increasing' =  0.5)
  
  # status
  r.status = rename(ddply(rk, .(region_id), function(x){ 
    mean(1 - w.risk_category[x$risk_category], na.rm=T) * 100 }), 
    c('V1'='score'))
  
  # trend
  r.trend = rename(ddply(rk, .(region_id), function(x){ 
    mean(w.popn_trend[x$popn_trend], na.rm=T) }), 
    c('V1'='score'))
  
  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='ICO'))
  return(scores)  
  
}

# Hila 9.9.14 - LSP status for OHI-israel was calculated using different data-layers.
# Input: status and trend layers recived from Harel
LSP = function(layers){
  #function(layers, ref_pct_cmpa=30, ref_pct_cp=30, status_year=2012, trend_years=2005:2009){
  # 2013: LSP(layers, status_year=2013, trend_years=2006:2010)
  # 2012: LSP(layers, status_year=2009, trend_years=2002:2006)
  
  # status
  r.status = rename(SelectLayersData(layers, layers='lsp_status', narrow=T), c('id_num'='region_id','val_num'='score'))
  r.status$score = r.status$score * 100
  
  # trend
  r.trend = rename(SelectLayersData(layers, layers='lsp_trend', narrow=T), c('id_num'='region_id','val_num'='score'))
  
  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='LSP'))
  return(scores)  
  
#   lyrs = list('r'  = c('rgn_area_inland1km'   = 'area_inland1km',
#                        'rgn_area_offshore3nm' = 'area_offshore3nm'),
#               'ry' = c('lsp_prot_area_offshore3nm' = 'cmpa',
#                        'lsp_prot_area_inland1km'   = 'cp'))              
#   lyr_names = sub('^\\w*\\.','', names(unlist(lyrs)))
#   
#   # cast data ----
#   d = SelectLayersData(layers, layers=lyr_names)  
#   r  = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['r']]))),
#               c('id_num'='region_id', lyrs[['r']]))
#   ry = rename(dcast(d, id_num + year ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['ry']]))),
#               c('id_num'='region_id', lyrs[['ry']]))
#   
#   # fill in time series from first year specific region_id up to max year for all regions and generate cumulative sum
#   yr.max = max(max(ry$year), status_year)
#   r.yrs = ddply(ry, .(region_id), function(x){
#     data.frame(region_id=x$region_id[1],
#                year=min(x$year):yr.max)
#   })
#   r.yrs = merge(r.yrs, ry, all.x=T)
#   r.yrs$cp[is.na(r.yrs$cp)]     = 0
#   r.yrs$cmpa[is.na(r.yrs$cmpa)] = 0
#   r.yrs = within(r.yrs, {
#     cp_cumsum    = ave(cp  , region_id, FUN=cumsum)
#     cmpa_cumsum  = ave(cmpa, region_id, FUN=cumsum)
#     pa_cumsum    = cp_cumsum + cmpa_cumsum
#   })
#   
#   # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
#   # and calculate status score
#   r.yrs = merge(r.yrs, r, all.x=T); head(r.yrs)
#   r.yrs = within(r.yrs,{
#     pct_cp    = pmin(cp_cumsum   / area_inland1km   * 100, 100)
#     pct_cmpa  = pmin(cmpa_cumsum / area_offshore3nm * 100, 100)
#     pct_pa    = pmin( (cp_cumsum + cmpa_cumsum) / (area_inland1km + area_offshore3nm) * 100, 100)
#     status    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2 * 100
#   })
#   
#   # extract status based on specified year
#   r.status = r.yrs[r.yrs$year==status_year, c('region_id','status')]; head(r.status)
#   
#   # calculate trend
#   r.trend = ddply(subset(r.yrs, year %in% trend_years), .(region_id), function(x){
#     data.frame(
#       trend = min(1, max(0, 5 * coef(lm(pct_pa ~ year, data=x))[['year']])))})      
#   
#   # return scores
#   scores = rbind.fill(
#     within(r.status, {
#       goal      = 'LSP'
#       dimension = 'status'
#       score     = status}),
#     within(r.trend, {
#       goal      = 'LSP'
#       dimension = 'trend'
#       score     = trend}))
#   return(scores[,c('region_id','goal','dimension','score')])    
}

SP = function(scores){
  
  d = within(
    dcast(
      scores, 
      region_id + dimension ~ goal, value.var='score', 
      subset=.(goal %in% c('ICO','LSP') & !dimension %in% c('pressures','resilience')))
    , {
      goal = 'SP'
      score = rowMeans(cbind(ICO, LSP), na.rm=T)})
  
  
  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}


CW = function(layers){
  
  # layers
  lyrs = c('po_pathogens' = 'a',
           'po_nutrients' = 'u',
           'po_chemicals' = 'l',
           'po_trash'     = 'd',
           'cw_metals_trend'   = 'chemi_trend',
           'cw_fertilizer_trend'  = 'fert_trend',
           'cw_coast_index_trend' = 'indx_trend',
           'cw_pathogen_trend'    = 'path_trend')
  
  # cast data
  d = SelectLayersData(layers, layers=names(lyrs))  
  r = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs))),
             c('id_num'='region_id', lyrs)); head(r); summary(r)
  
  # invert pressures
  r$a = 1 - r$a
  r$u = 1 - r$u
  r$l = 1 - r$l
  r$d = 1 - r$d
  
  # invert trends for CW - cancelled - Hila 8.9.14 - trend calculated as slope(status = 1-pressure)
  # r$popn_trend = -1 * r$popn_trend
  # r$path_trend = -1 * r$path_trend
  
  # status
  r$status = psych::geometric.mean(t(r[,c('a','u','l','d')]), na.rm=T) * 100
  
  # trend
  r$trend = rowMeans(r[,c('chemi_trend','fert_trend','indx_trend','path_trend')], na.rm=T)
  
  # return scores
  scores = rbind(
    within(r, {
      goal      = 'CW'
      dimension = 'status'
      score     = status}),
    within(r, {
      goal      = 'CW'
      dimension = 'trend'
      score     = trend}))[,c('region_id','goal','dimension','score')]
  return(scores)  
}


HAB = function(layers){
  # Israel HAB: sand dunes and softbottom.
  # email from Anat: For the rocky reef we only have the areal extent of reef. We did not succeed in composing 
  # a proxy for this habitatcondition, so we decided to remove it from the HAB subgoal. We remain with
  # the sand dunes, and softbottom
  
  # get layer data
  d = 
    join_all(
      list(
        
        layers$data[['hab_health']] %>%
          select(rgn_id, habitat, health),
        
        layers$data[['hab_trend']] %>%
          select(rgn_id, habitat, trend),
        
        layers$data[['hab_extent']] %>%
          select(rgn_id, habitat, extent=km2)),
      
      by=c('rgn_id','habitat'), type='full') %>% 
    select(rgn_id, habitat, extent, health, trend)
  
  # limit to habitats used for HAB, create extent presence as weight
  d = d %>%
    filter(habitat %in% c('sand_dunes', 'soft_bottom')) %>%    
    mutate(
      w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    filter(!is.na(w)) %>%
    group_by(rgn_id)
  
  # calculate scores
  scores_HAB = rbind_list(
    # status
    d %>% 
      filter(!is.na(health)) %>%
      summarize(      
        score = pmin(1, sum(w * health) / sum(w)) * 100,
        dimension = 'status'),
    # trend
    d %>% 
      filter(!is.na(trend)) %>%      
      summarize(      
        score =  sum(w * trend) / sum(w),
        dimension = 'trend')) %>%
    mutate(
      goal = 'HAB') %>%
    select(region_id=rgn_id, goal, dimension, score)
  
  # return scores
  return(scores_HAB)  
}

# Hila 26/8/14 - change SPP to run same calculation as ICO
SPP = function(layers){
  
  # layers
  lyrs = c('spp_extinction_status' = 'risk_category',
           'spp_popn_trend'        = 'popn_trend')
  
  # cast data ----
  layers_data = SelectLayersData(layers, layers=names(lyrs))  
  rk = rename(dcast(layers_data, id_num + category ~ layer, value.var='val_chr'),
              c('id_num'='region_id', 'category'='sciname', lyrs))
  
  # lookup for weights status
  w.risk_category = c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)
  
  # lookup for population trend
  w.popn_trend = c('Decreasing' = -0.5,
                   'Stable'     =  0,                                           
                   'Increasing' =  0.5)
  
  # status
  r.status = rename(ddply(rk, .(region_id), function(x){ 
    mean(1 - w.risk_category[x$risk_category], na.rm=T) * 100 }), 
    c('V1'='score'))
  
  # trend
  r.trend = rename(ddply(rk, .(region_id), function(x){ 
    mean(w.popn_trend[x$popn_trend], na.rm=T) }), 
    c('V1'='score'))
  
  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='SPP'))
  return(scores)  
  
}

BD = function(scores){
  
  d = within(
    dcast(
      scores, 
      region_id + dimension ~ goal, value.var='score', 
      subset=.(goal %in% c('HAB','SPP') & !dimension %in% c('pressures','resilience'))), 
{
  goal = 'BD'
  score = rowMeans(cbind(HAB, SPP), na.rm=T)})

# return all scores
return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}

PreGlobalScores = function(layers, conf, scores){
  
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
  
  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns[,'id_num'], 0))
  
  # apply NA to Antarctica
  id_ant = subset(rgns, val_chr=='Antarctica', id_num, drop=T)
  scores[scores$region_id==id_ant, 'score'] = NA
  
  return(scores)
}

FinalizeScores = function(layers, conf, scores){
  
  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T)
  
  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'), 
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors=F); head(d)
  d = subset(d, 
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) & 
               !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))
  scores = merge(scores, d, all=T)[,c('goal','dimension','region_id','score')]
  
  # order
  scores = arrange(scores, goal, dimension, region_id)
  
  # round scores
  scores$score = round(scores$score, 2)
  
  return(scores)
}
