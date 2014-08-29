Setup = function(){
  
  extra.packages.required = c('zoo') # zoo for MAR(), NP()
  
  # install packages if needed
  for (p in extra.packages.required){
    if (!suppressWarnings(library(p, character.only=T, logical.return=T))){
      cat(sprintf('\n\nInstalling %s...\n', p))
      install.packages(p)
      require(p, character.only=T)
    }
  }
}

FIS = function(layers, status_year=2011){
  
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
  
  # sum mangrove_offshore1km + mangrove_inland1km = mangrove to match with extent and trend
  m = layers$data[['hab_extent']] %>%
    filter(habitat %in% c('mangrove_inland1km','mangrove_offshore1km'))
  if (nrow(m)>0){
    m = m %>%
      group_by(rgn_id) %>%
      summarize(km2 = sum(km2, na.rm=T)) %>%
      mutate(habitat='mangrove') %>%
      ungroup() %>%
      select(rgn_id, habitat, km2)
  } else{
    m = m %>%
      mutate(
        km2 = NA) %>%
      select(rgn_id, habitat, km2)
  }
  
  
  # get layer data
  d = 
    join_all(
      list(
        layers$data[['hab_health']] %>%
          select(rgn_id, habitat, health),
        
        layers$data[['hab_trend']] %>%
          select(rgn_id, habitat, trend),
        
        # for habitat extent
        rbind_list(
          
          # do not use all mangrove
          layers$data[['hab_extent']] %>%
            filter(!habitat %in% c('mangrove','mangrove_inland1km','mangrove_offshore1km')) %>%
            select(rgn_id, habitat, km2),
        
          m)),
      
      by=c('rgn_id','habitat'), type='full') %>% 
    select(rgn_id, habitat, km2, health, trend)
    
  # limit to CP habitats and add rank
  habitat.rank = c('coral'            = 4,
                   'mangrove'         = 4,
                   'saltmarsh'        = 3,
                   'seagrass'         = 1,
                   'seaice_shoreline' = 4)
  
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
}

# Hila 24/8/14 - TR will be calculated according to Hotels occupancy scores
# Combined with Parks scores. Parks will be gived twice the weight as hotels.
# Final scores should be multiplied by sustainability. see TR documantaion.
TR = function(layers, year_max){
  #browser()
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
  ry = data.frame(ry, status = (ry$H*0.33 + ry$P*0.66)*ry$S*100)
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

LIV_ECO = function(layers, subgoal, liv_workforcesize_year=2009, eco_rev_adj_min_year=2000){
  # Israel: no wages data so LIV just based on jobs, removed missing regions addendum from 2013 global and georegional gapfilling.
  
  g.component = c('LIV'='livelihood','ECO'='economy')[[subgoal]]
  
  # get status_model
  status_model_long = SelectLayersData(
    layers, narrow=T,
    layers=c('le_jobs_cur_base_value','le_jobs_ref_base_value','le_jobs_cur_adj_value','le_jobs_ref_adj_value',
             'le_rev_cur_base_value','le_rev_ref_base_value','le_rev_cur_adj_value','le_rev_ref_adj_value',
             'le_wage_cur_base_value','le_wage_ref_base_value','le_wage_cur_adj_value','le_wage_ref_adj_value'))
  status_model = status_model_long %.%
    select(cntry_key = id_chr, sector = category, val_num, layer) %.%
    mutate(metric = str_replace(layer, 'le_(jobs|rev|wage)_(.*)', '\\1'),
           field  = str_replace(layer, 'le_(jobs|rev|wage)_(.*)', '\\2')) %.% 
    dcast(metric + cntry_key + sector ~ field, value.var='val_num')
  
  # get gdp per capita, at ppp
  ppp = SelectLayersData(layers, layers='le_gdp_pc_ppp') %.%
    select(cntry_key=id_chr, year, usd=val_num)
  
  # country to region aggregation weight for livelihood
  workforce_adj = SelectLayersData(layers, layers='le_workforcesize_adj') %.%
    select(cntry_key=id_chr, year, jobs=val_num)
  
  # country to region aggregation weight for economy
  rev_adj = SelectLayersData(layers, layers='le_revenue_adj') %.%
    select(cntry_key=id_chr, year, usd=val_num)
  
  # compute the corrected relative value per metric per country, for JOBS
  status_jobs_rev = status_model %.%
    filter(ref_base_value != 0 & ref_adj_value != 0 & metric %in% c('jobs', 'rev')) %.%
    group_by(metric, cntry_key) %.%
    summarise(
      score    = (sum(cur_base_value, na.rm=T) / sum(ref_base_value, na.rm=T)) / (mean(cur_adj_value, na.rm=T) / mean(ref_adj_value, na.rm=T)),
      n_sector = n()) %.%
    arrange(metric, cntry_key)
  
  # compute the corrected relative value per metric per country, for WAGE
  # 0. extract w'_i = (w_c/w_r)/(W_c/W_r) for each sector i per country
  # Israel: commenting below b/c no wage data
  #   t0 = status_model %.%
  #     filter(metric=='wage' & ref_base_value != 0 & ref_adj_value != 0) %.%
  #     mutate(w_prime_i = (cur_base_value / ref_base_value) / (cur_adj_value / ref_adj_value)) %.%
  #     select(metric, cntry_key, sector, w_prime_i) %.%
  #     group_by(metric, cntry_key) %.%
  #     summarise(w_prime  = mean(w_prime_i, na.rm=T),
  #               n_sector = n()) %.%
  #     arrange(metric, cntry_key)
  
  # 1. let w' = unweighted mean(w'_i) across all sector i per country
  # 2. multiple w' by the most recent purchasing power parity (PPP) value for the country  
  p = ppp %.%
    arrange(cntry_key, year) %.%
    group_by(cntry_key) %.%
    summarise(year     = last(year),
              ppp_last = last(usd)) %.%
    filter(!is.na(ppp_last)) %.%
    arrange(cntry_key)
  #   t2 = t0 %.%
  #     merge(p, by='cntry_key') %.%
  #     mutate(score = w_prime * ppp_last) %.%
  #     select(metric, cntry_key, score, n_sector) %.%
  #     arrange(metric, cntry_key)
  
  # 3. set the best country (PPP-adjusted average wage) equal to 1.0 and then rescale all countries to that max
  #   max_wage_score = max(t2$score, na.rm=T)
  #   status_wage = t2 %.%
  #     mutate(score = score / max_wage_score)
  
  # combine the corrected relative values into a single status score
  #   status_model_combined = ungroup(status_jobs_rev) %.%
  #     rbind(status_wage)
  status_model_combined = ungroup(status_jobs_rev)
  status_score = status_model_combined %.%
    # liv
    dcast(cntry_key ~ metric, value.var='score') %.%
    group_by(cntry_key) %.%
    mutate(
      #       value     = mean(c(jobs, wage), na.rm=T),
      value     = mean(c(jobs), na.rm=T),
      component = 'livelihood') %.%
    select(cntry_key, component, value) %.%
    ungroup() %.% 
    arrange(cntry_key, component, value) %.%
    # eco
    rbind(status_model_combined %.%
            filter(metric=='rev') %.%
            mutate(
              value     = score,
              component = 'economy') %.%
            select(cntry_key, component, value)) %.%
    # order
    filter(!is.na(value)) %.%
    arrange(cntry_key, component) %.%
    # clamp
    mutate(score = pmin(value, 1))
  
  # countries to regions
  cntry_rgn = layers$data[['cntry_rgn']] %.%
    select(rgn_id, cntry_key) %.%
    merge(
      SelectLayersData(layers, layers='rgn_labels') %.%
        select(rgn_id=id_num, rgn_name=val_chr),
      by='rgn_id', all.x=T) %.%
    arrange(rgn_name, cntry_key) %.%
    select(rgn_id, rgn_name, cntry_key)
  
  # get weights, for 1) aggregating to regions and 2) georegionally gap filling
  weights = workforce_adj %.%
    filter(year==liv_workforcesize_year) %.%
    select(cntry_key, w=jobs) %.%
    mutate(component='livelihood') %.%
    rbind(
      rev_adj %.%
        select(cntry_key, year, w=usd) %.%
        filter(year >= eco_rev_adj_min_year) %.%
        arrange(cntry_key, year) %.%
        group_by(cntry_key) %.%
        summarize(w=last(w)) %.%
        mutate(component='economy'))
  
  # aggregate countries to regions by weights
  s_r = status_score %.%
    merge(cntry_rgn, by='cntry_key', all.x=T) %.%
    merge(weights, by=c('cntry_key','component'), all.x=T) %.%
    select(component, rgn_id, rgn_name, cntry_key, score, w) %.%
    arrange(component, rgn_name, cntry_key) %.%
    group_by(component, rgn_id, rgn_name) %.%
    summarize(cntry_w     = paste(cntry_key[!is.na(w)], collapse=','),
              cntry_w_na  = paste(cntry_key[is.na(w)], collapse=','),
              n           = n(),
              n_w_na      = sum(is.na(w)),
              score_w_avg = weighted.mean(score, w),
              score_avg   = mean(score),
              w_sum       = sum(w, na.rm=T)) %.%
    mutate(score = ifelse(!is.na(score_w_avg), score_w_avg, score_avg)) %.%
    ungroup()
  
  status = s_r %.% 
    filter(component==g.component) %.%
    select(region_id=rgn_id, score) %.%    
    mutate(
      goal      = subgoal,
      dimension = 'status',
      score     = score * 100) %.%
    arrange(region_id)
  
  # TODO: trend individual layers
  trend = SelectLayersData(layers, layers='liveco_trend') %.%
    filter(category==g.component) %.%
    select(region_id=id_num, score=val_num) %.%
    mutate(goal=subgoal, dimension='trend') %.%
    arrange(region_id)
  
  scores = rbind(status, trend)
  return(scores)
}

LE = function(scores, layers, eez2012=F){
  
  if (eez2012){
    # replacing 2012 scores for ECO and LIV with 2013 data (email Feb 28, Ben H.)
    # ECO: Eritrea (just this one country)
    # LIV: Eritrea, Anguilla, Bermuda, Egypt, Ghana, Indonesia, Iceland, Saint Kitts, 
    #      Sri Lanka, Brunei, Malaysia, Trinidad & Tobago, and Taiwan
    
    # replacement data and region names
    scores_2013 <- read.csv('../eez2013/scores.csv')  
    rgns = SelectLayersData(layers, layers='rgn_labels', narrow=T) %.%
      select(region_id=id_num, label=val_chr) %.%
      arrange(label)
    
    # ECO
    ECO_rgn_id_replace = subset(rgns, label=='Eritrea', 'region_id', drop=T)
    scores = scores %.%
      filter(!(goal=='ECO' & dimension=='score' & region_id==ECO_rgn_id_replace)) %.%
      rbind(
        scores_2013 %.%
          filter(goal=='ECO' & dimension=='score' & region_id==ECO_rgn_id_replace))
    
    # LIV
    LIV_rgns_label_replace = c('Eritrea','Anguilla','Bermuda','Egypt','Ghana','Indonesia','Iceland','Saint Kitts and Nevis','Sri Lanka','Brunei','Malaysia','Trinidad and Tobago','Taiwan')
    LIV_rgns_id_replace = subset(rgns, label %in% LIV_rgns_label_replace, 'region_id', drop=T)
    stopifnot(length(LIV_rgns_label_replace)==length(LIV_rgns_id_replace))
    scores = scores %.%
      filter(!(goal=='LIV' & dimension=='score' & region_id %in% LIV_rgns_id_replace)) %.%
      rbind(
        scores_2013 %.%
          filter(goal=='LIV' & dimension=='score' & region_id %in% LIV_rgns_id_replace))
  }
  
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
  
  # LIV, ECO and LE: nullify unpopulated regions and those of the Southern Ocean Islands
  r_s_islands   = subset(SelectLayersData(layers, layers='rgn_georegions', narrow=T), 
                         category=='r2' & val_num==999, id_num, drop=T)
  r_unpopulated = subset(ddply(SelectLayersData(layers, layers='le_popn', narrow=T), .(id_num), summarize, 
                               count = val_num[which.max(year)]),
                         is.na(count) | count==0, id_num, drop=T)
  scores[with(scores, 
              goal %in% c('LIV','ECO','LE') & 
                !dimension %in% c('pressures','resilience') & 
                region_id %in% union(r_s_islands, r_unpopulated)),
         'score'] = NA
  
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

LSP = function(layers, ref_pct_cmpa=30, ref_pct_cp=30, status_year=2012, trend_years=2005:2009){
  # 2013: LSP(layers, status_year=2013, trend_years=2006:2010)
  # 2012: LSP(layers, status_year=2009, trend_years=2002:2006)
    
  lyrs = list('r'  = c('rgn_area_inland1km'   = 'area_inland1km',
                       'rgn_area_offshore3nm' = 'area_offshore3nm'),
              'ry' = c('lsp_prot_area_offshore3nm' = 'cmpa',
                        'lsp_prot_area_inland1km'   = 'cp'))              
  lyr_names = sub('^\\w*\\.','', names(unlist(lyrs)))
  
  # cast data ----
  d = SelectLayersData(layers, layers=lyr_names)  
  r  = rename(dcast(d, id_num ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['r']]))),
              c('id_num'='region_id', lyrs[['r']]))
  ry = rename(dcast(d, id_num + year ~ layer, value.var='val_num', subset = .(layer %in% names(lyrs[['ry']]))),
              c('id_num'='region_id', lyrs[['ry']]))
    
  # fill in time series from first year specific region_id up to max year for all regions and generate cumulative sum
  yr.max = max(max(ry$year), status_year)
  r.yrs = ddply(ry, .(region_id), function(x){
    data.frame(region_id=x$region_id[1],
               year=min(x$year):yr.max)
    })
  r.yrs = merge(r.yrs, ry, all.x=T)
  r.yrs$cp[is.na(r.yrs$cp)]     = 0
  r.yrs$cmpa[is.na(r.yrs$cmpa)] = 0
  r.yrs = within(r.yrs, {
    cp_cumsum    = ave(cp  , region_id, FUN=cumsum)
    cmpa_cumsum  = ave(cmpa, region_id, FUN=cumsum)
    pa_cumsum    = cp_cumsum + cmpa_cumsum
  })
  
  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
  r.yrs = merge(r.yrs, r, all.x=T); head(r.yrs)
  r.yrs = within(r.yrs,{
    pct_cp    = pmin(cp_cumsum   / area_inland1km   * 100, 100)
    pct_cmpa  = pmin(cmpa_cumsum / area_offshore3nm * 100, 100)
    pct_pa    = pmin( (cp_cumsum + cmpa_cumsum) / (area_inland1km + area_offshore3nm) * 100, 100)
    status    = ( pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1) ) / 2 * 100
  })
  
  # extract status based on specified year
  r.status = r.yrs[r.yrs$year==status_year, c('region_id','status')]; head(r.status)
  
  # calculate trend
  r.trend = ddply(subset(r.yrs, year %in% trend_years), .(region_id), function(x){
    data.frame(
      trend = min(1, max(0, 5 * coef(lm(pct_pa ~ year, data=x))[['year']])))})      
  
  # return scores
  scores = rbind.fill(
    within(r.status, {
      goal      = 'LSP'
      dimension = 'status'
      score     = status}),
    within(r.trend, {
      goal      = 'LSP'
      dimension = 'trend'
      score     = trend}))
  return(scores[,c('region_id','goal','dimension','score')])    
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
           'cw_pesticide_trend'   = 'pest_trend',
           'cw_fertilizer_trend'  = 'fert_trend',
           'cw_coastalpopn_trend' = 'popn_trend',
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
  
  # invert trends for CW
  r$popn_trend = -1 * r$popn_trend
  r$path_trend = -1 * r$path_trend
  
  # status
  r$status = psych::geometric.mean(t(r[,c('a','u','l','d')]), na.rm=T) * 100
  
  # trend
  r$trend = rowMeans(r[,c('pest_trend','fert_trend','popn_trend','path_trend')], na.rm=T)
  
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
    filter(habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom')) %>%    
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
