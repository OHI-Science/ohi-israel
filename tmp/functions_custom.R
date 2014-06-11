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

TR = function(layers, year_max){
  # Israel: skip georegional gapfill and assigning NA to uninhabitated islands
  
  # formula:
  #   E = Ed / (L - (L*U))
  #   Xtr = E * S 
  # 
  # Ed = Direct employment in tourism (tr_jobs_tourism): ** this has not been gapfilled. We thought it would make more sense to do at the status level.
  # L = Total labor force (tr_jobs_total) 2013: max(year)=2011; 2012: max(year)=2010
  # U = Unemployment (tr_unemployment) 2013: max(year)=2011; 2012: max(year)=2010 
  # so E is tourism  / employed
  # S = Sustainability index (tr_sustainability)

  # get regions
  rgns = layers$data[[conf$config$layer_region_labels]] %.%
    select(rgn_id, rgn_label = label)
  
  # merge layers and calculate score
  d = layers$data[['tr_jobs_tourism']] %.%
    select(rgn_id, year, Ed=count) %.%
    arrange(rgn_id, year) %.%
    merge(
      layers$data[['tr_jobs_total']] %.%
        select(rgn_id, year, L=count),
      by=c('rgn_id','year'), all=T) %.%
    merge(
      layers$data[['tr_unemployment']] %.%
        select(rgn_id, year, U=percent) %.%
        mutate(U = U/100),
      by=c('rgn_id','year'), all=T) %.%    
    merge(
      layers$data[['tr_sustainability']] %.%
        select(rgn_id, S=score),
      by=c('rgn_id'), all=T)  %.%
    mutate(
      E   = Ed / (L - (L * U)),
      Xtr = E * S ) %.%
    merge(rgns, by='rgn_id') %.%
    select(rgn_id, rgn_label, year, Ed, L, U, S, E, Xtr)
  
  # filter: limit to 5 intervals (6 years worth of data)
  #   NOTE: original 2012 only used 2006:2010 whereas now we're using 2005:2010
  d_g_f = d %.%
    filter((year <= year_max) & (year >= (year_max - 5)) )
  
  # rescale for
  #   status: 95 percentile value across all regions and filtered years
  #   trend: use the value divided by max bc that way it's rescaled but not capped to a lower percentile (otherwise the trend calculated for regions with capped scores, i.e. those at or above the percentile value, would be spurious)
  Xtr_95  = quantile(d_g_f$Xtr, probs=0.95, na.rm=T)
  Xtr_max = max(d_g_f$Xtr, na.rm=T)
  d_g_f_r = d_g_f %.%    
    mutate(
      Xtr_r95  = ifelse(Xtr / Xtr_95 > 1, 1, Xtr / Xtr_95), # rescale to 95th percentile, cap at 1
      Xtr_rmax = Xtr / Xtr_max )                            # rescale to max value   
  #   write.csv(d_g_f_r, sprintf('inst/extdata/reports%d.www2013/tr-%d_2-filtered-rescaled.csv', yr, yr), row.names=F, na='')
  
  # calculate trend
  d_t = d_g_f_r %.%
    arrange(year, rgn_id) %.%
    group_by(rgn_id) %.%
    do(mod = lm(Xtr_rmax ~ year, data = .)) %>%
    do(data.frame(
      rgn_id = .$rgn_id,
      dimension = 'trend',
      score = max(min(coef(.$mod)[['year']] * 5, 1), -1)))
  
  # get status (as last year's value)
  d_s = d_g_f_r %.%
    arrange(year, rgn_id) %.%
    group_by(rgn_id) %.%
    summarize(
      dimension = 'status',
      score = last(Xtr_r95) * 100)
  
  # bind rows
  d_b = rbind(d_t, d_s) %.%
    mutate(goal = 'TR')  
  
  # replace North Korea value with 0
  d_b$score[d_b$rgn_id == 21] = 0
  
  # final scores
  scores = d_b %.%
    select(region_id=rgn_id, goal, dimension, score)
  
  return(scores)
}
