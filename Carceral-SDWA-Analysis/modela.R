# Which prisons have more violations?

df <- sqldf("

  with facs as (
    select *
    
    from facilities
    left join frs_hifld using (HIFLD_FACILITYID)
    left join pws_reg using (FRS_ID)
  ),
  fac_years as (
    select *
    from facs, years
  )
            
  select 
  
  HIFLD_FACILITYID, 
  EPA_REGION,
  TYPE, 
  FISCAL_YEAR,
  HIFLD_POPULATION_2020,
  HIFLD_CAPACITY_2020,
  ICE_FACILITY,
  PRIVATE_FACILITY,
  
  sum(case when ACUTE_HEALTH_BASED = 'Y' then 1 else 0 end) as ACUTE_V,
  sum(case when HEALTH_BASED = 'Y' and ACUTE_HEALTH_BASED = 'N' then 1 else 0 end) as HEALTH_V,
  sum(case when MONITORING_REPORTING = 'Y' then 1 else 0 end) as MONITORING_V
  
  from fac_years
  left join violations using (PWSID, FISCAL_YEAR)
  
  group by 

  HIFLD_FACILITYID, 
  EPA_REGION,
  TYPE, 
  FISCAL_YEAR,
  HIFLD_POPULATION_2020,
  HIFLD_CAPACITY_2020,
  ICE_FACILITY,
  PRIVATE_FACILITY
            
")

# Unit of obs is fac-year


table(df$ACUTE_V)
table(df$HEALTH_V)
table(df$MONITORING_V)

xtabs(MONITORING_V~HIFLD_FACILITYID+FISCAL_YEAR, df) %>% as.matrix %>% (function(X) X[rowSums(X) > 0,])(.) %>% t %>% image
xtabs(MONITORING_V~HIFLD_FACILITYID+FISCAL_YEAR, df) %>% rowSums() %>% table %>% .[0:655] %>% pmax(0, na.rm=TRUE) %>% log1p %>% barplot()

m <- glmer(I(MONITORING_V > 0) ~ ICE_FACILITY + PRIVATE_FACILITY + (as.factor(FISCAL_YEAR) | EPA_Region), df, binomial)


