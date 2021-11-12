source("Carceral-SDWA-Analysis/dataprep.R", echo = TRUE)

# Which prisons have more violations?

df <- sqldf("

  with facs as (
    select *
    
    from facilities
    left join frs_hifld using (HIFLD_FACILITYID)
    left join pws_reg using (FRS_ID)
    left join demogs using (FRS_ID)
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
  DOJ_HOUSES_2005,
  DOJ_YEAR_BUILT_2005,
  SECURELVL,
  FAC_PERCENT_MINORITY,
  FAC_POP_DEN,
  
  sum(case when ACUTE_HEALTH_BASED = 'Y' then 1 else 0 end) as ACUTE_V,
  sum(case when HEALTH_BASED = 'Y' and ACUTE_HEALTH_BASED = 'N' then 1 else 0 end) as HEALTH_V,
  sum(case when MONITORING_REPORTING = 'Y' then 1 else 0 end) as MONITORING_V,
  sum(case when PUBLIC_NOTIF_OTHER = 'Y' then 1 else 0 end) as NOTIF_V
  
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
  PRIVATE_FACILITY,
  DOJ_HOUSES_2005,
  DOJ_YEAR_BUILT_2005,
  SECURELVL,
  FAC_PERCENT_MINORITY,
  FAC_POP_DEN
            
")

# Unit of obs is fac-year


table(df$ACUTE_V)
table(df$HEALTH_V)
table(df$MONITORING_V)

with(df, table(ACUTE_V ==0 & HEALTH_V ==0 & MONITORING_V == 0))

xtabs(MONITORING_V~HIFLD_FACILITYID+FISCAL_YEAR, df) %>% as.matrix %>% (function(X) X[rowSums(X) > 0,])(.) %>% t %>% image
xtabs(MONITORING_V~HIFLD_FACILITYID+FISCAL_YEAR, df) %>% rowSums() %>% table %>% .[0:655] %>% pmax(0, na.rm=TRUE) %>% log1p %>% barplot()

require(lme4)

# m <- glmer(I(MONITORING_V > 0 & HEALTH_V> 0 & MONITORING_V > 0) ~ 
#              ICE_FACILITY + 
#              PRIVATE_FACILITY + 
#              TYPE + system_size(HIFLD_CAPACITY_2020) +
#              I(pmin(HIFLD_POPULATION_2020 / HIFLD_CAPACITY_2020 - 1, 0, na.rm = TRUE)) +
#              (1 + as.factor(FISCAL_YEAR) | EPA_Region), data= df, family = binomial(), verbose=1)



pars <- getME(m, c("theta","fixef"))


m1.restart <- update(m, start=pars, nAGQ=1)


df <- transform(df,
                EPA_Region=as.factor(EPA_Region),
                DOJ_HOUSES_2005_male = grepl("male", DOJ_HOUSES_2005) %in% TRUE,
                DOJ_HOUSES_2005_female = grepl("female", DOJ_HOUSES_2005) %in% TRUE,
                DOJ_HOUSES_2005_both = grepl("both", DOJ_HOUSES_2005) %in% TRUE,
                FAC_PERCENT_MINORITY_na = is.na(FAC_PERCENT_MINORITY),
                FAC_PERCENT_MINORITY_fill = nafill(FAC_PERCENT_MINORITY, fill = 0),
                size = addNA(system_size(HIFLD_POPULATION_2020)),
                percent_over_capacity = pmin(HIFLD_POPULATION_2020 / HIFLD_CAPACITY_2020 - 1, 0, na.rm = TRUE),
                age = FISCAL_YEAR - nafill(DOJ_YEAR_BUILT_2005, fill=mean(DOJ_YEAR_BUILT_2005, na.rm=TRUE)),
                FISCAL_YEAR=as.factor(FISCAL_YEAR)
)

df[is.na(df$SECURELVL), "SECURELVL"] <- "NOT AVAILABLE"
df$SECURELVL <- relevel(as.factor(df$SECURELVL), "NOT AVAILABLE")

m <- glm(I(MONITORING_V > 0 & HEALTH_V> 0 & MONITORING_V > 0) ~
             ICE_FACILITY +
             PRIVATE_FACILITY +
             TYPE  +
           SECURELVL +
             DOJ_HOUSES_2005_male  + DOJ_HOUSES_2005_female + DOJ_HOUSES_2005_both +
             FAC_PERCENT_MINORITY_na + FAC_PERCENT_MINORITY_fill + 
           age + size + percent_over_capacity +
             FISCAL_YEAR + EPA_Region, 
         
         data= df, family = binomial())
summary(m)
