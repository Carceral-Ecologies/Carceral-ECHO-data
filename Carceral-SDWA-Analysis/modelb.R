source("dataprep.R")
source("ejscreen.R")
library(lme4)

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
    select *,
    floor(CENSUS_BLOCK / 1000) as FIPS
    from facs, years
  ),
  
  visits_distinct as (
  
    select PWSID, FISCAL_YEAR 
    from visits
    group by 1,2
  )
  
  

  select 
  
  HIFLD_FACILITYID, 
  EPA_REGION,
  TYPE, 
  fac_years.FISCAL_YEAR as FISCAL_YEAR,
  HIFLD_POPULATION_2020,
  HIFLD_CAPACITY_2020,
  ICE_FACILITY,
  PRIVATE_FACILITY,
  DOJ_HOUSES_2005,
  DOJ_YEAR_BUILT_2005,
  SECURELVL,
  MINORPCT * 100 as MINORPCT,

  max(FAC_PERCENT_MINORITY) as FAC_PERCENT_MINORITY,
  max(FAC_POP_DEN) as FAC_POP_DEN,

  max(POPULATION_SERVED_COUNT) as POPULATION_SERVED_COUNT,
  
  count(visits_distinct.PWSID) as VISITED,
  
  sum(case when ACUTE_HEALTH_BASED = 'Y' then 1 else 0 end) as ACUTE_V,
  sum(case when HEALTH_BASED = 'Y' and ACUTE_HEALTH_BASED = 'N' then 1 else 0 end) as HEALTH_V,
  sum(case when MONITORING_REPORTING = 'Y' then 1 else 0 end) as MONITORING_V,
  sum(case when PUBLIC_NOTIF_OTHER = 'Y' then 1 else 0 end) as NOTIF_V
  
  from fac_years
  left join violations using (PWSID, FISCAL_YEAR)
  left join pws_pop using (PWSID, FISCAL_YEAR)
  left join visits_distinct using (PWSID, FISCAL_YEAR)
  left join ejscreen using(FIPS, FISCAL_YEAR)


  -- Filters out inactive 
  -- left join inactive_systems_fac_years isfy on fac_years.PWSID = isfy.PWSID and fac_years.FISCAL_YEAR = isfy.FISCAL_YEAR
  --where isfy.PWSID IS NULL
  -- result - only dropped one row

  group by 

  HIFLD_FACILITYID, 
  EPA_REGION,
  TYPE, 
  FISCAL_YEAR, --fac_years.FISCAL_YEAR,
  HIFLD_POPULATION_2020,
  HIFLD_CAPACITY_2020,
  ICE_FACILITY,
  PRIVATE_FACILITY,
  DOJ_HOUSES_2005,
  DOJ_YEAR_BUILT_2005,
  SECURELVL,
  MINORPCT
            
")

sqldf("with d as (select HIFLD_FACILITYID, fiscal_year, count(1) as n from df group by 1, 2) select n, count(1) from d group by 1")

# use data-prep
# df query from modela

df$i <- with(df, (ACUTE_V == 0 & HEALTH_V == 0  & MONITORING_V == 0 & NOTIF_V == 0))

# Note starting .001 not 0
df$Zhat <- pmax(0.001, df$i)



df <- transform(df,
                EPA_Region=addNA(as.factor(EPA_Region)),
                TYPE=addNA(as.factor(TYPE)),
                PRIVATE_FACILITY=ifelse(is.na(PRIVATE_FACILITY), 'N', PRIVATE_FACILITY),
                ICE_FACILITY=ifelse(is.na(ICE_FACILITY), 'N', ICE_FACILITY),
                DOJ_HOUSES_2005_male = grepl("male", DOJ_HOUSES_2005) %in% TRUE,
                DOJ_HOUSES_2005_female = grepl("female", DOJ_HOUSES_2005) %in% TRUE,
                DOJ_HOUSES_2005_both = grepl("both", DOJ_HOUSES_2005) %in% TRUE,
                FAC_PERCENT_MINORITY_na = is.na(FAC_PERCENT_MINORITY),
                FAC_PERCENT_MINORITY_fill = nafill(FAC_PERCENT_MINORITY, fill = mean(FAC_PERCENT_MINORITY, na.rm=TRUE)),
                size = system_size(nafill(POPULATION_SERVED_COUNT, fill=mean(POPULATION_SERVED_COUNT, na.rm=TRUE))),
                percent_over_capacity = pmax(HIFLD_POPULATION_2020 / HIFLD_CAPACITY_2020 - 1, 0, na.rm = TRUE),
                age = FISCAL_YEAR - nafill(DOJ_YEAR_BUILT_2005, fill=mean(DOJ_YEAR_BUILT_2005, na.rm=TRUE)),
                FISCAL_YEAR=as.factor(FISCAL_YEAR),
                HIFLD_CAPACITY_2020_na = is.na(HIFLD_CAPACITY_2020),
                HIFLD_CAPACITY_2020_fill = nafill(HIFLD_CAPACITY_2020, fill = mean(HIFLD_CAPACITY_2020, na.rm=TRUE)),

                MINORPCT_na = is.na(MINORPCT),
                MINORPCT_fill = nafill(MINORPCT, fill = mean(MINORPCT, na.rm=TRUE))
                  
                
)

df$SECURELVL <- factor(df$SECURELVL, c("MAXIMUM", "JUVENILE", "MINIMUM", "MEDIUM", "CLOSE", NA), exclude = NULL) 

df$TYPE <- relevel(df$TYPE, "STATE")

df$DOJ_HOUSES_2005_gender <- c(NA, "male"="male", "both"="both", "female"="female", "male; male"="male", "female; male"="both", 
                               "male; both"="both")[df$DOJ_HOUSES_2005] |> factor( c("male", "female", "both")) |>  addNA()

df$size <- relevel(df$size, "Small")

df$VISITED <- pmin(df$VISITED, 1)

### SCALING
df$HIFLD_CAPACITY_2020_fill <- df$HIFLD_CAPACITY_2020_fill / 1000
df$MINORPCT_fill <- df$MINORPCT_fill / 100

###



base <- ~ ICE_FACILITY +
  PRIVATE_FACILITY +
  TYPE  +
  SECURELVL +
  #DOJ_HOUSES_2005_gender +
  MINORPCT_na + MINORPCT_fill + 
  #age + 
  #size + 
  HIFLD_CAPACITY_2020_fill + 
  VISITED +
  (1 | FISCAL_YEAR) + (1 | EPA_Region)

f_perf <- !i ~ .
#f_acute <- ACUTE_V ~ .
#f_health <- HEALTH_V ~ . 
f_monitoring <- MONITORING_V ~ . 
f_notif <- NOTIF_V ~ .

f_hhact <- I(ACUTE_V + HEALTH_V) ~ .

m <- new.env()

library(future)
plan(multicore, workers=8)

cont <- glmerControl(optCtrl = list(maxfun=80))
Sys.time()
m$perfect %<-% glmer(update(base, f_perf), df, family=binomial(), weights=ifelse(i, Zhat, 1), control = cont)  
Sys.time()
#m$y1 %<-% glmer(update(base, f_acute), df, family=poisson(), weights=ifelse(i, 1-Zhat, 1), control = cont)
#m$y2 %<-% glmer(update(base, f_health), df, family=poisson(), weights=ifelse(i, 1-Zhat, 1), control = cont)
m$hhact %<-% glmer(update(base, f_hhact), df, family=poisson(), weights=ifelse(i, 1-Zhat, 1), control = cont)
m$monit %<-% glmer(update(base, f_monitoring), df, family=poisson(), weights=ifelse(i, 1-Zhat, 1), control = cont)
m$notif %<-% glmer(update(base, f_notif), df, family=poisson(), weights=ifelse(i, 1-Zhat, 1), control = cont)

m0 <- as.list(m)


for(j in 1:6) {

  message("**************************************")
  message("* Iteration ", j, " at ", as.character(Sys.time()) )

  if(j %in% c(10, 25, 45)){
    cont$optCtrl$maxfun <- cont$optCtrl$maxfun * 2
  }
  
  
    
  # E step

  Zold <- df$Zhat
    
  preds <-  eapply(m, predict, type='response', newdata=df, allow.new.levels=TRUE)
  preds[names(preds) != 'perfect'] <- lapply(preds[names(preds) != 'perfect'], dpois, x=0)
  preds <- Reduce(`*`, preds)

  df$Zhat[df$i] <- (1 /  (1 + preds) )[df$i]
  
  dZ = sum(abs(Zold - df$Zhat))
  message("* dZ = ", dZ)
  message("**************************************")
  
  m$perfect %<-% update(m$perfect, start = getME(m$perfect, c("theta","fixef")), data=df, weights = ifelse(i, Zhat, 1), control=cont)
  
  # M step
  for(j in setdiff(ls(m), "perfect"))
    m[[j]] %<-% update(m[[j]], start = getME(m[[j]], c("theta","fixef")), data = df, weights = ifelse(i, 1-Zhat, 1), control=cont)
  
  #summary(m_perfect)

    
}


