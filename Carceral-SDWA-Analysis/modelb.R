source("dataprep.R")
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
                size = addNA(system_size(HIFLD_POPULATION_2020)),
                percent_over_capacity = pmax(HIFLD_POPULATION_2020 / HIFLD_CAPACITY_2020 - 1, 0, na.rm = TRUE),
                age = FISCAL_YEAR - nafill(DOJ_YEAR_BUILT_2005, fill=mean(DOJ_YEAR_BUILT_2005, na.rm=TRUE)),
                FISCAL_YEAR=as.factor(FISCAL_YEAR)
)

df[is.na(df$SECURELVL), "SECURELVL"] <- "NOT AVAILABLE"
df$SECURELVL <- relevel(as.factor(df$SECURELVL), "NOT AVAILABLE")

###




base <- ~ ICE_FACILITY +
  PRIVATE_FACILITY +
  TYPE  +
  SECURELVL +
  DOJ_HOUSES_2005_male  + DOJ_HOUSES_2005_female + DOJ_HOUSES_2005_both +
  FAC_PERCENT_MINORITY_na + FAC_PERCENT_MINORITY_fill + 
  age + size + percent_over_capacity +
  (1 | FISCAL_YEAR) + (1 | EPA_Region)

f_perf <- !i ~ .
f_acute <- ACUTE_V ~ .
f_health <- HEALTH_V ~ . 
f_monitoring <- MONITORING_V ~ . 
f_notif <- NOTIF_V ~ .


library(future)
plan(multicore, workers=8)

cont <- glmerControl(optCtrl = list(maxfun=100))
Sys.time()
m_perfect %<-% glmer(update(base, f_perf), df, family=binomial(), weights=ifelse(i, Zhat, 1), control = cont)  
Sys.time()
m_y1 %<-% glmer(update(base, f_acute), df, family=poisson(), weights=ifelse(i, 1-Zhat, 1), control = cont)
m_y2 %<-% glmer(update(base, f_health), df, family=poisson(), weights=ifelse(i, 1-Zhat, 1), control = cont)
m_y3 %<-% glmer(update(base, f_monitoring), df, family=poisson(), weights=ifelse(i, 1-Zhat, 1), control = cont)
m_y4 %<-% glmer(update(base, f_notif), df, family=poisson(), weights=ifelse(i, 1-Zhat, 1), control = cont)

m_perfect_0 %<-% m_perfect
m_y1_0 %<-% m_y1
m_y2_0 %<-% m_y2
m_y3_0 %<-% m_y3
m_y4_0 %<-% m_y4

pler <- function(m) predict(m, type='response', newdata=df, allow.new.levels=TRUE)

for(j in 1:100) {

  message("**************************************")
  message("* Iteration ", j, " at ", as.character(Sys.time()) )
  
  # E step

  Zold <- df$Zhat
    
  df$Zhat[df$i] <- (
    1 /  (1 + pler(m_perfect) * dpois(0, lambda = pler(m_y1)) * dpois(0, lambda=pler(m_y2)) * dpois(0, lambda=pler(m_y3)) * dpois(0, lambda=pler(m_y4)))
  )[df$i]
  
  dZ = sum(abs(Zold - df$Zhat))
  message("* dZ = ", dZ)
  message("**************************************")
  
  # M step
  
  m_y1 %<-% update(m_y1, start = getME(m_y1, c("theta","fixef")), data = df, weights = ifelse(i, 1-Zhat, 1))
  m_y2 %<-% update(m_y2, start = getME(m_y2, c("theta","fixef")), data = df, weights = ifelse(i, 1-Zhat, 1))
  m_y3 %<-% update(m_y3, start = getME(m_y3, c("theta","fixef")), data = df, weights = ifelse(i, 1-Zhat, 1))
  m_y4 %<-% update(m_y4, start = getME(m_y4, c("theta","fixef")), data = df, weights = ifelse(i, 1-Zhat, 1))
  
  m_perfect %<-% update(m_perfect, start = getME(m_perfect, c("theta","fixef")), weights = ifelse(i, Zhat, 1))
  #summary(m_perfect)
  
}


