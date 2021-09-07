require(ggplot2)
require(dplyr)
require(data.table)
require(readxl)
require(sqldf)

facilities <- readxl::read_xlsx("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/uncollapsed_with_cap_pop_doj_v2_3.xlsx")
# Notes
# FRS ID - facility registry service - https://www.epa.gov/frs
# HIFLD_FACILITYID - homeland infra -  https://hifld-geoplatform.opendata.arcgis.com/
# FID ? 
# Source vs SOURCE?

colnames(facilities)[colnames(facilities) == "Source"] <- "Source2"

# Junction table (HIFLD => FRS)
frs_hifld <- facilities[,c("HIFLD_FACILITYID", "FRS_ID")]


# reduce to 1 row / HIFLD_ID
facilities <- subset(facilities, ave(HIFLD_FACILITYID, HIFLD_FACILITYID, FUN=seq_along) %in% 1)
facilities$FRS_ID <- NULL


# Each FRS should roll up to a single HIFLD
# CHECK!
frs_hifld <- subset(frs_hifld, !duplicated(FRS_ID))

# check Folsom
subset(facilities, HIFLD_FACILITYID == 10000780)

# Note using fread here to specify columns
# 1 row / visit
visits <- fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Site_Visits.csv", 
                select=c("PWSID", "FISCAL_YEAR", "SITE_VISIT_DATE", "SANITARY_SURVEY", "REGISTRY_ID"))

violations <- fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Violations.csv",
                    select=c("PWSID", "FISCAL_YEAR", "VIOLATION_NAME", "VIOLATION_ID", 
                             "RULE_NAME", "BEGIN_YEAR", "END_YEAR", "RTC_YEAR", "ACUTE_HEALTH_BASED", 
                             "HEALTH_BASED", "MONITORING_REPORTING", "PUBLIC_NOTIF_OTHER", 
                             "REGISTRY_ID"))

enforcements <- fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Enforcements.csv",
                      # select=c("PWSID", "FISCAL_YEAR",
                      #          "ENFORCEMENT_DATE", "ENFORCEMENT_CATEGORY", "DESCRIPTION", "AGENCY", "ENFORCEMENT_ID",
                      #          "REGISTRY_ID"),
                      # NOTE Misaligned CSV?
                      header=FALSE, skip=1, col.names = 
c("PWSID", "PWS_NAME", "CITY_SERVED", "STATE", "STATE_NAME", 
  "PWS_TYPE_CODE", "PWS_TYPE_SHORT", "SOURCE_WATER", "PWS_SIZE", 
  "POPULATION_SERVED_COUNT", "FISCAL_YEAR", 
  "ENFORCEMENT_ID", "ENFORCEMENT_DATE", 
  "ENFORCEMENT_CATEGORY", "DESCRIPTION", "AGENCY",  
  "REGISTRY_ID", "FAC_NAME", "FAC_STREET", "FAC_CITY", "FAC_STATE", 
  "FAC_ZIP", "FAC_COUNTY", "FAC_EPA_REGION", "FAC_LAT", "FAC_LONG", 
  "FAC_DERIVED_WBD", "FAC_DERIVED_CD113", "FAC_PERCENT_MINORITY", 
  "FAC_POP_DEN", "FAC_DERIVED_HUC", "FAC_SIC_CODES", "FAC_NAICS_CODES", 
  "DFR_URL")
###
)

enforcements <- enforcements[, c("PWSID", "FISCAL_YEAR",
                                 "ENFORCEMENT_DATE", "ENFORCEMENT_CATEGORY", "DESCRIPTION", "AGENCY", "ENFORCEMENT_ID",
                                 "REGISTRY_ID")]


# Identifies serious violators?
serious <- fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Serious_Violators.csv",
                      select=c("PWSID", "FISCAL_YEAR","REGISTRY_ID"))



# PWS Data
pws <- fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Public_Water_Systems.csv")

# > sqldf("select SYSTEM_SIZE, min(POPULATION_SERVED_COUNT), max(POPULATION_SERVED_COUNT) from pws group by 1")
# SYSTEM_SIZE min(POPULATION_SERVED_COUNT) max(POPULATION_SERVED_COUNT)
# 1       Large                        11964                        39399
# 2      Medium                         3348                         9999
# 3       Small                          515                         3253
# 4  Very Small                            0                          500

# Helper, orders by size instead of alpha 
system_size <- function(x) cut(x, c(-1, 500, 3333, 10000, 999999), labels=c('Very Small', 'Small', 'Medium', 'Large'))
# table(system_size(pws$POPULATION_SERVED_COUNT), pws$SYSTEM_SIZE)

# (PWSID, FISCAL_YEAR) => Population
pws_pop <- pws[,c("PWSID", "FISCAL_YEAR", "POPULATION_SERVED_COUNT")]

# PWS, 1 row / PWSID
pws <- subset(pws, ave(PWSID, PWSID, FUN=seq_along) %in% 1, c("PWSID", "STATE", "STATE_NAME", "EPA_REGION", "PWS_TYPE_CODE",
                                                              "PWS_NAME", "CITY_SERVED", "STATE_CODE", "SOURCE_WATER", "IS_TRIBAL"))


years <- unique(pws_pop[, "FISCAL_YEAR", drop=FALSE])




### TODO dump data, move to notebook

# EPA Visits decreasing over time
visits %>% 
  count(FISCAL_YEAR, SANITARY_SURVEY) %>%  
  ggplot() + aes(x=FISCAL_YEAR, y = n, color=SANITARY_SURVEY) + 
             geom_point(size=3) + 
             geom_smooth(linetype='dashed') +
             scale_x_continuous(minor_breaks=2009:2021, breaks=seq(2010, 2020, 5), limits = c(2010, 2022)) + 
             scale_shape_discrete(guide=NULL) + 
             ggtitle("Site Visits per fiscal year")


sqldf("

select 
  FISCAL_YEAR,
  PRIVATE_FACILITY,
  count(distinct facilities.HIFLD_FACILITYID) as n
  
  from visits 
  join frs_hifld  on (visits.registry_id = frs_hifld.FRS_ID)
  join facilities on (frs_hifld.HIFLD_FACILITYID = facilities.HIFLD_FACILITYID)
  
  
  group by 1,2    
")


fac_years_visited <- 

sqldf("
      
with      hifld_visits as (

  select fiscal_year, HIFLD_FACILITYID, max(SANITARY_SURVEY) as SANITARY_SURVEY
  from visits 
  join frs_hifld  on (visits.registry_id = frs_hifld.FRS_ID)
  group by 1,2 
),
fac_years as (
  select years.fiscal_year, 
  facilities.*
  
  from facilities, years
)
select fac_years.*, 
  hifld_visits.HIFLD_FACILITYID is not null as visited,
  hifld_visits.SANITARY_SURVEY
  
from fac_years
left join hifld_visits on (hifld_visits.fiscal_year = fac_years.fiscal_year and hifld_visits.HIFLD_FACILITYID = fac_years.HIFLD_FACILITYID) 
")



count(fac_years_visited, fiscal_year, EPA_Region, visited) %>% subset(visited==1) %>%  ggplot() + aes(x=fiscal_year, y=n, color=EPA_Region) + geom_line()

fac_years_visited %>% ggplot() + 
  aes(x=fiscal_year - as.numeric(DOJ_YEAR_BUILT_2005), y=visited, color=EPA_Region) + geom_smooth() + xlab('age of building') + ylab("prob of visit")




  
