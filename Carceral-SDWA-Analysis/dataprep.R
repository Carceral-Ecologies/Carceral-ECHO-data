require(ggplot2)
require(dplyr)
require(data.table)
require(readxl)
require(sqldf)

if(Sys.getenv("USER") == 'nfultz' && grepl("amax", system("hostname", intern = TRUE))) setwd("~/projects/Carceral-ECHO-data/Carceral-SDWA-Analysis/")

ggfacilities <- readxl::read_xlsx("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/uncollapsed_with_cap_pop_doj_v2_3.xlsx")
# Notes
# FRS ID - facility registry service - https://www.epa.gov/frs
# HIFLD_FACILITYID - homeland infra -  https://hifld-geoplatform.opendata.arcgis.com/
# FID ? 
# Source vs SOURCE?

colnames(ggfacilities)[colnames(ggfacilities) == "SOURCE"] <- "Source2"

ggfacilities[vapply(ggfacilities, is.character, TRUE)] <- lapply(ggfacilities[vapply(ggfacilities, is.character, TRUE)], 
                                                                 function(x) {x[x %in% c("NA", "")] <- NA; x})


ggfacilities$HIFLD_POPULATION_2020 <- as.numeric(ggfacilities$HIFLD_POPULATION_2020)
ggfacilities$HIFLD_CAPACITY_2020 <- as.numeric(ggfacilities$HIFLD_CAPACITY_2020)

# 2/25 -999 in capacity?
ggfacilities$HIFLD_CAPACITY_2020 <- ifelse(ggfacilities$HIFLD_CAPACITY_2020 == -999, NA, ggfacilities$HIFLD_CAPACITY_2020)

ggfacilities$DOJ_YEAR_BUILT_2005 <- as.numeric(ggfacilities$DOJ_YEAR_BUILT_2005)


# ggfacilities$num_NAME <- with(ggfacilities, ave(NAME, NAME, FUN=length))
# ggfacilities$num_FID <- with(ggfacilities, ifelse(is.na(FID), 0, ave(NAME, FID, FUN=length)))
# ggfacilities$num_HIFLD_FACILITYID <- with(ggfacilities, ifelse(is.na(HIFLD_FACILITYID), 0, ave(NAME, HIFLD_FACILITYID, FUN=length)))
# ggfacilities$num_FRS_ID <- with(ggfacilities, ifelse(is.na(FRS_ID), 0, ave(NAME, FRS_ID, FUN=length)))



# Junction table (HIFLD => FRS)
frs_hifld <- ggfacilities[c("HIFLD_FACILITYID", "FRS_ID")]
frs_hifld <- frs_hifld[complete.cases(frs_hifld), ]

# reduce to 1 row / HIFLD_ID
facilities <- subset(ggfacilities, !is.na(HIFLD_FACILITYID) & !duplicated(HIFLD_FACILITYID), -FRS_ID)



# Each FRS should roll up to a single HIFLD
# CHECK!
#frs_hifld <- subset(frs_hifld, !duplicated(FRS_ID))

# check Folsom
#subset(ggfacilities, HIFLD_FACILITYID == 10000780)

# Note using fread here to specify columns
# 1 row / visit
# visits <- fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Site_Visits.csv", 
#                 select=c("PWSID", "FISCAL_YEAR", "SITE_VISIT_DATE", "SANITARY_SURVEY", "REGISTRY_ID"))

visits <- fread(cmd="unzip -p data/SDWA_downloads.zip SDWA_SITE_VISITS.csv",
                 select=c("PWSID", "FISCAL_YEAR", "SITE_VISIT_DATE", "SANITARY_SURVEY"))

# violations <- fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Violations.csv",
#                     select=c("PWSID", "FISCAL_YEAR", "VIOLATION_NAME", "VIOLATION_ID", 
#                              "RULE_NAME", "BEGIN_YEAR", "END_YEAR", "RTC_YEAR", "ACUTE_HEALTH_BASED", 
#                              "HEALTH_BASED", "MONITORING_REPORTING", "PUBLIC_NOTIF_OTHER", 
#                              "REGISTRY_ID"))

violations <- fread(cmd="unzip -p data/SDWA_downloads.zip SDWA_VIOLATIONS.csv", 
                    select=c("PWSID", "FISCAL_YEAR", "VIOLATION_NAME", "VIOLATION_ID", 
                              "RULE_NAME", "BEGIN_YEAR", "END_YEAR", "RTC_YEAR", "ACUTE_HEALTH_BASED", 
                              "HEALTH_BASED", "MONITORING_REPORTING", "PUBLIC_NOTIF_OTHER"))


# enforcements <- fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Enforcements.csv",
#                       # select=c("PWSID", "FISCAL_YEAR",
#                       #          "ENFORCEMENT_DATE", "ENFORCEMENT_CATEGORY", "DESCRIPTION", "AGENCY", "ENFORCEMENT_ID",
#                       #          "REGISTRY_ID"),
#                       # NOTE Misaligned CSV?
#                       header=FALSE, skip=1, col.names = 
# c("PWSID", "PWS_NAME", "CITY_SERVED", "STATE", "STATE_NAME", 
#   "PWS_TYPE_CODE", "PWS_TYPE_SHORT", "SOURCE_WATER", "PWS_SIZE", 
#   "POPULATION_SERVED_COUNT", "FISCAL_YEAR", 
#   "ENFORCEMENT_ID", "ENFORCEMENT_DATE", 
#   "ENFORCEMENT_CATEGORY", "DESCRIPTION", "AGENCY",  
#   "REGISTRY_ID", "FAC_NAME", "FAC_STREET", "FAC_CITY", "FAC_STATE", 
#   "FAC_ZIP", "FAC_COUNTY", "FAC_EPA_REGION", "FAC_LAT", "FAC_LONG", 
#   "FAC_DERIVED_WBD", "FAC_DERIVED_CD113", "FAC_PERCENT_MINORITY", 
#   "FAC_POP_DEN", "FAC_DERIVED_HUC", "FAC_SIC_CODES", "FAC_NAICS_CODES", 
#   "DFR_URL")
# ###
# )
# 
# enforcements <- enforcements[, c("PWSID", "FISCAL_YEAR",
#                                  "ENFORCEMENT_DATE", "ENFORCEMENT_CATEGORY", "DESCRIPTION", "AGENCY", "ENFORCEMENT_ID",
#                                  "REGISTRY_ID")]



enforcements <- fread(cmd="unzip -p data/SDWA_downloads.zip SDWA_ENFORCEMENTS.csv",
                      select=c("PWSID", "FISCAL_YEAR", "ENFORCEMENT_ID", "ENFORCEMENT_DATE", 
                               "ENFORCEMENT_CATEGORY", "DESCRIPTION", "AGENCY"))





# Identifies serious violators?
# serious <- fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Serious_Violators.csv",
#                       select=c("PWSID", "FISCAL_YEAR","REGISTRY_ID"))

serious <- fread(cmd="unzip -p data/SDWA_downloads SDWA_SERIOUS_VIOLATORS.csv", select=c("PWSID", "FISCAL_YEAR"))


# PWS Data

pws <- fread(cmd="unzip -p data/SDWA_downloads SDWA_PUB_WATER_SYSTEMS.csv")


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

#
#table(table(pws_pop$PWSID))

# PWS, 1 row / PWSID, take most recent fiscal year
pws <- subset(pws, ave(-FISCAL_YEAR, PWSID, FUN=rank) %in% 1, c("PWSID", "STATE", "STATE_NAME", "EPA_REGION", "PWS_TYPE_CODE",
                                                              "PWS_NAME", "CITY_SERVED", "STATE_CODE", "SOURCE_WATER", "IS_TRIBAL"))


# How was PWS joined to REGISTRY_ID?
pws_reg <- union(
  fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Site_Visits.csv", select=c("PWSID", "REGISTRY_ID")),
  fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Violations.csv", select=c("PWSID", "REGISTRY_ID"))
)

pws_reg$FRS_ID <- as.numeric(pws_reg$REGISTRY_ID)
pws_reg$REGISTRY_ID <- NULL


years <- unique(pws_pop[, "FISCAL_YEAR", drop=FALSE])


# NB below is a large file, so filter in-transit down to 2010+

sdwa_violations_enforcement <- fread(cmd="unzip -p data/SDWA_latest_downloads.zip SDWA_VIOLATIONS_ENFORCEMENT.csv | grep 'DATE\\|201\\|202\\|^$'") %>% as.data.frame

sdwa_violations_enforcement[grep("DATE", colnames(sdwa_violations_enforcement), value = TRUE)] <- lapply(
  
  sdwa_violations_enforcement[grep("DATE", colnames(sdwa_violations_enforcement), value = TRUE)],
  as.Date,
  format="%m/%d/%Y"

)


sdwa_violations_enforcement <- subset(sdwa_violations_enforcement,
      (                                
      as.Date("2010-01-01") <= 
        Reduce(function(x,y) pmax(x,y, na.rm=TRUE), 
               sdwa_violations_enforcement[grep("DATE", colnames(sdwa_violations_enforcement), value = TRUE)],
               as.Date("999-09-09")
               )
      ) 
)


sdwa_facilities <- fread(cmd="unzip -p data/SDWA_latest_downloads.zip SDWA_FACILITIES.csv")


# Demographics of population on water system

demogs <- rbind(
  fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Site_Visits.csv", select=c("REGISTRY_ID", "FAC_PERCENT_MINORITY","FAC_POP_DEN")),
  fread("drive/ECHO data analysis /!GG!/Spreadsheets/2021-07 Notebook/SDWA_Violations.csv", select=c("REGISTRY_ID", "FAC_PERCENT_MINORITY","FAC_POP_DEN"))
)

names(demogs)[1] <- "FRS_ID"
demogs <- subset(demogs, !duplicated(FRS_ID))
demogs$FRS_ID <- as.numeric(demogs$FRS_ID)
