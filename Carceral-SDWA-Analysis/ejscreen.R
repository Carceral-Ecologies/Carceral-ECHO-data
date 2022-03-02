ejscreen <- list()

ejscreen[["2015"]]  <- fread(cmd="unzip -p data/EJSCREEN_20150505.csv.zip  EJSCREEN_20150505.csv")
ejscreen[["2016"]]  <- fread(cmd="unzip -p data/EJSCREEN_V3_USPR_090216_CSV.zip  EJSCREEN_Full_V3_USPR_TSDFupdate.csv")
ejscreen[["2017"]]  <- fread("data/EJSCREEN_2017_USPR_Public.csv")
ejscreen[["2018"]]  <- fread(cmd="unzip -p data/EJSCREEN_2018_USPR_csv.zip  EJSCREEN_Full_USPR_2018.csv")
ejscreen[["2019"]]  <- fread(cmd="unzip -p data/EJSCREEN_2019_USPR.csv.zip  EJSCREEN_2019_USPR.csv")
ejscreen[["2020"]]  <- fread(cmd="unzip -p data/EJSCREEN_2020_USPR.csv.zip  EJSCREEN_2020_USPR.csv")



ejscreen[["2015"]] <- rename(ejscreen[["2015"]],
                             ID = FIPS, 
                             ACSTOTPOP = pop,
                             # ACSIPOVBAS =  povknownratio,
                             MINORPCT = pctmin  )




for(i in 2011:2014) {
  ejscreen[[as.character(i)]] <- ejscreen[["2015"]]
}

ejscreen[["2021"]] <- ejscreen[["2020"]]

for(i in 2011:2021) {
  ejscreen[[as.character(i)]]$FISCAL_YEAR <- i
}

ejscreen <- do.call(rbind, lapply(ejscreen, select, FIPS=ID, FISCAL_YEAR, ACSTOTPOP, MINORPCT))

# t(facilities[36,])
# 
# subset(demogs, FRS_ID %in% merge(facilities[36,], frs_hifld))
# 
# subset(ejscreen[["2015"]], grepl("^421090702001", FIPS))[,1:10]
# subset(ejscreen[["2016"]], grepl("^421090702001", ID))[,1:10]
# subset(ejscreen[["2017"]], grepl("^421090702001", ID))[,1:10]
# subset(ejscreen[["2018"]], grepl("^421090702001", ID))[,1:10]
# subset(ejscreen[["2019"]], grepl("^421090702001", ID))[,1:10]
# subset(ejscreen[["2020"]], grepl("^421090702001", ID))[,1:10]
# 
# 
# 
# 
# 
facilities %>% merge(frs_hifld) %>% merge(demogs) %>%
  subset(grepl("FOLSOM", NAME), c(NAME, FRS_ID,HIFLD_FACILITYID, FAC_PERCENT_MINORITY, FAC_POP_DEN, HIFLD_CAPACITY_2020, HIFLD_CAPACITY_2017, FRS_AT_LEAST_CAP_2020, FRS_AT_LEAST_CAP_2017, CENSUS_BLOCK)) %>%
  print(digits=20)
# 
# 
# facilities %>% merge(frs_hifld) %>% merge(demogs) %>% 
#   subset(grepl("FOLSOM", NAME), CENSUS_BLOCK) %>% mutate(FIPS = as.integer64(CENSUS_BLOCK %/% 1000)) %>% merge(ejscreen[["2015"]])
# 
# subset(ejscreen[["2015"]], grepl("^60679883001", FIPS))[,1:10]
# subset(ejscreen[["2016"]], grepl("^60679883001", ID))[,1:10]
# subset(ejscreen[["2017"]], grepl("^60679883001", ID))[,1:10]
# subset(ejscreen[["2018"]], grepl("^60679883001", ID))[,1:10]
# subset(ejscreen[["2019"]], grepl("^60679883001", ID))[,1:10]
# subset(ejscreen[["2020"]], grepl("^60679883001", ID))
