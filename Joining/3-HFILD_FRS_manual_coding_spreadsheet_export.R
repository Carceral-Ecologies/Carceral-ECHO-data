#---
#title: Manual Coding Spreadsheet to Match HIFLD Prisons to EPA Facilities
#
#author: Ben Millam
#
#date: December 28, 2019
#
#description: This file takes in HIFLD prison records, and outputs a CSV that lists each prison record along with our best 
#   matched EPA facilities via spatial join and record linkage; the CSV also has some fields to make manual coding easier like
#   a link to the online FRS search for respective counties.
#   
#references:
#   []
#
#notes: 
#   Requires data generated from files:
#   HIFLD_prisons_spatial_join_to_EPA_FRS_facilities.R
#   HIFLD_to_FRS_record_link_join.R
#   
#   Source data:
#   HIFLD prisons accessed 2019-11-21 https://hifld-geoplatform.opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0
#   EPA FRS facilities accessed 2019-11-27 'File Geodatabase' https://www.epa.gov/frs/geospatial-data-download-service
#
#   After this file's CSV was generated, it was uploaded as a Google Sheet, where it underwent sorting, formatting, and had a 
#   few columns renamed, and some blank columns added for coding; Google Sheet in UCD shared drive folder:
#   https://docs.google.com/spreadsheets/u/1/d/1p3abHwcpm8Sho8MrWU1TxSi7g7dS11QEYASAEafZhzk/edit
#---

############################################################################ - setup

################################### - user-config:
working_directory <- "C:/hack-ca-local/epa-facilities"
setwd(working_directory)

################################### - load-libraries
library(sf) #for working with shape files
library(tidyverse)


############################################################################ - load data and name columns

################################### - load HIFLD prisons
prisons_original <- st_read("Prison_Boundaries_Shapefiles/Prison_Boundaries.shp", promote_to_multi = FALSE, stringsAsFactors=FALSE) 

prisons <- prisons_original

################################### - get centroids of prison polygons, polygons to points

#reduce prisons from polygons to points (centroids) to get lat/long
#   ended up not using these coordinates, idea was might find useful for manual coding, like bringing up a map
prisons <- st_transform(prisons, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4326) #to 4326 for lat/long

################################### - retain columns of interest
prisons <- as.data.frame(prisons) #drop 'sf' class
prisons$match_source <- "hifld" 
prisons <- prisons[,c("match_source","FACILITYID","NAME","STATE","COUNTY","CITY","ZIP","ADDRESS","STATUS","POPULATION","CAPACITY","geometry")]
#rename for merge
names(prisons) <- c("match_source","HIFLD_FACILITYID","NAME","STATE","COUNTY","CITY","ZIP","ADDRESS","STATUS","POPULATION","CAPACITY","geometry")

################################### - load spatial join FRS facilities
frs_spatial_join_original <- readRDS("HIFLD_spatial_join_FRS_closest_match.RDS")
frs_spatial_join <- frs_spatial_join_original

#retain columns of interest
frs_spatial_join <- frs_spatial_join[,c("closest_FRS_id","HIFLD_id","closest_FRS_distance","match_source","PRIMARY_NAME","STATE_CODE","COUNTY_NAME","CITY_NAME","POSTAL_CODE","LOCATION_ADDRESS","Shape","LATITUDE83","LONGITUDE83","FAC_URL","INTEREST_TYPE","ACTIVE_STATUS")]
#rename for merge
names(frs_spatial_join) <- c("matched_FRS_id","HIFLD_FACILITYID","spatial_distance","match_source","NAME","STATE","COUNTY","CITY","ZIP","ADDRESS","geometry","LATITUDE83","LONGITUDE83","FAC_URL","INTEREST_TYPE","ACTIVE_STATUS")

################################### - load record link/spatial join FRS facilities
frs_linkage_join_original <- readRDS("HIFLD_record_link_join_on_spatial_cuta95_cutp8_jw05.RDS")
frs_linkage_join <- frs_linkage_join_original

#retain columns of interest
frs_linkage_join <- frs_linkage_join[,c("frs_id","HIFLD_FACILITYID","FRS_distance","match_source","PRIMARY_NAME","STATE_CODE","COUNTY_NAME","CITY_NAME","POSTAL_CODE","LOCATION_ADDRESS","Shape","LATITUDE83","LONGITUDE83","FAC_URL","INTEREST_TYPE","ACTIVE_STATUS")]
#rename for merge
names(frs_linkage_join) <- c("matched_FRS_id","HIFLD_FACILITYID","spatial_distance","match_source","NAME","STATE","COUNTY","CITY","ZIP","ADDRESS","geometry","LATITUDE83","LONGITUDE83","FAC_URL","INTEREST_TYPE","ACTIVE_STATUS")


################################### - add row and fields for manual coding
URLencode.vec <- Vectorize(URLencode) #hack to vectorize URLencode #https://r.789695.n4.nabble.com/RFE-vectorize-URLdecode-td901435.html

names_for_search <-  trimws(prisons$NAME)
names_for_search <-  URLencode.vec(names_for_search)

cities_for_search <-  trimws(prisons$CITY)
cities_for_search <-  URLencode.vec(cities_for_search)

states_for_search <-  trimws(prisons$STATE)
states_for_search <-  URLencode.vec(states_for_search)

counties_for_search <-  trimws(prisons$COUNTY)
counties_for_search <-  URLencode.vec(counties_for_search)

#URL to load EPA FRS web search results
FRS_urls <- paste0("https://ofmpub.epa.gov/frs_public2/fii_map_master.fii_retrieve?fac_search=primary_name&fac_value=&fac_search_type=Containing&postal_code=&location_address=&add_search_type=B&city_name=",
                   "&county_name=",
                   counties_for_search,
                   "&state_code=",
                   states_for_search,
                  "&epa_region_code=&cong_dist=&legis_dist=&huc_code=&fed_agency=&TribalLand=0&selectTribe=noselect&sic_type=Equal+to&sic_code_to=&naic_type=Equal+to&naic_to=&org_name=&duns_num=&contact_name=&prog_search=&int_search=&search_type=&search_type=all&all_programs=YES&sysname=&page_no=1&output_sql_switch=TRUE&report=1&database_type=FII&tribal_ind=&last_facility=&univ_search=&fac_search_term=&tribetype=&triballand=&selecttribe=&tribedistance1=#TOP"
                  )


#URL to load a Google Search for facility
google_search <- paste0(
  "https://www.google.com/search?q=",
  names_for_search,
  "+",
  cities_for_search,
  "+",
  states_for_search
)

#df to include a row for each HIFLD facility (row to add manually coded data)
manual_coding <- data.frame(
  HIFLD_FACILITYID = prisons$HIFLD_FACILITYID,
  match_source = "manual_coding",
  manual_FRS_id = NA,
  coder_inititals = NA,
  date_coded = NA,
  match_status = NA,
  match_means = NA,
  FRS_search = FRS_urls,
  google_search = google_search,
  stringsAsFactors = FALSE
  )

############################################################################ - merge sources

merged_results <- plyr::rbind.fill(list(manual_coding,prisons, frs_spatial_join, frs_linkage_join))
#re-order columns
merged_results <- merged_results[,c("coder_inititals",
                                    "date_coded",
                                    "match_status",
                                    "match_source",
                                    "match_means",
                                    "FRS_search",
                                    "google_search",
                                    "manual_FRS_id",
                                    "HIFLD_FACILITYID",
                                    "matched_FRS_id",
                                    "NAME", "ADDRESS", "CITY", "ZIP", "STATE", "spatial_distance","STATUS","POPULATION","CAPACITY",
                                    "ACTIVE_STATUS", "INTEREST_TYPE", "FAC_URL", "LONGITUDE83", "LATITUDE83"
                                    )]
getwd()
write_csv(merged_results, path = '2020-01-05_manual_coding_HIFLD_to_FRS_data_for_Google_Sheet.csv')