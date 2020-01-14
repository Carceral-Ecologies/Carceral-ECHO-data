#---
#title: HIFLD and EPA FRS Spatial Join
#
#author: Ben Millam
#
#date: December 23, 2019
#
#description: performs a spatial join, finding all EPA facilities within a radius of prisons.
#   Prisons are from HIFLD shapefiles downloaded from same source as HIFLD metadata csv we've been working with 
#   and include all the same info, accessed 2019-11-21:
#   https://hifld-geoplatform.opendata.arcgis.com/datasets/prison-boundaries
#
#   EPA facilities are from the Facility Registry Service https://www.epa.gov/frs/geospatial-data-download-service
#   the File Geodatabase accessed 2019-11-27: https://www3.epa.gov/enviro/html/fii/downloads/FRS_INTERESTS_download.zip
#   "Geospatial information for all publicly available FRS facilities that have latitude/longitude data."
#
#references:
#   legit parallelization on windows: http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
#
#notes:
#   Joining 2.7 million x 6,704 records, so we block by state; join returns 434,074 facilities. 
#---


############################################################################ - setup
require(sf) #for working with shape files
require(tidyverse)
require(parallel) #parallel computing
setwd("C:/hack-ca-local/epa-facilities")


############################################################################ - load data and filter

################################### - load HIFLD prisons
#   note we aren't filtering out closed prisons, which number ~500; if desired can be done during manual coding
#

#read shapefile, returns class "sf" "data.frame", dfs with extra info for sf package
prisons <- st_read("C:/hack-ca-local/epa-facilities/Prison_Boundaries_Shapefiles/Prison_Boundaries.shp", promote_to_multi = FALSE, stringsAsFactors=FALSE) 

################################### - load EPA FRS facilities geodatabase
#   4.3 million records, we'll filter down to 2.7 million

#exploratory, geodatabases can contain 1+ layers
#st_layers("C:/hack-ca-local/epa-facilities-geodatabase/FRS_INTERESTS_download/FRS_INTERESTS.gdb")

#load the FRS facilities geodatabase files, can take ~15-20 min
fac_gdb <- st_read("C:/hack-ca-local/epa-facilities-geodatabase/FRS_INTERESTS_download/FRS_INTERESTS.gdb", layer="FACILITY_INTERESTS", promote_to_multi = FALSE, stringsAsFactors=FALSE)
nrow(fac_gdb) #4,369,201

duplicated_frs_ids <- duplicated(fac_gdb$REGISTRY_ID) #logical, T for duplicated, facilities are listed multiple times by interest type/statute, where basic facility info is duplicated
mean(duplicated_frs_ids) #38.6% for our needs are duplicates
fac_reduced <- fac_gdb[!duplicated_frs_ids,] #filter out duplicates
nrow(fac_reduced) #2,684,342


############################################################################ - exploratory: can be block FRS by state?
#   answer: yes
#   is the STATE_CODE field consistent, in need of cleaning?
#   answer: no cleaning needed, effectively no missing data, okay to block by state

#inspect FRS state values
frs_states <- table(fac_reduced$STATE_CODE, useNA = "always")
frs_states <- frs_states[order(frs_states, decreasing = T)]
table(nchar(fac_reduced$STATE_CODE)) #all non-NA are 2 char values

#inspect HIFLD state values
p_states <- table(prisons$STATE, useNA = "always")
p_states <- p_states[order(p_states, decreasing = T)]
table(nchar(prisons$STATE)) #all non-NA are 2 char values

#refuse
#inspecting INTEREST_TYPE (statute), eliminating duplicates indiscriminantly removes INTEREST_TYPE variants
#p_interest <- table(fac_reduced$INTEREST_TYPE, useNA = "always")
#p_interest <- p_interest[order(p_interest, decreasing = T)]


############################################################################ - exploratory: how precise are the FRS lat long coordinates?
#   answer: precise enough
#
#   the COLLECT_MTH_DESC field describes the intended point of reference for the coordinates, e.g. facility address
#   or in some cases, zip code centroid (imprecise!)
#   277,717 records have no COLLECT_MTH_DESC
#   the most numerous imprecise method appears to be "ZIP CODE-CENTROID" at 12,586 or 0.5% of records
#   other presumably imprecise methods have lower numbers, e.g. 'CENSUS BLOCK/GROUP-1990-CENTROID' applies to 164 records
#   
#   Ultimately we'll be manually coding, so let's proceed and spend the time there vs inspecting for dynamics of these minority records

collect_method <- table(fac_reduced$COLLECT_MTH_DESC, useNA="always")
collect_method <- collect_method[order(collect_method, decreasing = T)]

#visually inspect
#collect_method

zip_method <- fac_reduced$COLLECT_MTH_DESC == "ZIP CODE-CENTROID"
sum(zip_method, na.rm = T) #12,586
mean(zip_method, na.rm = T) #0.5%

maps_url <- paste0('https://www.google.com/maps/search/?api=1&query=',fac_sample[["LATITUDE83"]],',',fac_sample[["LONGITUDE83"]])


############################################################################ - get centroids of prison polygons, polygons to points

#convert prisons to match (larger) FRS fac data set Coordinate Reference System
prisons <- st_transform(prisons, crs = 4269)

#reduce prisons from polygons to points (centroids) to reduces distance calculation times
prisons <- st_transform(prisons, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

#inspect, we see FRS facilities are all already geometry type POINT
#fac_reduced$Shape


############################################################################ - function to perform spatial join by state

find_facilities_by_distance_by_state <- function(state, prisons, facilities, radius_in_meters) 
{
  start_time <- Sys.time()
  cat("\nStarting state ",state,"\n") #because we'll parallelize this, we won't see this output from the clusters
  prisons_subset  <-  prisons$STATE == state
  prisons_subset[is.na(prisons_subset)] <- FALSE
  #state <- feature$STATE
  #str(feature)
  #feature <- st_as_sf(as.data.frame(feature))
  facilities_subset <-  facilities$STATE_CODE == state
  facilities_subset[is.na(facilities_subset)] <- FALSE
  
  geo_predicate <- st_is_within_distance(prisons[prisons_subset,], facilities[facilities_subset,], dist = radius_in_meters, sparse = FALSE) #dense nxm matrix of logicals
  
  state_distances <- vector(mode = "list", length = nrow(geo_predicate)) #initialize list
  names(state_distances) <- as.character(prisons[prisons_subset,][['FACILITYID']]) #HIFLD facility ID, as.character to drop sf class stickiness
  
  for (i in 1:nrow(geo_predicate)) {
    distances <- st_distance(prisons[prisons_subset,][i,], facilities[facilities_subset,][geo_predicate[i,],]) #1xm matrix
    distances <- as.numeric(distances) #in case sf classes are sticky here/drop units class  
    
    within_radius_facilities <- facilities[facilities_subset,][geo_predicate[i,],"REGISTRY_ID"]  #stickiness of sf classes will retain class/geo column! not a simple char vector
    names(distances) <- as.character(within_radius_facilities[['REGISTRY_ID']]) #and hence need access 'REGISTRY_ID' to isolate
    
    distances <- distances[order(distances)]
    
    state_distances[[i]] <- distances
  }
  
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  cat("Finished state",state,":\n")
  print(elapsed_time) #elapsed_time is class difftime, we want the "Time difference of..." message from print 
  cat("\n\n")
  
  return(state_distances)
  
}


############################################################################ - perform spatial join by state


################################### - vector of states to apply over
prison_states <- unique(prisons$STATE)
prison_states <- prison_states[order(prison_states)]


################################### - configure parallelization
#   legit parallelization on windows: http://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/parallel.html
numCores <- 4 #I have 8, but unsure how current R process is affected, or memory issues

cl <- makeCluster(numCores) #socket is default type (vs fork like mcapply on UNIX)

#load libraries on each cluster
clusterEvalQ(cl, {
  library(sf) #for working with shape files, assume need to retain sf classes
  library(tidyverse)
})

#make vars available on each cluster
clusterExport(cl, c("prisons","fac_reduced","find_facilities_by_distance_by_state")) #wonder if I'll hit memory limits...


################################### - run join on clusters
results_states <- parLapply(cl, prison_states, function(state)
{
  find_facilities_by_distance_by_state(state = state, prisons = prisons, facilities = fac_reduced, radius_in_meters = 1609.344)
}
)

stopCluster(cl)

names(results_states) <- prison_states

#computation ran overnight, let's save results
getwd()
saveRDS(results_states, file = "HIFLD_spatial_join_FRS_1_mile_radius_computation_result.RDS")


############################################################################ - save 'closest facility' results
#   what we're calling the 'match' from the spatial join

################################### - munge results by HIFLD ID
results_by_HIFLD_ID <- unlist(results_states, recursive = FALSE)
HIFLD_IDs <- names(results_by_HIFLD_ID) #e.g. "AK.10006895" "AK.10003758", etc. state is retained, let's drop it...
HIFLD_IDs <- substring(HIFLD_IDs, first = 4) # "1379" "1433" "1520"
names(results_by_HIFLD_ID) <- HIFLD_IDs


################################### - create results df
#length(results_by_HIFLD_ID)

closest_facility_id <- sapply(results_by_HIFLD_ID, function(x){return(names(x[1]))}) #facilities are ordered by distance, no accounting for ties
closest_facility_distance <- sapply(results_by_HIFLD_ID, function(x){return(x[1])})
closest_facility_distance <- unname(closest_facility_distance)
results_df <- data.frame(HIFLD_id = names(closest_facility_id), closest_FRS_id = closest_facility_id, closest_FRS_distance = closest_facility_distance, match_source = rep("spatial",length(closest_facility_id)), stringsAsFactors = FALSE)


################################### - add FRS data for each spatial join match
results_final <- merge(results_df, fac_reduced, by.x = "closest_FRS_id", by.y = "REGISTRY_ID", all.x = TRUE)

getwd()
saveRDS(results_final, file = "HIFLD_spatial_join_FRS_closest_match.RDS")


############################################################################ - save all facility results
#   we'll be doing fastLink record link join on all facilities within our radius of prisons

################################### - 'flattening' data manually
#   We have results in lists by HIFLD ID, but we want one row per FRS facility
#   Likely more elegant way to do this.
HIFLD_IDs <- names(results_by_HIFLD_ID)
HIFLD_IDs_results_count <- sapply(results_by_HIFLD_ID, length)
HIFLD_IDs_repeated <- 
sapply(1:length(HIFLD_IDs), function(i)
  {
  return(rep(HIFLD_IDs[i],HIFLD_IDs_results_count[i]))
  }
)
HIFLD_IDs_repeated <- unlist(HIFLD_IDs_repeated)

FRS_IDs <- unlist(lapply(results_by_HIFLD_ID,function(x){return(names(x))}), use.names = FALSE)

FRS_distances <- unlist(lapply(results_by_HIFLD_ID,function(x){return(x)}), use.names = FALSE)

length(HIFLD_IDs_repeated) == length(FRS_IDs) #TRUE
length(FRS_IDs) == length(FRS_distances) #TRUE

all_FRS_facilities_from_spatial_join_1_mile <- data.frame(HIFLD_id = HIFLD_IDs_repeated, FRS_id = FRS_IDs, FRS_distance = FRS_distances, stringsAsFactors = FALSE)


################################### - exploratory
nrow(all_FRS_facilities_from_spatial_join_1_mile) #434074 ~16% of all fac_reduced facilities!
#decent amount of overlap
sum(duplicated(all_FRS_facilities_from_spatial_join_1_mile$FRS_id)) #134359
sum(duplicated(all_FRS_facilities_from_spatial_join_1_mile)) #0


################################### - add FRS data for all facilities, save final results
results_all_facilities_final <- merge(all_FRS_facilities_from_spatial_join_1_mile, fac_reduced, by.x = "FRS_id", by.y = "REGISTRY_ID", all.x = TRUE)

getwd()
saveRDS(results_all_facilities_final, file = "HIFLD_spatial_join_FRS_all_within_1_mile.RDS")