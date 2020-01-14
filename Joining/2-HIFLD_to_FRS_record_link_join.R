#---
#title: Joining HIFLD prisons to EPA facilities via 'probabilistic' record linking, on facilities within 1 mile of prisons.
#
#author: Ben Millam, adapted from state_prisons.Rmd by Lindsay Poirier
#
#date: December 21, 2019
#
#description: Here we join HIFLD prison records with EPA FRS facilities, according to name, address, etc., using R's fastLink
#   probabilistic record linking package, retaining the best match. With 6,704 prisons and 4+ million FRS facilities, we only 
#   consider a subset: all facilities within a 1 mile radius of a prison, determined by a spatial join in 
#   HIFLD_prisons_spatial_join_to_EPA_FRS_facilities.R and further blocked by state.
#
#   In the future I'd like to better understand the fastLink package model (paper below) and its application here; it relies on 
#   field comparisons that use string edit distance cutoffs to produce per-record binary agreement vectors ([0, 1, 1, etc.] for
#   say [name, address, state, etc.]; I'd like to see some face-value examples of how these cutoffs classify agreement for our
#   fields and affect overall accuracy, and I'd like to better understand how blocking into low sample sizes might affect the 
#   latent variable model estimators, which are estimated by block, [and whether in practice variablity translates to 
#   performance differences]).
#
#references:
#   fastLink model: https://imai.fas.harvard.edu/research/files/linkage.pdf
#
#notes:
#   HIFLD prisons accessed 2019-11-21 https://hifld-geoplatform.opendata.arcgis.com/datasets/2d6109d4127d458eaf0958e4c5296b67_0
#   EPA FRS facilities accessed 2019-11-27 'File Geodatabase' https://www.epa.gov/frs/geospatial-data-download-service
#       FRS facilities here a subset from HIFLD_prisons_spatial_join_to_EPA_FRS_facilities.R
#---


############################################################################ - setup

################################### - user-config:
working_directory <- "C:/hack-ca-local/epa-facilities"
setwd(working_directory)

################################### - load-libraries
library(fastLink)
library(sf) #for working with shape files
library(tidyverse)


############################################################################ - helper-functions

################################### - preprocess text
preprocess_text <- function(char_vec, is_name = FALSE, is_address = FALSE, is_city = FALSE, is_state = FALSE, is_zip = FALSE, is_county = FALSE) 
{
  # """
  # Preprocess text/standardize to improve fastLink's string distance metrics
  # 
  #
  # Args:
  #   char_vec (chr): character string(s).
  #   is_name... etc. (logical) flags to indicate type of text, will apply processing for that type, only one can be TRUE
  # 
  # Returns:
  #   result (chr)
  # """
  
  result <- NULL
  
  record_type_flags <- sum(c(is_name, is_address, is_city, is_state, is_zip, is_county))
  
  if(record_type_flags == 0) {
    cat("preprocess_text requires at least one record type flag to be TRUE, e.g. is_address")
    result <- NULL #already NULL, being explicit here
  }
  
  if(record_type_flags > 1) {
    cat("preprocess_text requires only one record type flag be TRUE")
    result <- NULL
  }
  
  if (is_name) {
    result <- fastLink::preprocText(char_vec, convert_text=TRUE, tolower=TRUE, remove_whitespace=TRUE, remove_punctuation=TRUE)
  }

  if (is_address) {
    result <- fastLink::preprocText(char_vec, convert_text=TRUE, tolower=TRUE, remove_whitespace=TRUE, usps_address=TRUE, remove_punctuation=TRUE)
    
    result <- gsub(" s ", " south ", result)
    result <- gsub(" e ", " east ", result)
    result <- gsub(" n ", " north ", result)
    result <- gsub(" w ", " west ", result)
    
    result <- gsub(" s$", " south ", result)
    result <- gsub(" e$", " east ", result)
    result <- gsub(" n$", " north ", result)
    result <- gsub(" w$", " west ", result)
    
    result <- gsub(" ste .*", "", result)
    result <- gsub(" stes .*", "", result)
    result <- gsub(" unit .*", "", result)
    result <- gsub(" suite .*", "", result)
    result <- gsub(" apt .*", "", result)
    
    result <- gsub("highway", "hwy", result)
    
    #Remaining issues: ____ + "7th Floor"
    
  }
  
  if (is_city) {
    result <- fastLink::preprocText(char_vec, convert_text=TRUE, tolower=TRUE, remove_whitespace=TRUE, remove_punctuation=TRUE)
    result <- gsub("^s ", "south ", result)
    result <- gsub("^e ", "east ", result)
    result <- gsub("^n ", "north ", result)
    result <- gsub("^w ", "west ", result)
  }

  if (is_state) {
    result <- fastLink::preprocText(char_vec, convert_text=TRUE, tolower=TRUE, remove_whitespace=TRUE, remove_punctuation=TRUE)
  }
  
  if (is_zip) {
    result <- fastLink::preprocText(char_vec, convert_text=TRUE, tolower=TRUE, remove_whitespace=TRUE, remove_punctuation=TRUE)
    result <- substring(result,1,5) #coerces to character, returns only first 5 characters, should trim +4 if present
  }
  
  if (is_county) {
    result <- fastLink::preprocText(char_vec, convert_text=TRUE, tolower=TRUE, remove_whitespace=TRUE, remove_punctuation=TRUE)
    result <- gsub(" county", "", result)
  }

  return(result)

}

############################################################################ - load data and name columns

################################### - load prisons
prisons_original <- st_read("Prison_Boundaries_Shapefiles/Prison_Boundaries.shp", promote_to_multi = FALSE, stringsAsFactors=FALSE) 

prisons <- prisons_original

prisons <- as.data.frame(prisons) #drop 'sf' class

#retain columns of interest
prisons <- prisons[,c("FACILITYID","NAME","ADDRESS","CITY","STATE","ZIP","COUNTY")]

names(prisons) <- c("HIFLD_FACILITYID","NAME","ADDRESS","CITY","STATE","ZIP","COUNTY")

################################### - load spatial join FRS facilities
frs_facilities_original <- readRDS("HIFLD_spatial_join_FRS_all_within_1_mile.RDS")

frs_facilities <- frs_facilities_original

#retain columns of interest
frs_facilities <- frs_facilities[,c("HIFLD_id","PRIMARY_NAME","LOCATION_ADDRESS","CITY_NAME","STATE_CODE","POSTAL_CODE","COUNTY_NAME")]

names(frs_facilities) <- c("HIFLD_FACILITYID","NAME","ADDRESS","CITY","STATE","ZIP","COUNTY")


############################################################################ - separate street numbers from text
# Lindsay found a boost doing this, I added extra="merge" to make `separate` only use first separator which avoided discarding 
#   some regex match groups in cases of multiple separators found.
#   https://community.rstudio.com/t/tidyr-separate-at-first-whitespace/26269
prisons <- prisons %>% separate(ADDRESS, into = c("ADDRESS_NUM", "ADDRESS_STREET"), sep = "(?<=[0-9])[[:space:]]", extra = "merge")
frs_facilities <- frs_facilities %>% separate(ADDRESS, into = c("ADDRESS_NUM", "ADDRESS_STREET"), sep = "(?<=[0-9])[[:space:]]", extra = "merge")

# Lindsay said: "Issues: 30755c auld rd --> not a master regexer: how to separate by string that immediately follows a set of digits that *may* contain a character"
# I didn't address that

############################################################################ - preprocess text
prisons$NAME <- preprocess_text(prisons$NAME, is_name = T)
prisons$ADDRESS_NUM <- preprocess_text(prisons$ADDRESS_NUM, is_address = T)
prisons$ADDRESS_STREET <- preprocess_text(prisons$ADDRESS_STREET, is_address = T)
prisons$CITY <- preprocess_text(prisons$CITY, is_city = T)
prisons$STATE <- preprocess_text(prisons$STATE, is_state = T)
prisons$ZIP <- preprocess_text(prisons$ZIP, is_zip = T)
prisons$COUNTY <- preprocess_text(prisons$COUNTY, is_county = T)

frs_facilities$NAME <- preprocess_text(frs_facilities$NAME, is_name = T)
frs_facilities$ADDRESS_NUM <- preprocess_text(frs_facilities$ADDRESS_NUM, is_address = T)
frs_facilities$ADDRESS_STREET <- preprocess_text(frs_facilities$ADDRESS_STREET, is_address = T)
frs_facilities$CITY <- preprocess_text(frs_facilities$CITY, is_city = T)
frs_facilities$STATE <- preprocess_text(frs_facilities$STATE, is_state = T)
frs_facilities$ZIP <- preprocess_text(frs_facilities$ZIP, is_zip = T)
frs_facilities$COUNTY <- preprocess_text(frs_facilities$COUNTY, is_county = T)

############################################################################ - block by state
blocks_by_state <- fastLink::blockData(prisons, frs_facilities, varnames = c("STATE"))

############################################################################ - run matches by state block
match_by_block <- function(block, prisons, frs_facilities) {
  
  #for status messages, unfortunately we won't see these with parallelization clusters
  block_state <- toupper(prisons[block$dfA.inds[[1]],"STATE"])
  block_lengths <- c(length(block$dfA.inds),block$dfB.inds)
  print(sprintf('Starting matches for the state of %s with %i by %i records.', block_state, block_lengths[[1]], block_lengths[[2]]))
  
  start_time <- Sys.time()
  
  #perform matches
  block_matches <- fastLink::fastLink(prisons[block$dfA.inds,], frs_facilities[block$dfB.inds,],
                            return.df = FALSE,
                            n.cores = 1,
                            varnames = c("HIFLD_FACILITYID", "NAME", "ADDRESS_NUM", "ADDRESS_STREET", "CITY", "ZIP", "COUNTY"),
                            stringdist.match = c("HIFLD_FACILITYID", "NAME", "ADDRESS_NUM", "ADDRESS_STREET", "CITY", "ZIP", "COUNTY"),
                            partial.match = c("NAME"),
                            cut.a = 0.95,
                            cut.p = 0.8,
                            stringdist.method = "jw",
                            jw.weight = 0.05 ) #ignored unless using jaro winkler
                            #Note: I lowered the jw.weight value from Lindsay's of 0.25, I did not test; the weight increases the 
                            #   edit distance if the fields' first few characters match; I reason this is most useful with human
                            #   names, which the fastLink method paper examples were based on (voter records), 
                            #   but maybe less useful for our purposes, where facility names can vary altogether semantically.
  
  
  end_time <- Sys.time()
  
  time_diff <- end_time - start_time
  
  print(sprintf('Finished matches for the state of %s, with an elapsed time of %0.2f minutes.', block_state, time_diff))
  
  return(block_matches)
}

############################################################################ - parallelize
#configure parallelization
library(parallel)
numCores <- 6 #I have 8, but unsure how current R process is affected, or memory issues

cl <- makeCluster(numCores) #socket is default type (vs fork like mcapply on UNIX)

#load libraries on each cluster
clusterEvalQ(cl, {
  library(fastLink)
})

#make vars available on each cluster
clusterExport(cl, c("prisons","frs_facilities","blocks_by_state","match_by_block"))


matches_by_state <- parLapply(cl, blocks_by_state, function(state)
{
  match_by_block(block = state, prisons = prisons, frs_facilities = frs_facilities)
}
)

stopCluster(cl)

#saveRDS(matches_by_state, file = "match_indices_by_state_cuta95_cutp8_jw05.RDS")

#need to get names, unsure how states were ordered by fastLink blocking... err (not really necessary)
#frs_facilities[blocks_by_state$block.3$dfB.inds[[1]],"STATE"]
#names(results_states) <- prison_states

############################################################################ - get matches FRS IDs

matches_FRS_IDs <- do.call(rbind,lapply(1:length(matches_by_state), function(state_i)
{
  df <- data.frame(hifld = prisons[blocks_by_state[[state_i]]$dfA.inds,][matches_by_state[[state_i]]$matches$inds.a,"HIFLD_FACILITYID"],
                   frs_id = frs_facilities_original[blocks_by_state[[state_i]]$dfB.inds,][matches_by_state[[state_i]]$matches$inds.b,"FRS_id"], stringsAsFactors = FALSE)
}
  )) #note we're using frs_facilities_original, because I didn't bring over the FRS ID into the match dfs

#note we're missing some records from prisons, let's proceed for now anyhow, unsure why,
#   likely that matches were not found for some records (although fastLink has a default arg to return best match)
#   sum(sapply(blocks_by_state,function(state){length(state$dfA.inds)})) #6703, 1 short of nrow(prisons), so blocking
#   doesn't appear to be losing much. One state in prisons table(prisons$STATE, useNA="always") has invalid value "mp" so blocking
#   may have dropped a block if not found in frs_facilities. Further inspection reveals prisons that had no spatial joined
#   facilities in our given radius, so likely source of missing prisons here.
#   Because we're manually coding anyhow I'm moving on for now without inspecting this further/definitively.
nrow(matches_FRS_IDs) #6420
nrow(prisons) #6704

#add missing HIFLD IDs, fill with NA (so for whatever reason (presumably rural prisons), spatial record linking had no matches)
matches_FRS_IDs <- merge(prisons[,c("HIFLD_FACILITYID","NAME")], matches_FRS_IDs, by.x = "HIFLD_FACILITYID", by.y = "hifld", all.x = TRUE)

########################################################## join to get FRS data for final results

#note: frs_facilities_original is the result of the spatial join, and has duplicate FRS IDs (e.g. facilities within 1 mile of several prisons)
#   all facility info is the same, but merging also on HIFLD_ID returns the spatial join distance for that specific prison, and avoids
#   a result with > nrow(prisons) rows (where all matching FRS IDs, dupes, are returned)
results_final <- merge(matches_FRS_IDs, frs_facilities_original, by.x = c("HIFLD_FACILITYID","frs_id"), by.y =c("HIFLD_id","FRS_id"), all.x = TRUE)
results_final$match_source <- "fastLink_on_spatial" #recycling
getwd()
saveRDS(results_final, file = "HIFLD_record_link_join_on_spatial_cuta95_cutp8_jw05.RDS")