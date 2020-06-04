
library(tidyverse)
ca_validated <- read.csv("CA-data/CA_Validated.csv", stringsAsFactors = FALSE)
pb_ca <- read.csv("CA-data/Prison_Boundaries_CA.csv", stringsAsFactors = FALSE)

reformatted_ca_validated <- 
  ca_validated %>% 
  separate_rows(Total_FRS_IDS) %>% 
  select(-(automated_FRS_id:LATITUDE83)) %>%
  left_join(pb_ca, by="HIFLD_FACILITYID")

write.csv(reformatted_ca_validated, "reformatted_ca_validated.csv")