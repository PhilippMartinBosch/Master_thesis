### first data explo ###


# packages ----------------------------------------------------------------

source(file = "scripts/packages.R")


# read in data ------------------------------------------------------------


county_voterwaittimes <- read_tsv(file = "data/smarthpone/county_voterwaittimes.tab") 

polling_places <- read_tsv(file = "data/smarthpone/PollingPlaces2016_w_TimeZones_and_Buildings.tab")

waittimes <- read_tsv(file = "data/smarthpone/voterwaittimes_filtered.tab")


cces_16 <- read_dta("data/CCES/CCES16_Common_OUTPUT_Feb2018_VV.dta")

USA_Counties <- read_csv("data/shapefiles/usa_csv/USA_Counties.csv")

cces_16$countyfips

table(polling_places_waittimes$Zip_PollingPlace)
table(cces_16$lookupzip)


# calculate Zip ICC -------------------------------------------------------

waittimes %>% 
  inner_join(polling_places, by = c("PollingPlace_ID" = "PollingPlace_ID")) -> tryout

n_distinct(tryout$PollingPlace_ID)

tryout %>% 
  drop_na(Zip_PollingPlace)


waittimes %>% 
  group_by(PollingPlace_ID) %>% 
  summarise(average_wait = mean(waittime)) %>% 
  left_join(polling_places, by = c("PollingPlace_ID" = "PollingPlace_ID")) %>% 
  drop_na(Zip_PollingPlace) %>% 
  left_join(cces_16, by = c("Zip_PollingPlace" = "lookupzip"))
  
cces_16 %>% 
  group_by(lookupzip) %>% 
  count() %>% 
  arrange(desc(n))
  



left_join(polling_places, by = c("PollingPlace_ID" = "PollingPlace_ID")) %>% 
  dplyr::select(PollingPlace_ID, waittime, Zip_PollingPlace) %>% 
  summarise(isna = sum(is.na(Zip_PollingPlace)))


