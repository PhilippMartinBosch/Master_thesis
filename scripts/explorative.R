### first data explo ###


# packages ----------------------------------------------------------------

source(file = "scripts/packages.R")


# read in data ------------------------------------------------------------


smartphone_df <- read_tsv(file = "data/PollingPlaces2016_w_TimeZones_and_Buildings.tab") 

cces_16 <- read_dta("data/CCES16_Common_OUTPUT_Feb2018_VV.dta")



