# Replication Material Master Thesis "The Great Poll Closure - How neighborhood composition is related to polling place changes"

Script and data sets to replicate the Master Thesis "The Great Poll Closure - How neighborhood composition is related to polling place changes"

## Caveats

⚠️ In order to replicate the geocoding carried out [here](scripts/geocoding.R) you need to supply your own Google Maps API key in line 14 of the script. You can get your key [here](https://developers.google.com/maps/documentation/javascript/get-api-key) ⚠️

⚠️ You also have to download the [repo](https://github.com/PublicI/us-polling-places/tree/add-california/data) of the Center for Public Integrity which provide the original raw data. Data was last acessed November 2021, structure of database changed in the meantime! ⚠️

⚠️ I do not supply all shapefiles used to match coordinates of polling places with county polygons. This is due to the limited capacity of GitHub repos. Shapefiles used in this analysis can be accessed [here](https://www2.census.gov/geo/tiger/TIGER_DP/2012ACS/ACS_2012_5YR_COUNTY.gdb.zip) and[here](https://www2.census.gov/geo/tiger/TIGER_DP/2016ACS/ACS_2016_5YR_COUNTY.gdb.zip)
Caution: Both files for 2012 and 2016 are > 150MB ⚠️

⚠️ It is not possible to supply the full dataset of geocoded polling places due to the size limit of GitHub repos. To get access to the full data please contact: [Philipp Bosch](mailto:philipp.bosch@uni-konstanz.de) ⚠️

## Replicate main analysis

✅ To replicate the substantial analysis I supply the [reduced dataset](data/model_df/12_16.csv) aggregated on county level for the 2012 and 2016 elections ✅

In order to replicate the analysis please first run the [package script](scripts/packages.R) and afterwards the [modelling script](modelling.R).

Generated Stan Code for both models is supplied [here](stan).


