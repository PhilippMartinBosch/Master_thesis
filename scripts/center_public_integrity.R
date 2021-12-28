# clean polling place data ------------------------------------------------



# load packages -----------------------------------------------------------


source(file = "scripts/packages.R")


# read in data ------------------------------------------------------------

key <- read_lines(file = "google_maps_key.txt")


list_of_files <- list.files(path = "data/copy_polling_places/",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)
df <- list_of_files %>%
  set_names() %>% 
  map_df(data.table::fread, colClasses = 'character')  


# check for election year and state variable ------------------------------


df %>% 
  mutate(election_year = str_extract(election_date, pattern = "\\d{4}")) -> df


df %>% 
  mutate(state = case_when(
    is.na(state) ~ "NE",
    TRUE ~ state
  )) -> df

df %>% 
  dplyr::mutate(across(.cols = everything(), ~ dplyr::na_if(x = ., y = ""))) -> df

df %>% 
  split(f = as.factor(.$state)) -> df_list

# create zip variable -----------------------------------------------------


map(df_list, ~ {
  .x %>% 
    rowid_to_column() %>% 
    mutate(rowid = str_c(state, as.character(rowid)))
}) -> df_list



saveRDS(df_list, file = "data/temp/df_list.RDS")











### nächste Schritte: checken wie sich Adresse und ZIP Code verhalten
### fehlt immer beides?
### testballon steigen lassen für Alaska




doc <- mapsapi::mp_geocode(
  addresses = "Anchorage Zion Lutheran Church",
  key = key,
  quiet = TRUE
)

pnt <- mp_get_points(doc)
pnt



# Alaska geocode ----------------------------------------------------------

df_list[["AK"]] -> AK_df
  

AK_df %>% 
  mutate(lookup_address = case_when(
    !is.na(jurisdiction) ~ str_c(jurisdiction, name, sep = " "),
    is.na(jurisdiction) ~ str_c(precinct_name, name, sep = " "))) %>% 
  mutate(new_col = coalesce(address, lookup_address)) %>% 
  dplyr::pull(new_col) %>% 
  geo(address = ., method = "osm",
      lat = latitude, long = longitude, full_results = TRUE) -> alaska_geocodes



saveRDS(alaska_geocodes, file = "data/temp/alaska_geocodes.RDS")

alaska_geocodes %>% 
  dplyr::select(latitude, longitude, osm_id, boundingbox, display_name, class, type) %>% 
  dplyr::bind_cols(AK_df) %>% 
  dplyr::filter(is.na(latitude)) -> AK_check_google

AK_check_google %>% 
  mutate(lookup_address = case_when(
    !is.na(jurisdiction) ~ str_c(jurisdiction, name, sep = " "),
    is.na(jurisdiction) ~ str_c(precinct_name, name, sep = " "))) %>% 
  mutate(new_col = coalesce(address, lookup_address)) %>% 
  dplyr::pull(new_col) %>% 
  geo(address = ., method = "google",
      lat = latitude, long = longitude, full_results = TRUE) -> AK_google

saveRDS(AK_google, file = "data/temp/AK_google.RDS")


alaska_geocodes %>% 
  dplyr::select(latitude, longitude, osm_id, boundingbox, display_name, class, type) %>% 
  dplyr::bind_cols(AK_df) -> AK_df



AK_check_google %>% 
  dplyr::select(rowid) %>% 
  dplyr::bind_cols(AK_google) -> AK_google_combinded

AK_df %>% 
  left_join(AK_google_combinded, by = c("rowid" = "rowid")) -> AK_final

AK_final %>% 
  dplyr::mutate(latitude = coalesce(latitude.x, latitude.y),
                longitude = coalesce(longitude.x, longitude.y),
                formatted_address_both = coalesce(display_name, formatted_address)) %>%
  dplyr::select(-c(ends_with(".x"), ends_with(".y"))) -> AK_final




# Arkansas goecode ---------------------------------------------------------

df_list[["AR"]] -> AR_df

AR_df %>% 
  mutate(lookup_address = str_c(name, address, sep = " ")) %>% 
  dplyr::pull(lookup_address) %>% 
  geo(address = ., method = "google",
      lat = latitude, long = longitude, full_results = TRUE) -> arkansas_geocodes
  

saveRDS(arkansas_geocodes, file = "data/temp/arkansas_geocodes.RDS")

AR_df %>% 
  bind_cols(arkansas_geocodes) %>% 
  filter(is.na(latitude)) %>% 
  dplyr::select(name, "address...20", county_name) %>% 
  mutate(vote_center = str_extract(string = address...20, pattern = "([Oo]r)? ([Aa]ny )?([Oo]ther )?Vote Center"),
         address...20 = str_remove(string = address...20, pattern = "AR( 0)?"),
         address...20 = coalesce(name, address...20),
         lookup_address = str_c("Arkansas ", address...20),
         lookup_address = str_remove_all(string = lookup_address, pattern = "([Oo]r)? ([Aa]ny )?([Oo]ther )?Vote Center"),
         county_name = dplyr::na_if(county_name, "Arkansas"),
         lookup_address = case_when(
           !is.na(county_name) ~ str_c(county_name, "county", lookup_address, sep = " "),
           TRUE ~ lookup_address
         )) -> AR_lookup_df
         

AR_lookup_df %>% 
  dplyr::pull(lookup_address) %>% 
  geo(address = ., method = "google",
      lat = latitude, long = longitude, full_results = TRUE) -> arkansas_geocodes_second_iteration


saveRDS(arkansas_geocodes_second_iteration, file = "data/temp/arkansas_geocodes_second_iteration.RDS")


AR_df %>% 
  bind_cols(arkansas_geocodes) %>% 
  filter(is.na(latitude)) %>% 
  dplyr::select(rowid) %>% 
  bind_cols(arkansas_geocodes_second_iteration) -> AR_google_second_iteration

AR_df %>% 
  dplyr::select(rowid) %>% 
  bind_cols(arkansas_geocodes) %>% 
  left_join(AR_google_second_iteration, by = c("rowid" = "rowid")) %>% 
  dplyr::mutate(latitude = coalesce(latitude.x, latitude.y),
                longitude = coalesce(longitude.x, longitude.y),
                address = coalesce(address.x, address.y),
                formatted_address = coalesce(formatted_address.x, formatted_address.y),
                types = coalesce(types.x, types.y)) %>% 
  dplyr::select(-c(ends_with(".x"))) %>% 
  dplyr::select(-c(ends_with(".y"))) -> AR_final

  

saveRDS(AR_final, file = "data/temp/AR_final.RDS")




# California geocode ------------------------------------------------------

df_list[["CA"]] -> CA_df

CA_df %>% 
  mutate(address = str_c(address, ", California")) %>%
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> CA_osm_google


saveRDS(CA_osm_google, file = "data/temp/CA_final.RDS")



# Connecticut geocode -----------------------------------------------------

df_list[["CT"]] -> CT_df


CT_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> CT_osm_google

saveRDS(CT_osm_google, file = "data/temp/CT_osm_google.RDS")


# Delaware geocode --------------------------------------------------------

df_list[["DE"]] -> DE_df


DE_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> DE_osm_google


saveRDS(DE_osm_google, file = "data/temp/DE_osm_google.RDS")



# Georgia geocode ---------------------------------------------------------

df_list[["GA"]] -> GA_df

GA_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> GA_osm_google


saveRDS(GA_osm_google, file = "data/temp/GA_osm_google.RDS")




# Iowa geocode ------------------------------------------------------------

df_list[["IA"]] -> IA_df

IA_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> IA_osm_google


saveRDS(IA_osm_google, file = "data/temp/IA_osm_google.RDS")


# Ilinois geocode ---------------------------------------------------------

df_list[["IL"]] -> IL_df

IL_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> IL_osm_google


saveRDS(IL_osm_google, file = "data/temp/IL_osm_google.RDS")

# spatial tryout ----------------------------------------------------------



county_data <- sf::st_read(dsn = "data/shapefiles/census/ACS_2019_5YR_COUNTY.gdb/")

state_data <- sf::st_read(dsn = "data/shapefiles/census/ACS_2019_5YR_STATE.gdb/", layer = c("ACS_2019_5YR_STATE"))

st_layers(dsn = "data/shapefiles/census/ACS_2019_5YR_STATE.gdb/")

st_layers(dsn = "data/shapefiles/census/ACS_2019_5YR_COUNTY.gdb/")


head(state_data)

plot(state_data)

str(state_data)

class(state_data)

state_data[1:10, ]

st_crs(state_data)

state_data %>% 
  st_geometry() %>% 
  plot()
