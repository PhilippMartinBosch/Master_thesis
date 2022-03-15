# clean polling place data ------------------------------------------------



# load packages -----------------------------------------------------------


source(file = "scripts/packages.R")
source(file = "scripts/helper.R")


# read in data ------------------------------------------------------------

key <- read_lines(file = "google_maps_key.txt")


list_of_files <- list.files(path = "data/copy_polling_places/",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)
df <- list_of_files %>%
  set_names() %>% 
  map_df(data.table::fread, colClasses = 'character')  

bounding_boxes_states <- read_csv("data/temp/bounding_boxes_states.csv") %>% 
  dplyr::select(STUSPS, xmin, ymin, xmax, ymax)


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


purrr::map(df_list, ~ {
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
  mutate(address = case_when(
    is.na(address) ~ str_c(precinct_name, name, "Alaska", sep = ", "),
    TRUE ~ address
  )) %>% 
  mutate(address = str_remove(string = address, pattern = " No. \\d")) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> AK_osm_google


saveRDS(AK_osm_google, file = "data/temp/AK_osm_google.RDS")


map(.x = AK_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> AK_osm_google

AK_osm_google %>% 
  filter(!dplyr::between(as.numeric(lat), 51.21418, 71.36516) | 
         !dplyr::between(as.numeric(long), -179.1489, 179.7785)) -> flagged_AK


AK_coordinates <- tribble(~lat, ~long,
                    61.181457603353714, -149.8263463859426,
                    61.21376491356381, -149.80369602032897,
                    61.20605138647016, -149.82447025895667,
                    61.214013176032154, -149.89699577005965,
                    61.09488886820813, -149.83286079943795,
                    61.110683715548795, -149.86492224546703,
                    61.12234227771198, -149.80335540868992,
                    61.178421444870295, -149.80857583970493,
                    65.26143663493318, -166.36257678120865,
                    65.61158838785454, -168.08667931009623,
                    61.59912377865953, -149.10990670809085) %>% 
  dplyr::mutate(across(.cols = everything(), as.character))

flagged_AK %>% 
  dplyr::select(-c("lat", "long")) %>% 
  bind_cols(AK_coordinates) -> flagged_AK

AK_osm_google %>% 
  anti_join(flagged_AK, by = c("rowid" = "rowid")) %>% 
  bind_rows(flagged_AK) -> AK_osm_google_clean


AK_osm_google_clean %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 51.21418, 71.36516) | 
           !dplyr::between(as.numeric(long), -179.1489, 179.7785))

saveRDS(AK_osm_google_clean, file = "data/temp/cleaned_geocodes/AK_osm_google_clean.RDS")


write_csv(AK_osm_google_clean, file = "data/temp/finished_df/AK.csv")



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


### sanity check lat & long



AR_final %>% 
  filter(is.na(latitude) | is.na(longitude) | !dplyr::between(as.numeric(latitude), 33.00411, 36.49960) | 
           !dplyr::between(as.numeric(longitude), -94.61792, -89.64440)) -> flagged_AR


AR_df %>% 
  semi_join(flagged_AR, by = c("rowid" = "rowid")) -> check_AR

write_csv(check_AR, file = "data/temp/excal_handcode/flagged_AR.csv")

### handcoding of missings

AR_final %>% 
  left_join(AR_df, by = c("rowid" = "rowid")) -> AR_final


flagged_AR <- read_delim("data/temp/cleaned_geocodes/flagged_AR.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = "c"))

flagged_AR %>% 
  separate(col = latlong, into = c("lat", "long"), sep = ",") -> flagged_AR

AR_final %>% 
  anti_join(flagged_AR, by = c("rowid" = "rowid")) %>% 
  bind_rows(flagged_AR) -> AR_final

write_csv(AR_final, file = "data/temp/finished_df/AR.csv")


# Arizona geocode ---------------------------------------------------------

df_list[["AZ"]] -> AZ_df

AZ_df %>% 
  mutate(address = str_remove(address, "\\s*\\([^\\)]+\\)")) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> AZ_osm_google


saveRDS(AZ_osm_google, file = "data/temp/AZ_osm_google.RDS")

### sanity check lat & long

map(.x = AZ_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> AZ_osm_google






AZ_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 31.33218, 37.00426) | 
           !dplyr::between(as.numeric(long), -114.81651, -109.04522)) -> flagged_AZ


write_csv(flagged_AZ, file = "data/temp/excal_handcode/flagged_AZ.csv")

### handcoding of missings

combine_handcode("AZ") -> flagged_AZ

AZ_osm_google %>% 
  anti_join(flagged_AZ, by = c("rowid" = "rowid")) %>% 
  bind_rows(flagged_AZ) -> AZ_final

write_csv(AZ_final, file = "data/temp/finished_df/AZ.csv")


# California geocode ------------------------------------------------------

df_list[["CA"]] -> CA_df

CA_df %>% 
  mutate(address = str_c(address, ", California")) %>%
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> CA_osm_google


saveRDS(CA_osm_google, file = "data/temp/CA_final.RDS")


### sanity check lat & long

map(.x = CA_final, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> CA_osm_google



CA_osm_google %>% 
  filter(!dplyr::between(as.numeric(lat), 32.53416, 42.00952) | 
           !dplyr::between(as.numeric(long), -124.40959, -114.13121)) -> flagged_CA


write_csv(flagged_CA, file = "data/temp/excal_handcode/flagged_CA.csv")


### handcoding of missings

read_handcode("CA") -> flagged_CA

CA_osm_google %>% 
  anti_join(flagged_CA, by = c("rowid" = "rowid")) %>% 
  bind_rows(flagged_CA) -> CA_final

write_csv(CA_final, file = "data/temp/finished_df/CA.csv")


# Connecticut geocode -----------------------------------------------------

df_list[["CT"]] -> CT_df


CT_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> CT_osm_google

saveRDS(CT_osm_google, file = "data/temp/CT_osm_google.RDS")


### sanity check lat & long

map(.x = CT_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> CT_osm_google







CT_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 40.98014, 42.05059) | 
           !dplyr::between(as.numeric(long), -73.72777, -71.78699)) -> flagged_CT


write_csv(flagged_CT, file = "data/temp/excal_handcode/flagged_CT.csv")


### handcoding of missings

combine_handcode("CT")



# Delaware geocode --------------------------------------------------------

df_list[["DE"]] -> DE_df


DE_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> DE_osm_google


saveRDS(DE_osm_google, file = "data/temp/DE_osm_google.RDS")


### sanity check lat & long

map(.x = DE_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> DE_osm_google




DE_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 38.45101, 39.83901) | 
           !dplyr::between(as.numeric(long), -75.78866, -75.04894)) -> flagged_DE


write_csv(flagged_DE, file = "data/temp/excal_handcode/flagged_DE.csv")

### handcoding of missings


combine_handcode("DE")


# Georgia geocode ---------------------------------------------------------

df_list[["GA"]] -> GA_df

GA_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> GA_osm_google

### sanity check lat & long


saveRDS(GA_osm_google, file = "data/temp/GA_osm_google.RDS")


map(.x = GA_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> GA_osm_google





GA_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 30.35785, 35.00066) | 
           !dplyr::between(as.numeric(long), -85.60516, -80.83973)) -> flagged_GA

write_csv(flagged_GA, file = "data/temp/excal_handcode/flagged_GA.csv")

### handcoding of missings


combine_handcode("GA")



# Ilinois geocode ---------------------------------------------------------

df_list[["IL"]] -> IL_df

IL_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> IL_osm_google


saveRDS(IL_osm_google, file = "data/temp/IL_osm_google.RDS")


purrr::map(.x = IL_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> IL_osm_google





IL_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 36.97030, 42.50848) | 
           !dplyr::between(as.numeric(long), -91.51308, -87.49476)) -> flagged_IL

write_csv(flagged_IL, file = "data/temp/excal_handcode/flagged_IL.csv")

### handcoding of missings


combine_handcode("IL")


# Iowa geocode ------------------------------------------------------------

df_list[["IA"]] -> IA_df

IA_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> IA_osm_google


saveRDS(IA_osm_google, file = "data/temp/IA_osm_google.RDS")

### sanity check lat & long

map(.x = IA_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> IA_osm_google






IA_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 40.37550, 43.50120) | 
           !dplyr::between(as.numeric(long), -96.63970, -90.14006)) -> flagged_IA

write_csv(flagged_IA, file = "data/temp/excal_handcode/flagged_IA.csv")

### handcoding of missings


combine_handcode("IA")

# Kentucky geocode --------------------------------------------------------

df_list[["KY"]] -> KY_df

KY_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> KY_osm_google


saveRDS(KY_osm_google, file = "data/temp/KY_osm_google.RDS")

### sanity check lat & long


map(.x = KY_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> KY_osm_google




KY_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 36.49713, 39.14746) | 
           !dplyr::between(as.numeric(long), -89.57151, -81.96497)) -> flagged_KY

write_csv(flagged_KY, file = "data/temp/excal_handcode/flagged_KY.csv")


### handcoding of missings


combine_handcode("KY")

# Louisiana geocode -------------------------------------------------------

df_list[["LA"]] -> LA_df

LA_df %>% 
  mutate(address = str_c(address, ", Louisiana")) %>%
  geocode_combine(queries = list(list(method = 'google'),
                                 list(method = "osm")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> LA_osm_google


saveRDS(LA_osm_google, file = "data/temp/LA_osm_google.RDS")


### sanity check lat & long

map(.x = LA_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> LA_osm_google



LA_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 28.92861, 33.01946) | 
           !dplyr::between(as.numeric(long), -94.04315, -88.81702)) -> flagged_LA

write_csv(flagged_LA, file = "data/temp/excal_handcode/flagged_LA.csv")


### handcoding of missings


combine_handcode("LA")


# Massachusetts geocode ---------------------------------------------------

df_list[["MA"]] -> MA_df

MA_df %>% 
  mutate(address = str_remove(address, "\\s*\\([^\\)]+\\)")) %>% 
  mutate(address = str_c(name, address, sep = ", ")) %>% 
  mutate(address = tolower(address)) %>% 
  mutate(address = str_remove(address, "(vote).*")) %>% 
  mutate(address = case_when(
    jurisdiction_type == "town" ~ str_c(address, jurisdiction, sep = ", "),
    TRUE ~ address
  )) %>% 
  mutate(address = str_c(address, ", ", county_name, " ", "county")) %>% 
  mutate(address = tolower(address)) %>% 
  geocode_combine(queries = list(list(method = 'google')), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> MA_osm_google


saveRDS(MA_osm_google, file = "data/temp/MA_osm_google.RDS")


### sanity check lat & long

map(.x = MA_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> MA_osm_google




MA_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 41.23796, 42.88659) | 
           !dplyr::between(as.numeric(long), -73.50814, -69.92839)) -> flagged_MA

write_csv(flagged_MA, file = "data/temp/excal_handcode/flagged_MA.csv")


### handcoding of missings

combine_handcode("MA")

# Maine geocode -----------------------------------------------------------

df_list[["ME"]] -> ME_df

ME_df %>% 
  mutate(address = str_c(name, address, sep = ", ")) %>% 
  mutate(address = str_c(address, ", ", county_name, " ", "county", ", Maine")) %>% 
  mutate(address = tolower(address)) %>% 
  geocode_combine(queries = list(list(method = 'google'),
                                 list(method = "osm")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> ME_osm_google


saveRDS(ME_osm_google, file = "data/temp/ME_osm_google.RDS")


### sanity check lat & long

map(.x = ME_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> ME_osm_google



ME_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 42.97776, 47.45969) | 
           !dplyr::between(as.numeric(long), -71.08392, -66.94989)) -> flagged_ME

write_csv(flagged_ME, file = "data/temp/excal_handcode/flagged_ME.csv")

### handcoding of missings

combine_handcode("ME")

# Maryland geocode --------------------------------------------------------

df_list[["MD"]] -> MD_df

MD_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> MD_osm_google


saveRDS(MD_osm_google, file = "data/temp/MD_osm_google.RDS")


### sanity check lat & long

map(.x = MD_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> MD_osm_google



MD_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 37.91172, 39.72304) | 
           !dplyr::between(as.numeric(long), -79.48765, -75.04894)) -> flagged_MD

write_csv(flagged_MD, file = "data/temp/excal_handcode/flagged_MD.csv")

write_csv(MD_osm_google, file = "data/temp/finished_df/MD.csv")

### no handcoding need! 

# Michigan geocode --------------------------------------------------------

df_list[["MI"]] -> MI_df

MI_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> MI_osm_google


saveRDS(MI_osm_google, file = "data/temp/MI_osm_google.RDS")

### sanity check lat & long

map(.x = MI_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> MI_osm_google



MI_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 41.69612, 48.23880) | 
           !dplyr::between(as.numeric(long), -90.41814, -82.41347)) -> flagged_MI

write_csv(flagged_MI, file = "data/temp/excal_handcode/flagged_MI.csv")

### handcoding of missings

combine_handcode("MI")


# Minnesota geocode -------------------------------------------------------

### check mail only boxes!

df_list[["MN"]] -> MN_df

MN_df %>% 
  mutate(address = case_when(
    !is.na(county_name) ~ str_c(address, ", ", county_name, " ", "county"),
    TRUE ~ address)) %>% 
  mutate(address = tolower(address)) %>%
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> MN_osm_google

saveRDS(MN_osm_google, file = "data/temp/MN_osm_google.RDS")


### sanity check lat & long

map(.x = MN_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> MN_osm_google



MN_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 43.49936, 49.38436) | 
           !dplyr::between(as.numeric(long), -97.23921, -89.49174)) -> flagged_MN

write_csv(flagged_MN, file = "data/temp/excal_handcode/flagged_MN.csv")

### handcoding of missings

combine_handcode("MN")

# Mississippi -------------------------------------------------------------

df_list[["MS"]] -> MS_df

MS_df %>% 
  mutate(address = na_if(address, "NO ADDRESS")) %>% 
  mutate(address = case_when(
    is.na(address) ~ str_c(name, ", ", county_name, " county", ", MS"),
    TRUE ~ address
  )) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> MS_osm_google


saveRDS(MS_osm_google, file = "data/temp/MS_osm_google.RDS")


### sanity check lat & long

map(.x = MS_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> MS_osm_google




MS_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 30.17394, 34.99605) | 
           !dplyr::between(as.numeric(long), -91.65501, -88.09789)) -> flagged_MS

write_csv(flagged_MS, file = "data/temp/excal_handcode/flagged_MS.csv")

### handcoding of missings

combine_handcode("MS")


# Montana geocode ---------------------------------------------------------

df_list[["MT"]] -> MT_df

MT_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> MT_osm_google


saveRDS(MT_osm_google, file = "data/temp/MT_osm_google.RDS")


### sanity check lat & long

map(.x = MT_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> MT_osm_google




MT_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 44.35822, 49.00139) | 
           !dplyr::between(as.numeric(long), -116.05000, -104.03914)) -> flagged_MT

write_csv(flagged_MT, file = "data/temp/excal_handcode/flagged_MT.csv")

### handcoding of missings

combine_handcode("MT")


# Nebraska geocode --------------------------------------------------------


df_list[["NE"]] -> NE_df

NE_df %>% 
  mutate(address = case_when(
    str_detect(string = name, pattern = "Direct Mail") ~ NA_character_,
    TRUE ~ address
  )) %>% 
  mutate(address = case_when(
    str_detect(string = name, pattern = "Mail") ~ NA_character_,
    TRUE ~ address
  )) %>% 
  mutate(address = str_remove(string = address, pattern = ", NE nan")) %>% 
  mutate(address = case_when(
    stringi::stri_isempty(address) ~ str_c(name, " ", county_name, " county", ", NE"),
    TRUE ~ address
  )) %>% 
  mutate(address = str_remove(string = address, pattern = " \\s*\\([^\\)]+\\)")) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> NE_osm_google


saveRDS(NE_osm_google, file = "data/temp/NE_osm_google.RDS")


### sanity check lat & long

map(.x = NE_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> NE_osm_google





NE_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 40.00000, 43.00171) | 
           !dplyr::between(as.numeric(long), -104.05351, -95.30829)) -> flagged_NE

write_csv(flagged_NE, file = "data/temp/excal_handcode/flagged_NE.csv")

### handcoding of missings

combine_handcode("NE")

# New Hampshire geocode ---------------------------------------------------

df_list[["NH"]] -> NH_df

NH_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> NH_osm_google


saveRDS(NH_osm_google, file = "data/temp/NH_osm_google.RDS")


### sanity check lat & long

map(.x = NH_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> NH_osm_google




NH_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 42.69699, 45.30548) | 
           !dplyr::between(as.numeric(long), -72.55725, -70.61062)) -> flagged_NH

write_csv(flagged_NH, file = "data/temp/excal_handcode/flagged_NH.csv")

### handcoding of missings

combine_handcode("NH")

# New Jersey geocode ------------------------------------------------------

df_list[["NJ"]] -> NJ_df

NJ_df %>% 
  mutate(address = str_remove(string = address, pattern = " \\s*\\([^\\)]+\\)")) %>% 
  mutate(election_year = as.numeric(election_year)) %>% 
  filter(election_year <= 2014) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> NJ_osm_google_2014

saveRDS(NJ_osm_google_2014, file = "data/temp/NJ_osm_google_2014.RDS")


NJ_df %>% 
  mutate(address = str_remove(string = address, pattern = " \\s*\\([^\\)]+\\)")) %>% 
  mutate(election_year = as.numeric(election_year)) %>% 
  filter(election_year > 2014) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> NJ_osm_google_2018


saveRDS(NJ_osm_google_2018, file = "data/temp/NJ_osm_google_2018.RDS")


NJ_osm_google <- c(NJ_osm_google_2014, NJ_osm_google_2018)

### sanity check lat & long

map(.x = NJ_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> NJ_osm_google

saveRDS(NJ_osm_google, file = "data/temp/NJ_osm_google.RDS")



NJ_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 38.92852, 41.35742) | 
           !dplyr::between(as.numeric(long), -75.55961, -73.89398)) 


### no handcoding needed

write_csv(NJ_osm_google, file = "data/temp/finished_df/NJ.csv")

# New Mexico geocode ------------------------------------------------------

df_list[["NM"]] -> NM_df

NM_df %>% 
  mutate(address = str_squish(address)) %>% 
  mutate(address = str_c(address, ", ", county_name, " County")) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> NM_osm_google


saveRDS(NM_osm_google, file = "R_project/data/temp/NM_osm_google.RDS")

### sanity check lat & long

map(.x = NM_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> NM_osm_google



NM_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 31.33230, 37.00023) | 
           !dplyr::between(as.numeric(long), -109.05017, -103.00196)) -> flagged_NM

write_csv(flagged_NM, file = "data/temp/excal_handcode/flagged_NM.csv")


### handcoding of missings

combine_handcode("NM")

# North Carolina geocode --------------------------------------------------

df_list[["NC"]] -> NC_df

NC_df %>% 
  mutate(address = str_c(address, ", ", county_name, " county"),
         address = tolower(address)) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> NC_osm_google

saveRDS(NC_osm_google, file = "data/temp/NC_osm_google.RDS")


### sanity check lat & long

map(.x = NC_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> NC_osm_google


NC_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 33.84232, 36.58812) | 
           !dplyr::between(as.numeric(long), -84.32187, -75.46062)) -> flagged_NC

write_csv(flagged_NC, file = "data/temp/excal_handcode/flagged_NC.csv")

### handcoding of missings

combine_handcode("NC")

# North Dakota geocode ----------------------------------------------------

df_list[["ND"]] -> ND_df

ND_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> ND_osm_google

saveRDS(ND_osm_google, file = "data/temp/ND_osm_google.RDS")

### sanity check lat & long

map(.x = ND_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> ND_osm_google



ND_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 45.93505, 49.00057) | 
           !dplyr::between(as.numeric(long), -104.04890, -96.55451)) -> flagged_ND

write_csv(flagged_ND, file = "data/temp/excal_handcode/flagged_ND.csv")


### handcoding of missings

combine_handcode("ND")

# Pennsylvania geocode ----------------------------------------------------

df_list[["PA"]] -> PA_df

PA_df %>% 
  mutate(address = str_remove(address, "\\s*\\([^\\)]+\\)")) %>% 
  mutate(across(.cols = everything(), ~ str_remove_all(.x, "&amp;amp;"))) %>% 
  mutate(address = case_when(
    !str_detect(string = address, pattern = "1\\d{4}") ~ str_c(name, address, sep = " "),
    TRUE ~ address
  )) %>% 
  mutate(election_year = as.numeric(election_year)) %>% 
  filter(election_year <= 2014) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                           global_params = list(address = 'address', full_results = TRUE), 
                           return_list = TRUE) -> PA_osm_google_2014

saveRDS(PA_osm_google_2014, file = "data/temp/PA_osm_google_2014.RDS")


PA_df %>% 
  mutate(address = str_remove(address, "\\s*\\([^\\)]+\\)")) %>% 
  mutate(across(.cols = everything(), ~ str_remove_all(.x, "&amp;amp;"))) %>% 
  mutate(address = case_when(
    !str_detect(string = address, pattern = "1\\d{4}") ~ str_c(name, address, sep = " "),
    TRUE ~ address
  )) %>% 
  mutate(election_year = as.numeric(election_year)) %>% 
  filter(election_year > 2014) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> PA_osm_google_2018
  
saveRDS(PA_osm_google_2018, file = "data/temp/PA_osm_google_2018.RDS")


### sanity check lat & long


PA_osm_google <- c(PA_osm_google_2014, PA_osm_google_2018)

map(.x = PA_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> PA_osm_google

saveRDS(PA_osm_google, file = "data/temp/PA_osm_google.RDS")


PA_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 39.71980, 42.26986) | 
           !dplyr::between(as.numeric(long), -80.51989, -74.68952)) -> flagged_PA

write_csv(flagged_PA, file = "data/temp/excal_handcode/flagged_PA.csv")

### handcoding of missings

combine_handcode("PA")

# Rhode Island geocode ----------------------------------------------------

df_list[["RI"]] -> RI_df

RI_df %>% 
  mutate(address = str_remove(string = address, pattern = "00000")) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> RI_osm_google

saveRDS(RI_osm_google, file = "R_project/data/temp/RI_osm_google.RDS")


### sanity check lat & long


map(.x = RI_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> RI_osm_google


RI_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 41.14634, 42.01880) | 
           !dplyr::between(as.numeric(long), -71.86277, -71.12057)) -> flagged_RI


write_csv(RI_osm_google, file = "data/temp/finished_df/RI.csv")

# Oklahoma geocode --------------------------------------------------------

df_list[["OK"]] -> OK_df

OK_df %>% 
  mutate(address = str_remove(string = address, pattern = " \\s*\\([^\\)]+\\)")) %>% 
  mutate(address = case_when(
    str_detect(string = address, pattern = "--") ~ str_c(name, address, sep = " "),
    TRUE ~ address
  )) %>% 
  mutate(address = str_remove(string = address, pattern = "--")) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> OK_osm_google


saveRDS(OK_osm_google, file = "R_project/data/temp/OK_osm_google.RDS")


### sanity check lat & long


map(.x = OK_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> OK_osm_google


OK_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 33.61583, 37.00221) | 
           !dplyr::between(as.numeric(long), -103.00257, -94.43066)) -> flagged_OK

write_csv(flagged_OK, file = "data/temp/excal_handcode/flagged_OK.csv")

### handcoding of missings

combine_handcode("OK")


# Ohio geocode ------------------------------------------------------------

df_list[["OH"]] -> OH_df

OH_df %>% 
  mutate(address = str_c(address, ", ", county_name, " County")) %>% 
  group_by(election_year) %>% 
  slice_head(prop = 0.5) -> OH_head

OH_df %>% 
  mutate(address = str_c(address, ", ", county_name, " County")) %>% 
  group_by(election_year) %>% 
  slice_tail(prop = 0.5) -> OH_tail

OH_head %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> OH_osm_google_head

OH_tail %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> OH_osm_google_tail


OH_osm_google_both <- list(OH_osm_google_head, OH_osm_google_tail)

saveRDS(OH_osm_google_both, file = "data/temp/OH_osm_google_both.RDS")



### sanity check lat & long

OH_osm_google_both <- c(OH_osm_google_both[[1]], OH_osm_google_both[[2]])

map(.x = OH_osm_google_both, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> OH_osm_google

saveRDS(OH_osm_google, file = "data/temp/OH_osm_google.RDS")


#### IMPORTANT TWO PLACES MISSING!

OH_df %>% 
  anti_join(OH_osm_google, by = c("rowid" = "rowid")) -> OH_left

OH_left %>% 
  mutate(address = str_c(address, ", ", county_name, " County")) %>% 
  geocode_combine(queries = list(list(method = 'google'),
                                 list(method = "osm")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> OH_left_geocode

OH_left_geocode <- OH_left_geocode[["google"]]

OH_left_geocode %>% 
  dplyr::mutate(across(.cols = everything(), ~ as.character(.))) %>% 
  bind_rows(OH_osm_google) -> OH_osm_google



OH_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 38.40320, 41.97752) | 
           !dplyr::between(as.numeric(long), -84.82016, -80.51869)) -> flagged_OH

write_csv(flagged_OH, file = "data/temp/excal_handcode/flagged_OH.csv")

### handcoding of missings

combine_handcode("OH")

# South Carolina geocode --------------------------------------------------

df_list[["SC"]] -> SC_df

SC_df %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> SC_osm_google

saveRDS(SC_osm_google, file = "R_project/data/temp/SC_osm_google.RDS")


### sanity check lat & long


map(.x = SC_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> SC_osm_google



SC_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 32.03460, 35.21540) | 
           !dplyr::between(as.numeric(long), -83.35391, -78.54203)) -> flagged_SC

write_csv(flagged_SC, file = "data/temp/excal_handcode/flagged_SC.csv")

### handcoding of missings

combine_handcode("SC")

# Texas geocode -----------------------------------------------------------

df_list[["TX"]] -> TX_df

TX_df %>% 
    mutate(address = case_when(
      !str_detect(string = address, pattern = "7\\d{4}") ~ str_c(name, " ", address, ", ", county_name, " county"),
      TRUE ~ address
    )) %>% 
    mutate(address = str_remove(string = address, pattern = "\\s*\\([^\\)]+\\)")) %>% 
    mutate(address = str_remove(string = address, pattern = "\\(|\\)")) %>% 
    group_by(election_year) %>% 
    slice_head(prop = 0.5) -> TX_head

TX_df %>% 
  mutate(address = case_when(
    !str_detect(string = address, pattern = "7\\d{4}") ~ str_c(name, " ", address, ", ", county_name, " county"),
    TRUE ~ address
  )) %>% 
  mutate(address = str_remove(string = address, pattern = "\\s*\\([^\\)]+\\)")) %>% 
  mutate(address = str_remove(string = address, pattern = "\\(|\\)")) %>% 
  group_by(election_year) %>% 
  slice_tail(prop = 0.5) -> TX_tail

TX_head %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> TX_osm_google_head

TX_tail %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> TX_osm_google_tail

TX_osm_google_both <- list(TX_osm_google_head, TX_osm_google_tail)

saveRDS(TX_osm_google_both, file = "data/temp/TX_osm_google_both.RDS")

### carefull!! missing 3 places 

TX_osm_google_both <- c(TX_osm_google_both[[1]], TX_osm_google_both[[2]])

map(.x = TX_osm_google_both, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> TX_osm_google_both

saveRDS(TX_osm_google_both, file = "data/temp/TX_osm_google.RDS")


TX_df %>% 
  anti_join(TX_osm_google_both, by = c("rowid" = "rowid")) -> TX_left

TX_left %>% 
  mutate(address = case_when(
    !str_detect(string = address, pattern = "7\\d{4}") ~ str_c(name, " ", address, ", ", county_name, " county"),
    TRUE ~ address
  )) %>% 
  mutate(address = str_remove(string = address, pattern = "\\s*\\([^\\)]+\\)")) %>% 
  mutate(address = str_remove(string = address, pattern = "\\(|\\)")) %>% 
  geocode(method = "google", address = "address", full_results = TRUE) -> TX_left


TX_left %>% 
  dplyr::mutate(across(.cols = everything(), ~ as.character(.))) %>% 
  bind_rows(TX_osm_google_both) -> TX_osm_google



TX_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 25.83738, 36.50070) | 
           !dplyr::between(as.numeric(long), -106.64565, -93.50829)) -> flagged_TX

write_csv(flagged_TX, file = "data/temp/excal_handcode/flagged_TX.csv")

### handcoding of missings

combine_handcode("TX")


# Wisconsin geocode -------------------------------------------------------

df_list[["WI"]] -> WI_df

WI_df %>% 
  mutate(address = str_remove(string = address, pattern = " \\s*\\([^\\)]+\\)")) %>% 
  group_by(election_year) %>% 
  slice_head(prop = 0.5) -> WI_head

WI_df %>% 
  mutate(address = str_remove(string = address, pattern = " \\s*\\([^\\)]+\\)")) %>% 
  group_by(election_year) %>% 
  slice_tail(prop = 0.5) -> WI_tail


WI_head %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> WI_osm_google_head



WI_tail %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> WI_osm_google_tail


WI_osm_google_both <- list(WI_osm_google_head, WI_osm_google_tail)

saveRDS(WI_osm_google_both, file = "data/temp/WI_osm_google_both.RDS")

### sanity check lat & long

WI_osm_google_both <- c(WI_osm_google_both[[1]], WI_osm_google_both[[2]])

map(.x = WI_osm_google_both, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> WI_osm_google_both

saveRDS(WI_osm_google_both, file = "data/temp/WI_osm_google.RDS")


WI_df %>% 
  anti_join(WI_osm_google_both, by = c("rowid" = "rowid")) -> WI_left

WI_left %>% 
  geocode(method = "google", address = "address", full_results = TRUE) -> WI_left


WI_left %>% 
  dplyr::mutate(across(.cols = everything(), ~ as.character(.))) %>% 
  bind_rows(WI_osm_google_both) -> WI_osm_google


WI_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 42.49198, 47.08062) | 
           !dplyr::between(as.numeric(long), -92.88811, -86.80541)) -> flagged_WI

write_csv(flagged_WI, file = "data/temp/excal_handcode/flagged_WI.csv")


### handcoding of missings

combine_handcode("WI")


# South Dakota geocode ----------------------------------------------------

df_list[["SD"]] -> SD_df


SD_df %>% 
  mutate(address = case_when(
    is.na(address) ~ name,
    TRUE ~ address)) %>% 
  mutate(address = str_c(address, ", ", county_name, " county", ", SD")) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> SD_osm_google

saveRDS(SD_osm_google, file = "data/temp/SD_osm_google.RDS")

### sanity check lat & long


map(.x = SD_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> SD_osm_google



SD_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 42.47964, 45.94545) | 
           !dplyr::between(as.numeric(long), -104.05770, -96.43659)) -> flagged_SD

write_csv(flagged_SD, file = "data/temp/excal_handcode/flagged_SD.csv")

### handcoding of missings

combine_handcode("SD")

# Vermont geocode ---------------------------------------------------------

df_list[["VT"]] -> VT_df

VT_df %>% 
  mutate(address = case_when(
    is.na(address) ~ name,
    !is.na(address) ~ str_c(name, address, sep = ", "))) %>% 
  mutate(address = str_c(address, municipality, "VT", sep = ", ")) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> VT_osm_google
  
saveRDS(VT_osm_google, file = "data/temp/VT_osm_google.RDS")

### sanity check lat & long

map(.x = VT_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> VT_osm_google


VT_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 42.72685, 45.01666) | 
           !dplyr::between(as.numeric(long), -73.43774, -71.46456)) -> flagged_VT

write_csv(flagged_VT, file = "data/temp/excal_handcode/flagged_VT.csv")

### handcoding of missings

combine_handcode("VT")

# Virginia geocode --------------------------------------------------------

df_list[["VA"]] -> VA_df

VA_df %>% 
  mutate(address = str_remove(string = address, pattern = " \\s*\\([^\\)]+\\)")) %>% 
  mutate(address = case_when(
    !str_detect(string = address, pattern = "2\\d{4}") ~ str_c(name, address, county_name, sep = ", "),
    TRUE ~ address
  )) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> VA_osm_google

saveRDS(VA_osm_google, file = "R_project/data/temp/VA_osm_google.RDS")


### sanity check lat & long

map(.x = VA_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> VA_osm_google


VA_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 36.54074, 39.46601) | 
           !dplyr::between(as.numeric(long), -83.67539, -75.24227)) -> flagged_VA

write_csv(flagged_VA, file = "data/temp/excal_handcode/flagged_VA.csv")

### handcoding of missings

combine_handcode("VA")

# West Virginia geocode ---------------------------------------------------

df_list[["WV"]] -> WV_df

WV_df %>% 
  mutate(address = case_when(
    is.na(address) ~ name,
    !str_detect(string = address, pattern = "2\\d{4}") ~ str_c(name, address, sep = ", "),
    TRUE ~ address)) %>% 
  mutate(address = str_c(address, ", ", county_name, " county")) %>% 
  mutate(address = case_when(
    !str_detect(string = address, pattern = "WV") ~ str_c(address, "WV", sep = ", "),
    TRUE ~ address
    )) %>% 
  geocode_combine(queries = list(list(method = 'osm'),
                                 list(method = "google")), 
                  global_params = list(address = 'address', full_results = TRUE), 
                  return_list = TRUE) -> WV_osm_google


saveRDS(WV_osm_google, file = "data/temp/WV_osm_google.RDS")


### sanity check lat & long

map(.x = WV_osm_google, ~ {
  .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
}) %>% bind_rows() -> WV_osm_google


WV_osm_google %>% 
  filter(is.na(lat) | is.na(long) | !dplyr::between(as.numeric(lat), 37.20148, 40.63880) | 
           !dplyr::between(as.numeric(long), -82.64474, -77.71952)) -> flagged_WV

write_csv(flagged_WV, file = "data/temp/excal_handcode/flagged_WV.csv")

### handcoding of missings

combine_handcode("WV")


# create final dataframes -------------------------------------------------

list_of_states <- list.files(path = "data/temp/finished_df",
                             full.names = TRUE)

model_df <- list_of_states %>%
  set_names() %>% 
  map_df(data.table::fread, colClasses = 'character') 

model_df %>% 
  dplyr::mutate(lat = coalesce(lat, latitude),
                long = coalesce(long, longitude),
                lat = as.numeric(lat),
                long = as.numeric(long)) -> model_df



# find unique polling places for each election ----------------------------

model_df %>% 
  drop_na(lat) %>% 
  mutate(lat = as.numeric(formatC(lat, digits = 4, format = "f")),
         long = as.numeric(formatC(long, digits = 4, format = "f"))) %>% 
  group_by(state, election_year) %>% 
  distinct(lat, long, .keep_all = TRUE) %>% 
  ungroup() -> unique_df


write_csv(unique_df, file = "data/model_df/unique_polling_places.csv")
