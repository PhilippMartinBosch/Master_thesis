
# read in data ------------------------------------------------------------

unique_df <- read_csv("data/model_df/unique_polling_places.csv")

### add model variables ###

nort_carolina_shelby <- read_csv("data/nort_carolina_shelby.csv", col_names = FALSE)
gov_vote <- read_delim("data/gov_vote.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(conv_voting = str_remove(conv_voting, "%")) %>% 
  mutate(conv_voting = as.numeric(conv_voting))

unique_df %>% 
  dplyr::select(rowid, state, name, address, election_year, lat, long, display_name, formatted_address) %>% 
  dplyr::mutate(lookup_address = coalesce(display_name, formatted_address)) %>% 
  dplyr::select(-c(display_name, formatted_address)) %>% 
  dplyr::filter(election_year == 2012 | election_year == 2016) -> base_model_df

write_csv(base_model_df, file = "data/model_df/base_model_df.csv")

### make it spatial

base_model_df %>% 
  st_as_sf(coords = c("long", "lat"), remove = FALSE, 
           crs = 4269, agr = "constant") -> spacial_model_df


### read in shapefiles


# 2012 --------------------------------------------------------------------


sf::st_read(dsn = "data/shapefiles/county_big_model/ACS_2012_5YR_COUNTY.gdb/", layer = "COUNTY_METADATA_2012") %>% View()

st_layers(dsn = "data/shapefiles/county_big_model/ACS_2012_5YR_COUNTY.gdb/")

county_shape_12 <- sf::st_read(dsn = "data/shapefiles/county_big_model/ACS_2012_5YR_COUNTY.gdb/", layer = "ACS2012_COUNTY_GEOGRAPHY")

county_data_12_age <- sf::st_read(dsn = "data/shapefiles/county_big_model/ACS_2012_5YR_COUNTY.gdb/", layer = "X02_RACE") %>% 
  dplyr::select(GEOID, total_pop = B01001e1, u_five = B01001e3, five_nine = B01001e4, nine_fourteen = B01001e5, fifteen_seventeen = B01001e6,
                white_pop = B01001Ae1, white_u5 = B01001Ae3, white_five_nine = B01001Ae4, white_nine_fourteen = B01001Ae5, white_fifteen_seventeen = B01001Ae6,
                black_pop = B01001Be1, black_u5 = B01001Be3, black_five_nine = B01001Be4, black_nine_fourteen = B01001Be5, black_fifteen_seventeen = B01001Be6)


county_data_12_age <- sf::st_read(dsn = "data/shapefiles/county_big_model/ACS_2012_5YR_COUNTY.gdb/", layer = "X01_AGE_AND_SEX") %>% 
  dplyr::select(GEOID, total_pop = B01001e1, u_five = B01001e3, five_nine = B01001e4, nine_fourteen = B01001e5, fifteen_seventeen = B01001e6,
                u_five_female = B01001e27, five_nine_female = B01001e28, nine_fourteen_female = B01001e29, fifteen_seventeen_female = B01001e30) %>% 
  tibble()


county_data_12_age %>% 
  mutate(non_voting_pop = u_five + five_nine + nine_fourteen + fifteen_seventeen + u_five_female + five_nine_female + nine_fourteen_female + fifteen_seventeen_female ) %>% 
  mutate(voting_pop = total_pop - non_voting_pop) %>% 
  dplyr::select(total_pop_12 = total_pop, voting_pop_12 = voting_pop, GEOID) -> vote_12
  


# 2016 --------------------------------------------------------------------

st_layers(dsn = "data/shapefiles/county_big_model/ACS_2016_5YR_COUNTY.gdb/")

county_shape_16 <- sf::st_read(dsn = "data/shapefiles/county_big_model/ACS_2016_5YR_COUNTY.gdb/", layer = "ACS_2016_5YR_COUNTY")

county_data_16_age <- sf::st_read(dsn = "data/shapefiles/county_big_model/ACS_2016_5YR_COUNTY.gdb/", layer = "X01_AGE_AND_SEX") %>% 
  dplyr::select(GEOID, total_pop = B01001e1, u_five = B01001e3, five_nine = B01001e4, nine_fourteen = B01001e5, fifteen_seventeen = B01001e6,
                u_five_female = B01001e27, five_nine_female = B01001e28, nine_fourteen_female = B01001e29, fifteen_seventeen_female = B01001e30) %>% 
  mutate(non_voting_pop = u_five + five_nine + nine_fourteen + fifteen_seventeen + u_five_female + five_nine_female + nine_fourteen_female + fifteen_seventeen_female ) %>% 
  mutate(voting_pop = total_pop - non_voting_pop) %>% 
  dplyr::select(GEOID, total_pop, voting_pop) %>% 
  tibble()


county_data_16_race <- sf::st_read(dsn = "data/shapefiles/county_big_model/ACS_2016_5YR_COUNTY.gdb/", layer = "X01_AGE_AND_SEX") %>% 
  tibble() %>% 
  dplyr::select(total_pop = B01001e1, GEOID, black_pop = B01001Be1, black_u5 = B01001Be3, 
                black_five_nine = B01001Be4, black_nine_fourteen = B01001Be5, black_fifteen_seventeen = B01001Be6,
                black_u5_female = B01001Be18, black_five_nine_female = B01001Be19, 
                black_nine_fourteen_female = B01001Be20, black_fifteen_seventeen_female = B01001Be21,
                hisp_total = B01001Ie1, hisp_u5 = B01001Ie3, hisp_o5 = B01001Ie4, hisp_o9 = B01001Ie5,
                hisp_o15 = B01001Ie6, hisp_fem_u_5 = B01001Ie18, hisp_fem_o_5 = B01001Ie19, 
                hisp_fem_o_9 = B01001Ie20, hisp_fem_o_15 = B01001Ie21) %>% 
  mutate(black_non_vote = black_u5 + black_five_nine + black_nine_fourteen + black_fifteen_seventeen +
           black_u5_female + black_five_nine_female + black_nine_fourteen_female + black_fifteen_seventeen_female,
         hispanice_non_vote = hisp_u5 + hisp_o5 + hisp_o9 + hisp_o15 + hisp_fem_u_5 + 
           hisp_fem_o_5 + hisp_fem_o_9 + hisp_fem_o_15) %>% 
  mutate(black_vote = black_pop - black_non_vote,
         hisp_vote = hisp_total - hispanice_non_vote) %>% 
  dplyr::select(GEOID, black_vote, hisp_vote, black_pop, hisp_total)

### an model df hängen

county_data_16_age %>% 
  left_join(county_data_16_race) %>% 
  mutate(prop_black_vote = black_vote / voting_pop,
         prop_hisp_vote = hisp_vote / voting_pop) -> vote_pop_16
  
vote_pop_16 %>% 
  left_join(vote_12) -> vote_join

county_shape_16 %>% 
  right_join(vote_join, by = c("GEOID_Data" = "GEOID")) -> vote_join_spacial

# add shelby variable

usmap::fips_info(vote_join_spacial$STATEFP) %>% 
  tibble() %>% 
  distinct(fips, abbr, full) -> states_with_fips

vote_join_spacial %>% 
  left_join(states_with_fips, by = c("STATEFP" = "fips")) -> vote_join_spacial

nort_carolina_shelby$X1 -> nort_carolina_shelby_vector


vote_join_spacial %>% 
  mutate(shelby = FALSE) %>% 
  mutate(shelby = case_when(
    full %in% c("Alabama", "Alaska", "Arizona", "Georgia", "Louisiana", "Mississippi", "South Carolina", "Texas", "Virginia") ~ TRUE,
    TRUE ~ shelby)) %>% 
  mutate(shelby = case_when(
    full == "California" & NAMELSAD %in% c("Kings County", "Monterey County", "Yuba County") ~ TRUE,
                                           TRUE ~ shelby)
  ) %>% 
  mutate(shelby = case_when(
    full == "Florida" & NAMELSAD %in% c("Collier County", "Hardee County", "Hendry County" ,"Hillsborough County", "Monroe County") ~ TRUE,
    TRUE ~ shelby)
  ) %>% 
  mutate(shelby = case_when(
    full == "New York" & NAMELSAD %in% c("Bronx County", "Kings County", "New York County") ~ TRUE,
    TRUE ~ shelby)
  ) %>% 
  mutate(shelby = case_when(
    full == "North Carolina" & NAMELSAD %in% nort_carolina_shelby_vector ~ TRUE,
    TRUE ~ shelby)
  ) %>%
  mutate(shelby = case_when(
    full == "South Dakota" & NAMELSAD %in% c("Shannon County", "Todd County") ~ TRUE,
    TRUE ~ shelby)
  ) %>% 
  mutate(shelby = case_when(
    full == "Michigan" & NAMELSAD %in% c("Allegan County", "Saginaw County") ~ TRUE,
    TRUE ~ shelby)
  ) -> vote_join_spacial
  
vote_join_spacial %>% 
  dplyr::select(shelby, GEOID_Data) %>% 
  st_drop_geometry() %>% 
  tibble() -> shelby_df

spacial_model_df %>% 
  st_join(vote_join_spacial) -> spacial_model_df



spacial_model_df %>% 
  st_drop_geometry() %>% 
  janitor::clean_names() %>% 
  rename(county = name_2) -> model_df_final



model_df_final %>% 
  count(election_year, geoid_data) %>% 
  left_join(county_shape_16, by = c("geoid_data" = "GEOID_Data")) %>% 
  left_join(vote_join, by = c("geoid_data" = "GEOID")) -> aggregate_model

aggregate_model %>% 
  dplyr::select(election_year, geoid_data, n, STATEFP, NAMELSAD, total_pop, voting_pop,
                black_vote, hisp_vote, black_pop, hisp_total, prop_black_vote, prop_hisp_vote,
                total_pop_12, voting_pop_12) -> reduced_model


reduced_model %>% 
  pivot_wider(names_from = election_year, values_from = n) %>% 
  rename(polls_2012 = "2012", polls_2016 = "2016") -> reduced_model_wide

usmap::fips_info(reduced_model_wide$STATEFP) %>% 
  bind_cols(reduced_model_wide) %>% 
  dplyr::select(-STATEFP) %>% 
  tibble() -> reduced_model_wide

reduced_model_wide %>% 
  drop_na(polls_2012, polls_2016) -> final_df

final_df %>% 
  left_join(gov_vote, by = c("abbr" = "state")) %>% 
  left_join(shelby_df, by = c("geoid_data" = "GEOID_Data")) %>% 
  mutate(prop_16 = voting_pop/polls_2016,
         prop_12 = voting_pop_12/polls_2012,
         change = prop_16 - prop_12,
         prop = (change / (voting_pop_12/polls_2012)) * 100) %>% 
  drop_na(prop) -> brm_df


write_csv(brm_df, file = "data/model_df/12_16.csv")





# moving_variable ---------------------------------------------------------


county_shape_16 %>% 
  st_drop_geometry() %>% 
  dplyr::select(GEOID_Data, NAMELSAD) %>% 
  right_join(model_df_final, by = c("GEOID_Data" = "geoid_data")) %>% 
  janitor::clean_names() %>% 
  tibble() %>% 
  left_join(gov_vote, by = c("abbr" = "state")) %>% 
  dplyr::select(name, county_name = namelsad, address, lat, long, lookup_address, election_year, geoid_data, total_pop, voting_pop,
                black_vote, hisp_vote, black_pop, hisp_total, prop_black_vote, prop_hisp_vote,
                total_pop_12, voting_pop_12, abbr, full, shelby, governor, conv_voting) %>% 
  write_csv(file = "data/model_df/moves_df.csv")


moves_df %>% 
  filter(county_name == "Cuming County") -> cuming_test

moves_df %>% 
  filter(election_year == 2012) -> moves_12

moves_df %>% 
  filter(election_year == 2016) -> moves_16

moves_16 %>% 
  add_count(geoid_data) %>% 
  semi_join(moves_12, by = c("lat" = "lat", "long" = "long")) %>% 
  add_count(geoid_data) %>%
  anti_join(extreme_df, by = c("geoid_data" = "geoid_data")) %>% 
  mutate(prop_new = nn / n) %>% 
  mutate(prop_not_new = 1 - prop_new) -> zero_inflated_model

zero_inflated_model %>% 
  group_by(geoid_data) %>% 
  slice(1) %>% 
  ungroup() -> reduced_zero_inflatd
  



# EAVS data ---------------------------------------------------------------

eavs_2012 <- read_delim("data/EAVS/eavs_2012.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

eavs_2016 <- read_csv("data/EAVS/EAVS 2016 Final Data for Public Release v.3 2.csv", col_types = "c") %>% 
  dplyr::select(D2a, FIPSCode, State) -> eavs_2016


eavs_2016 %>% 
  mutate(FIPSCode = as.character(FIPSCode)) %>% 
  mutate(FIPSCode = case_when(
    State == "AK" ~ str_c("0", FIPSCode),
    State == "AL" ~ str_c("0", FIPSCode),
    State == "AR" ~ str_c("0", FIPSCode),
    State == "CA" ~ str_c("0", FIPSCode),
    State == "CO" ~ str_c("0", FIPSCode),
    State == "CT" ~ str_c("0", FIPSCode),
    TRUE ~ FIPSCode
  )) %>% 
  mutate(county_fips = str_sub(FIPSCode, end = 5)) %>% 
  mutate(D2a = case_when(
    str_detect(D2a, "[^0-9.-]") ~ NA_character_,
    TRUE ~ D2a
  )) %>% 
  mutate(D2a = as.numeric(D2a)) %>% 
  group_by(county_fips) %>% 
  summarise(count_polling = sum(D2a)) -> eavs_2016_count

balance_df %>% 
  mutate(county_fips = str_sub(geoid_data, start = -5)) -> balance_df

eavs_2012 %>% 
  mutate(county_fips = str_sub(FIPSCode, end = 5)) -> eavs_2012

eavs_2012 %>% 
  group_by(county_fips) %>% 
  summarise(n_2012 = sum(QD2a)) -> eavs_2012_count


eavs_2016_count %>% 
  left_join(eavs_2012_count, by = c("county_fips" = "county_fips")) -> EAVS_count

balance_df %>% 
  mutate(fips = str_sub(geoid_data, -5)) %>%
  inner_join(EAVS_count, by = c("fips" = "county_fips")) %>% 
  filter(n_2012 > 0) %>% 
  mutate(prop_16_eavs = voting_pop/count_polling,
         prop_12_eavs = voting_pop_12/n_2012,
         change = prop_16_eavs - prop_12_eavs,
         prop_eavs = (change / (voting_pop_12/n_2012)) * 100) %>% 
  filter(is.finite(prop_eavs)) -> eavs_brm
  
  

  


