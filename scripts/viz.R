### viz ###



# all states --------------------------------------------------------------


table(model_df$state, model_df$election_year) %>%  
  as.data.frame.matrix() -> cross_tab

rownames(cross_tab) -> states_covered


as.data.frame.matrix(cross_tab) %>% 
  tibble() -> cross_tab






cross_tab %>% 
  bind_cols(states = states_covered) %>% 
  mutate(across(.cols = c(1:4), ~ case_when(
    .x > 0 ~ "yes",
    TRUE ~ "no"
  ))) -> cross_tab

cross_tab %>% 
  relocate(states, .before = 1) -> cross_tab


check_format <- formatter("span",
                          x ~ icontext(ifelse(x == "yes", "ok", "remove")),
                          style = x ~ style(color = ifelse(x == "no", "red", "green")))

bind_cols(abbr = state.abb, full = state.name) -> state_names_abbr


cross_tab %>% 
  left_join(state_names_abbr, by = c("states" = "abbr")) %>% 
  relocate(full, .before = 1) %>% 
  dplyr::select(-states) -> sample_states





state_names_abbr %>% 
  anti_join(cross_tab, by = c("abbr" = "states")) -> missing_states

tibble(`2012` = "no", `2014` = "no", `2016` = "no", `2018` = "no") %>% 
  bind_cols(missing_states) %>% 
  relocate(states = full, .before = 1) %>% 
  dplyr::select(-abbr) -> missing_states 

sample_states %>% 
  rename(states = full) %>% 
  bind_rows(missing_states) %>% 
  arrange(states) %>% 
  rename(State = states) %>% 
  slice_head(n = 25) %>% 
    formattable( 
    align = c("r","c", "c", "c", "c"),
    list(`2012` = check_format,
         `2014` = check_format,
         `2016` = check_format,
         `2018` = check_format)) -> formatted_table_25


sample_states %>% 
  rename(states = full) %>% 
  bind_rows(missing_states) %>% 
  arrange(states) %>% 
  rename(State = states) %>% 
  slice_tail(n = 25) %>% 
  formattable( 
    align = c("r","c", "c", "c", "c"),
    list(`2012` = check_format,
         `2014` = check_format,
         `2016` = check_format,
         `2018` = check_format)) -> formatted_table_50


formatted_table_25 %>% 
  as.htmlwidget() %>% 
  htmlwidgets::saveWidget(file = "test_25.html")

formatted_table_50 %>% 
  as.htmlwidget() %>% 
  htmlwidgets::saveWidget(file = "test_50.html")

# states in model ---------------------------------------------------------


df %>% 
  dplyr::filter(rowid == "AK1" | 
         rowid == "MS167" | 
           rowid == "MS810" |
         rowid == "MA70" |
           rowid == "PA110" |
           rowid == "WV1") %>% 
  dplyr::select(state, jurisdiction, jurisdiction_type, precinct_name, name, address,
         county_name, rowid) %>% 
  mutate(across(.cols = everything() ,~ tolower(.x))) -> dirty_adresses
  


model_df %>% 
  filter(rowid == "AK1" | 
           rowid == "MS167" |
           rowid == "MS810" |
           rowid == "MA70" |
           rowid == "PA110" |
           rowid == "WV1" |
         rowid == "PA11365") %>% 
  dplyr::select(rowid, tidy_address = address) %>% 
  mutate(across(.cols = everything(), ~tolower(.x))) %>% 
  right_join(dirty_adresses) %>% 
  dplyr::select(-rowid) %>% 
  relocate(tidy_address, .after = county_name) %>% 
  kbl() %>% 
  kable_classic_2(full_width = F) %>% 
  kable_styling(bootstrap_options = c("striped"), html_font = "Georgia") %>% 
  row_spec(0, bold = T) %>% 
  column_spec(8, background = "#009E60") %>% 
  save_kable(file = "viz/data_cleaning.png", zoom = 10)




# missing data ------------------------------------------------------------

# read in handcoded states

list_of_handcoded <- list.files(path = "data/temp/excal_handcode/unique",
                             full.names = TRUE)

handcode_df <- list_of_handcoded %>%
  set_names() %>% 
  map_df(data.table::fread, colClasses = 'character') 


table(handcode_df$state)

table(model_df$state)

model_df %>% 
  tibble() %>% 
  group_by(state) %>% 
  summarise(count = n()) -> raw_count_df

handcode_df %>% 
  tibble() %>% 
  group_by(state) %>% 
  summarise(count_handcode = n()) %>% 
  right_join(raw_count_df) -> hand_and_all_df

hand_and_all_df %>% 
  mutate(count_handcode = replace_na(count_handcode, 0)) %>% 
  mutate(percentage_missing = count_handcode/count) -> hand_and_all_df


model_df %>% 
  group_by(state, licence) %>% 
  summarise(count_licence = n()) %>% 
  mutate(licence = replace_na(licence, "google")) %>% 
  mutate(licence = str_replace(licence, "Data © OpenStreetMap contributors, ODbL 1.0. https://osm.org/copyright", "osm")) %>% 
  pivot_wider(id_cols = state, names_from = licence, values_from = count_licence) %>% 
  left_join(hand_and_all_df) %>% 
  dplyr::select(state, count, osm, google, count_handcode, percentage_missing) -> geo_outcome_df

  
model_df %>% 
  group_by(state) %>% 
  summarise(na = sum(is.na(lat))) %>% 
  right_join(geo_outcome_df) %>% 
  relocate(na_after_handcode = na, .after = percentage_missing) %>% 
  mutate(osm = replace_na(osm, 0)) %>% 
  left_join(state_names_abbr, by = c("state" = "abbr")) %>% 
  relocate(full, .before = 1) %>% 
  dplyr::select(-state) %>% 
  mutate(percentage_missing = percentage_missing * 100,
         percentage_missing = as.numeric(format(round(percentage_missing, 2), nsmall = 2))) -> final_geo_table

final_geo_table %>% 
  slice_head(n = 17) %>% 
  gt() %>% 
  cols_label(full = md("**State**"),
             count = md("**Total Observations**"),
             osm = md("**OSM**"),
             google = md("**Google**"),
             count_handcode = md("**Missing after geocoding**"),
             percentage_missing = md("**Percentage missing**"),
             na_after_handcode = md("**Missing after handcoding**")) %>% 
  tab_footnote(footnote = md("Note: Sending addresses to OSM failed multiple times
                             due to connection problems, these states were therefore geocoded 
                             solely using Google"),
               locations = cells_body(
                 columns = osm,
                 rows = osm == 0
)) %>% 
  tab_options(table.font.names = "Georgia") %>% 
  gtsave(filename = "viz/summary_1.png", zoom = 10)
  
final_geo_table %>% 
  slice_tail(n = 18) %>% 
  gt() %>% 
  cols_label(full = md("**State**"),
             count = md("**Total Observations**"),
             osm = md("**OSM**"),
             google = md("**Google**"),
             count_handcode = md("**Missing after geocoding**"),
             percentage_missing = md("**Percentage missing**"),
             na_after_handcode = md("**Missing after handcoding**")) %>% 
  tab_footnote(footnote = md("Note: Sending addresses to OSM failed multiple times
                             due to connection problems, these states were therefore geocoded 
                             solely using Google"),
               locations = cells_body(
                 columns = osm,
                 rows = osm == 0
               )) %>% 
  tab_options(table.font.names = "Georgia") %>% 
  gtsave(filename = "viz/summary_2.png", zoom = 10)

    
sum(final_geo_table$count_handcode)
sum(final_geo_table$na_after_handcode)



# map missing state -------------------------------------------------------




final_geo_table %>% 
  dplyr::select(state = full, values = percentage_missing) %>% 
  mutate(values = values/100) -> us_map_missing

plot_usmap(data = us_map_missing, values = "values", labels = F) +
  scale_fill_continuous(low = "antiquewhite", high = "darkred", name = "Percentage of missing\nvalues after geocoding", label = scales::percent) +
  theme(legend.position = "right", text = element_text(family = "Times New Roman"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) -> missing_plot


ggsave(filename = "viz/missing_darkred.svg", plot = missing_plot)



# model counties ----------------------------------------------------------

brm_df %>% 
  dplyr::select(fips = geoid_data, values = shelby) %>% 
  mutate(fips = str_sub(fips, -5)) -> brm_fips

map_data("state") %>% 
  usmap_transform(input_names = c("long", "lat")) -> states


plot_usmap(data = brm_fips, values = "values") +
  theme(legend.position = "right", text = element_text(family = "Times New Roman"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  geom_polygon(data = states, aes(x = x,
                                  y = y,
                                  group = group),
               color = "black",
               alpha = 0, size = 0.75) +
  scale_fill_manual(values = c("cadetblue1" ,"cadetblue3", "aquamarine3", "aquamarine", 
                               "chartreuse1","chartreuse3", "cyan","cyan3", "darkgreen","slateblue4", 
                               "blueviolet","brown", "orangered2","chocolate1", "chocolate3","coral3", "coral1", "gold",
                               "darkgoldenrod1", "darkgoldenrod3", "hotpink", "hotpink3", "darkorchid1", "darkmagenta","deepskyblue", "deepskyblue3",
                               "dodgerblue", "dodgerblue3","darkslateblue", "mediumblue"), na.translate = F, name = "States in Sample") -> sample_counties

ggsave(filename = "viz/sample_counties.svg", plot = sample_counties)




# EAVS --------------------------------------------------------------------


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


brm_df %>% 
  dplyr::select(fips = geoid_data, values = abbr, polls_2016, total_pop) %>% 
  mutate(fips = str_sub(fips, -5)) %>% 
  inner_join(eavs_2016_count, by = c("fips" = "county_fips")) %>% 
  drop_na(polls_2016, count_polling) %>% 
  mutate(diff = polls_2016 - count_polling) %>% 
  ggplot(aes(log(count_polling), log(polls_2016))) + geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick") +
  labs(x = "Polling Places own geocoding (Logarithm)",
       y = "Polling Places EAVS (Logarithm)\n") +
  theme_bw() +
  theme(text = element_text(size = 16, family = "Times New Roman")) -> eavs_own_coding_plot

  
ggsave(filename = "viz/eavs_and_geo.svg", plot = eavs_own_coding_plot)



# share of foreigners -----------------------------------------------------



brm_df %>% 
  group_by(full)  %>% 
  ggplot(aes(x = prop_black_vote, y = full)) +
  geom_boxplot(width = 0.5, fill = "white", outlier.alpha = 0.3) +
  theme_minimal()  +
  labs(x = "Share of Black Voting Age Population",
       y = "States\n") +
  theme(text = element_text(size = 12, family = "Times New Roman")) -> black_summary_plot

ggsave(filename = "viz/black_box.svg", plot = black_summary_plot)


brm_df %>% 
  group_by(full)  %>% 
  ggplot(aes(x = prop_hisp_vote, y = full)) +
  geom_boxplot(width = 0.5, fill = "white", outlier.alpha = 0.3) +
  theme_minimal() +
  theme(text = element_text(size = 12, family = "Times New Roman")) +
  labs(x = "Share of Hispanic Voting Age Population",
       y = "States\n") -> hispanic_summary_plot

ggsave(filename = "viz/hispanic_box.svg", plot = hispanic_summary_plot)




# VRA coverage plot -------------------------------------------------------

plot_usmap(data = brm_fips, values = "values") +
  theme(legend.position = "right", text = element_text(family = "Times New Roman"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  geom_polygon(data = states, aes(x = x,
                                  y = y,
                                  group = group),
               color = "black",
               alpha = 0, size = 0.75) +
  scale_fill_brewer(palette = "Dark2", labels = c("Former VRA Coverage", "No Coverage"),
                    na.translate = F, name = "") -> vra_sample_map

ggsave(filename = "viz/vra_sample.svg", plot = vra_sample_map)





# PCVPP table -------------------------------------------------------------



brm_df %>% 
  filter(NAMELSAD == "Hunterdon County" |
         NAMELSAD == "Muhlenberg County") %>% 
  mutate(County = str_c(NAMELSAD, abbr, sep = ", ")) %>% 
  mutate(prop = prop/100) %>% 
  dplyr::select(County, voting_pop_12, polls_2012 ,Population_2016 = voting_pop, polls_2016, prop) %>% 
  gt() %>% 
  tab_options(table.font.names = "Georgia") %>% 
  cols_label(County = md("**County**"),
             voting_pop_12 = md("**CVAP 2012**"),
             polls_2012 = md("**Polling Places 2012**"),
             Population_2016 = md("**CVAP 2016**"),
             polls_2016 = md("**Polling Places 2016**"),
             prop = md("**PCVPP**")) %>% 
  cols_align(
    align = "center",
    columns = c(voting_pop_12, polls_2012, Population_2016, polls_2016)
  ) %>% 
  tab_footnote(
    footnote = "Citizen Voting Age Population",
    locations = cells_column_labels(
      columns = c(voting_pop_12, Population_2016)
    )
  ) %>% 
  fmt_percent(columns = prop, decimals = 2) %>% 
  gtsave(filename = "viz/PCVPP.png", zoom = 10)



# PCVPP values ------------------------------------------------------------

brm_df %>% 
  group_by(full)  %>% 
  ggplot(aes(x = prop, y = full)) +
  geom_point(alpha = 0.3) +
  theme_minimal()  +
  labs(x = "PCVPP",
       y = "States\n") +
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000)) +
  theme(text = element_text(size = 16, family = "Times New Roman")) -> PCVPP_plot

ggsave(filename = "viz/PCVPP.svg", plot = PCVPP_plot)



# former VRA coverage -----------------------------------------------------

usmap::us_map(regions = "counties")  %>% 
  mutate(shelby = FALSE) %>% 
  mutate(shelby = case_when(
    full %in% c("Alabama", "Alaska", "Arizona", "Georgia", "Louisiana", "Mississippi", "South Carolina", "Texas", "Virginia") ~ TRUE,
    TRUE ~ shelby)) %>% 
  mutate(shelby = case_when(
    full == "California" & county %in% c("Kings County", "Monterey County", "Yuba County") ~ TRUE,
    TRUE ~ shelby)
  ) %>% 
  mutate(shelby = case_when(
    full == "Florida" & county %in% c("Collier County", "Hardee County", "Hendry County" ,"Hillsborough County", "Monroe County") ~ TRUE,
    TRUE ~ shelby)
  ) %>% 
  mutate(shelby = case_when(
    full == "New York" & county %in% c("Bronx County", "Kings County", "New York County") ~ TRUE,
    TRUE ~ shelby)
  ) %>% 
  mutate(shelby = case_when(
    full == "North Carolina" & county %in% nort_carolina_shelby_vector ~ TRUE,
    TRUE ~ shelby)
  ) %>%
  mutate(shelby = case_when(
    full == "South Dakota" & county %in% c("Shannon County", "Todd County") ~ TRUE,
    TRUE ~ shelby)
  ) %>% 
  mutate(shelby = case_when(
    full == "Michigan" & county %in% c("Allegan County", "Saginaw County") ~ TRUE,
    TRUE ~ shelby)
  ) %>% 
  dplyr::select(fips, shelby) %>% 
  plot_usmap(data = ., values = "shelby") +
  theme(legend.position = "right", text = element_text(family = "Times New Roman"),
       legend.title = element_text(size = 16),
       legend.text = element_text(size = 16)) +
  geom_polygon(data = states, aes(x = x,
                                  y = y,
                                  group = group),
               color = "black",
               alpha = 0, size = 0.75) +
  scale_fill_brewer(palette = "Dark2", labels = c("No Coverage","Former VRA Coverage"),
                    na.translate = F, name = "") -> vra_coverage_whole

ggsave(filename = "viz/VRA_whole.svg", plot = vra_coverage_whole)
  


# posterior viz -----------------------------------------------------------

full_random_interaction %>% 
  gather_draws(c(b_prop_hisp_vote, b_prop_black_vote, `b_prop_hisp_vote:shelbyTRUE`, `b_shelbyTRUE:prop_black_vote`)) %>% 
  ggplot(aes(x = .value, y = .variable, fill = .variable)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi") +
  scale_fill_okabe_ito() +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Times New Roman" ,size = 16)) +
  scale_y_discrete(labels = c("Share Black", "Share Hisp.", "Hisp x VRA", "Black x VRA")) -> base_model_population_eff

ggsave(filename = "viz/racial_coeff.svg", base_model_population_eff)

full_random_interaction %>% 
  gather_draws(c(b_shelbyTRUE)) %>% 
  ggplot(aes(x = .value, y = .variable, fill = .variable)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi") +
  scale_fill_okabe_ito(order = 5:9) +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Times New Roman",size = 16)) +
  scale_y_discrete(labels = c("VRA")) -> base_model_VRA

ggsave(filename = "viz/VRA_coeff.svg", base_model_VRA)


full_random_interaction %>% 
  gather_draws(c(b_conv_voting)) %>% 
  ggplot(aes(x = .value, y = .variable, fill = .variable)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi") +
  scale_fill_okabe_ito(order = 6:9) +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Times New Roman", size = 16)) +
  scale_y_discrete(labels = c("Convenience Voting")) -> base_model_conv_voting

ggsave(filename = "viz/conv_voting_coeff.svg", base_model_conv_voting)


ggarrange(base_model_VRA, base_model_conv_voting, ncol = 1, align = "v") -> arragend_coeff

ggsave(filename = "viz/arranged_coef.svg", arragend_coeff)


full_random_interaction %>% 
  gather_draws(c(b_prop_hisp_vote, b_prop_black_vote, `b_prop_hisp_vote:shelbyTRUE`, `b_shelbyTRUE:prop_black_vote`)) %>% 
  ggplot(aes(x = .value, y = .variable, fill = .variable)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi") +
  scale_fill_okabe_ito() +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Times New Roman" ,size = 16)) +
  scale_y_discrete(labels = c("Share Black", "Share Hisp.", "Hisp x VRA", "Black x VRA")) -> base_model_population_eff

ggsave(filename = "viz/racial_coeff.svg", base_model_population_eff)

full_random_interaction %>% 
  gather_draws(c(b_shelbyTRUE)) %>% 
  ggplot(aes(x = .value, y = .variable, fill = .variable)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi") +
  scale_fill_okabe_ito(order = 5:9) +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Times New Roman",size = 16)) +
  scale_y_discrete(labels = c("VRA")) -> base_model_VRA

ggsave(filename = "viz/VRA_coeff.svg", base_model_VRA)


full_random_interaction %>% 
  gather_draws(c(b_conv_voting)) %>% 
  ggplot(aes(x = .value, y = .variable, fill = .variable)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi") +
  scale_fill_okabe_ito(order = 6:9) +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Times New Roman", size = 16)) +
  scale_y_discrete(labels = c("Convenience Voting")) -> base_model_conv_voting

ggsave(filename = "viz/conv_voting_coeff.svg", base_model_conv_voting)


ggarrange(base_model_VRA, base_model_conv_voting, ncol = 1, align = "v") -> arragend_coeff

ggsave(filename = "viz/arranged_coef.svg", arragend_coeff)





full_random_interaction %>% 
  emtrends(~ full,
           var = "prop_hisp_vote",
           epred = TRUE, re_formula = NULL) %>% 
  gather_emmeans_draws() %>% 
  filter(full == "Arizona") %>% 
  ggplot(aes(x = .value)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi", slab_fill = "#E69F00") +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "Coefficient Share of Hispanic Population",
       y = "Density") +
  theme(text = element_text(family = "Times New Roman", size = 16)) -> random_effect_arizona

ggsave(filename = "viz/random_effect_arizona.svg", random_effect_arizona)





coef(full_random_interaction)$full %>% 
  as_tibble(rownames = "state") %>% 
  rename(q_2_5 = 4) %>% 
  filter(q_2_5 > 0) %>% 
  arrange(desc(Estimate.Intercept)) %>% 
  dplyr::select(State = state, Intercept = 2, error_intercept = 3, 4,5)

final_geo_table %>% 
  slice_tail(n = 18) %>% 
  gt() %>% 
  cols_label(full = md("**State**"),
             count = md("**Total Observations**"),
             osm = md("**OSM**"),
             google = md("**Google**"),
             count_handcode = md("**Missing after geocoding**"),
             percentage_missing = md("**Percentage missing**"),
             na_after_handcode = md("**Missing after handcoding**")) %>% 
  tab_footnote(footnote = md("Note: Sending addresses to OSM failed multiple times
                             due to connection problems, these states were therefore geocoded 
                             solely using Google"),
               locations = cells_body(
                 columns = osm,
                 rows = osm == 0
               )) %>% 
  tab_options(table.font.names = "Georgia") %>% 
  gtsave(filename = "viz/summary_2.png", zoom = 10)



full_random_interaction %>% 
  spread_draws(b_Intercept, r_full[state,coeff]) %>% 
  filter(coeff == "Intercept") %>% 
  filter(state %in% c("Texas", "Ohio", "North.Dakota", "Kentucky")) %>% 
  mutate(mu = b_Intercept + r_full,
         state = str_replace(state, "\\.", " ")) %>% 
  ggplot(aes(x = mu, y = reorder(state, mu), fill = state)) +
  geom_vline(xintercept = fixef(full_random_interaction)[1, 1], color = "black", size = 1) +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi") +
  scale_fill_okabe_ito() + 
  theme_minimal() +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Times New Roman", size = 16)) -> random_intercepts


ggsave(filename = "viz/random_intercept.svg", plot = random_intercepts)



full_random_interaction %>% 
  spread_draws(b_Intercept, r_full[state,coeff]) %>% 
  filter(coeff == "Intercept") %>% 
  filter(state %in% state.name[1:25]) %>% 
  mutate(mu = b_Intercept + r_full,
         state = str_replace(state, "\\.", " ")) %>% 
  ggplot(aes(x = mu, y = reorder(state, mu), fill = state)) +
  geom_vline(xintercept = fixef(full_random_interaction)[1, 1], color = "black", size = 1) +
  geom_vline(xintercept = 0, color = "red", size = 0.5, linetype = "dotdash") +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi", fill = "grey") +
  theme_minimal() +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Times New Roman", size = 16)) -> random_intercepts_full_first_half


ggsave(filename = "viz/random_intercept_full_25.svg", plot = random_intercepts_full_first_half)


full_random_interaction %>% 
  spread_draws(b_Intercept, r_full[state,coeff]) %>% 
  filter(coeff == "Intercept") %>% 
  filter(state %in% state.name[26:50]) %>% 
  mutate(mu = b_Intercept + r_full,
         state = str_replace(state, "\\.", " ")) %>% 
  ggplot(aes(x = mu, y = reorder(state, mu), fill = state)) +
  geom_vline(xintercept = fixef(full_random_interaction)[1, 1], color = "black", size = 1) +
  geom_vline(xintercept = 0, color = "red", size = 0.5, linetype = "dotdash") +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi", fill = "grey") +
  theme_minimal() +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Times New Roman", size = 16)) -> random_intercepts_full_second_half


ggsave(filename = "viz/random_intercept_full_50.svg", plot = random_intercepts_full_second_half)


dummy_model %>% 
  gather_draws(c(b_pre_clearence_detaillarge_black, b_pre_clearence_detaillarge_hisp, 
                 b_pre_clearence_detailpreMclearance_white)) %>% 
  ggplot(aes(x = .value, y = .variable, fill = .variable)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(.width = c(0.8, 0.95), alpha = 0.8, point_interval = "median_hdi") +
  scale_fill_okabe_ito(order = 6:9) +
  guides(fill = "none") +
  theme_minimal() +
  labs(x = "",
       y = "") +
  theme(text = element_text(family = "Times New Roman", size = 16)) +
  scale_y_discrete(labels = c("> 30% black voters with former preclearance",
                              "> 15% hispanic voters with former preclearance",
                              "Former preclearance without large minority")) -> dummy_graph


ggsave(filename = "viz/dummygraph.svg", plot = dummy_graph)
