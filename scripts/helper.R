#### helper functions ####



combine_handcode <- function(state){
  path <- str_c("data/temp/", state, "_osm_google.RDS")
  
  auto_code_df <- readRDS(file = path)
  
  if (is.data.frame(auto_code_df)) {
    long_auto_code <- auto_code_df
  } else {
  
  purrr::map(.x = auto_code_df, ~ {
    .x %>% dplyr::mutate(across(.cols = everything(), ~ as.character(.)))
  }) %>% bind_rows() -> long_auto_code
  }
  
  path_flagged <- str_c("data/temp/cleaned_geocodes/flagged_", state, ".csv")
  
  read_delim(file = path_flagged, delim = ";", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = "c")) %>% 
    separate(col = latlong, into = c("lat", "long"), sep = ",") %>% 
    mutate(long = str_squish(long)) -> df_flagged
  
  long_auto_code %>% 
    anti_join(df_flagged, by = c("rowid" = "rowid")) %>% 
    bind_rows(df_flagged) -> df_final
  
  dest_path <- str_c( "data/temp/finished_df/", state, ".csv")
  
  write_csv(df_final, file = dest_path)
  
}



geo_check <- function(df1, df2, state_name){
  df1 %>% 
    filter(state == {{state_name}}) -> inter_1
  
  df2 %>% 
    filter(STUSPS == {{state_name}}) -> inter_2
  
  check_df <- inter_1[!lengths(st_intersects(inter_1, inter_2)), ]
  
  check_df %>% 
    st_drop_geometry() -> check_df
  
  return(check_df)
  
}
