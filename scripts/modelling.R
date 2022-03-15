### model ###

brm_df <- read_csv("data/model_df/12_16.csv")


options(mc.cores = parallel::detectCores())


# create balanced df ------------------------------------------------------


brm_df %>% 
  slice_max(prop, prop = 0.005) -> max_df

brm_df %>% 
  slice_min(prop, prop = 0.005) -> min_df

bind_rows(max_df, min_df) -> extreme_df


brm_df %>% 
  anti_join(extreme_df, by = c("geoid_data" = "geoid_data")) -> balance_df

balance_df %>% 
  mutate(prop_black_vote = prop_black_vote*100,
         prop_hisp_vote = prop_hisp_vote*100) -> balance_df



# create factorial minority variable --------------------------------------


extreme_mino <- balance_df %>% 
  mutate(pre_clearence_detail = case_when(
    shelby == FALSE ~ "non-preclearance",
    shelby == TRUE & prop_black_vote >= 30 ~ "large_black",
    shelby == TRUE & prop_hisp_vote >= 15 ~ "large_hisp",
    TRUE ~ "pre-clearance_white"
  )) %>% 
  mutate(pre_clearence_detail = as.factor(pre_clearence_detail)) %>% 
  mutate(pre_clearence_detail = relevel(pre_clearence_detail, ref = 3))

# create brms models ------------------------------------------------------


brm(prop ~ 1 + pre_clearence_detail + conv_voting + (1|full),
    data = extreme_mino,
    family = "gaussian", control = list(adapt_delta = 0.99),
    chains = 4, iter = 10000, warmup = 1000,
    cores = 4) -> dummy_model



brm(formula = prop ~ 1 + prop_hisp_vote*shelby + prop_black_vote*shelby + conv_voting  + (1 + prop_hisp_vote + prop_black_vote | full),
    data = balance_df, control = list(adapt_delta = 0.99),
    family = "gaussian",
    chains = 4, iter = 10000, warmup = 1000,
    cores = 4) -> full_random_interaction


stancode(full_random_interaction)





























