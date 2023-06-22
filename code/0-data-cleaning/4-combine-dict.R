source(here::here('code', '0-functions.R'))

# Read in vehicle specs dictionaries

dict_fe <- read_parquet(
    here::here('data', 'dict_fe.parquet')) %>% 
    rename(range_fe = range) %>%
    select(-vehicle_key, -matching)
ranges <- read_csv(
    here::here('data', 'pev-ranges.csv')) %>%
    filter(!is.na(range)) %>% 
    select(-rangeCity, -rangeHwy)
dict_carsheet <- read_parquet(
    here::here('data', 'dict_carsheet.parquet')) %>% 
    select(-vehicle_key, -matching, -class) %>% 
    rename(mpg_carsheet = mpg, range_carsheet = range_mi)
dict_vehicle <- read_parquet(here::here('data', 'dict_raw.parquet')) 

# Join on missing vars from other dictionaries

dict <- dict_vehicle %>% 
    left_join(dict_fe, 
        by = c('powertrain', 'year', 'make', 'model', 'trim')) %>% 
    left_join(ranges, 
        by = c('powertrain', 'vehicle_type', 'make', 'model', 'trim', 'year')) %>% 
    left_join(dict_carsheet,
        by = c('powertrain', 'year', 'make', 'model', 'trim')) %>% 
    mutate(
        # Remove range values for non-PEVs
        range = ifelse(
            ! powertrain %in% c('bev', 'phev'), NA, range),
        range_fe = ifelse(
            ! powertrain %in% c('bev', 'phev'), NA, range_fe),
        range_carsheet = ifelse(
            ! powertrain %in% c('bev', 'phev'), NA, range_carsheet),
        # Fill in missing ranges from AFDC with carsheet
        range_fe = ifelse(
            is.na(range_fe), range_carsheet, range_fe),
        range = ifelse(is.na(range), range_fe, range),
        # Fill in missing mpgs from AFDC with carsheet for HEVs and CVs
        mpg = ifelse(
            (powertrain %in% c('hybrid', 'conventional')) & is.na(mpg), 
            mpg_carsheet, mpg)
    ) %>% 
    select(
        -mpg_carsheet, -range_fe, -range_carsheet,
        -city_mpg, -highway_mpg
    ) %>% 
    arrange(powertrain, vehicle_type, year, make, model, trim, class) %>%  
    group_by(powertrain, vehicle_type, year, make, model, trim, class) %>%  
    summarise(
        msrp = mean(msrp, na.rm = TRUE),
        range = mean(range, na.rm = TRUE),
        mpg = mean(mpg, na.rm = TRUE), 
        mpge = mean(mpge, na.rm = TRUE), 
        kwhp100mi = mean(kwhp100mi, na.rm = TRUE),
        gal100mi = mean(gal100mi, na.rm = TRUE),
        phev_uf = mean(phev_uf, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
        msrp = ifelse(is.nan(msrp), NA, msrp),
        range = ifelse(is.nan(range), NA, range),
        mpg = ifelse(is.nan(mpg), NA, mpg),
        mpge = ifelse(is.nan(mpge), NA, mpge),
        kwhp100mi = ifelse(is.nan(kwhp100mi), NA, kwhp100mi),
        gal100mi = ifelse(is.nan(gal100mi), NA, gal100mi),
        phev_uf = ifelse(is.nan(phev_uf), NA, phev_uf)
    ) %>% 
    group_by(powertrain, vehicle_type, year, make, model) %>%
    mutate(
        msrp_med = median(msrp, na.rm = TRUE),
        range_med = median(range, na.rm = TRUE),
        mpg_med = median(mpg, na.rm = TRUE), 
        mpge_med = median(mpge, na.rm = TRUE), 
        kwhp100mi_med = median(kwhp100mi, na.rm = TRUE),
        gal100mi_med = median(gal100mi, na.rm = TRUE),
        phev_uf_med = median(phev_uf, na.rm = TRUE)
    ) %>% 
    mutate(
        msrp = ifelse(
            trim == 'unknown' & is.na(msrp), msrp_med, msrp),
        range = ifelse(
            trim == 'unknown' & is.na(range), range_med, range),
        mpg = ifelse(
            trim == 'unknown' & is.na(mpg), mpg_med, mpg),
        mpge = ifelse(
            trim == 'unknown' & is.na(mpge), mpge_med, mpge),
        kwhp100mi = ifelse(
            trim == 'unknown' & is.na(kwhp100mi), kwhp100mi_med, kwhp100mi),
        gal100mi = ifelse(
            trim == 'unknown' & is.na(gal100mi), gal100mi_med, gal100mi),
        phev_uf = ifelse(
            trim == 'unknown' & is.na(phev_uf), phev_uf_med, phev_uf)
    ) %>% 
    as.data.table() %>% 
    distinct() %>% 
    select(-ends_with("_med"))

dim(dict)
glimpse(dict)

write_parquet(dict, here::here('data', 'dict_final.parquet'))
