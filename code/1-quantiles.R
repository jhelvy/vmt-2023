source(here::here('code', '0-functions.R'))

# Open miles arrow dataset

ds <- open_dataset(PATH_DB)

# Compute quantiles

quantiles <- ds %>%
    filter(age_years <= 9) %>%
    collect() %>%
    group_by(age_months, powertrain, vehicle_type) %>% 
    summarise(
        miles25 = fquantile(miles, 0.25),
        miles50 = fquantile(miles, 0.5),
        miles75 = fquantile(miles, 0.75)
    )
    
# Save

write_csv(quantiles, here::here('data', 'quantiles.csv'))

# Separately compute the quantiles for BEVs only, separating out Tesla

quantiles_bev <- ds %>%
    filter(age_years <= 9) %>%
    filter(powertrain == "bev", vehicle_type == 'car') %>% 
    collect() %>% 
    mutate(tesla = ifelse(make == "tesla", 1, 0)) %>% 
    group_by(age_months, tesla) %>% 
    summarise(
        miles25 = fquantile(miles, 0.25),
        miles50 = fquantile(miles, 0.5),
        miles75 = fquantile(miles, 0.75)
    )

# Save

write_csv(quantiles_bev, here::here('data', 'quantiles_bev.csv'))

