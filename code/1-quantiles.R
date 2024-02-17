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

write_parquet(quantiles, here::here('data', 'quantiles.parquet'))

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

write_parquet(quantiles_bev, here::here('data', 'quantiles_bev.parquet'))

# Compute detailed quantiles - full PDF in increments of 5%

quantiles_full <- ds %>%
    filter(age_years <= 9) %>%
    collect() %>%
    group_by(age_months, powertrain, vehicle_type) %>% 
    summarise(
        miles01 = fquantile(miles, 0.01),
        miles05 = fquantile(miles, 0.05),
        miles10 = fquantile(miles, 0.10),
        miles15 = fquantile(miles, 0.15),
        miles20 = fquantile(miles, 0.20),
        miles25 = fquantile(miles, 0.25),
        miles30 = fquantile(miles, 0.30),
        miles35 = fquantile(miles, 0.35),
        miles40 = fquantile(miles, 0.40),
        miles45 = fquantile(miles, 0.45),
        miles50 = fquantile(miles, 0.50),
        miles55 = fquantile(miles, 0.55),
        miles60 = fquantile(miles, 0.60),
        miles65 = fquantile(miles, 0.65),
        miles70 = fquantile(miles, 0.70),
        miles75 = fquantile(miles, 0.75),
        miles80 = fquantile(miles, 0.80),
        miles85 = fquantile(miles, 0.85),
        miles90 = fquantile(miles, 0.90),
        miles95 = fquantile(miles, 0.95),
        miles99 = fquantile(miles, 0.99)
    ) %>% 
    pivot_longer(
        names_to = 'quantile', 
        values_to = 'mileage',
        cols = starts_with('miles')
    ) %>% 
    separate(quantile, into = c('drop', 'quantile'), sep = 'miles') %>% 
    select(-drop) %>% 
    mutate(quantile = as.numeric(quantile))
quantiles_full_bev <- ds %>%
    filter(age_years <= 9) %>%
    filter(powertrain == "bev") %>% 
    collect() %>% 
    mutate(powertrain = ifelse(make == "tesla", 'bev_tesla', 'bev_non_tesla')) %>%
    group_by(age_months, powertrain, vehicle_type) %>% 
    summarise(
        miles01 = fquantile(miles, 0.01),
        miles05 = fquantile(miles, 0.05),
        miles10 = fquantile(miles, 0.10),
        miles15 = fquantile(miles, 0.15),
        miles20 = fquantile(miles, 0.20),
        miles25 = fquantile(miles, 0.25),
        miles30 = fquantile(miles, 0.30),
        miles35 = fquantile(miles, 0.35),
        miles40 = fquantile(miles, 0.40),
        miles45 = fquantile(miles, 0.45),
        miles50 = fquantile(miles, 0.50),
        miles55 = fquantile(miles, 0.55),
        miles60 = fquantile(miles, 0.60),
        miles65 = fquantile(miles, 0.65),
        miles70 = fquantile(miles, 0.70),
        miles75 = fquantile(miles, 0.75),
        miles80 = fquantile(miles, 0.80),
        miles85 = fquantile(miles, 0.85),
        miles90 = fquantile(miles, 0.90),
        miles95 = fquantile(miles, 0.95),
        miles99 = fquantile(miles, 0.99)
    ) %>% 
    pivot_longer(
        names_to = 'quantile', 
        values_to = 'mileage',
        cols = starts_with('miles')
    ) %>% 
    separate(quantile, into = c('drop', 'quantile'), sep = 'miles') %>% 
    select(-drop) %>% 
    mutate(quantile = as.numeric(quantile))

# Save

quantiles_full <- rbind(quantiles_full, quantiles_full_bev) %>% 
    mutate(
        age_years = age_months / 12, 
        powertrain = ifelse(powertrian == 'bev', 'bev_all', powertrain)
    ) 
write_csv(quantiles_full, here::here('data', 'quantiles_full.csv'))

# Preview CDFs

plot <- quantiles_full %>% 
    filter(vehicle_type == 'car') %>% 
    mutate(mileage = mileage / 1000) %>% 
    ggplot() + 
    geom_line(aes(x = age_years, y = mileage, group = quantile, color = quantile)) +
    facet_wrap(vars(powertrain)) +
    plot_theme() + 
    theme(legend.position = 'bottom') + 
    scale_color_distiller(
        'Quantile', palette = 'Spectral', 
         breaks = c(1, seq(10, 90, 10), 99),
        limits = c(1, 99 )
    ) + 
    guides(color = guide_colourbar(
        barwidth = 10, barheight = 1, label.position = "bottom")
    ) + 
    labs(
        x = 'Age (Years)', 
        y = 'Mileage (1,000)'
    )

ggsave(here::here('figs', 'quantiles_full.png'), plot, width = 9, height = 6)

