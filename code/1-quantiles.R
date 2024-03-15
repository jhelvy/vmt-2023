source(here::here('code', '0-functions.R'))

# Function to compute detailed quantiles

get_quantiles_detailed <- function(df, var) {
    result <- df %>% 
        summarise(
            var01 = fquantile({{var}}, 0.01),
            var02 = fquantile({{var}}, 0.02),
            var03 = fquantile({{var}}, 0.03),
            var04 = fquantile({{var}}, 0.04),
            var05 = fquantile({{var}}, 0.05),
            var06 = fquantile({{var}}, 0.06),
            var07 = fquantile({{var}}, 0.07),
            var08 = fquantile({{var}}, 0.08),
            var09 = fquantile({{var}}, 0.09),
            var10 = fquantile({{var}}, 0.10),
            var11 = fquantile({{var}}, 0.11),
            var12 = fquantile({{var}}, 0.12),
            var13 = fquantile({{var}}, 0.13),
            var14 = fquantile({{var}}, 0.14),
            var15 = fquantile({{var}}, 0.15),
            var16 = fquantile({{var}}, 0.16),
            var17 = fquantile({{var}}, 0.17),
            var18 = fquantile({{var}}, 0.18),
            var19 = fquantile({{var}}, 0.19),
            var20 = fquantile({{var}}, 0.20),
            var21 = fquantile({{var}}, 0.21),
            var22 = fquantile({{var}}, 0.22),
            var23 = fquantile({{var}}, 0.23),
            var24 = fquantile({{var}}, 0.24),
            var25 = fquantile({{var}}, 0.25),
            var26 = fquantile({{var}}, 0.26),
            var27 = fquantile({{var}}, 0.27),
            var28 = fquantile({{var}}, 0.28),
            var29 = fquantile({{var}}, 0.29),
            var30 = fquantile({{var}}, 0.30),
            var31 = fquantile({{var}}, 0.31),
            var32 = fquantile({{var}}, 0.32),
            var33 = fquantile({{var}}, 0.33),
            var34 = fquantile({{var}}, 0.34),
            var35 = fquantile({{var}}, 0.35),
            var36 = fquantile({{var}}, 0.36),
            var37 = fquantile({{var}}, 0.37),
            var38 = fquantile({{var}}, 0.38),
            var39 = fquantile({{var}}, 0.39),
            var40 = fquantile({{var}}, 0.40),
            var41 = fquantile({{var}}, 0.41),
            var42 = fquantile({{var}}, 0.42),
            var43 = fquantile({{var}}, 0.43),
            var44 = fquantile({{var}}, 0.44),
            var45 = fquantile({{var}}, 0.45),
            var46 = fquantile({{var}}, 0.46),
            var47 = fquantile({{var}}, 0.47),
            var48 = fquantile({{var}}, 0.48),
            var49 = fquantile({{var}}, 0.49),
            var50 = fquantile({{var}}, 0.50),
            var51 = fquantile({{var}}, 0.51),
            var52 = fquantile({{var}}, 0.52),
            var53 = fquantile({{var}}, 0.53),
            var54 = fquantile({{var}}, 0.54),
            var55 = fquantile({{var}}, 0.55),
            var56 = fquantile({{var}}, 0.56),
            var57 = fquantile({{var}}, 0.57),
            var58 = fquantile({{var}}, 0.58),
            var59 = fquantile({{var}}, 0.59),
            var60 = fquantile({{var}}, 0.60),
            var61 = fquantile({{var}}, 0.61),
            var62 = fquantile({{var}}, 0.62),
            var63 = fquantile({{var}}, 0.63),
            var64 = fquantile({{var}}, 0.64),
            var65 = fquantile({{var}}, 0.65),
            var66 = fquantile({{var}}, 0.66),
            var67 = fquantile({{var}}, 0.67),
            var68 = fquantile({{var}}, 0.68),
            var69 = fquantile({{var}}, 0.69),
            var70 = fquantile({{var}}, 0.70),
            var71 = fquantile({{var}}, 0.71),
            var72 = fquantile({{var}}, 0.72),
            var73 = fquantile({{var}}, 0.73),
            var74 = fquantile({{var}}, 0.74),
            var75 = fquantile({{var}}, 0.75),
            var76 = fquantile({{var}}, 0.76),
            var77 = fquantile({{var}}, 0.77),
            var78 = fquantile({{var}}, 0.78),
            var79 = fquantile({{var}}, 0.79),
            var80 = fquantile({{var}}, 0.80),
            var81 = fquantile({{var}}, 0.81),
            var82 = fquantile({{var}}, 0.82),
            var83 = fquantile({{var}}, 0.83),
            var84 = fquantile({{var}}, 0.84),
            var85 = fquantile({{var}}, 0.85),
            var86 = fquantile({{var}}, 0.86),
            var87 = fquantile({{var}}, 0.87),
            var88 = fquantile({{var}}, 0.88),
            var89 = fquantile({{var}}, 0.89),
            var90 = fquantile({{var}}, 0.90),
            var91 = fquantile({{var}}, 0.91),
            var92 = fquantile({{var}}, 0.92),
            var93 = fquantile({{var}}, 0.93),
            var94 = fquantile({{var}}, 0.94),
            var95 = fquantile({{var}}, 0.95),
            var96 = fquantile({{var}}, 0.96),
            var97 = fquantile({{var}}, 0.97),
            var98 = fquantile({{var}}, 0.98),
            var99 = fquantile({{var}}, 0.99)
        ) %>% 
        pivot_longer(
            names_to = 'quantile', 
            values_to = 'val',
            cols = starts_with('var')
        ) %>% 
        separate(quantile, into = c('drop', 'quantile'), sep = 'var') %>% 
        select(-drop) %>% 
        mutate(quantile = as.numeric(quantile))
    return(result)
}

# Open miles arrow dataset

ds <- open_dataset(PATH_DB)

# Basic quantiles by age ----

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





# Detailed quantiles by age ----

quantiles_miles_full <- ds %>%
    filter(age_years <= 9) %>%
    collect() %>%
    group_by(age_months, powertrain, vehicle_type) %>% 
    get_quantiles_detailed(miles) %>% 
    rename(miles = val)
quantiles_miles_full_bev <- ds %>%
    filter(age_years <= 9) %>%
    filter(powertrain == "bev") %>% 
    collect() %>% 
    mutate(powertrain = ifelse(make == "tesla", 'bev_tesla', 'bev_non_tesla')) %>%
    group_by(age_months, powertrain, vehicle_type) %>% 
    get_quantiles_detailed(miles) %>% 
    rename(miles = val)

# Save

quantiles_miles_full <- rbind(quantiles_miles_full, quantiles_miles_full_bev) %>% 
    mutate(
        age_years = age_months / 12, 
        powertrain = ifelse(powertrain == 'bev', 'bev_all', powertrain)
    ) 
write_csv(quantiles_miles_full, here::here('data', 'quantiles_miles_full.csv'))

# Preview CDFs

plot <- quantiles_miles_full %>% 
    filter(vehicle_type == 'car') %>% 
    mutate(miles = miles / 1000) %>% 
    ggplot() + 
    geom_line(aes(x = age_years, y = miles, group = quantile, color = quantile)) +
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

ggsave(here::here('figs', 'quantiles_miles.png'), plot, width = 9, height = 6)



# Quantiles of DVMT ----

# First, compute separately by vehicle type and powertrain 

quantiles_dvmt <- ds %>%
    mutate(dvmt = miles / age_days) %>% 
    select(powertrain, vehicle_type, dvmt) %>% 
    collect() %>% 
    group_by(powertrain, vehicle_type) %>% 
    get_quantiles_detailed(dvmt) %>% 
    rename(dvmt = val)
quantiles_dvmt_bev <- ds %>%
    filter(powertrain == "bev") %>% 
    mutate(dvmt = miles / age_days) %>% 
    select(powertrain, make, vehicle_type, dvmt) %>% 
    collect() %>% 
    mutate(powertrain = ifelse(make == "tesla", 'bev_tesla', 'bev_non_tesla')) %>%
    group_by(powertrain, vehicle_type) %>% 
    get_quantiles_detailed(dvmt) %>% 
    rename(dvmt = val)

# Now compute for all vehicles

quantiles_dvmt_all <- ds %>%
    mutate(dvmt = miles / age_days) %>% 
    select(dvmt) %>% 
    collect() %>% 
    get_quantiles_detailed(dvmt) %>% 
    rename(dvmt = val) %>% 
    mutate(
        powertrain = 'all', 
        vehicle_type = 'all'
    )

# Combine and save

quantiles_dvmt <- rbind(quantiles_dvmt_all, quantiles_dvmt, quantiles_dvmt_bev) %>% 
    mutate(powertrain = ifelse(powertrain == 'bev', 'bev_all', powertrain)) 
write_csv(quantiles_dvmt, here::here('data', 'quantiles_dvmt.csv'))

# Preview CDFs

# Compare powertrains within vehicle type

quantiles_dvmt %>% 
    filter(powertrain != 'all') %>% 
    ggplot() + 
    geom_line(aes(x = dvmt, y = quantile, color = powertrain)) + 
    geom_line(
        data = quantiles_dvmt %>% 
            filter(powertrain == 'all'), 
        aes()
    ) + 
    facet_wrap(vars(vehicle_type)) + 
    plot_theme() + 
    theme(legend.position = 'right') + 
    labs(
        x = 'DVMT', 
        y = '%'
    )

ggsave(here::here('figs', 'quantiles_dvmt_powertrain.png'), width = 10, height = 3.5)

# Compare vehicle type within powertrain

quantiles_dvmt %>% 
    ggplot() + 
    geom_line(aes(x = dvmt, y = quantile, color = vehicle_type)) + 
    facet_wrap(vars(powertrain)) + 
    plot_theme() + 
    theme(legend.position = 'right') + 
    labs(
        x = 'DVMT', 
        y = '%'
    )

ggsave(here::here('figs', 'quantiles_dvmt_vehicle_type.png'), width = 10, height = 6)





















