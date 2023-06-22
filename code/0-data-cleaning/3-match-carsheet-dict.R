source(here::here('code', '0-functions.R'))

# Get the unique set of year, make, model, trim from the vehicle dictionary
dt_ymmtp <- read_parquet(here::here('data', 'dict_raw.parquet')) %>% 
    filter(!is.na(year), !is.na(make), !is.na(model), !is.na(powertrain)) %>% 
    select(powertrain, year, make, model, trim) %>% 
    mutate(trim = ifelse(trim == "unknown", "", trim)) %>% 
    distinct(powertrain, year, make, model, trim) %>% 
    arrange(powertrain, year, make, model, trim) %>% 
    mutate(
        model = str_trim(model),
        model_trim = paste(model, trim), 
        model_trim = str_trim(model_trim)
    )

# Read in carsheet.parquet from carsheet.io
v <- read_parquet(here::here('data', 'carsheet.parquet')) %>%
    select(
        year, make, model, trim,
        msrp, body_size, class = body_style,
        range_mi, electric_range_mi,
        mpg = combined_fuel_economy_mpg,
        fuel_type
    ) %>% 
    mutate(
        make  = str_to_lower(make),
        model = str_to_lower(model),
        class = str_to_lower(class),
        body_size = str_to_lower(body_size),
        fuel_type = str_to_lower(fuel_type),
        # Drop pickup
        model = str_trim(str_replace_all(model, " pickup", ''))
    ) %>% 
    # Consolidate powertrains
    mutate(
        powertrain = ifelse(
            fuel_type == "gasoline", "conventional", ifelse(
            fuel_type == "electric", "bev", ifelse(
            fuel_type == "diesel", "conventional", ifelse(
            fuel_type == "fuel cell", "fcev", ifelse(
            str_detect(model, "plug-in hybrid"), "phev", ifelse(
            model %in% c("prius plug-in", "prius prime"), "phev", ifelse(
            model == "volt", "phev", ifelse(
            fuel_type == "", "unknown", ifelse(
            fuel_type == "flex fuel", "conventional", fuel_type
        )))))))))
    ) %>% 
    # Consolidate classes
    mutate(
        class = ifelse(
            class %in% c("suv", "convertible suv"), "suv", ifelse(
            class %in% c(
                "passenger minivan", "cargo minivan", "cargo van", 
                "passenger van"
                ), "van", ifelse(
            (class %in% c("sedan", "coupe")) & (body_size == 'compact'), 
                'small', ifelse(
            (class %in% c("sedan", "coupe")) & (body_size == 'midsize'), 
                'midsize', ifelse(
            (class %in% c("sedan", "coupe")) & (body_size == 'large'), 
                'large',
            class
        )))))
    ) %>% 
    arrange(year, make, model, powertrain, class) %>%  
    group_by(year, make, model, powertrain, class) %>% 
    summarise(
        msrp = mean(msrp, na.rm = TRUE),
        range_mi = mean(range_mi, na.rm = TRUE),
        electric_range_mi = mean(electric_range_mi, na.rm = TRUE),
        mpg = mean(mpg, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
        msrp = ifelse(is.nan(msrp), NA, msrp),
        range_mi = ifelse(is.nan(range_mi), NA, range_mi),
        electric_range_mi = ifelse(is.nan(electric_range_mi), NA, electric_range_mi),
        mpg = ifelse(is.nan(mpg), NA, mpg), 
        range_mi = ifelse(
            powertrain %in% c('bev', 'phev'), electric_range_mi, range_mi)
    ) %>% 
    filter(powertrain %in% c('conventional', 'hybrid', 'bev', 'phev')) %>%
    select(-electric_range_mi) %>% 
    as.data.table()
    

# First, need to align the makes in the v dataset to dt_ymmt
dt_ymmtp %>% 
    distinct(make) %>% 
    left_join(
        v %>% 
            distinct(make) %>% 
            mutate(make_v = make), 
        by = "make"
    ) %>% 
    filter(is.na(make_v))

# Now the v data.table only contains matching years and makes with dt_ymmt

# Make match keys 
v <- v %>% 
    mutate(key = as.character(row_number()))

# Set v_powertrain based on fuelType
v_cv <- filter(v, powertrain == 'conventional')
v_hev <- filter(v, powertrain == 'hybrid')
v_phev <- filter(v, powertrain == 'phev')
v_bev <- filter(v, powertrain == 'bev')

# Now for each ymm, try to find a match with vehicle and keep the match key
dt_ymm <- distinct(dt_ymmtp, year, make, model, powertrain)
dt_ymm_cv <- dt_ymm %>% filter(powertrain == 'conventional')
dt_ymm_hev <- dt_ymm %>% filter(powertrain == 'hybrid')
dt_ymm_phev <- dt_ymm %>% filter(powertrain == 'phev')
dt_ymm_bev <- dt_ymm %>% filter(powertrain == 'bev')

# BEVs ----

results_bev <- list()
tictoc::tic()
index <- 1
for (i in 1:nrow(dt_ymm_bev)) {

    # Get the ymmt's for matching and the potential matches from v 
    # i = 100
    ymm <- dt_ymm_bev[i,]
    ymmt <- dt_ymmtp %>% 
        filter(
            powertrain == 'bev',
            year == ymm$year, 
            make == ymm$make, 
            model == ymm$model
        )
    matches <- v_bev %>% 
        filter(year == ymm$year, make == ymm$make) %>% 
        select(-year, -make, -powertrain)
    
    # Skip if no matches
    if (nrow(matches) == 0) { next }
    
    results_bev[[index]] <- get_match(ymmt, matches)
    index <- index + 1
}
tictoc::toc()
# 11.23 sec elapsed

dict_bev <- do.call(rbind, results_bev) %>%
    select(-model_trim) %>% 
    rename(vehicle_key = key)

# PHEVs ----

results_phev <- list()
tictoc::tic()
index <- 1
for (i in 1:nrow(dt_ymm_phev)) {
    
    # Get the ymmt's for matching and the potential matches from v 
    # i = 100
    ymm <- dt_ymm_phev[i,]
    ymmt <- dt_ymmtp %>% 
        filter(
            powertrain == 'phev',
            year == ymm$year, 
            make == ymm$make, 
            model == ymm$model
        )
    matches <- v_phev %>% 
        filter(year == ymm$year, make == ymm$make) %>% 
        select(-year, -make, -powertrain)
    
    # Skip if no matches
    if (nrow(matches) == 0) { next }
    
    results_phev[[index]] <- get_match(ymmt, matches)
    index <- index + 1
}
tictoc::toc()
# 5.11 sec elapsed

dict_phev <- do.call(rbind, results_phev) %>%
    select(-model_trim) %>% 
    rename(vehicle_key = key)

# HEVs ----

results_hev <- list()
tictoc::tic()
index <- 1
for (i in 1:nrow(dt_ymm_hev)) {
    
    # Get the ymmt's for matching and the potential matches from v 
    # i = 100
    ymm <- dt_ymm_hev[i,]
    ymmt <- dt_ymmtp %>% 
        filter(
            powertrain == 'hybrid',
            year == ymm$year, 
            make == ymm$make, 
            model == ymm$model
        )
    matches <- v_hev %>% 
        filter(year == ymm$year, make == ymm$make) %>% 
        select(-year, -make, -powertrain)
    
    # Skip if no matches
    if (nrow(matches) == 0) { next }
    
    results_hev[[index]] <- get_match(ymmt, matches)
    index <- index + 1
}
tictoc::toc()
# 13 sec elapsed

dict_hev <- do.call(rbind, results_hev) %>%
    select(-model_trim) %>% 
    rename(vehicle_key = key)

# CVs ----

results_cv <- list()
tictoc::tic()
index <- 1
for (i in 1:nrow(dt_ymm_cv)) {
    
    # Get the ymmt's for matching and the potential matches from v 
    # i = 100
    ymm <- dt_ymm_cv[i,]
    ymmt <- dt_ymmtp %>% 
        filter(
            powertrain == 'conventional',
            year == ymm$year, 
            make == ymm$make, 
            model == ymm$model
        )
    matches <- v_cv %>% 
        filter(year == ymm$year, make == ymm$make) %>% 
        select(-year, -make, -powertrain)
    
    # Skip if no matches
    if (nrow(matches) == 0) { next }
    
    results_cv[[index]] <- get_match(ymmt, matches)
    index <- index + 1
}
tictoc::toc()
# 84 sec elapsed

dict_cv <- do.call(rbind, results_cv) %>%
    select(-model_trim) %>% 
    rename(vehicle_key = key)

# Merge all dictionaries
dict <- rbind(dict_bev, dict_phev, dict_hev, dict_cv)

# Print summary of matching techniques
count(dict, powertrain, matching) %>%
    group_by(powertrain) %>% 
    mutate(p = scales::percent(round(n / sum(n), 2)))

# Join on final match results, then save both the matched dictionary
write_parquet(dict, here::here('data', 'dict_carsheet.parquet'))
