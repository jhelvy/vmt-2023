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

# Read in vehicles.parquet from fueleconomy.gov 
v <- read_parquet(here::here('data', 'vehicles.parquet')) %>% 
    select(
        year, make, model,
        kwhp100mi = combE, 
        gal100mi = combinedCD,
        mpg = comb08U, 
        mpge = combA08U,
        phev_uf = combinedUF,
        atvType, # determines powertrain!
        range, rangeA, # rangeA is PHEV range
        class = VClass
    ) %>% 
    mutate(
        make  = str_to_lower(make),
        model = str_to_lower(model),
        class = str_to_lower(class),
        # Drop drivetrains
        model = str_trim(str_replace_all(model, " awd$", '')),
        model = str_trim(str_replace_all(model, " fwd$", '')),
        model = str_trim(str_replace_all(model, " 2wd$", '')),
        model = str_trim(str_replace_all(model, " 4wd$", '')),
        # Drop pickup
        model = str_trim(str_replace_all(model, " pickup$", '')), 
        powertrain = ifelse(
            is.na(atvType), "conventional", str_to_lower(atvType))) %>% 
        # Consolidate powertrains
    mutate(
        powertrain = ifelse(
            powertrain == "plug-in hybrid", "phev", ifelse(
            powertrain == "ev", "bev", ifelse(
            powertrain == "hybrid", "hybrid", ifelse(
            powertrain == "conventional", "conventional", ifelse(
            powertrain == "ffv", "conventional", "other"
        )))))
    ) %>% 
    # Consolidate classes
    filter(class %in% c(
        "two seaters", "small station wagons", "compact cars", 
        "minicompact cars", "subcompact cars", "midsize station wagons", 
        "midsize cars", "midsize-large station wagons", "large cars", 
        "small pickup trucks", "standard pickup trucks", 
        "small pickup trucks 2wd", "standard pickup trucks 2wd", 
        "standard pickup trucks 4wd", "small pickup trucks 4wd", 
        "standard pickup trucks/2wd", "sport utility vehicle - 4wd", 
        "sport utility vehicle - 2wd", "small sport utility vehicle 4wd", 
        "standard sport utility vehicle 2wd", 
        "standard sport utility vehicle 4wd", "small sport utility vehicle 2wd"
    )) %>% 
    mutate(
        class = ifelse(
            class %in% c(
                "two seaters", "small station wagons", "compact cars", 
                "minicompact cars", "subcompact cars"
                ), "small", ifelse(
            class %in% c(
                "midsize station wagons", "midsize cars"
                ), "midsize", ifelse(
            class %in% c(
                "midsize-large station wagons", "large cars"
                ), "large", ifelse(
            class %in% c(
                "small pickup trucks", "standard pickup trucks", 
                "small pickup trucks 2wd", "standard pickup trucks 2wd",
                "standard pickup trucks 4wd", "small pickup trucks 4wd",
                "standard pickup trucks/2wd"
                ), "pickup", ifelse(
            class %in% c(
                "minivan - 2wd", "minivan - 4wd"
                ), "minivan", ifelse(
            class %in% c(
                "sport utility vehicle - 4wd", "sport utility vehicle - 2wd",
                "small sport utility vehicle 4wd", 
                "standard sport utility vehicle 2wd",
                "standard sport utility vehicle 4wd",
                "small sport utility vehicle 2wd"
                ), "suv", 'other'
                ))))))
    ) %>% 
    filter(powertrain != "other") %>% 
    # Fix range
    mutate(range = ifelse(
        (powertrain == 'phev') & !is.na(rangeA), 
        as.numeric(rangeA), as.numeric(range))
    ) %>% 
    arrange(year, make, model, powertrain, class) %>%  
    group_by(year, make, model, powertrain, class) %>% 
    summarise(
        range = mean(range, na.rm = TRUE),
        kwhp100mi = mean(kwhp100mi, na.rm = TRUE),
        mpg = mean(mpg, na.rm = TRUE),
        mpge = mean(mpge, na.rm = TRUE),
        gal100mi = mean(gal100mi, na.rm = TRUE),
        phev_uf = mean(phev_uf, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    mutate(
        range = ifelse(is.nan(range), NA, range),
        kwhp100mi = ifelse(kwhp100mi == 0, NA, kwhp100mi),
        mpg = ifelse(is.nan(mpg), NA, mpg),
        mpge = ifelse(is.nan(mpge), NA, mpge),
        gal100mi = ifelse(is.nan(gal100mi), NA, gal100mi),
        phev_uf = ifelse(is.nan(phev_uf), NA, phev_uf)
    ) %>% 
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
v <- v %>% 
    mutate(make = ifelse(make == "mclaren automotive", "mclaren", make))
v <- v[make %chin% unique(dt_ymmtp$make)]

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
# 5.241 sec elapsed

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
# 2.46 sec elapsed

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
# 8.3 sec elapsed

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
# 45 sec elapsed

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
write_parquet(dict, here::here('data', 'dict_fe.parquet'))
