# This file computes the cents_per_mile variable from the pre-formatted 
# raw database. This variable is already present in the sample db.parquet
# file in the 'data' folder. This code is just here to show how it was 
# computed.

source(here::here('code', '0-functions.R'))

# Read in model counts

models <- read_csv(here::here('data', 'model_counts_top.csv'))

# Open arrow dataset

ds <- open_dataset(PATH_DB_RAW) %>%
    filter(inventory_type == 'used') %>% 
    filter(!is.na(state)) %>% 
    filter(state != "") %>% 
    filter(state != " ") %>% 
    filter(model %in% models$model)

# Read in only a few vars to match the gas and electricity prices

dt <- ds %>%
    select(
        inventory_type, status_date, powertrain, vehicle_type, 
        state, year, listing_year
    ) %>% 
    collect()
dt[, inventory_type := NULL]

dim(dt) 
# [1] 43377085        6

# Electricity prices ----

# The logic here is that I am assuming the vehicle
# stays in the state it was listed in for its entire life. I then look
# at the annual electricity prices in that state during the vehicle's life and
# take the mean and median across those years and record that. 

# Read in price data 

states_dict <- read_csv(here::here('data', 'states.csv')) %>%
    select('State', 'State Code')
elec_prices <- readxl::read_excel(
    here::here('data', 'us_electricity_price.xlsx')) %>%
    left_join(states_dict, by = 'State') %>%
    filter(!is.na(`State Code`)) %>%
    select(year, price = Residential, state = `State Code`) %>% 
    as.data.table()

# Functions that returns the mean and median electricity price given the
# state, start date, and status_date from a row in dt

get_mean_elec_price <- function(var_state, var_year, var_listing_year, prices) {
    rows <- prices[
        (state == var_state) &
        (year >= var_year) &
        (year <= var_listing_year)]
    return(mean(rows$price, na.rm = TRUE))
}

# Only perform the operation of the unique subset of rows in dt
dt_match_elec <- dt[powertrain %chin% c('bev', 'phev')] %>% 
    select(-status_date) %>% 
    distinct()

dt_match_elec[, elec_price := get_mean_elec_price(
    state, year, listing_year, elec_prices), 
    by = seq_len(nrow(dt_match_elec))
]

# Drop missing matches
dt_match_elec <- dt_match_elec %>% 
    filter(!is.nan(elec_price))


# Gasoline prices ----

# The logic here is that I am assuming the vehicle
# stays in the state it was listed in for its entire life. I then look
# at the monthly gas prices in that state during the vehicle's life and
# take the mean and median across those months and record that. 

# Create the start date for each listing based on the model year

dt[, start := ymd(paste0(year, "-01-01"))]

# Read in gas price data

gas_prices <- read_parquet(
    here::here('data', 'gasoline-prices.parquet')) %>%
    select(state = state_code, date, price) %>% 
    as.data.table()

# Functions that returns the mean and median gas price given the state, 
# start date, and status_date from a row in dt

get_mean_gas_price <- function(var_state, var_start, var_status_date, prices) {
    rows <- prices[
        (state == var_state) &
        (date >= var_start) &
        (date <= var_status_date)]
    return(mean(rows$price, na.rm = TRUE))
}

# Only perform the operation of the unique subset of rows in dt
dt_match_gas <- dt %>% 
    select(state, start, status_date, year) %>% 
    distinct()

dt_match_gas[, gas_price := get_mean_gas_price(
    state, start, status_date, gas_prices), 
    by = seq_len(nrow(dt_match_gas))
]

# Drop the start variable
dt_match_gas[, start := NULL]

# Now read in all the used listings to join on the electricity and gas prices
rm(dt)
dt <- ds %>% 
    select(
        vehicle_id, make, model, year, trim, 
        powertrain, vehicle_type, inventory_type,
        state, zip, miles, price,
        status_date, listing_year, listing_index, 
        age_days, age_years, age_months
    ) %>% 
    collect()
dt[, inventory_type := NULL]

dim(dt)
# count(dt, powertrain, vehicle_type)

# Merge ----

# Merge the gas prices onto dt
dt <- merge(
    dt, dt_match_gas, 
    by = c("status_date", "state", "year"), 
    all.x = TRUE
)

# Merge the electricity prices onto dt
dt <- merge(
    dt, dt_match_elec,
    by = c("powertrain", "vehicle_type", "state", "year", "listing_year"),
    all.x = TRUE
)

# Fuel Economy ----

dt <- compute_fuel_cost(dt)

# Final fixes

# Remove small n cars
dt <- dt %>%
    group_by(powertrain, vehicle_type, make, model) %>%
    mutate(n = n()) %>%
    filter(n >= 1000) %>%
    select(-n) %>%
    ungroup()

# Drop models not in top models
 
dt <- merge(
    dt, 
    models %>% 
        select(powertrain, vehicle_type, make, model) %>% 
        mutate(keep = TRUE), 
    by = c('powertrain', 'vehicle_type', 'make', 'model'), 
    all.x = TRUE
) %>% 
    filter(!is.na(keep)) %>%
    select(-keep)

# Write to disc

write_parquet(dt, PATH_DB)


# Create formatted sample for repo

dt <- read_parquet(PATH_DB) %>%
    filter(between(age_years, AGE_YEARS_MIN, AGE_YEARS_MAX)) %>%
    mutate(
        powertrain2 = as.character(powertrain),
        powertrain2 = ifelse(make == 'tesla', 'bev_tesla', powertrain2)
    ) %>%
    group_by(powertrain2, vehicle_type) %>%
    sample_frac(0.01) %>%
    ungroup() %>%
    select(
        powertrain,
        vehicle_type,
        class,
        state,
        year,
        listing_year,
        make,
        model,
        trim,
        miles,
        age_years,
        age_months,
        gas_price,
        elec_price,
        range,
        mpg,
        mpge,
        cents_per_mile
    )

write_parquet(dt, here::here('data', 'db-sample.parquet'))

