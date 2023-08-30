# This file cleans the NHTS data for preparation for modeling

# NOTE that NHTS age is completely based on 2017 - model year and is
# therefore +/- 6 months on both sides.

source(here::here("code", "0-functions.R"))

dt_nhts <- read_parquet(here::here("data", "nhts", "vehpub.parquet"))

# Clean NHTS data

makelookup <- readxl::read_excel(
  here::here("data", "nhts", "makemodeldict.xlsx"),
  sheet = "make"
)
modellookup <- readxl::read_excel(
  here::here("data", "nhts", "makemodeldict.xlsx"),
  sheet = "model"
)

# 1st cleaning + data augmentation (based on original AY nhts_odometer.R)
# part first year now cleaned up
dt_nhts <- dt_nhts %>%
  left_join(
    makelookup %>%
      rename(
        nhts2017_vehrespsofthismake = nhts2017_vehresps,
        nhts2017_wtvehsofthismake = nhts2017_weightedvehs
      ),
    by = c("MAKE" = "nhts2017_makecode")
  ) %>%
  left_join(
    modellookup %>%
      rename(
        nhts2017_vehrespsofthismodel = nhts2017_vehresps,
        nhts2017_wtvehsofthismodel = nhts2017_weightedvehs
      ),
    by = c("MODEL" = "nhts2017_modelcode")
  ) %>%
  mutate(
    powertrain = case_when(
      HFUEL == "02" ~ "phev",
      HFUEL == "03" ~ "bev",
      HFUEL == "04" ~ "hybrid",
      TRUE ~ "conventional"
    ) %>%
      fct_relevel("conventional", "hybrid", "phev", "bev"),
    hh_vehicles = if_else(HHVEHCNT == 1, "single veh hh", "multiple veh hh"),
    vehs_excess_adlt = if_else(HHVEHCNT >= NUMADLT, "equal or more vehs than adults", "fewer vehs than adults"),
    vehs_excess_wrk = if_else(HHVEHCNT >= WRKCOUNT, "equal or more vehs than workers", "fewer vehs than workers"),
    urb_rur = if_else(URBRUR == "01", "Urban", "Rural") %>% fct_rev()
  ) %>%
  group_by(HOUSEID) %>%
  mutate(vehicle_VMT_rank = min_rank(desc(ANNMILES))) %>%
  ungroup() %>%
  mutate(
      primary_vehicle = if_else(
          vehicle_VMT_rank == 1, "primary vehicle", "secondary vehicle"),
      primary_vehicle_alt = if_else(
          vehicle_VMT_rank %in% c(1, 2), "primary vehicle", "secondary vehicle"),
  ) %>% 
  # primary_vehicle = if_else(vehicle_VMT_rank <= NUMADLT, "primary vehicle", "secondary vehicle")) %>%
  filter(
      VEHAGE > 0 & VEHAGE < 40 & VEHYEAR > 0 & OD_READ <= 999990 & OD_READ > 0
  ) %>% 
  filter(VEHTYPE %in% c("01", "02", "03", "04", "05")) %>%
  mutate(
    MY = VEHYEAR,
    survey_year = 
        (TDAYDATE %>% 
             as.character() %>% 
             str_sub(1, 4) %>% 
             as.numeric()) + (TDAYDATE %>% 
                                  as.character() %>% 
                                  str_sub(-2, -1) %>% 
                                  as.numeric() + 0.5) / 12,
    # to be consistent with listings database
    # survey date minus Jan 1 of model year
    vehicle_age_from_surveydate_minus_MY = survey_year - MY,
    # TODO: compare against VEHAGE
    age_years = case_when(
      # Is a new car and owned for less than 1 year
      (VEHAGE <= 1 | vehicle_age_from_surveydate_minus_MY <= 1) & VEHOWNED == "02" & as.numeric(VEHOWNMO) >= 0 ~ as.numeric(VEHOWNMO) / 12,
      vehicle_age_from_surveydate_minus_MY <= 0 ~ 0,
      TRUE ~ vehicle_age_from_surveydate_minus_MY
    ),
    age_months = age_years * 12,
    age_days = age_years * 365.24
  ) %>%
  filter(age_years > 0) %>%
  mutate(avgmilesperage = OD_READ / age_years)

dt_nhts <- dt_nhts %>%
  group_by(powertrain, primary_vehicle, urb_rur, VEHAGE) %>%
  mutate(median_od = median(OD_READ / 1000)) %>%
  ungroup()


# Then, clean & augment data in similar way as used listing data

# Change miles to thousands
dt_nhts <- dt_nhts %>%
  mutate(miles = OD_READ / 1000)

# Set reference factor levels for models
dt_nhts <- dt_nhts %>%
  mutate(vehicle_type = case_when(
    VEHTYPE == "01" ~ "car",
    VEHTYPE %in% c("02", "03") ~ "suv",
    VEHTYPE %in% c("04", "05") ~ "pickup",
    TRUE ~ "other"
  ) %>%
    factor(
      # Set "car" as the reference level
      levels = c("car", "suv", "pickup")
    )) # , "other"))) # others already filtered out

# Make Tesla dummy
dt_nhts <- dt_nhts %>%
  mutate(tesla = ifelse(MODEL == "29005", 1, 0))

# Join states / regions
states <- read_csv(here::here("data", "states.csv")) %>%
  janitor::clean_names() %>%
  select(-state) %>%
  rename(state = state_code) %>%
  # Separate out CA as a separate region
  mutate(division = ifelse(state == "CA", "CA", division))
dt_nhts <- dt_nhts %>%
  left_join(states, by = c("HHSTATE" = "state")) %>%
  select(-starts_with("padd")) %>%
  mutate(state = HHSTATE)

# Join range dictionary to dt_nhts
range_path <- here::here("data", "pev-range-matching.csv")
ranges <- read_csv(range_path) %>%
  filter(!is.na(range)) %>%
  mutate(
    nhts2017_make_upper = str_to_upper(make),
    nhts2017_model_upper = str_to_upper(model),
    VEHYEAR = year
  ) %>%
  group_by(nhts2017_make_upper, nhts2017_model_upper, VEHYEAR) %>%
  summarize(
    range = mean(range, na.rm = T),
    rangeCity = mean(rangeCity, na.rm = T),
    rangeHwy = mean(rangeHwy, na.rm = T)
  )

dt_nhts <- dt_nhts %>%
  mutate(
    nhts2017_make_upper = str_to_upper(nhts2017_make),
    nhts2017_model_upper = str_to_upper(nhts2017_model)
  ) %>%
  mutate(
      nhts2017_make_upper = if_else(
          nhts2017_make_upper == "NISSAN/DATSUN", "NISSAN", nhts2017_make_upper)
  ) %>%
  left_join(ranges, by = c("nhts2017_make_upper", "nhts2017_model_upper", "VEHYEAR"))

# dt_nhts %>% filter(!is.na(range)) %>% View()
# dt_nhts %>% filter(nhts2017_model_upper == "LEAF") %>% View()

dt_nhts <- dt_nhts %>%
  mutate(
    powertrain2 = as.factor(ifelse(tesla, "bev_tesla",
      ifelse(powertrain == "bev", "bev_nontesla",
        as.character(powertrain)
      )
    )) %>%
      factor(
        # Set "conventional" as the reference level
        levels = c("conventional", "hybrid", "phev", "bev_nontesla", "bev_tesla")
      ),
    trim = "unknown"
  )

# join gasoline price data
#
# Add gas price data

# This is tricky. The logic here is that I am assuming the vehicle
# stays in the state it was listed in for its entire life. I then look
# at the monthly gas prices in that state during the vehicle's life and
# take the mean across those months and record that as the "mean_gas_price"
# that the driver(s) / owner(s) of that vehicle saw during the life of the
# vehicle. I haven't found an efficient way to do this. The code below takes
# roughly 5 hours to run on my computer. (for 40M used listings)
#
# AY: I wonder if left_joining relevant gas prices and then summarizing, instead of doing this row-by-row, would be more efficient

# Create the start date for each listing based on the model year
dt_nhts <- as.data.table(dt_nhts)
dt_nhts[, start := ymd(paste0(MY - 1, "-01-01"))]
dt_nhts <- dt_nhts %>% mutate(status_date = ym(TDAYDATE))

# Read in gas price data

gas_prices <- open_dataset(here::here("data", "gasoline-prices.parquet")) %>%
  select(state = state_code, date, price) %>%
  collect()

# Function that returns the mean gas price given the state, start date, and
# status_date from a row in dt

get_mean_gas_price <- function(state, start, status_date, gas_prices) {
  return(mean(gas_prices[which(
    (gas_prices$state == state) &
      (gas_prices$date >= start) & # start at model year, go to listing date
      (gas_prices$date <= status_date)
  ), ]$price, na.rm = TRUE))
}

# Compute the mean_gas_price for every row in dt_nhts
# NHTS is small (200k rows), compared to used listings dt, so this is doable in 215 seconds on my laptop

system.time(
  dt_nhts[, mean_gas_price := get_mean_gas_price(state, start, status_date, gas_prices), by = seq_len(nrow(dt_nhts))]
)
# user  system elapsed 
# 61.847   5.838  67.656 

# Compute cost per mile

dt_nhts <- dt_nhts %>%
  mutate(cents_per_mile = 100*mean_gas_price / FEGEMPG)

# Drop the start variable

dt_nhts <- dt_nhts %>%
  select(-start)

# Save formatted NHTS data for modeling, 
# keeping only variables needed for models

dt_nhts %>% 
    select(
        miles, age_years, powertrain, cost_per_mile,
        primary_vehicle, primary_vehicle_alt, vehicle_type,
        MY, HHVEHCNT, HHSIZE, state
    ) %>% 
    write_parquet(here::here('data', 'nhts', 'nhts_formatted.parquet'))
