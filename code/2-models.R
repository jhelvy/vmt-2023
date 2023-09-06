# Load packages and custom functions

source(here::here('code', '0-functions.R'))
source(here::here('code', '0-set-paths.R'))

# Models

# A | Conventional powertrain only - car, SUV, pickup ----

dt_cv <- load_dt_cv() %>%
    select(miles, age_years, vehicle_type, cents_per_mile)

# a1 

tictoc::tic()
a1 <- feols(
    fml = miles ~ age_years*vehicle_type,
    data = dt_cv
)
tictoc::toc()

summary(a1)
age_years_coefs(a1)
save_model_results(a1)
rm(a1)

# a2

tictoc::tic()
a2 <- feols(
    fml = miles ~ age_years*vehicle_type*cents_per_mile,
    data = dt_cv
)
tictoc::toc()
summary(a2)
age_years_coefs(a2)
save_model_results(a2)
rm(a2)

rm(dt_cv)
gc()



# B  | Multiple powertrains - cars only ----

dt_car <- load_dt_car() %>% 
    select(miles, age_years, powertrain, cents_per_mile, make, model, state)

# b1 

tictoc::tic()
b1 <- feols(
    fml = miles ~ age_years*powertrain,
    data = dt_car
)
tictoc::toc()

summary(b1)
age_years_coefs(b1)
save_model_results(b1)
rm(b1)

# b2 

dt_car <- dt_car %>% 
    mutate(
        powertrain = as.character(powertrain),
        powertrain = ifelse(make == 'tesla', 'bev_tesla', powertrain),
        powertrain = ifelse(
            make != 'tesla' & powertrain == 'bev', 
            'bev_non_tesla', powertrain),
    )

dt_car$powertrain <- factor(
    dt_car$powertrain, levels = c(
        "conventional", "hybrid", "phev", 'bev_non_tesla', 'bev_tesla'
    )
)

tictoc::tic()
b2 <- feols(
    fml = miles ~ age_years*powertrain,
    data = dt_car
)
tictoc::toc()

summary(b2)
age_years_coefs(b2)
save_model_results(b2)
rm(b2)

rm(dt_car)

# b3 - only keep common zip codes
 
# Robustness test - compare only zips that also have a BEV

dt_car <- load_dt_car() %>% 
    select(
        miles, age_years, powertrain, cents_per_mile, make, model, state, 
        zip
    )

bev_zips <- dt_car %>%
    filter(powertrain == 'bev') %>% 
    distinct(zip) %>% 
    pull(zip)

dt_car <- dt_car %>% 
    filter(zip %in% bev_zips)

tictoc::tic()
b3 <- feols(
    fml = miles ~ age_years*powertrain,
    data = dt_car
)
tictoc::toc()

summary(b3)
age_years_coefs(b3)
save_model_results(b3)
rm(b3)


# Compute RMSE for CV and BEV models

model <- feols(
    fml = miles ~ age_years,
    data = dt_car %>% filter(powertrain == 'conventional')
)
rmse_cv <- sqrt(mean(model$residuals^2))
rm(model)
model <- feols(
    fml = miles ~ age_years,
    data = dt_car %>% filter(powertrain == 'bev')
)
rmse_bev <- sqrt(mean(model$residuals^2))
rm(model)
round(rmse_cv, 1)
round(rmse_bev, 1)

rm(dt_car)
gc()



# C  | CV powertrain - cars only ----

dt_cv_car <- load_dt_cv_car()

# c1 

tictoc::tic()
c1 <- feols(
    fml = miles ~ 
        age_years*cents_per_mile + 
        age_years*model + 
        state + 
        my,
    data = dt_cv_car
)
tictoc::toc()

summary(c1)
age_years_coefs(c1)
save_model_results(c1)
rm(c1)
rm(dt_cv_car)
gc()



# D | BEV powertrain - cars only ----

dt_bev_car <- load_dt_bev_car() %>% 
    mutate(
        range_type = ifelse(
            range < 100, 'low', ifelse(
            between(range, 100, 200), 'mid', 'high'))
    ) 

# Range cutoffs based on range distribution:
# hist(dt_bev_car$range, 50)

# d1

tictoc::tic()
d1 <- feols(
    fml = miles ~ 
        age_years*cents_per_mile + 
        age_years*range_type*range + 
        age_years*model +
        state + 
        my,
    data = dt_bev_car
)
tictoc::toc()

summary(d1)
age_years_coefs(d1)
save_model_results(d1)

# Compute range effects
coef_range_high <- coef(d1)['age_years:range']
coef_range_low <- coef_range_high + coef(d1)['age_years:range_typelow:range']
coef_range_mid <- coef_range_high + coef(d1)['age_years:range_typemid:range']
round(10000*coef_range_low)
round(10000*coef_range_mid)
round(10000*coef_range_high)

rm(d1)

# time factors
tictoc::tic()
d2 <- feols(
    fml = miles ~ age_years^2 +
        age_years*cents_per_mile + 
        age_years*range_type*range + 
        age_years*model + 
        state +
        my,
    data = dt_bev_car
)
tictoc::toc()

summary(d2)
age_years_coefs(d2)
save_model_results(d2)
rm(d2)

# Drop model years with very few observations and reset MY baseline
dt_bev_car <- dt_bev_car %>%
    filter(year > 2011, year < 2020) %>% 
    select(-my) %>% 
    mutate(my = as.factor(year))

tictoc::tic()
d3 <- feols( 
    fml = miles ~ 
        age_years*cents_per_mile + 
        age_years*range_type*range + 
        age_years*model + 
        age_years*my + 
        state,
    data = dt_bev_car
)
tictoc::toc()

summary(d3)
age_years_coefs(d3)
save_model_results(d3)
rm(d3)

tictoc::tic()
d4 <- feols( 
    fml = miles ~ 
        age_years^2 +
        age_years*cents_per_mile + 
        age_years*range_type*range + 
        age_years*model + 
        age_years*my + 
        state,
    data = dt_bev_car
)
tictoc::toc()

summary(d4)
age_years_coefs(d4)
save_model_results(d4)
rm(d4)


rm(dt_bev_car)
gc()



# E  | HEV powertrain, cars only ----

dt_hev_car <- load_dt_hev_car() %>% 
    filter(year > 2011, year < 2020) %>% 
    select(-my) %>% 
    mutate(my = as.factor(year))

# e1

tictoc::tic()
e1 <- feols(
    fml = miles ~ 
        age_years*cents_per_mile + 
        age_years*model + 
        state +
        my,
    data = dt_hev_car
)
tictoc::toc()

summary(e1)
age_years_coefs(e1)
save_model_results(e1)
rm(e1)
rm(dt_hev_car)
gc()



# F  | PHEV powertrain, cars only ----

dt_phev_car <- load_dt_phev_car() 

tictoc::tic()
f1 <- feols(
    fml = miles ~ 
        age_years*cents_per_mile + 
        age_years*range + 
        age_years*model + 
        state + 
        my,
    data = dt_phev_car
)
tictoc::toc()

summary(f1)
age_years_coefs(f1)
save_model_results(f1)
rm(f1)
rm(dt_phev_car)
gc()





# Top 10% of VMT for BEVs ----

quantile90 <- load_dt_bev_car() %>% 
    group_by(age_months) %>% 
    summarise(miles90 = fquantile(miles, 0.90))
dt_bev_car <- load_dt_bev_car() %>% 
    left_join(quantile90, by = 'age_months') %>% 
    filter(miles >= miles90)

tictoc::tic()
model <- feols(
    fml = miles ~ age_years + model,
    data = dt_bev_car
)
tictoc::toc()

summary(model)
age_years_coefs(model)

dt_cv_car <- load_dt_cv_car() %>% 
    left_join(quantile90, by = 'age_months') %>% 
    mutate(below_top_bev = miles >= miles90)
dt_cv_car %>% 
    count(below_top_bev) %>% 
    mutate(p = n / sum(n))
rm(dt_cv_car)




# I NHTS models------

dt_nhts_cars <- read_parquet(
    here::here('data', 'nhts', 'nhts_formatted.parquet')
) %>% 
    filter(between(age_years, 2, 9), vehicle_type == 'car') %>%   
    select(-vehicle_type) %>% 
    mutate(
        HHVEHCNT = as.factor(HHVEHCNT),
        MY = as.factor(MY),
        HHSIZE = if_else(HHSIZE>= 6, 6,HHSIZE),
        HHSIZE = as.factor(HHSIZE)
    )

dt_nhts_cars$powertrain <- factor(
    dt_nhts_cars$powertrain, 
    levels = c("conventional", "hybrid",'phev','bev')
)

dt_nhts_cars$HHSIZE <- factor(
    dt_nhts_cars$HHSIZE,levels = c(2,3,4,5,6)
)

# I1 

I1 <- feols(
    fml = miles ~ age_years*powertrain,
    data = dt_nhts_cars
)

summary(I1)
age_years_coefs(I1)
save_model_results(I1)
rm(I1)

I2 <- feols(
    fml = miles ~ age_years*primary_vehicle,
    data = dt_nhts_cars[powertrain=='conventional']
)

summary(I2)
age_years_coefs(I2)
save_model_results(I2)
rm(I2)

I3 <- feols(
    fml = miles ~ 
        age_years*primary_vehicle +
        age_years*HHSIZE +
        age_years*cents_per_mile +
        MY +
        state,
    data = dt_nhts_cars[powertrain=='conventional']
)

summary(I3)
age_years_coefs(I3)
save_model_results(I3)
rm(I3)

I4 <- feols(
    fml = miles ~ 
        age_years*primary_vehicle +
        age_years*HHSIZE +
        age_years*cents_per_mile +
        MY +
        state,
    data = dt_nhts_cars[dt_nhts_cars$powertrain=='hybrid',]
)

summary(I4)
age_years_coefs(I4)
save_model_results(I4)
rm(I4)

I5 <- feols(
    fml = miles ~ 
        age_years*primary_vehicle +
        age_years*HHSIZE +
        age_years*cents_per_mile +
        MY +
        state,
    data = dt_nhts_cars[powertrain == 'conventional'] %>% 
        select(-primary_vehicle) %>% 
        rename(primary_vehicle = primary_vehicle_alt)
)

summary(I5)
age_years_coefs(I5)
save_model_results(I5)
rm(I5)
