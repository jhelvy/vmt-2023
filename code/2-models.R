source(here::here('code', '0-functions.R'))

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
        powertrain = ifelse(make == 'tesla', 'bev_tesla', powertrain)
    )

dt_car$powertrain <- factor(
    dt_car$powertrain, levels = c(
        "conventional", "hybrid", "phev", 'bev', 'bev_tesla'
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
gc()



# C  | CV powertrain - cars only ----

dt_cv_car <- load_dt_cv_car()

# c1 

tictoc::tic()
c1 <- feols(
    fml = miles ~ age_years*cents_per_mile + age_years*model + state,
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
    fml = miles ~ age_years*cents_per_mile + age_years*range_type*range + age_years*model + state,
    data = dt_bev_car
        
)
tictoc::toc()

summary(d1)
age_years_coefs(d1)
save_model_results(d1)
rm(d1)
rm(dt_bev_car)
gc()




# E  | HEV powertrain, cars only ----

dt_hev_car <- load_dt_hev_car()

# e1

tictoc::tic()
e1 <- feols(
    fml = miles ~ age_years*cents_per_mile + age_years*model + state,
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
    fml = miles ~ age_years*cents_per_mile + age_years*range + age_years*model + state,
    data = dt_phev_car
)
tictoc::toc()

summary(f1)
age_years_coefs(f1)
save_model_results(f1)
rm(f1)
rm(dt_phev_car)
gc()






# Top 10% of VMT for BEVs

quantile90 <- load_dt_bev_car() %>% 
    group_by(age_months) %>% 
    summarise(miles90 = fquantile(miles, 0.90))
dt_bev_car <- load_dt_bev_car() %>% 
    left_join(quantile90, by = 'age_months') %>% 
    filter(miles >= miles90)

model <- feols(
    fml = miles ~ age_years + model,
    data = dt_bev_car
)

summary(model)
age_years_coefs(model)

dt_cv_car <- load_dt_cv_car() %>% 
    left_join(quantile90, by = 'age_months') %>% 
    mutate(below_top_bev = miles >= miles90)
dt_cv_car %>% 
    count(below_top_bev) %>% 
    mutate(p = n / sum(n))
