# Load packages and custom functions

source(here::here('code', '0-functions.R'))
source(here::here('code', '0-globals.R'))

# Models

# B  | Multiple powertrains - SUVs only ----

dt_suv <- load_dt_suv() %>% 
    select(miles, age_years, powertrain, cents_per_mile, make, model, state)

# b1 

tictoc::tic()
b1s <- feols(
    fml = miles ~ age_years*powertrain,
    data = dt_suv
)
tictoc::toc()

summary(b1s)
age_years_coefs(b1s)
save_model_results(b1s)
rm(b1s)

# b2 

dt_suv <- dt_suv %>% 
    mutate(
        powertrain = as.character(powertrain),
        powertrain = ifelse(make == 'tesla', 'bev_tesla', powertrain),
        powertrain = ifelse(
            make != 'tesla' & powertrain == 'bev', 
            'bev_non_tesla', powertrain),
    )

dt_suv$powertrain <- factor(
    dt_suv$powertrain, levels = c(
        "conventional", "hybrid", "phev", 'bev_non_tesla', 'bev_tesla'
    )
)

tictoc::tic()
b2s <- feols(
    fml = miles ~ age_years*powertrain,
    data = dt_suv
)
tictoc::toc()

summary(b2s)
age_years_coefs(b2s)
save_model_results(b2s)
rm(b2s)

rm(dt_suv)
gc()




# C  | CV powertrain - SUVs only ----

dt_cv_suv <- load_dt_cv_suv()

# c1 

tictoc::tic()
c1s <- feols(
    fml = miles ~ 
        age_years*cents_per_mile + 
        age_years*model + 
        state + 
        my,
    data = dt_cv_suv
)
tictoc::toc()

summary(c1s)
age_years_coefs(c1s)
save_model_results(c1s)
rm(c1s)
rm(dt_cv_suv)
gc()



# D | BEV powertrain - SUVs only ----

dt_bev_suv <- load_dt_bev_suv() 

# d1s

tictoc::tic()
d1s <- feols(
    fml = miles ~ 
        age_years*cents_per_mile + 
        age_years*range + 
        age_years*model + 
        state + 
        my,
    data = dt_bev_suv
)
tictoc::toc()

summary(d1s)
age_years_coefs(d1s)
save_model_results(d1s)
rm(d1s)


rm(dt_bev_suv)
gc()



# E  | HEV powertrain, SUVs only ----

dt_hev_suv <- load_dt_hev_suv()

# e1s

tictoc::tic()
e1s <- feols(
    fml = miles ~ 
        age_years*cents_per_mile + 
        age_years*model + 
        state + 
        my,
    data = dt_hev_suv
)
tictoc::toc()

summary(e1s)
age_years_coefs(e1s)
save_model_results(e1s)
rm(e1s)
rm(dt_hev_suv)
gc()



# F  | PHEV powertrain, SUVs only ----

# There are no PHEV SUVs
