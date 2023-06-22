source(here::here('code', '0-functions.R'))

# Summary statistics for cars only ----

dt_car <- load_dt_car() %>% 
    mutate(
        powertrain = as.character(powertrain),
        powertrain = ifelse(
            make == 'tesla', 'bev-tesla', ifelse(
                powertrain == 'bev' & make != 'tesla', 'bev-other', powertrain))
    )

car_data_summary <- dt_car %>%
    group_by(powertrain) %>% 
    summarise(
        n = n(),
        miles_mean = round(mean(miles, na.rm = TRUE)),
        miles_sd = round(sd(miles, na.rm = TRUE)), 
        age_mean = round(mean(age_years, na.rm = TRUE), 1),
        age_sd = round(sd(age_years, na.rm = TRUE), 1),
        price_mean = round(mean(price, na.rm = TRUE)),
        price_sd = round(sd(price, na.rm = TRUE)), 
        range_mean = round(mean(range, na.rm = TRUE)),
        range_sd = round(sd(range, na.rm = TRUE)),
        range_min = round(min(range, na.rm = TRUE)),
        range_max = round(max(range, na.rm = TRUE))
    ) %>% 
    left_join(
        dt_car %>% 
            distinct(powertrain, model) %>% 
            group_by(powertrain) %>% 
            summarise(n_models = n()),
        by = 'powertrain'
    )

# Reformat for latex

car_data_summary <- car_data_summary %>%
    select(powertrain, n, n_models, everything()) %>% 
    mutate(
        powertrain = fct_recode(powertrain,
            'BEV (Non-Tesla)' = 'bev-other',
            'BEV (Tesla)' = 'bev-tesla', 
            'Conventional' = 'conventional',
            'Hybrid' = 'hybrid', 
            'PHEV' = 'phev'
        )
    ) %>% 
    pivot_longer(
        names_to = 'stat', 
        values_to = 'val', 
        cols = -'powertrain'
    ) %>% 
    mutate(
        stat = ifelse(stat == 'n', 'num_n', stat), 
        val = ifelse(val %in% c(Inf, -Inf, NaN, NA), "", val)
    ) %>% 
    separate(stat, into = c('cat', 'stat')) %>% 
    pivot_wider(
        names_from = powertrain, 
        values_from = val
    ) %>% 
    select(` ` = stat, Conventional, Hybrid, PHEV, everything()) %>% 
    select(-cat) %>% 
    kbl(format = "latex", booktabs = T) %>%
    kable_styling() %>% 
    pack_rows("Number of Listings", 1, 1) %>%
    pack_rows("Number of Models", 2, 2) %>%
    pack_rows("Miles (1,000)", 3, 4) %>%
    pack_rows("Age (years)", 5, 6) %>%
    pack_rows("Price ($USD)", 7, 8) %>%
    pack_rows("Electric Range (miles)", 9, 12)

save_raw(car_data_summary, here::here('tables', 'car_data_summary.txt'))


# List of all vehicle models in dataset ----

models <- read_csv(here::here('data', 'model_counts_top.csv')) %>% 
    # Fix e-tron (it's an SUV)
    mutate(
        model = ifelse(str_detect(model, 'e-tron'), 'e-tron', model),
        vehicle_type = ifelse(str_detect(model, 'e-tron'), 'suv', vehicle_type)
    ) %>% 
    group_by(powertrain, vehicle_type, make, model) %>% 
    mutate(
        n = sum(n), 
        p = sum(p)
    ) %>% 
    group_by(powertrain, vehicle_type) %>% 
    mutate(cumsum = cumsum(p)) %>% 
    ungroup() %>% 
    mutate(
        n = round(n, 3),
        p = round(p, 3),
        cumsum = round(cumsum, 3)
    )

# Cars 

car_models <- models %>% 
    filter(vehicle_type == 'car') %>% 
    select(powertrain, everything()) %>% 
    select(-vehicle_type)
car_models$powertrain <- factor(
    car_models$powertrain, levels = c("conventional", "hybrid", "phev",'bev')
)
car_models <- car_models %>% 
    arrange(desc(powertrain)) %>% 
    mutate(
        n = scales::comma(n), 
        powertrain = str_to_upper(powertrain), 
        make = ifelse(
            make == 'bmw', 'BMW', ifelse(
                make == 'kia', 'KIA', str_to_title(make))), 
        model = str_replace(model, " plug-in hybrid", ""),
        model = str_replace(model, " hybrid", ""),
        model = ifelse(
            model == 'cr-z', 'CR-Z', ifelse(
            model == 'ct', 'ct', ifelse(
            model == 'elr', 'ELR', ifelse(
            model == 'e-golf', 'e-Golf', ifelse(
            model == 'i8', 'i8', ifelse(
            model == 'bolt ev', 'Bolt', ifelse(
            model == 'es', 'ES', ifelse(
            model == 'mkz', 'MKZ', str_to_title(model)))))))))
    ) %>% 
    kbl(
        format = "latex", 
        booktabs = TRUE, 
        longtable = TRUE,
        caption = "Counts of vehicle model listings included in analysis."
    ) %>%
    kable_styling(latex_options = c("repeat_header")) %>% 
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle")

save_raw(car_models, here::here('tables', 'car_models.txt'))


# Regression tables ----

# A | Conventional powertrain only - car, SUV, pickup

a1 <- qread(here::here('models-temp', 'a1.qs'))
a2 <- qread(here::here('models-temp', 'a2.qs'))
texreg(
    list(a1, a2), 
    booktabs = TRUE, 
    dcolumn = TRUE,
    digits = 3, 
    file = here::here('tables', 'reg_pooled_cv_vehicle_type.txt')
)
rm(a1, a2)

# B  | Multiple powertrains - cars only

b1 <- qread(here::here('models-temp', 'b1.qs'))
b2 <- qread(here::here('models-temp', 'b2.qs'))
texreg(
    list(b1, b2), 
    booktabs = TRUE, 
    dcolumn = TRUE,
    digits = 3, 
    file = here::here('tables', 'reg_pooled_powertrain_cars.txt')
)
rm(b1, b2)

# Separate powertrain models - cars only ----

c1 <- qread(here::here('models-temp', 'c1.qs'))
f1 <- qread(here::here('models-temp', 'f1.qs'))
d1 <- qread(here::here('models-temp', 'd1.qs'))
e1 <- qread(here::here('models-temp', 'e1.qs'))
texreg(
    list(d1, f1, e1, c1), 
    booktabs = TRUE, 
    dcolumn = TRUE,
    digits = 3, 
    file = here::here('tables', 'reg_separate_powertrain_cars.txt')
)
rm(c1, f1, d1, e1)

# Full table exports to csv files

c1 <- qread(here::here('models', 'c1.qs'))
f1 <- qread(here::here('models', 'f1.qs'))
d1 <- qread(here::here('models', 'd1.qs'))
e1 <- qread(here::here('models', 'e1.qs'))

write_csv(d1$summary, here::here('tables', 'table4a.csv'))
write_csv(f1$summary, here::here('tables', 'table4b.csv'))
write_csv(e1$summary, here::here('tables', 'table4c.csv'))
write_csv(c1$summary, here::here('tables', 'table4d.csv'))
