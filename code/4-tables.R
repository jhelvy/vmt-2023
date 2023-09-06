source(here::here('code', '0-functions.R'))
source(here::here('code', '0-globals.R'))

# Summary statistics for cars and SUVs ----

make_summary_table <- function(df) {
    df <- separate_bev_tesla(df)
    df <- df %>%
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
            df %>%
                distinct(powertrain, model) %>%
                group_by(powertrain) %>%
                summarise(n_models = n()),
            by = 'powertrain'
        )
}

format_summary_table_latex <- function(df) {
    df <- df %>%
        select(powertrain, n, n_models, everything()) %>%
        mutate(
            powertrain = fct_recode(
                powertrain,
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
    return(df)
}

# Make summary tables

data_summary_car <- make_summary_table(load_dt_car())
data_summary_suv <- make_summary_table(load_dt_suv()) %>%
    rbind(data.frame(
      powertrain = 'phev',
      n = 0,
      miles_mean = NA,
      miles_sd = NA,
      age_mean = NA,
      age_sd = NA,
      price_mean = NA,
      price_sd = NA,
      range_mean = NA,
      range_sd = NA,
      range_min = NA,
      range_max = NA,
      n_models = 0
    ))

# Reformat for latex and save

save_raw(
    format_summary_table_latex(data_summary_car),
    here::here('tables', 'data_summary_car.txt')
)

save_raw(
    format_summary_table_latex(data_summary_suv),
    here::here('tables', 'data_summary_suv.txt')
)


# Counts of vehicle models in dataset ----

models <- read_csv(here::here('data', 'model_counts_top.csv'))

# Car counts ----

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
        p = round(p, 3),
        cumsum = round(cumsum, 3),
        n = scales::comma(n),
        powertrain = str_to_upper(powertrain),
        make = ifelse(
            make == 'bmw', 'BMW', ifelse(
                make == 'kia', 'KIA', str_to_title(make))),
        model = str_replace(model, " plug-in hybrid", ""),
        model = str_replace(model, " hybrid", ""),
        model = ifelse(
            model == 'cr-z', 'CR-Z', ifelse(
            model == 'ct', 'CT', ifelse(
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
        caption = "Counts of car model listings included in analysis."
    ) %>%
    kable_styling(latex_options = c("repeat_header")) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle")

save_raw(car_models, here::here('tables', 'car_models.txt'))


# SUV counts ----

suv_models <- load_dt_suv() %>%
  distinct(powertrain, make, model)
suv_models <- models %>%
    filter(vehicle_type == 'suv') %>%
    right_join(suv_models, by = c('powertrain', 'make', 'model')) %>%
    filter(!is.na(n)) %>%
    select(powertrain, make, model, n, p, cumsum) %>%
    mutate(
      p = round(p, 3),
      cumsum = round(cumsum, 3)
    )
suv_models$powertrain <- factor(
    suv_models$powertrain, levels = c("conventional", "hybrid", "phev",'bev')
)
suv_models <- suv_models %>%
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
        model == 'e-tron', 'e-tron', ifelse(
        model == 'nx phev', 'NX PHEV', ifelse(
        model == 'nx', 'NX', ifelse(
        model == 'ux', 'UX', ifelse(
        model == 'xv crosstrek', 'XV Crosstrek', ifelse(
        model == 'rav4', 'RAV4', ifelse(
        model == 'rx', 'RX', ifelse(
        model == 'mdx', 'MDX', ifelse(
        model == 'cr-v', 'CR-V', ifelse(
        model == 'cx-5', 'CX-5', ifelse(
        model == 'mx', 'MDX', ifelse(
        model == 'qx60', 'QX60', ifelse(
        model == 'xc40', 'XC40', ifelse(
        model == 'ev6', 'EV6', ifelse(
        model == 'bolt euv', 'Bolt EUV', ifelse(
        model == 'kona ev', 'Kona EV', ifelse(
        model == 'mkz', 'MKZ', str_to_title(model))))))))))))))))))
    ) %>%
    kbl(
        format = "latex",
        booktabs = TRUE,
        longtable = TRUE,
        caption = "Counts of SUV model listings included in analysis."
    ) %>%
    kable_styling(latex_options = c("repeat_header")) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle")

save_raw(suv_models, here::here('tables', 'suv_models.txt'))




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

# Table 3 | Multiple powertrains - cars and SUVs

b1 <- qread(here::here('models-temp', 'b1.qs'))
b2 <- qread(here::here('models-temp', 'b2.qs'))
b1s <- qread(here::here('models-temp', 'b1s.qs'))
b2s <- qread(here::here('models-temp', 'b2s.qs'))
texreg(
    list(b1, b2, b1s, b2s),
    booktabs = TRUE,
    dcolumn = TRUE,
    digits = 3,
    file = here::here('tables', 'reg_pooled_powertrain.txt')
)
rm(b1, b2, b1s, b2s)

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

# Separate powertrain models - SUVs only ----

c1s <- qread(here::here('models-temp', 'c1s.qs'))
d1s <- qread(here::here('models-temp', 'd1s.qs'))
e1s <- qread(here::here('models-temp', 'e1s.qs'))
texreg(
  list(d1s, e1s, c1s),
  booktabs = TRUE,
  dcolumn = TRUE,
  digits = 3,
  file = here::here('tables', 'reg_separate_powertrain_suvs.txt')
)
rm(d1s, e1s, c1s)

# Full table exports to Excel file

# Cars

write_csv(
    qread(here::here('models', 'd1.qs'))$summary,
    here::here('tables', 'table3a.csv')
)
write_csv(
    qread(here::here('models', 'f1.qs'))$summary,
    here::here('tables', 'table3b.csv')
)
write_csv(
    qread(here::here('models', 'e1.qs'))$summary,
    here::here('tables', 'table3c.csv')
)
write_csv(
    qread(here::here('models', 'c1.qs'))$summary,
    here::here('tables', 'table3d.csv')
)

# SUVs

write_csv(
  qread(here::here('models', 'd1s.qs'))$summary,
  here::here('tables', 'table4a.csv')
)
write_csv(
  qread(here::here('models', 'e1s.qs'))$summary,
  here::here('tables', 'table4b.csv')
)
write_csv(
  qread(here::here('models', 'c1s.qs'))$summary,
  here::here('tables', 'table4c.csv')
)

# NHTS Regression Table -------

I3 <- qread(here::here('models-temp', 'I3.qs'))
I4 <- qread(here::here('models-temp', 'I4.qs'))
I5 <- qread(here::here('models-temp', 'I5.qs'))

texreg(
    list(I3,I4,I5),
    booktabs = TRUE,
    dcolumn = TRUE,
    digits = 3,
    file = here::here('tables', 'NHTS.txt')
)
rm(I3,I4,I5)


# Time Effects ------

d1 <- qread(here::here('models-temp', 'd1.qs'))
d2 <- qread(here::here('models-temp', 'd2.qs'))
d3 <- qread(here::here('models-temp', 'd3.qs'))
d4 <- qread(here::here('models-temp', 'd4.qs'))
texreg(
    list(d1, d2, d3, d4),
    booktabs = TRUE,
    dcolumn = TRUE,
    digits = 3,
    file = here::here('tables', 'time_effects.txt')
)
rm(d1, d2, d3, d4)
