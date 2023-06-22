set.seed(5678)

# Packages

# install.packages(c(
#     'arrow', 'data.table', 'tidyverse', 'logitr',
#     'here', 'tictoc', 'broom', 'fixest', 'cowplot',
#     'scattermore', 'readxl', 'fuzzyjoin', 'zipcodeR',
#     'tictoc', 'terra', 'lubridate', 'texreg', 'geomtextpath'
# ))

library(arrow)
library(qs)
library(tidyverse)
library(data.table)
library(cowplot)
library(scattermore)
library(fixest)
library(broom)
library(logitr)
library(readxl)
library(fuzzyjoin)
library(zipcodeR)
library(lubridate)
library(texreg)
library(ggpubr)
library(ggrepel)
library(xtable)
library(geomtextpath)
library(kableExtra)

# Settings

options(dplyr.width = Inf)
# set_cpu_count(1)
# set_io_thread_count(1)

# Load global pars
source(here::here('code', '0-globals.R'))

save_raw <- function(text, path) {
    fileConn <- file(path)
    writeLines(text, fileConn)
    close(fileConn)
}

# General theme for all plots

plot_theme <- function() {
    return(
        theme_minimal_grid(
            font_family = 'Roboto Condensed',
            font_size = 12
        ) + 
            panel_border() +
            theme(
                plot.background = element_rect(fill = "white"),
                panel.background = element_rect(fill = "white"),
                plot.title.position = "plot",
                strip.background = element_rect(fill = "grey80"),
                strip.text = element_text(face = "bold"),
                panel.grid.major = element_line(size = 0.3, colour = "grey90"),
                axis.line.x = element_blank(),
                plot.caption.position = "plot",
                plot.caption = element_text(
                    hjust = 1, size = 11, face = "italic"),
                plot.title = element_text(face = "bold"),
                legend.position = "none"
            ) 
    )
}

# Function for matching vehicle model specs from
# fueleconomy.gov and carsheet.io

get_match <- function(ymmt, matches) {

    # First do perfect join on model_trim
    match_model_trim <- ymmt %>%
        left_join(
            matches %>%
                rename(model_trim = model),
            by = 'model_trim'
        ) %>%
        mutate(matching = "model_trim")

    # If all matches are perfect, we're done!
    if (!any(is.na(match_model_trim$key))) {
        return(match_model_trim)
    }

    # Now for any missed matches, try match on just 'model'
    match_model <- match_model_trim %>%
        filter(is.na(key)) %>%
        select(year, make, model, trim, powertrain, model_trim) %>%
        left_join(matches, by = 'model') %>%
        mutate(matching = "model")

    # If all remaining matches are perfect, we're done!
    if (!any(is.na(match_model$key))) {
        result <- rbind(match_model_trim, match_model) %>%
            filter(!is.na(key))
        return(result)
    }

    # Now for any missed matches, try match on just 'trim'
    match_trim <- match_model %>%
        filter(is.na(key)) %>%
        select(year, make, model, trim, powertrain, model_trim) %>%
        left_join(
            matches %>%
                rename(trim = model),
            by = 'trim'
        ) %>%
        mutate(matching = "trim")

    # If all remaining matches are perfect, we're done!
    if (!any(is.na(match_model$key))) {
        result <- rbind(match_model_trim, match_model, match_trim) %>%
            filter(!is.na(key))
        return(result)
    }

    # Finally, for any remaining missed matches, try fuzzy join on 'model'
    match_fuzzy <- stringdist_join(
        x = match_trim %>%
            filter(is.na(key)) %>%
            select(year, make, model, trim, powertrain, model_trim),
        y = matches,
        by = 'model',
        mode = 'left',
        method = 'jw',
        max_dist = 99,
        distance_col = 'dist'
    ) %>%
        arrange(model.x, dist) %>%
        filter(dist <= 0.3) %>% # Match threshold for similarity
        select(-dist, -model.y) %>%
        rename(model = model.x) %>%
        group_by(year, make, model, trim) %>%
        slice(1) %>%
        mutate(matching = "fuzzy")
    result <- rbind(
        match_model_trim, match_model, match_trim, match_fuzzy) %>%
        filter(!is.na(key))
    return(result)
}

# Functions for computing fuel costs and data cleaning

compute_fuel_cost <- function(dt, paths) {

    # Join range, mpg, and motor efficiencies

    dt <- dt %>%
        left_join(
            read_parquet(here::here('data', 'dict_final.parquet')),
            by = c(
                'powertrain', 'vehicle_type', 'year', 'make', 'model',
                'trim'
            )
        )

    # Compute cents_per_mile

    dt <- dt %>%
        mutate(
            cents_per_mile = 100 * gas_price / mpg,
            cents_per_mile = ifelse(
                powertrain == 'phev',
                phev_uf*((elec_price*kwhp100mi / 100) + (gas_price*gal100mi / 100)) +
                (1 - phev_uf)*cents_per_mile,
                cents_per_mile
            ),
            cents_per_mile = ifelse(
                powertrain == 'bev',
                elec_price*kwhp100mi / 100,
                cents_per_mile
            )
        ) %>%
        filter(!is.na(cents_per_mile)) %>%
        filter(cents_per_mile != Inf) %>%
        select(-kwhp100mi, -gal100mi, -phev_uf)

    return(dt)

}

final_fixes <- function(dt) {

    dt <- dt %>%
        mutate(

            # Kia Niro PHEV has wrong range
            range = ifelse(
                powertrain == 'phev' & model == 'niro',
                33, range),

            # Merge e-tron listings (some trim levels in model names)
            model = ifelse(
                powertrain == 'bev' & str_detect(model, 'e-tron'),
                'e-tron', model)
        ) %>%
        # Drop mis-matched vehicle types by dropping small n (< 50 listings)
        group_by(powertrain, vehicle_type, model) %>%
        mutate(n = n()) %>%
        filter(n > 50) %>%
        select(-n)

    # Remove small n cars
    dt <- dt %>%
        group_by(powertrain, make, model) %>%
        mutate(n = n()) %>%
        filter(n >= 500) %>%
        select(-n) %>%
        ungroup()

    return(dt)
}

# Function to harmonize factor levels for powertrain

set_powertrain_levels <- function(df) {
    df$powertrain_label <- df$powertrain
    df <- df %>%     
        mutate(
            powertrain_label = fct_recode(
                powertrain_label,
                "Conventional" = 'conventional',
                'Hybrid' = 'hybrid',
                "Plug-in Hybrid" = 'phev',
                "Battery Electric" = "bev"
            ),
            powertrain_label = fct_relevel(
                powertrain_label, 
                c("Conventional", "Hybrid", "Plug-in Hybrid", "Battery Electric"))
        )
    return(df)
}

age_years_coefs <- function(m) {
    coefs <- coef(m)[which(str_detect(names(coef(m)), 'age_years'))]
    return(data.frame(coefs))
}

# Function for saving results of a large model

save_model_results <- function(model, pred = NULL) {
    
    name <- deparse(substitute(model))
    
    if(!dir.exists(here::here('models'))) {
        dir.create(here::here('models'))
    }
    
    # Save full model in temp dir (ignored on github)
    qsave(model, here::here('models-temp', paste0(name, '.qs')))
    
    # Save only model results in models dir (pushed to github)
    qsave(
        list(
            coefs = coef(model), 
            stats = glance(model), 
            summary = tidy(model), 
            pred = pred
        ),
        file = here::here('models', paste0(name, '.qs'))
    )

}

# Extracts age effect estimates by adding together interaction terms

get_age_effects <- function(coefs, name) {
    
    age <- c(coefs[1], coefs['age_years'])
    
    # Get intercepts
    
    ints <- coefs[str_detect(names(coefs), name)]
    ints <- age[1] + ints[!str_detect(names(ints), ":")]
    
    # Get slopes
    
    slopes <- coefs[str_detect(names(coefs), paste0(':', name))]
    slopes <- age[2] + slopes
    
    # Summarize in data frame
    
    effects <- data.frame(
        var = c('conventional', 
                str_split(names(ints), name, simplify = TRUE)[,2]),
        int = c(age[1], ints),
        slope = c(age[2], slopes)
    )
    
    return(effects)
}

# Functions for loading data ----

load_dt <- function(
    pt = c('bev', 'phev', 'hybrid', 'conventional'), 
    vt = c('car', 'suv', 'pickup')
) {
    dt <- open_dataset(PATH_DB) %>%
        filter(between(age_years, AGE_YEARS_MIN, AGE_YEARS_MAX)) %>%
        filter(powertrain %in% pt) %>%
        filter(vehicle_type %in% vt) %>%
        collect() %>% 
        common_cleaning()
    return(dt)
}   

sample_cv <- function(dt, p_cv) {
    dt_cv <- dt %>%
        filter(powertrain == 'conventional') %>%
        group_by(model) %>%
        sample_frac(p_cv)
    dt <- dt %>%
        filter(powertrain != 'conventional') %>%
        rbind(dt_cv)
    return(dt)
}

load_dt_car <- function() {
    return(load_dt(vt = 'car'))
}

load_dt_suv <- function() {
    return(load_dt(vt = 'suv'))
}

load_dt_pev_car <- function() {
    return(load_dt(pt = c('bev', 'phev'), vt = 'car'))
}

load_dt_bev <- function() {
    return(load_dt(pt = 'bev', vt = c('car', 'suv')))
}  

load_dt_bev_car <- function() {
    dt <- load_dt(pt = 'bev', vt = 'car')
    
    # Set model levels based on lowest to highest range
    dt$model <- factor(
        dt$model, levels = c(
            "leaf", "fortwo", "spark", "focus", "i3", "e-golf", "500e", 
            "soul", "bolt ev", "model 3",  "model s"
        )
    )
    return(dt)
}

load_dt_bev_suv <- function() {
    dt <- load_dt(pt = 'bev', vt = 'suv')
    return(dt)
}

load_dt_hev <- function() {
    return(load_dt(pt = 'hybrid'))
}   

load_dt_hev_car <- function() {
    return(load_dt(pt = 'hybrid', vt = 'car'))
}

load_dt_phev <- function() {
    return(load_dt(pt = 'phev'))
}   

load_dt_phev_car <- function() {
    dt <- load_dt(pt = 'phev', vt = 'car')
    
    # Set model levels based on lowest to highest range
    dt$model <- factor(
        dt$model, levels = c(
            "prius prime", "prius plug-in", "i8", "fusion energi", 
            "sonata plug-in hybrid", "optima", "elr", "volt"
        )
    )
    return(dt)
}

load_dt_cv <- function() {
    return(load_dt(pt = 'conventional'))
}   

load_dt_cv_car <- function() {
    return(load_dt(pt = 'conventional', vt = 'car'))
}

load_dt_cv_suv <- function() {
    return(load_dt(pt = 'conventional', vt = 'suv'))
}

load_dt_cv_pickup <- function() {
    return(load_dt(pt = 'conventional', vt = 'pickup'))
}

# Common cleaning steps across any datasets

common_cleaning <- function(dt) {

    # Change miles to thousands
    dt$miles <- dt$miles / 1000
    
    # Make Tesla dummy
    dt <- dt %>% 
        mutate(tesla = ifelse(make == "tesla", 1, 0))
    
    # Set common reference levels for factor variables
    dt <- set_reference_levels(dt)

    return(dt)
}

set_reference_levels <- function(dt) {
    
    # Set "car" as the reference level for vehicle_type
    
    dt$vehicle_type <- factor(
        dt$vehicle_type, levels = c("car", "suv", "pickup")
    )

    # Set "small" as the reference level for class
    
    dt$class <- factor(
        dt$class, levels = c("small", "midsize", "large", "suv")
    )
    
    # Set "conventional" as the reference level for powertrain
    
    dt$powertrain <- factor(
        dt$powertrain, levels = c("conventional", "hybrid", "phev",'bev')
    )
    
    return(dt)

}
