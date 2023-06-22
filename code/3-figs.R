source(here::here('code', '0-functions.R'))

color_cv <- "grey42" # 'indianred1'
color_ev <- "#00BA38" # "forestgreen"
color_tesla <- "#619CFF" # "dodgerblue"

# Explore Quantiles ----

# Read in quantile data, quick formatting

quantiles <- read_parquet(here::here('data', 'quantiles.parquet')) %>% 
    mutate(
        vehicle = paste(powertrain, vehicle_type, sep = "_"), 
        age_years = age_months / 12
    ) %>% 
    # Drop non-conventional SUVs - there's just not enough to make the figures
    filter(! ((powertrain != 'conventional') & (vehicle_type == 'suv'))) %>% 
    filter(between(age_years, AGE_YEARS_MIN, AGE_YEARS_MAX)) # Set in '0-functions.R'

# Plot of all powertrain-type combos ----

quantiles %>% 
    ggplot() +  
    geom_ribbon(
        aes(
            x = age_years, 
            ymin = miles25, 
            ymax = miles75
        ), 
        fill = color_cv,
        alpha = 0.25) +
    geom_line(
        aes(
            x = age_years, 
            y = miles50, 
            group = vehicle
        ),
        color = color_cv
    ) + 
    facet_wrap(vars(vehicle)) + 
    scale_x_continuous(
        breaks = seq(2, 9, 1), 
        limits = c(2, 9)
    ) +
    scale_y_continuous(
        labels = scales::comma, 
        breaks = seq(0, 125000, 25000) 
    ) +
    plot_theme()

# FIG 1a ----

# Plot of bev, phev, and hybrid overlay over conventional ----

quantiles_car <- quantiles %>% 
    filter(vehicle_type == 'car')
quantiles_conventional <- quantiles_car %>% 
    filter(powertrain == 'conventional')
quantiles_other <- quantiles_car %>% 
    filter(powertrain != 'conventional') %>% 
    arrange(powertrain) %>% 
    mutate(category = 'other')
rep_length <- nrow(quantiles_conventional)
quantiles_conventional <- quantiles_conventional[rep(1:rep_length, 3),]
quantiles_conventional$powertrain <- rep(
    c('bev', 'hybrid', 'phev'), each = rep_length)
quantiles_conventional$category <- 'conventional'
df_fig1 <- rbind(quantiles_other, quantiles_conventional)

# Save plot data for reproduction
qsave(df_fig1, here::here('data', 'df_fig1.qs'))

fig1 <- df_fig1 %>%
    set_powertrain_levels() %>% 
    ggplot() +  
    geom_ribbon(
        aes(
            x = age_years, 
            ymin = miles25, 
            ymax = miles75, 
            fill = category
        ), 
        alpha = 0.25) +
    geom_line(
        aes(
            x = age_years,
            y = miles50,
            color = category, 
            group = category
        )
    ) +
    facet_wrap(vars(powertrain_label)) + 
    scale_x_continuous(
        breaks = seq(2, 9, 1), 
        limits = c(2, 9)
    ) +
    scale_y_continuous(
        labels = scales::comma, 
        breaks = seq(0, 125000, 25000) 
    ) +
    scale_fill_manual(values = c(color_cv, color_ev)) +
    scale_color_manual(values = c(
        color_ev, color_cv, color_cv, color_ev,
        color_ev, color_ev)
    ) +
    plot_theme() +
    labs(
        x = "Vehicle age (years)", 
        y = 'Vehicle mileage'
    ) + 
    geom_label(
        data = data.frame(
            x = rep(5, 6),
            y = 10000*c(10, 1.2, 10, 2, 10, 2.5), 
            label = c(
                'Conventional', 'BEV', 
                'Conventional', 'PHEV', 
                'Conventional', 'Hybrid'
            ), 
            powertrain_label = as.factor(c(
                rep('Battery Electric', 2), rep('Plug-in Hybrid', 2), 
                rep('Hybrid', 2))
            )
        ), 
        mapping = aes(x = x, y = y, label = label, color = label), 
        size = 4, 
        family = 'Roboto Condensed'
    )

ggsave(here::here('figs', 'fig1.png'), fig1, width = 11, height = 3.5)

# FIG 1b ----

# Alternative to Fig 1 but with Tesla and non-Tesla BEVs separated out

# Read in quantile_bev data, quick formatting

quantiles_bev <- read_parquet(here::here('data', 'quantiles_bev.parquet')) %>% 
    mutate(
        age_years = age_months / 12, 
        powertrain = 'bev', 
        vehicle_type = 'car',
        vehicle = paste(powertrain, vehicle_type, sep = "_"), 
        category = 'other'
    ) %>% 
    filter(between(age_years, AGE_YEARS_MIN, AGE_YEARS_MAX))

df_fig1b <- df_fig1 %>%
    filter(category == 'conventional' & powertrain == 'bev') %>% 
    mutate(
        tesla = 0, 
        powertrain = 'conventional'
    ) %>% 
    select(names(quantiles_bev)) %>% 
    rbind(quantiles_bev) %>% 
    mutate(
        vehicle = case_when(
            powertrain == 'bev' & tesla == 1 ~ 'Tesla BEV',
            powertrain == 'bev' & tesla == 0 ~ 'Non-Tesla BEV',
            TRUE ~ 'Conventional'
    )) 

# Save plot data for reproduction
qsave(df_fig1b, here::here('data', 'df_fig1b.qs'))

fig1b <- df_fig1b %>% 
    ggplot() +  
    geom_ribbon(
        aes(
            x = age_years, 
            ymin = miles25, 
            ymax = miles75, 
            fill = vehicle
        ), 
        alpha = 0.25) +
    geom_line(
        aes(
            x = age_years,
            y = miles50,
            color = vehicle, 
            group = vehicle
        )
    ) +
    scale_y_continuous(
        labels = scales::comma, 
        breaks = seq(0, 125000, 25000) 
    ) +
    scale_fill_manual(values = c(color_cv, color_ev, color_tesla)) +
    scale_color_manual(values = c(color_cv, color_ev, color_tesla)) +
    plot_theme() +
    labs(
        x = "Vehicle age (years)", 
        y = 'Vehicle mileage'
    ) +
    # Add labels
    geom_text(
        data = df_fig1b %>%
            filter(age_months == max(age_months)),
        aes(            
            x = age_years,
            y = miles50,
            label = vehicle, 
            color = vehicle
        ),
        hjust = 0, nudge_x = 0.1, size = 4, 
        family = 'Roboto Condensed'
    ) +
    # Create space for labels on right side
    scale_x_continuous(
        breaks = seq(2, 9, 1), 
        expand = expansion(add = c(0.5, 1.8))
    )
        
ggsave(here::here('figs', 'fig1b.png'), fig1b, width = 6, height = 4)



# FIG 2 ----

# Load data

dt_car <- load_dt_car() %>% 
    select(powertrain, age_years, miles)

# Load estimated results

x1 <- qread(here::here('models', 'b1.qs'))

# Make data frame for annotations and fit line for each powertrain

effects <- count(dt_car, powertrain) %>% 
    mutate(slope = c(
        x1$coefs['age_years'], 
        sum(x1$coefs[c('age_years', 'age_years:powertrainhybrid')]),
        sum(x1$coefs[c('age_years', 'age_years:powertrainphev')]),
        sum(x1$coefs[c('age_years', 'age_years:powertrainbev')])
    )) %>% 
    set_powertrain_levels() %>% 
    mutate(
        label_n = paste0(powertrain_label, " (N = ", scales::comma(n), ")"),
        label_fit = paste0(
            "Fit: ", scales::comma(round(slope*1000)), " miles / year")
    ) 

# Set factor levels
facet_levels <- effects$label_n
levels(effects$powertrain) <- facet_levels
levels(dt_car$powertrain) <- facet_levels

# Save plot data for reproduction
qsave(effects, here::here('data', 'fig2-effects.qs'))

fig2 <- dt_car %>%
    ggplot(aes(x = age_years, y = miles)) +
    geom_scattermore(size = 0.1, alpha = 0.1) + 
    geom_smooth(
        color = 'red',
        linewidth = 0.7,
        method = 'lm',
        se = FALSE,
        fullrange = TRUE
    ) +
    facet_wrap(vars(powertrain)) +
    scale_x_continuous(limits = c(2, 9), breaks = seq(2, 9, 1)) +
    scale_y_continuous(
        limits = c(0, 250),
        breaks = seq(0, 250, 50),
        labels = scales::comma
    ) +
    plot_theme() +
    labs(
        x = "Vehicle age (years)",
        y = 'Vehicle mileage (thousands)'
    ) +
    geom_text(
        data = effects,
        mapping = aes(x = 2.1, y = 240, label = label_fit),
        color = "red",
        hjust = 0,
        family = 'Roboto Condensed'
    )

tictoc::tic()
ggsave(here::here('figs', 'fig2.png'), fig2, width = 7, height = 6)
tictoc::toc()

# Drop objects for space

rm(fig2)
rm(dt_car) 




# FIG 3 -----

# Distribution of cost_per_mile by powertrain

dt_car <- load_dt_car() %>% 
    select(powertrain, cents_per_mile)

fig3 <- dt_car %>%
    mutate(
        powertrain = as.character(str_to_title(powertrain)),
        powertrain = ifelse(
            powertrain  == 'Bev', 'BEV', ifelse(
                powertrain == 'Phev', 'PHEV', powertrain)),
        powertrain = fct_relevel(powertrain, c(
            "BEV", "PHEV", "Hybrid", "Conventional"
        ))
    ) %>% 
    ggplot()+
    geom_boxplot(
        aes(
            x = cents_per_mile,
            y = powertrain), 
        alpha = 0.2,
        width = 0.7,
        fill = color_ev,
        outlier.shape = NA
    ) +
    plot_theme() + 
    labs(
        x = "Cents per mile", 
        y = ""
    ) +
    coord_cartesian(xlim = c(0, 16)) +
    scale_x_continuous(breaks = seq(0, 16, 2))

ggsave(
    here::here('figs', 'fig3.png'), fig3,
    width = 6, height = 3
)



# FIG 4 ----

# Min and max gas prices and ranges

dt <- read_parquet(here::here('data', 'gasoline-prices.parquet'))
gas_price_min <- min(dt$price)
gas_price_max <- max(dt$price)
dt <- load_dt_bev_car() %>% 
    filter(make == 'tesla', model == 'model 3') %>% 
    distinct(model, range) %>% 
    summarise(min = min(range), max = max(range))
range_min <- dt$min
range_max <- dt$max

# First get the mpg and annual mileage for each CV car

cv_mpg <- load_dt_cv_car() %>%
    filter(mpg > 0) %>% 
    group_by(make, model) %>%
    summarise(mpg = round(mean(mpg, na.rm = TRUE))) %>%
    arrange(mpg)
coef_cv <- qread(here::here('models', 'c1.qs'))$coefs
cv_miles <- coef_cv['age_years'] + coef_cv[names(coef_cv)[str_detect(names(coef_cv), "age_years:model")]]
names(cv_miles) <- str_replace(names(cv_miles), "age_years:model", "")
cv_miles <- c(coef_cv['age_years'], cv_miles)
names(cv_miles)[1] <- '3 series'
mileage <- data.frame(
    model = names(cv_miles), 
    mileage = cv_miles
) %>% 
    left_join(cv_mpg, by = 'model') %>% 
    mutate(
        make = ifelse(
            make == 'bmw', 'BMW', ifelse(
                make == 'kia', 'KIA', str_to_title(make))), 
        model = ifelse(
            model == 'es', 'ES', ifelse(
                model == 'mkz', 'MKZ', str_to_title(model))), 
        vehicle = paste0(make, " ", model, " (", mpg, " mpg)")
    ) %>% 
    # Include only select models
    filter(model %in% c(
        '3 Series', 'Accord', "Camry", "Civic", "Cruze", 'Outback', 
        'Spark'
    ))

# Then compute the BEV mileage (based on range)

coef_bev <- qread(here::here('models', 'd1.qs'))$coefs
range_coef <- coef_bev['age_years:range']
mileage_model3 <- coef_bev['age_years'] + coef_bev['age_years:modelmodel 3']
mileage_bev <- data.frame(range = seq(200, 400, 10)) %>% 
    mutate(mileage = mileage_model3 + range*range_coef)

# Compute gas price to meet gap 

cpm_cv <- coef_cv['age_years:cents_per_mile']
result <- list()
for (i in 1:nrow(mileage_bev)) {
    row <- mileage_bev[i,]
    result[[i]] <- mileage %>% 
        mutate(
            range = row$range,
            mileage_bev = row$mileage,
            gap_miles = mileage - mileage_bev, 
            gas_price = mpg*(gap_miles / abs(cpm_cv) / 100),
        )
}
df_fig4 <- do.call(rbind, result) %>% 
    arrange(vehicle)

# Save plot data for reproduction
write_csv(df_fig4, here::here('data', 'fig4.csv'))

fig4 <- df_fig4 %>% 
    ggplot(aes(x = range, y = gas_price)) +
    geom_textline(
        aes(group = vehicle, label = vehicle), 
        family = 'Roboto Condensed', 
        size = 3, hjust = 0.2
    ) +
    plot_theme() + 
    scale_x_continuous(
        breaks = seq(200, 400, 50),
        sec.axis = sec_axis(
            ~(.*range_coef + mileage_model3)*1000,
            labels = scales::comma_format(),
            name = "Annual Mileage",
            breaks = seq(8900, 11000, by = 200)
        )
    ) +
    scale_y_continuous(
        breaks = seq(2.5, 10, 2.5),
        labels = scales::dollar_format()
    ) +
    coord_cartesian(
        xlim = c(200, 400),
        ylim = c(2, 11)
    ) +
    labs(
        x = "Tesla Model 3 Range (miles)",
        y = "Gasoline Price ($/gal)",
        title = "Equivalent Annual Mileage for Tesla Model 3 BEV and Select CVs",
        subtitle = "Each line shows the gasoline price and BEV driving range where the CV and BEV have equal annual mileage,\nwhich increases as BEV driving range increases and gasoline price decreases."
    ) +
    # Add labels
    annotate(
        geom = "rect",
        xmin = range_min, xmax = range_max,
        ymin = gas_price_min, ymax = gas_price_max,
        fill = "grey55", alpha = 0.3
    ) +
    annotate(
        geom = "text",
        x = range_min + 2, y = 2.1,
        label = "Historically observed gas prices\nand Model 3 driving ranges",
        hjust = 0, size = 3,
        family = 'Roboto Condensed'
    )

ggsave(here::here('figs', 'fig4.png'), fig4, width = 6.5, height = 5)
