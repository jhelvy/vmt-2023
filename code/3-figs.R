source(here::here('code', '0-functions.R'))
source(here::here('code', '0-globals.R'))

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
    filter(between(age_years, AGE_YEARS_MIN, AGE_YEARS_MAX))

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
df_fig1a <- rbind(quantiles_other, quantiles_conventional)

# Save plot data for reproduction
write_csv(df_fig1a, here::here('data', 'fig1a.csv'))

# Make figure

df_fig1a <- read_csv(here::here('data', 'fig1a.csv'))

fig1a <- df_fig1a %>%
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

ggsave(here::here('figs', 'fig1a.png'), fig1a, width = 11, height = 3.5)
ggsave(
    here::here('figs', 'fig1a.pdf'),
    fig1a, width = 11, height = 3.5,  device = cairo_pdf
)

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
df_fig1b <- df_fig1a %>%
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
write_csv(df_fig1b, here::here('data', 'fig1b.csv'))

# Make figure 
df_fig1b <- read_csv(here::here('data', 'fig1b.csv'))

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
ggsave(
    here::here('figs', 'fig1b.pdf'), fig1b, width = 6, height = 4,
    device = cairo_pdf
)




# FIG 2 ----

# 2a - cars ----

# Load data

dt_car <- load_dt_car() %>%
    select(powertrain, age_years, miles, make) %>%
    separate_bev_tesla() %>%
    select(-make)

# Load estimated results

x1 <- qread(here::here('models', 'b2.qs'))

# Make data frame for annotations and fit line for each powertrain

effects_2a <- count(dt_car, powertrain) %>%
    mutate(slope = c(
        x1$coefs['age_years'],
        sum(x1$coefs[c('age_years', 'age_years:powertrainhybrid')]),
        sum(x1$coefs[c('age_years', 'age_years:powertrainphev')]),
        sum(x1$coefs[c('age_years', 'age_years:powertrainbev_non_tesla')]),
        sum(x1$coefs[c('age_years', 'age_years:powertrainbev_tesla')])
    )) %>%
    set_powertrain_levels_tesla() %>%
    mutate(
        label_n = paste0(powertrain_label, " (N = ", scales::comma(n), ")"),
        label_fit = paste0(
            "Fit: ", scales::comma(round(slope*1000)), " miles / year")
    )

# Set factor levels
facet_levels <- effects_2a$label_n
levels(effects_2a$powertrain) <- facet_levels
levels(dt_car$powertrain) <- facet_levels

# Save plot data for reproduction
qsave(effects_2a, here::here('data', 'fig2a-effects.qs'))

fig2a <- dt_car %>%
    ggplot(aes(x = age_years, y = miles)) +
    geom_scattermore(size = 0.1, alpha = 0.1) +
    # Dotted line first for extrapolation, solid line for interpolation
    geom_smooth(
        color = 'red',
        linewidth = 0.7,
        method = 'lm',
        se = FALSE,
        fullrange = TRUE,
        linetype = "dotted"
    ) +
    geom_smooth(
        color = 'red',
        linewidth = 0.7,
        method = 'lm',
        se = FALSE,
        fullrange = FALSE
    ) +
    facet_wrap(vars(powertrain), nrow = 1) +
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
        data = effects_2a,
        mapping = aes(x = 2.1, y = 240, label = label_fit),
        color = "red",
        hjust = 0,
        family = 'Roboto Condensed'
    )

tictoc::tic()
ggsave(here::here('figs', 'fig2a.png'), fig2a, width = 12, height = 3)
ggsave(
    here::here('figs', 'fig2a.pdf'), fig2a, width = 12, height = 3,
    device = cairo_pdf
)
tictoc::toc()

# Drop objects for space

rm(fig2a)
rm(dt_car)

# 2b - SUVs ----

# Load data

dt_suv <- load_dt_suv() %>%
    select(powertrain, age_years, miles, make) %>%
    separate_bev_tesla() %>%
    select(-make)

# Load estimated results

x1 <- qread(here::here('models', 'b2s.qs'))

# Make data frame for annotations and fit line for each powertrain

effects_2b <- count(dt_suv, powertrain) %>%
    rbind(data.frame(powertrain = 'phev', n = 0)) %>%
    mutate(slope = c(
        x1$coefs['age_years'],
        sum(x1$coefs[c('age_years', 'age_years:powertrainhybrid')]),
        sum(x1$coefs[c('age_years', 'age_years:powertrainbev_non_tesla')]),
        sum(x1$coefs[c('age_years', 'age_years:powertrainbev_tesla')]),
        0
    )) %>%
    set_powertrain_levels_tesla() %>%
    mutate(
        label_n = paste0(powertrain_label, " (N = ", scales::comma(n), ")"),
        label_fit = paste0(
            "Fit: ", scales::comma(round(slope*1000)), " miles / year")
    ) %>%
    arrange(powertrain_label)

# Set factor levels
facet_levels <- effects_2b$label_n
levels(effects_2b$powertrain) <- facet_levels
levels(dt_suv$powertrain) <- facet_levels
effects_2b$label_fit[3] <- NA

# Save plot data for reproduction
qsave(effects_2b, here::here('data', 'fig2a-effects.qs'))

fig2b <- dt_suv %>%
    ggplot(aes(x = age_years, y = miles)) +
    geom_scattermore(size = 0.1, alpha = 0.1) +
    # Dotted line first for extrapolation, solid line for interpolation
    geom_smooth(
        color = 'red',
        linewidth = 0.7,
        method = 'lm',
        se = FALSE,
        fullrange = TRUE,
        linetype = "dotted"
    ) +
    geom_smooth(
        color = 'red',
        linewidth = 0.7,
        method = 'lm',
        se = FALSE,
        fullrange = FALSE
    ) +
    facet_wrap(vars(powertrain), nrow = 1) +
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
        data = effects_2b,
        mapping = aes(x = 2.1, y = 240, label = label_fit),
        color = "red",
        hjust = 0,
        family = 'Roboto Condensed'
    )

tictoc::tic()
ggsave(
    here::here('figs', 'fig2b.png'), fig2b, width = 12, height = 3
)
ggsave(
    here::here('figs', 'fig2b.pdf'), fig2b, width = 12, height = 3,
    device = cairo_pdf
)
tictoc::toc()

# Drop objects for space

rm(fig2b)
rm(dt_suv)


# FIG 3 -----

# Distribution of cost_per_mile by powertrain

# Here I manually compute the boxplot summary statistics so
# that the data frame can be saved. Followed this guide:
# https://www.sharpsightlabs.com/blog/ggplot-boxplot/

dt_fig3 <- load_dt_car() %>%
    select(powertrain, vehicle_type, cents_per_mile) %>%
    rbind(
        load_dt_suv() %>%
        select(powertrain, vehicle_type, cents_per_mile)
    ) %>%
    group_by(powertrain, vehicle_type) %>%
    mutate(
        median = median(cents_per_mile),
        iqr = IQR(cents_per_mile),
        q25 = fquantile(cents_per_mile, 0.25),
        q75 = fquantile(cents_per_mile, 0.75),
        lower = q25 - 1.5*iqr,
        upper = q75 + 1.5*iqr
    ) %>%
    # Drop outliers
    filter(cents_per_mile > lower, cents_per_mile < upper) %>%
    mutate(
        min = min(cents_per_mile),
        max = max(cents_per_mile)
    ) %>%
    slice(1)

# Save plot data for reproduction
qsave(dt_fig3, here::here('data', 'dt_fig3.qs'))

# dt_fig3 <- qread(here::here('data', 'dt_fig3.qs'))

fig3 <- dt_fig3 %>%
    mutate(
        powertrain = as.character(str_to_title(powertrain)),
        powertrain = ifelse(
            powertrain  == 'Bev', 'BEV', ifelse(
                powertrain == 'Phev', 'PHEV', powertrain)),
        powertrain = as.factor(powertrain),
        powertrain = fct_relevel(powertrain, c(
            "BEV", "PHEV", "Hybrid", "Conventional"
        )),
        vehicle_type = ifelse(vehicle_type == 'car', 'Car', 'SUV')
    ) %>%
    ggplot(aes(y = powertrain)) +
    geom_boxplot(
        aes(
            xlower = q25,
            xupper = q75,
            xmiddle = median,
            xmin = min,
            xmax = max,
            fill = vehicle_type
        ),
        position = position_dodge(preserve = "single"),
        stat = "identity",
        alpha = 0.5,
        width = 0.7,
        size = 0.2,
        outlier.shape = NA
    ) +
    plot_theme() +
    theme(legend.position = 'bottom') +
    scale_fill_manual(values = c(color_cv, color_ev)) +
    coord_cartesian(xlim = c(0, 18)) +
    scale_x_continuous(breaks = seq(0, 18, 2)) +
    labs(
        x = "Cents per mile",
        y = "",
        fill = "Vehicle Type"
    ) +
    geom_label(
        data = data.frame(
            powertrain = 'PHEV', vehicle_type = 'SUV',
            x = 6, label = "No data for PHEV SUVs"
        ),
        mapping = aes(x = x, label = label),
        color = color_ev,
        size = 2,
        family = 'Roboto Condensed',
        nudge_y = 0.2
    )

ggsave(
    here::here('figs', 'fig3.png'), fig3,
    width = 6, height = 3.2
)
ggsave(
    here::here('figs', 'fig3.pdf'), fig3,
    width = 6, height = 3.2, device = cairo_pdf
)


# Figure on non-linear range relationship with annual mileage -----

dt_bev_car <- load_dt_bev_car() %>%
    mutate(
        range_type = ifelse(
            range < 100, 'low', ifelse(
                between(range, 100, 200), 'mid', 'high')),
        my = as.factor(year),
        annual_mileage = miles/age_years
    )
dt_bev_car$range_type <- factor(dt_bev_car$range_type, c(
    'low', 'mid', 'high'))

dt_bev_car %>%
    group_by(range, range_type) %>%
    summarise(
        annual_mileage_mean = mean(annual_mileage),
        annual_mileage_median = median(annual_mileage)
    ) %>%
    pivot_longer(
        values_to = 'value',
        names_to = 'stat',
        cols = starts_with('annual_mileage')
    ) %>%
    separate(stat, into = c('drop1', 'drop2', 'stat')) %>%
    mutate(stat = paste(stat, 'annual miles')) %>%
    ggplot(
        aes(
            x = range,
            y = value,
            color = range_type
        )
    ) +
    geom_point() +
    facet_wrap(vars(stat)) +
    plot_theme() +
    labs(
        x = 'Range',
        y = 'Miles per year',
        color = 'Range type'
    ) +
    theme(legend.position = 'bottom')

ggsave(
    here::here('figs', 'fig-non-linear-range.png'), 
    width = 9, height = 5
)
ggsave(
    here::here('figs', 'fig-non-linear-range.pdf'), 
    width = 9, height = 5, device = cairo_pdf
)
