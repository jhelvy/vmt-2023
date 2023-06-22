source(here::here('code', '0-functions.R'))

padds <- tribble(
    ~'file_name', ~'padd',
    'tx', 'texas',
    'ca', 'california',
    'central-atlantic', 'central_atlantic_padd_1b',
    'co', 'colorado',
    'fl', 'florida',
    'gulf-coast', 'gulf_coast',
    'lower-atlantic', 'lower_atlantic_padd_1c',
    'ma', 'massachusetts',
    'midwest', 'midwest',
    'mn', 'minnesota',
    'new-england', 'new_england_padd_1a',
    'ny', 'new_york',
    'oh', 'ohio',
    'rocky-mountain', 'rocky_mountain',
    'wa', 'washington',
    'west-coast-minus-ca', 'west_coast_padd_5_except_california'
)

read_gas_price_file <- function(file, replacement) {
    df <- read_excel(
        file.path(
            here::here('data', 'gasoline-prices'), paste0(file, '.xls')
        ),
        sheet = 'Data 1', 
        skip = 2) %>% 
        janitor::clean_names()
    names(df) <- str_replace(names(df), replacement, "")
    names(df) <- str_replace(
        names(df), 
        "retail_gasoline_prices_dollars_per_gallon", 
        ""
    )
    df <- df %>% 
        select(date, contains("regular")) %>%
        janitor::clean_names() %>% 
        filter(!is.na(date)) %>% 
        mutate(region = file) %>% 
        rename(all = regular_all_formulations)
    return(df)
}

gas_price_list <- list()

for (i in 1:nrow(padds)) {
    
    df <- read_gas_price_file(padds$file_name[i], padds$padd[i])
    temp <- df %>% 
        select(-c("all", "region"))
    if (ncol(temp) == 3) {
        temp <- temp %>% 
            mutate(regular = ifelse(
                is.na(regular_conventional) & is.na(regular_reformulated), NA, ifelse(
                is.na(regular_conventional), regular_reformulated, ifelse(
                is.na(regular_reformulated), regular_conventional, 
                (regular_conventional + regular_reformulated) / 2)))
            ) %>% 
            select(date, regular)
    } else {
        names(temp)[2] <- 'regular'
    }
    df <- df %>% 
        left_join(temp, by = "date") %>% 
        mutate(price = ifelse(is.na(all), regular, all))

    gas_price_list[[i]] <- df %>% 
        select(date, price, region)
}

data <- do.call(rbind, gas_price_list) %>% 
    filter(!is.na(price))

# Link padd to state

states <- read_csv(here::here('data', 'states.csv')) %>% 
    janitor::clean_names()

data <- states %>% 
    full_join(data %>% rename(padd_name = region), by = 'padd_name') %>% 
    select(state, state_code, date, price, padd, padd_name)

# Save 

write_parquet(data, here::here('data', 'gasoline-prices.parquet'))
