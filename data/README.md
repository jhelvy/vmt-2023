Files:

- db-sample.parquet

Description:

A sample of 1% of the full listings database.

------------------------------------------------------------------------------

Files:

- quantiles_bev.csv
- quantiles.csv

Description:

A summary of the 25th, 50th, and 75th quantiles of the vehicle miles for the following vehicle_type and powertrain combinations:

vehicle_type | powertrain
-------------|----------
car          | gasoline
car          | hybrid
car          | phev
car          | bev
pickup       | gasoline
suv          | gasoline
suv          | hybrid
suv          | phev
suv          | bev

------------------------------------------------------------------------------

Files:

- model_counts_top.csv

Description:

Counts of the number of make-model listings for each of the
following vehicle_type and powertrain combinations:

vehicle_type: car, suv, pickup
powertrain: conventional, hybrid, phev, bev

The "model_counts_top.csv" file only contains the subset with vehicles that comprise at least 1% of the number of listings

---------------------------------------------------

Files:

- states.csv

Description:

Categorization of US states by region, from https://www.kaggle.com/datasets/omer2040/usa-states-to-region

---------------------------------------------------

Files:

- gasoline-prices.parquet

Folders:

- gasoline-prices

Description:

Monthly gasoline prices in different US regions and states. Original source:
https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_m.htm

Cleaned data file created in ./code/0clean-gas-data.R.

Description of PADDs: https://www.eia.gov/tools/glossary/index.php?id=petroleum+administration+for+defense+district

The PADDs are integrated into the ./data/states.csv file

---------------------------------------------------

Files:

- vehicles.parquet

Description:

Vehicle model fuel economy and range specifications from fueleconomy.gov, available here: https://www.fueleconomy.gov/feg/epadata/vehicles.csv

Detailed data description is also available here: https://www.fueleconomy.gov/feg/ws/

---------------------------------------------------

Files:

- carsheet.parquet

Description:

Vehicle specifications dataset webscraped from carsheet.io. Webscrape code is available at https://github.com/jhelvy/carsheet and was conducted on January 12, 2023.

---------------------------------------------------

Files:

- dict_raw.parquet
- dict_fe.parquet
- dict_carsheet.parquet
- dict_final.parquet

Description:

Dictionaries matching the vehicle year, make, model, trim, powertrain, and vehicle types to data from fueleconomy.gov and carsheet.io. Vehicle fuel economies, ranges, motor efficiencies, and utility factors are primarily from fueleconomy.gov, with supplemented data from carsheet.io where missing.

---------------------------------------------------

Files:

- us_electricity_price.xlsx

Description:

US electricity price from 2012 to 2021. The price is annual average by states in cent/kwh. 
downloaded from EIA, https://www.eia.gov/electricity/sales_revenue_price/

---------------------------------------------------

Files:

- df_fig1.qs
- df_fig1b.qs
- df_fig2_effects.qs
- df_fig4.qs

Description:

Saved data files used to create figures 1 - 4.

---------------------------------------------------

Files:

- pev-ranges.csv

Description:

Hand-checked driving range for select PEV models. These were collected by searching OEM websites for specific trim-level ranges.
