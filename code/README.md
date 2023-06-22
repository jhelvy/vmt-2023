
Descriptions of each file:

### `0-data-cleaning`

This folder contains scripts used to prepare the data for analysis, including the following steps:

- `1-gas-price-data.R`: This code merges the gasoline price data files in the "/data/gasoline-prices/" folder into a single file saved in "/data/gasoline-prices.parquet"
- `2-match-fuel-economy-dict.R`: This code matches vehicles from the "/data/vehicles.parquet" file (from fueleconomy.gov) to the dictionary of vehicles extracted from the full database ("/data/dict_raw.parquet")
- `3-match-carsheet-dict.R`: This code matches vehicles from the "/data/carsheet.parquet" file (from carsheet.io) to the dictionary of vehicles extracted from the full database ("/data/dict_raw.parquet")
- `4-combine-dict.R`: This code merges all of the dictionaries together into a single file saved in "/data/dict_final.parquet"
- `5-format-db.R`: This code formats the final database by merging data from the final dictionary and computing the `cents_per_mile` variable.

### `0-functions.R`

This code contains all custom functions used throughout the analysis and is loaded at the top of each script.

### `0-globals.R`

This code contains some global parameters, including hard-coded paths to the databases used in the analyses as well as two variables that define the age ranges of vehicles included in the analyses (`AGE_YEARS_MIN` and `AGE_YEARS_MAX`)

### `1-quantiles.R`

This code computes the quantiles across all listings shown in Figure 1.

### `2-models.R`

This code estimates all models. Model results are stored in the "models" folder, and the full estimated model objects are stored in a "models-temp" folder that is not pushed to this repo as they contain copies of the full database and are very large in size.

### `3-figs.R`

This code creates and saves all figures as png files in the "figs" folder. The code also saves the data frames used to create each figure in the "data" folder.

### `4-tables.R`

This code creates and saves LaTeX code for creating the tables included in the study in the "tables" folder. This code requires the full estimated model objects in the "models-temp" folder to run.
