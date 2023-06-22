Replication materials for study comparing CV and PEV mileage

All code to replicate results are in the "code" folder.

Replication steps:

1. Install R and RStudio.
2. Open the vmt-2023.Rproj file to open RStudio.
3. Open 0-globals.R and set `PATH_DB` to the location of the listings database.
4. Run code scripts 1 through 4.

To perfectly replicate the same results as in our study, you'll need the full database, but this cannot be made public under the license agreement to obtain the data.

Instead, a sample of 1% of the database is included in `/data/db.parquet`. And `PATH_DB` in `0-globals.R` is set to this. The models will not produce the same exact results, but they will be close. The purpose of including this sample is not to be able to perfectly replicate the study results (as this cannot be done without the original data) but rather to make transparent the code used to generate our results for review purposes.

The relevant variables in the full original database can be shared on an individual bases for review purposes only to reproduce the study results. Should this be needed, please contact the study corresponding author at jph (at) gwu (dot) edu.
