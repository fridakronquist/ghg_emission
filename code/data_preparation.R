library("tidyverse")

# Import data
ghg_wide <- read_csv2("data/processed/ghg.csv")

# Transform data from wide to long format
ghg_long <- gather(ghg_wide, year, ghg_emission, '1990':'2019', factor_key=FALSE)

# Change data type for year from chr to dbl.
ghg_long$year <- as.numeric(ghg_long$year)
