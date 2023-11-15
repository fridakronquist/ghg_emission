library(tidyverse)

pop_changes_df <- data.frame(
  Software = c(rep("Excel", 8), rep("R", 3)),
  Alternation = c(rep("Removed", 6), rep("Changed", 2), rep("Changed.", 1), rep("Added", 2)),
  Details = c("Row 1-3, containing 'data source' and 'last updated'", 
              "Column B, containing 'country code'", 
              "Column C, containing 'indicator name'", 
              "Column D, containing 'indicator code'", 
              "Column E-BK + BM-BN, columns containing population data for the years 1960-2018 and 2020-2021'", 
              "Rows 49 rows containing economies, regions, etc., instead of a country*", 
              "Sorted data in alphabetical order based on country name", 
              "Renamed variables using the snake case naming convention (stylized as snake_case)", 
              "Merged population dataframe with the long format greenhouse gas emission dataframe", 
              "Column with greenhouse gas emission per capita in 2019 for each country",
              "Column with greenhouse gas emission per capita in 2019 for each country"))