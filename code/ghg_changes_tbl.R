library(tidyverse)

ghg_changes_df <- data.frame(
  Software = c(rep("Excel", 8), rep("R", 2)),
  Alternation = c(rep("Removed", 4), rep("Added", 2), rep("Changed", 2), rep("Changed.", 2)),
  Details = c("Row 1-3, containing 'data source' and 'last updated'", 
              "Column D, containing 'indicator code'", 
              "Column E-AH + BM-BN, empty columns reserved for the years 1960-1989 and 2020-2021", 
              "Rows 49 rows containing economies, regions, etc., instead of a country*", 
              "Column containing 'Income group' from the sheet 'Metadata -Countries'", 
              "Column containing 'Region' from the sheet 'Metadata - Countries'", 
              "Sorted data in alphabetical order based on country name", 
              "Renamed variables using the snake case naming convention (stylized as snake_case)", 
              "Transformed data from wide to long format", 
              "Changed data type for 'year' from chr to dbl"))


