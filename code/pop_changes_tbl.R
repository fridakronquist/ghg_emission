devtools::install_github(repo="haozhu233/kableExtra", ref="a6af5c0")
library(tidyverse)
library(kableExtra)

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

pop_changes_tbl <- kbl(ghg_changes_df, align = "l") %>%
  kable_paper(full_width = F) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1:2, valign = "top") %>%
  footnote(symbol = "Compared to the World Bank’s list of countries (World bank, 2023c). For full list see appendix A1.") 

