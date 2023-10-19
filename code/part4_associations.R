library(tidyverse)
library(corrplot)

source("code/data_preparation.R")
source("code/part3_ghg_income_aov.R")

# Import dataframe with population data
pop_df <- read_csv2("data/processed/pop.csv")

# Merge population dataframe with ghg dataframe from part 3
ghg_merge <- merge(ghg_income_aov, pop_df, by="country_name")

# Calculate sum of GHG emission in 2019, and save as a value
ghg_sum_2019 <- ghg_merge %>%
  summarise(ghg_2019 = sum(ghg_emission))

ghg_sum_2019 <- ghg_sum_2019$ghg_2019

# Calculate sum of population in 2019, and save as a value
pop_2019 <- ghg_merge %>%
  summarise(sum_pop = sum(population_2019))

pop_2019 <- pop_2019$sum_pop

# Calculate carbon budget per capita and year until 2050
budget_tot = 500000000
budget_2019 = budget_tot + ghg_sum_2019

pop_2050 = 9.8e9
pop_avg = (pop_2019+pop_2050)/2

years_left = 2050-2019

budget_capita <- budget_2019 / pop_avg / years_left

# Add column with ghg emission per capita in 2019 for each country
ghg_merge <- ghg_merge %>%
  mutate(ghg_capita = ghg_emission / population_2019)

# Add column showing if under or over carbon budget per capita
ghg_merge <- ghg_merge %>%
  mutate(budget_comparison = case_when(ghg_capita > budget_capita ~ "Over",
                                       TRUE ~ "Under"))
# Count countries in each category
ghg_n <-
  ghg_merge %>%
  group_by(income_group, budget_comparison) %>%
  dplyr::summarise(n = n())

# Specify the factor levels to reorder income groups
ghg_n$income_group <- factor(ghg_n$income_group, levels = c("High income", 
                                                            "Upper middle income", 
                                                            "Lower middle income", 
                                                            "Low income"))

# Transform dataframe into a contingency table
ghg_ct <- xtabs(n ~ income_group + budget_comparison, data=ghg_n)

# Specify the factor levels to reorder bars
ghg_n$income_group <- factor(ghg_n$income_group, levels = c("Low income", 
                                                            "Lower middle income", 
                                                            "Upper middle income", 
                                                            "High income"))

ghg_n$budget_comparison = factor(ghg_n$budget_comparison, 
                                 levels = c("Under","Over"), ordered = TRUE)

# Plot bar chart
ggplot(data = ghg_n, aes(x=income_group, y=n, 
                         fill=budget_comparison)) +
  geom_bar(stat = "identity", 
           position = position_dodge()) + 
  labs(title = "Countries Fullfilling Carbon Budget/Capita", 
       subtitle = "Based on income group",
       y = "Frequency", x = "Income Group") + 
  theme_bw() + 
  theme(plot.title = element_text(face="bold", size=14),
        plot.subtitle = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=0),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.position="top",
        strip.text.x = element_text(size = 14)) +
  scale_fill_manual(breaks = c("Over", "Under"),
                    values = c("#9C2B35", "#2C9C46"),
                    labels = c("Over carbon budget", "Under carbon budget")) +
  coord_flip()

ggsave("figures/part4/fullfillment_bar_plot.png", width = 6, height = 4)

# Check so all expected counts > 5. Else Fisher.
chisq.test(ghg_ct, correct = FALSE)$expected

# Perform chi2 test
chisq_test1 <- chisq.test(ghg_ct, correct = FALSE)

# Chi-square Components
round(chisq_test1$residuals^2, 2) 

# Calculate the percentage contribution
contrib1 <- 100*chisq_test1$residuals^2/chisq_test1$statistic

# Make correlation plot
png("figures/part4/corr_plot.png",
    width = 800, height = 800)
corrplot(contrib1, is.cor = FALSE, cl.align.text = "l", tl.col = "black", 
         col= COL1('Purples'), col.lim=c(0, 40), addCoef.col = 'grey50')
dev.off()

# Counts
ghg_ct

# Expected Counts
chisq_test1$expected 

# Calculate the difference between counts and expected counts
ghg_ct - chisq_test1$expected

