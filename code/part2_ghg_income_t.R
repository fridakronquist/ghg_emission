library(tidyverse)
library(Rcmdr) # numSummary
library(cowplot)

source("code/data_preparation.R")
ghg_income_t <-
  ghg_long %>%
  select(country_name, income_group, year, ghg_emission) %>%
  filter(year == "2019",
         income_group %in% c("High income","Low income")) %>%
  drop_na(ghg_emission)
ghg_income_t

# Data normally distributed?
ggplot(data=ghg_income_t, aes(x=ghg_emission, fill=income_group)) +
  geom_histogram(bins=100) +
  facet_wrap(~income_group, ncol = 1) +
  labs(title = "Greenhouse Gas Emission Distribution 2019",
       subtitle = "For high income and low income countries",
       y = "Frequency",
       x = "GHG emission (kt)") +
  theme_bw() +
  theme(plot.title = element_text(face="bold", size=18),
        plot.subtitle = element_text(size=15),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.position = "none") +
  scale_fill_manual(values=c("#5D5292", "#84B0A5"))

ggsave("figures/part2/distribution.png", width = 6, height = 4)

# Data skewed to right -> Wilcoxon’s rank-sum	test
ghg_income_t$income_group <- factor(ghg_income_t$income_group)

# Wilcoxon Test: ghg_emission ~ income_group
Tapply(ghg_emission ~ income_group, median, na.action=na.omit, 
       data=ghg_income_t) # medians by group
wilcox.test(ghg_emission ~ income_group, alternative="two.sided", 
            data=ghg_income_t)

# Numerical summary
numSummary(ghg_income_t[,"ghg_emission", drop=FALSE], groups=ghg_income_t$income_group,
           statistics=c("mean", "sd", "quantiles"), quantiles=c(0, .25, .5, .75, 1))


# Plot boxplot
plot_box_outliers <- ggplot(ghg_income_t, aes(x=reorder(income_group, ghg_emission), y=ghg_emission, color=income_group)) + 
  geom_boxplot() + 
  labs(title = "Greenhouse Gas Emission 2019",
       subtitle = "For high income and low income countries",
       y = "GHG emission (kt)", 
       x = "Income group") +
  theme_bw() + 
  theme(plot.title = element_text(face="bold", size=18),
        plot.subtitle = element_text(size=15),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), 
        legend.position = "none") +
  scale_color_manual(values=c("#5D5292","#84B0A5")) +
  coord_flip() +
  annotate("text", x = 1.9, y = 6001210, label = "USA") +
  annotate("text", x = 1.9, y = 1166510, label = "JPN")

# Same plot without outliers
plot_box_nooutliers <- ggplot(ghg_income_t, aes(x=reorder(income_group, ghg_emission), y=ghg_emission, color=income_group)) + 
  geom_boxplot(outlier.shape = NA) + 
  labs(title = " ",
       y = "GHG emission (kt)", 
       x = "Income group") +
  theme_bw() + 
  theme(plot.title = element_text(face="bold", size=18),
        plot.subtitle = element_text(size=15),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.position = "none") +
  scale_color_manual(values=c("#5D5292","#84B0A5")) +
  coord_flip(ylim=quantile(ghg_income_t$ghg_emission, c(0.1, 0.9))) +
  annotate("text", x = 1.9, y = 6001210, label = "USA") +
  annotate("text", x = 1.9, y = 1166510, label = "JPN")

# both box plots in same figure
plot_grid(plot_box_outliers, plot_box_nooutliers, ncol = 1, nrow = 2)

ggsave("figures/part2/box_plots.png", width = 6, height = 4)
