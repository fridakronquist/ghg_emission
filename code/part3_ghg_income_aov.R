library(tidyverse)
library(Rcmdr) # numSummary

source("code/data_preparation.R")

# Filter out Venezuela as it has not been categorised into an income group
ghg_income_aov <-
  ghg_long %>%
  select(country_name, income_group, year, ghg_emission) %>%
  filter(year == "2019",
         income_group != 0) %>%
  drop_na(ghg_emission)

# Change income group into factor
ghg_income_aov$income_group <- as.factor(ghg_income_aov$income_group)

# Numerical summary
numSummary(ghg_income_aov[,"ghg_emission", drop=FALSE], groups=ghg_income_aov$income_group,
           statistics=c("mean", "sd", "quantiles"), quantiles=c(0, .25, .5, .75, 1))

# One-Way Analysis of Variance (ANOVA): ghg_emission ~ income_group
aov_ghg <- aov(ghg_emission ~ income_group, data=ghg_income_aov)
# Summary of the analysis
summary(aov_ghg)

# Check	if model good	fit
# Plot diagnostic diagrams
png("figures/part3/aov_lm_plots.png",
    width = 800, height = 800)
par(mfrow=c(2,2))
plot(aov_ghg)
dev.off()

# log10 transformation
ghg_income_aov$ghg_emission_log <- log10(ghg_income_aov$ghg_emission)
# One-Way Analysis of Variance (ANOVA): log10(ghg_emission) ~ income_group
aov_ghg_log <- aov(ghg_emission_log ~ income_group, data=ghg_income_aov)
# Summary of the analysis
summary(aov_ghg_log)
# Plot diagnostic diagrams
png("figures/part3/aov_log_plots.png",
    width = 800, height = 800)
par(mfrow=c(2,2))
plot(aov_ghg_log)
dev.off()

# sqrt transformation
ghg_income_aov$ghg_emission_sqrt <- sqrt(ghg_income_aov$ghg_emission)
# One-Way Analysis of Variance (ANOVA): sqrt(ghg_emission) ~ region
aov_ghg_sqrt <- aov(ghg_emission_sqrt ~ income_group, data=ghg_income_aov)
# Summary of the analysis
summary(aov_ghg_sqrt)
# Plot diagnostic diagrams
png("figures/part3/aov_sqrt_plots.png",
    width = 800, height = 800)
par(mfrow=c(2,2))
plot(aov_ghg_sqrt)
dev.off()

# Kruskal Wallis rank sum test
tapply(ghg_emission ~ income_group, FUN=median, na.action=na.omit, data=ghg_income_aov)
# medians by group
kruskal.test(ghg_emission ~ income_group, data=ghg_income_aov)

# Boxplot
# Change order of boxes
ghg_income_aov$income_group <- factor(ghg_income_aov$income_group, 
                                      levels=c("Low income", 
                                               "Lower middle income", 
                                               "Upper middle income", 
                                               "High income"))
ggplot(data=ghg_income_aov, aes(x=income_group, y=ghg_emission, color=income_group)) +
  geom_boxplot() +
  labs(title = "Greenhouse Gas Emission Distribution 2019",
       subtitle = "Depending on income group",
       y = "Frequency",
       x = "GHG emission (kt)") +
  theme_bw() +
  theme(plot.title = element_text(face="bold", size=15),
        plot.subtitle = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.position = "none") +
  scale_color_manual(values=c("#84B0A5","#77919F","#6A7198", "#5D5292")) +
  coord_flip() +
  annotate("text", x = 3.9, y = 1166510, label = "JPN") +
  annotate("text", x = 3.9, y = 6001210, label = "USA") +
  annotate("text", x = 2.9, y = 2476840, label = "RUS") +
  annotate("text", x = 2.9, y = 12705090, label = "CHN") +
  annotate("text", x = 1.9, y = 3394870, label = "IND")

ggsave("figures/part3/box_plot.png", width = 6, height = 4)

# Same plot but without outliers
ggplot(data=ghg_income_aov, aes(x=income_group, y=ghg_emission, color=income_group)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Greenhouse Gas Emission 2019",
       subtitle = "Grouped by income",
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
  scale_color_manual(values=c("#84B0A5","#77919F","#6A7198", "#5D5292")) +
  coord_flip(ylim=quantile(ghg_income_aov$ghg_emission, c(0.1, 0.9)))

ggsave("figures/part3/box_plot_zoomed.png", width = 6, height = 4)
