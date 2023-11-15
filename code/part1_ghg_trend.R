library(tidyverse)

source("code/data_preparation.R")

# Group by year and calculate total ghg emission per year.
ghg_tot <- ghg_long %>%
  group_by(year) %>%
  drop_na(ghg_emission) %>% # droppes rows with NA
  summarise(total_ghg = sum(ghg_emission))

# Linear Model: model_lin: total_ghg~year
model_lin <- lm(total_ghg~year, data=ghg_tot)
summary(model_lin)

# Basic Model Plots: model_lin
png("figures/part1/model_lin_plots.png",
    width = 800, height = 800)
par(mfrow=c(2,2))
plot(model_lin)
dev.off()

# Scatter plot model_lin
ggplot(ghg_tot, aes(x=year, y=total_ghg)) +
  geom_point()
ggsave("figures/part1/model_lin_scatter.png", width = 6, height = 4)

# Try	to	find	a	better	fit

# Linear Model: model_log: log10(total_ghg)~year
model_log <- lm(log10(total_ghg)~year, data=ghg_tot)
summary(model_log)

# Basic Model Plots: model_log
png("figures/part1/model_log_plots.png",
    width = 800, height = 800)
par(mfrow=c(2,2)) 
plot(model_log)
dev.off()

# Scatter plot model_log
ggplot(ghg_tot, aes(x=year, y=log10(total_ghg))) +
  geom_point()
ggsave("figures/part1/model_log_scatter.png", width = 6, height = 4)

# Linear Model: model_sqrt: sqrt(total_ghg)~year
model_sqrt <- lm(sqrt(total_ghg)~year, data=ghg_tot)
summary(model_sqrt)

# Basic Model Plots: model_sqrt
png("figures/part1/model_sqrt_plots.png",
    width = 800, height = 800)
par(mfrow=c(2,2))
plot(model_sqrt)
dev.off()

# Scatter plot model_sqrt
ggplot(ghg_tot, aes(x=year, y=sqrt(total_ghg))) +
  geom_point()
ggsave("figures/part1/model_sqrt_scatter.png", width = 6, height = 4)

# Best fit: model_log
# Scatter plot w regression line
ggplot(ghg_tot, aes(x=year, y=log10(total_ghg))) +
  geom_smooth(method=lm, se=FALSE, color="#D5542C") +
  geom_point(color="#D5542C") +
  labs(title = "Total yearly GHG Emission 1990-2019",
       y = "log10(GHG emission (kt))", 
       x = "Year") +
  theme_bw() + 
  theme(plot.title = element_text(face="bold", size=16),
        plot.subtitle = element_text(size=15),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.margin = margin(0.5,2,1,1, "cm")) +
  scale_y_log10() 

ggsave("figures/part1/regression_plot.png", width = 6, height = 4)


