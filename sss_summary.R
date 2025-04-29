# PACKAGES ----
library(tidyverse)
library(RColorBrewer) # colour pallets
library(colorBlindness) # colour blindness checker

# SET PLOT SIZINGS ----
# Define your sizes
title <- 20 # title size
subtitle <- 14 # subtitle size
title_size <- 16 # set axis title size
text_size <- 12 # set axis text size

# Custom theme function
custom_theme <- function() {
  theme(
    legend.position = "none",
    plot.title = element_text(size = title, face = "bold", color = "black"),
    plot.subtitle = element_text(size = subtitle, face = "bold", color = "black"),
    axis.text.x = element_text(size = text_size, face = "bold", color = "black"),     # X-axis text 
    axis.text.y = element_text(size = text_size, face = "bold", color = "black"),     # Y-axis text 
    axis.title.x = element_text(size = title_size, face = "bold", color = "black", margin = margin(10, 10, 10, 10)),   # X-axis title 
    axis.title.y = element_text(size = title_size, face = "bold", color = "black", margin = margin(10, 10, 10, 10)))  # Y-axis title
}

# LOAD DATA ----
sss25 <- read_csv("data/sss2025_data.csv")

glimpse(sss25)
summary(sss25)

sss25$Age <- factor(sss25$Age, levels = c(
  "<6", "7", "8", "9", "10", "11", "12", "13-19", "20>"))

age_plot2 <- sss25 %>%
  ggplot(aes(x = Age, y = Blocks, color = Age)) +
  geom_jitter(width = 0.35, size = 3) +
  theme_classic() +
  labs(
    title = "Super Science Saturday 2025: Speedy Skyscrapers",
    subtitle = "Performance of participants in constructing a 12-block tower within a 30-second interval before gravity wins (n=158).",
    y = "Height (Number of blocks after 30 sec)",
    x = "Age group") +
  theme(
    plot.caption = element_text(hjust = 0, vjust = 1, size = text_size) # Align caption to the bottom left
  ) +
  scale_y_continuous(limits = c(0, 12), breaks = 0:12) + # Set y-axis limits and breaks
  scale_x_discrete(labels = c(
    "<6", "7", "8", "9", "10", "11", "12", "13-19", "20>")) +
  custom_theme()

age_plot2

# OUTPUT FIGURE TO FILE
ggsave("figures/sss25_clean_scatter.png", dpi=600)

# flipped plot
age_plot_flip <- sss25 %>%
  ggplot(aes(y = Age, x = Blocks, color = Age)) +
  geom_jitter(width = 0.4, size = 3) +
  theme_classic() +
  labs(
    title = "Super Science Saturday 2025: Speedy Skyscrapers",
    subtitle = "Performance of participants in constructing a 12-block tower within a 30-second interval before gravity wins (n=158).",
    x = "Height (Number of blocks after 30 sec)",
    y = "Age group") +
  theme(
    plot.caption = element_text(hjust = 0, vjust = 1, size = text_size) # Align caption to the bottom left
  ) +
  scale_x_continuous(limits = c(0, 12), breaks = 0:12) + # Set y-axis limits and breaks
  scale_y_discrete(labels = c(
    "<6", "7", "8", "9", "10", "11", "12", "13-19", "20>")) +
  custom_theme()

age_plot_flip

# OUTPUT FIGURE TO FILE
ggsave("figures/sss25_clean_scatter_flip.png", dpi=600)

age_plot_box <- sss25 %>%
  ggplot(aes(x = Age, y = Blocks, color = Age)) +
  geom_boxplot() +
  theme_classic() +
  labs(
    title = "Super Science Saturday 2025: Speedy Skyscrapers",
    subtitle = "Performance of participants in constructing a 12-block tower within a 30-second interval before gravity wins (n=158).",
    y = "Height (Number of blocks after 30 sec)",
    x = "Age group") +
  theme(
    plot.caption = element_text(hjust = 0, vjust = 1, size = text_size) # Align caption to the bottom left
  ) +
  scale_y_continuous(limits = c(0, 12), breaks = 0:12) + # Set y-axis limits and breaks
  scale_x_discrete(labels = c(
    "<6", "7", "8", "9", "10", "11", "12", "13-19", "20>")) +
  custom_theme()

age_plot_box

# OUTPUT FIGURE TO FILE
ggsave("figures/sss25_clean_scatter_box.png", dpi=600)