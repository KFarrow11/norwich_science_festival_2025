library(tidyverse)

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
nsf25 <- read_csv("data/nsf2025_data_collection_1.csv")

glimpse(nsf25)
summary(nsf25)

nsf25$Age <- factor(nsf25$Age, levels = c(
  "Toddler (0-4)", "Young Child (5-8)", "Child (9-12)", "Teen (13-19)", 
  "Young Adult (20-35)", "Adult (36-60)", "Older Adult (61-70)", "Senior (70+)"
))

age_plot <- nsf25 %>%
  ggplot(aes(x=Age, y=Blocks)) +
  geom_jitter() +
  theme_classic()

age_plot <- nsf25 %>%
  ggplot(aes(x = Age, y = Blocks, colour = Color)) +
  geom_jitter(width = 0.35, size = 4) +
  scale_color_identity() + # Use the exact color values provided (no automatic scaling)
  theme_bw() +
  labs(
    title = "Norwich Science Festival 2025 Speedy Skyscrapers",
    subtitle = "Performance of Participants in Building a 10-Block Tower Within 30 Seconds",
    y = "Height (Number of blocks) after 30 seconds",
    x = "Age group",
    caption = "Performance of participants in constructing a 10-block tower within a 30-second interval (n=402).
Each colour denotes a participant's selected favorite color from the following options: red, yellow, pink, green, orange, purple, and blue."
  ) + # Set labels for axes and add figure legend
  theme(
    plot.caption = element_text(hjust = 0, vjust = 1, size = text_size) # Align caption to the bottom left
  ) +
  scale_y_continuous(limits = c(0, 10.5), breaks = 0:10.5) + # Set y-axis limits and breaks
  scale_x_discrete(labels = c(
    "Toddler (0-4)", "Young Child (5-8)", "Child (9-12)", "Teen (13-19)", 
    "Young Adult (20-35)", "Adult (36-60)", "Older Adult (61-70)", "Senior (70+)"
  )) +
  custom_theme()

age_plot
