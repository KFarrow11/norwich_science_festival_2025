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
nsf25 <- read_csv("data/nsf2025_data_collection_1.csv")

glimpse(nsf25)
summary(nsf25)

nsf25$Age <- factor(nsf25$Age, levels = c(
  "Toddler (0-4)", "Young Child (5-8)", "Child (9-12)", "Teen (13-19)", 
  "Young Adult (20-35)", "Adult (36-60)", "Older Adult (61-70)", "Senior (70+)"
))

nsf25$Color <- factor(nsf25$Color)

age_plot <- nsf25 %>%
  ggplot(aes(x = Age, y = Blocks, colour = Color)) +
  geom_jitter(width = 0.35, size = 4) +
  scale_color_identity() + # Use the exact color values provided (no automatic scaling)
  theme_bw() +
  labs(
    title = "Norwich Science Festival 2025: Speedy Skyscrapers",
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

# OUTPUT FIGURE TO FILE
ggsave("figures/nsf25_clean_scatter.png", dpi=600)


# Sum of participants per Age group ----
unique(nsf25$Age)
summary(nsf25$Age)
nsf25

# Create the data frame
nsf25b <- data.frame(
  AgeGroup = c("Toddler (0-4)", "Young Child (5-8)", "Child (9-12)", "Teen (13-19)", 
               "Young Adult (20-35)", "Adult (36-60)", "Older Adult (61-70)", "Senior (70+)"),
  participants = c(48, 141, 100, 22, 27, 47, 8, 9))

nsf25b
# Convert AgeGroup to factor with specified levels
nsf25b$AgeGroup <- factor(nsf25b$AgeGroup, 
                          levels = c("Toddler (0-4)", "Young Child (5-8)", 
                                     "Child (9-12)", "Teen (13-19)", 
                                     "Young Adult (20-35)", "Adult (36-60)", 
                                     "Older Adult (61-70)", "Senior (70+)"))

# Create the bar plot
participants_plot <- nsf25b %>%
  ggplot(aes(x = AgeGroup, y = participants)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Norwich Science Festival 2025: Speedy Skyscrapers",
    subtitle = "Distribution of participants across age groups.",
    x = "Age Group",
    y = "Number of Participants (n=402)",
    caption = "The 'Young Child' cohort (5-8 years) exhibits the highest number of participants (n=141), whereas the 'Older Adult' (61-70 years) and 'Senior' (70+ years) cohorts exhibit the lowest (8, 9).") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 10)) + # Set y-axis limits and breaks
  theme(
    plot.caption = element_text(hjust = 0, vjust = 1, size = text_size), # Align caption to the bottom left
    axis.text.x = element_text(size = text_size, angle = 45, hjust = 1)) +
  custom_theme()

participants_plot
    

colour <- c("#E41A1C",  # Red
            "#377EB8",  # Blue
            "#4DAF4A",  # Green
            "#984EA3",  # Purple
            "#FF7F00",  # Orange
            "#FFFF33",  # Yellow
            "#A65628",  # Brown
            "#999999")  # Gray


participants_plot2 <- nsf25b %>%
  ggplot(aes(x = AgeGroup, y = participants)) +
  geom_bar(stat = "identity", aes(fill = factor(AgeGroup)), color = "black") +
  scale_fill_manual(values = colour, guide = FALSE) +  # Remove the legend
  labs(
    title = "Norwich Science Festival 2025: Speedy Skyscrapers",
    subtitle = "Distribution of participants across age groups.",
    x = "Age Group",
    y = "Number of Participants (n=402)",
    caption = "The 'Young Child' cohort (5-8 years) exhibits the highest number of participants (n=141), whereas the 'Older Adult' (61-70 years) and 'Senior' (70+ years) cohorts exhibit the lowest (8, 9).") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 10)) + # Set y-axis limits and breaks
  theme(
    plot.caption = element_text(hjust = 0, vjust = 1, size = text_size), # Align caption to the bottom left
    axis.text.x = element_text(size = text_size, angle = 45, hjust = 1)) +
  custom_theme()

participants_plot2
colorBlindness::cvdPlot() # will automatically run on the last plot you made

# RcolorBrewer palettes
colourbrewer <- brewer.pal(n = 8, name = "Dark2")

participants_plot3 <- nsf25b %>%
  ggplot(aes(x = AgeGroup, y = participants)) +
  geom_bar(stat = "identity", aes(fill = factor(AgeGroup)), color = "black") +
  scale_fill_manual(values = colourbrewer, guide = FALSE) +  # Remove the legend
  labs(
    title = "Number of Participants by Age Group",
    subtitle = "This bar chart illustrates the distribution of participants across various age groups.",
    x = "Age Group",
    y = "Number of Participants (n=402)",
    caption = "The 'Young Child' cohort (5-8 years) exhibits the highest number of participants (n=141), whereas the 'Older Adult' (61-70 years) and 'Senior' (70+ years) cohorts exhibit the lowest (8, 9).") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 10)) + # Set y-axis limits and breaks
  theme(
    plot.caption = element_text(hjust = 0, vjust = 1, size = 14), # Align caption to the bottom left
    axis.text.x = element_text(size = text_size)) +
  custom_theme()

participants_plot3

colorBlindness::cvdPlot() # will automatically run on the last plot you made

# Favorite color ----
summary(nsf25$Color)
summary(nsf25$Color, nsf25$Age)

# Create the data frame
favorite_colour <- data.frame(
color <- c("#0000FF", "#008000", "#800080", "#FF0000", "#FF0080", "#FFA500", "#FFD700"),
popular_col <- c(99, 65, 72, 58, 56, 31, 21) )


# Group by AgeGroup and count occurrences of each Color
color_popularity <- nsf25 %>%
  group_by(Age, Color) %>%
  summarize(count = n())
color_popularity

# Find the most popular color for each age group
most_popular_color <- color_popularity %>%
  group_by(Age) %>%
  slice_max(count, n = 1)
most_popular_color

# Create the dataframe
pop_colour <- data.frame(
  Age = c("Toddler (0-4)", "Young Child (5-8)", "Child (9-12)", "Child (9-12)", "Teen (13-19)",
          "Young Adult (20-35)", "Adult (36-60)", "Older Adult (61-70)", "Senior (70+)"),
  Color = c("#6699FF", "#6699FF", "#6699FF", "#B266B2", "#B266B2", "#66B266", "#6699FF", "#66B266", "#B266B2"),
  Count = c(12, 37, 22, 22, 6, 6, 15, 3, 4))

Color_pop = c("#0000FF", "#0000FF", "#0000FF", "#800080", "#800080", "#008000", "#0000FF", "#008000", "#800080")

Color_pop2 <- c("#6699FF", "#6699FF", "#6699FF", "#B266B2", "#B266B2", "#66B266", "#6699FF", "#66B266", "#B266B2")


# Convert AgeGroup to factor with specified levels
pop_colour$Age <- factor(pop_colour$Age, 
                          levels = c("Toddler (0-4)", "Young Child (5-8)", 
                                     "Child (9-12)", "Teen (13-19)", 
                                     "Young Adult (20-35)", "Adult (36-60)", 
                                     "Older Adult (61-70)", "Senior (70+)"))

# Create the bar plot
pop_colour_plot <- pop_colour %>%
  ggplot(aes(x = Age, y = Count, fill = Color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  theme_classic() +
  custom_theme() +
  labs(
    title = "Population Distribution by Age Group",
    subtitle = "Colours represent different categories",
    caption = "Data Source: Example Dataset") +
  theme(legend.position = "none")
pop_colour_plot


# plot 2
# Separate the "Child (9-12)" category
child_data <- data.frame(
  Age = c("Child (9-12)", "Child (9-12)"),
  Color = c("#6699FF", "#B266B2"),
  Count = c(22, 22))

# Combine the two dataframes
pop_colour <- pop_colour %>%
  filter(Age != "Child (9-12)") %>%
  bind_rows(child_data)

pop_colour
summary(pop_colour)

# Convert AgeGroup to factor with specified levels
pop_colour$Age <- factor(pop_colour$Age, 
                         levels = c("Toddler (0-4)", "Young Child (5-8)", 
                                    "Child (9-12)", "Teen (13-19)", 
                                    "Young Adult (20-35)", "Adult (36-60)", 
                                    "Older Adult (61-70)", "Senior (70+)"))

# Plot
pop_colour_plot <- ggplot(pop_colour, aes(x = Age, y = Count, fill = Color)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_identity() +
  theme_classic() +
  custom_theme() +
  labs(
    title = "Norwich Science Festival 2025: Speedy Skyscrapers",
    subtitle = "Favorite Colour by Age Group",
    caption = "There was a significant preference for the color blue among children under 8."
  ) +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0, vjust = 1, size = 14), # Align caption to the bottom left
          axis.text.x = element_text(size = text_size)) +
  custom_theme()

pop_colour_plot
