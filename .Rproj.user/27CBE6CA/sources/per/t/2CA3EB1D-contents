nsf25 <- nsf2025_data_collection_1

glimpse(nsf25)
summary(nsf25)

nsf25$Age <- factor(nsf25$Age, levels = c(
  "Toddler (0-4)", "Young Child (5-8)", "Child (9-12)", "Teen (13-19)", 
  "Young Adult (20-35)", "Adult (36-60)", "Older Adult (61-70)", "Senior (70+)"
))

# We can use the recode function from dplyr to rename the categorical variables
nsf25 <- nsf25 %>%
  mutate(Color = case_when(Color == "#FF0000" ~ "red",
                       Color == "#FFD700" ~ "yellow",
                       Color == "#FF0080" ~ "pink",
                       Color == "#008000" ~ "green",
                       Color == "#FFA500" ~ "orange",
                       Color == "#800080" ~ "purple",
                       Color == "#0000FF" ~ "blue"))
age <- nsf25 %>%
    ggplot(aes(x=Age, y=Blocks, colour = Color)) +
    geom_jitter(size = 5, fill = Colour, width = 0.3) + # Add points to the plot with customized size and colour
    theme_classic() +
  labs(y = "Number of Blocks (HEIGHT)", x = "Age") + # Set labels for axes
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) + # Set y-axis limits and breaks
  scale_x_discrete(labels = c(
    "Toddler (0-4)" = "Toddler", 
    "Young Child (5-8)" = "YC", 
    "Child (9-12)" = "Child", 
    "Teen (13-19)" = "Teen", 
    "Young Adult (20-35)" = "YA", 
    "Adult (36-60)" = "Adult", 
    "Older Adult (61-70)" = "OA", 
    "Senior (70+)" = "Senior"))
  
  age
  