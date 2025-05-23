# Packages ----
library(shiny)
library(tidyverse)
library(plotly)

# Purple + grey + pink

# CODE ----
ui_nsf25_4 <- fluidPage(
  # Define custom styles/theme for app page
  tags$head(
    tags$style(HTML(" 
      body { 
          background-color: #F0FFFF;  /* Set background color of the body */
          font-family: 'Lucida Sans Typewriter', Monaco, monospace;  /* Set font family for the page */
      }
      .sidebar {
          background-color:#BBC8DE;  /* Set background color for sidebar */
          padding: 20px;  /* Add padding inside the sidebar */
          border: 3px solid black;  /* Add border to sidebar */
          border-radius: 10px;  /* Round corners of the sidebar */
          font-size: 20px;  /* Set font size for sidebar text */
          font-weight: bold;  /* Make sidebar text bold */
      }
      .title {
          color: white;  /* Set text color for title */
          font-size: 40px;  /* Set font size for title */
          font-weight: bold;  /* Make title bold */
          text-align: center;  /* Center-align the title text */
          border: 5px solid #BF0D70;  /* Add border around the title */
          border-radius: 10px;  /* Round corners of the title */
          padding: 10px;  /* Add padding inside the title */
          background-color: #4D34A0;  /* Set background color for the title */
          box-shadow: 2px 2px 8px rgba(0,0,0,0.3);  /* Add shadow to the title */
      }
      h3 {
          color: black;  /* Set text color for h3 elements */
          font-size: 20px;  /* Set font size for h3 elements */
          font-weight: bold;  /* Set text bold */
      }
      p {
          color: black;  /* Set text color for paragraphs */
          font-size: 18px;  /* Set font size for paragraphs */
          font-weight: bold;  /* Make paragraphs bold */
      }
      .action-button {
          background-color:#BF0D70;  /* Set background color for action buttons */
          color: white;  /* Set text color for action buttons */
          border: 3px solid white;  /* Add border for action buttons */
          border-radius: 5px;  /* Round corners of action buttons */
          padding: 10px 20px;  /* Add padding inside action buttons */
          font-size: 20px;  /* Set font size for action buttons */
          font-weight: bold;  /* Set text bold */
      }
    "))
  ),
  
  # Title displayed at the top of the app
  div(class = "title", HTML("Speedy Skyscrapers<br>Does Age Effect Tower Building?")), br(), 
  
  # Define layout of the page with a sidebar and a main panel
  # radioButtons = checkboxes
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      sliderInput("blocks", "How many blocks?", min = 1, max = 10, value = 1), 
      radioButtons("age", "Age", 
                   choices = c("Toddler (0-4)" = "Toddler (0-4)", 
                               "Young Child (5-8)" = "Young Child (5-8)",
                               "Child (9-12)" = "Child (9-12)", 
                               "Teen (13-19)" = "Teen (13-19)",
                               "Young Adult (20-35)" = "Young Adult (20-35)", 
                               "Adult (36-60)" = "Adult (36-60)",
                               "Older Adult (61-70)" = "Older Adult (61-70)", 
                               "Senior (70+)" = "Senior (70+)")),
      radioButtons("color", "Favorite Colour", 
                   choices = c("Red" = "#FF0000", 
                               "Yellow" = "#FFD700", 
                               "Pink" = "#FF0080", 
                               "Green" = "#008000", 
                               "Orange" = "#FFA500", 
                               "Purple" = "#800080", 
                               "Blue" = "#0000FF")),
      actionButton("add", "Plot Data"),
      br(), br(),
    ),
    
    # Main panel to display the scatter plot
    mainPanel(
      width = 8,
      plotlyOutput("scatterPlot", height = "600px", width = "1200px")
    )))

# Server logic to handle user inputs and generate outputs
server <- function(input, output, session) {
  # Define the path for saving the inputs as a CSV
  tower <- "data/nsf2025_data_collection.csv"
  
  # Load existing data if it exists
  if (file.exists(tower)) {
    tower_data <- read.csv(tower)
  } else {
    tower_data <- data.frame(Age = character(), Blocks = numeric(), Color = character())
  }
  
  # Create a reactive dataframe to store user inputs
  data <- reactiveVal(tower_data)
  
  # Update the dataframe when the action button is clicked
  observeEvent(input$add, {
    new_row <- data.frame(Age = input$age, Blocks = input$blocks, Color = input$color, stringsAsFactors = FALSE)
    updated_data <- rbind(data(), new_row)
    data(updated_data)
    # Save updated data to CSV
    write.csv(updated_data, tower, row.names = FALSE)
  })
  
  # Generate and render the scatter plot when the data changes
  output$scatterPlot <- renderPlotly({
    # Retrieve the current data
    plot_data <- data()
    
    # Define the levels and order for Age
    plot_data$Age <- factor(plot_data$Age, levels = c(
      "Toddler (0-4)", "Young Child (5-8)", "Child (9-12)", "Teen (13-19)", 
      "Young Adult (20-35)", "Adult (36-60)", "Older Adult (61-70)", "Senior (70+)"
    ))
    
    # Create the ggplot scatter plot
    p <- ggplot(plot_data, aes(y = Blocks, x = Age, color = Color, text = paste("<br>Age:", Age, "<br>Blocks:", Blocks))) +
      geom_point(size = 6, shape = 21, fill = plot_data$Color) + # Add points to the plot with customized size and color
      scale_color_identity() + # Use the exact color values provided (no automatic scaling)
      theme_minimal(base_size = 15) + # Apply a minimal theme to the plot
      labs(y = "Number of Blocks", x = "Age") + # Set labels for axes
      scale_y_continuous(limits = c(0, 10), breaks = 0:10) + # Set y-axis limits and breaks
      scale_x_discrete(labels = c(
        "Toddler (0-4)" = "Toddler", 
        "Young Child (5-8)" = "YC", 
        "Child (9-12)" = "Child", 
        "Teen (13-19)" = "Teen", 
        "Young Adult (20-35)" = "YA", 
        "Adult (36-60)" = "Adult", 
        "Older Adult (61-70)" = "OA", 
        "Senior (70+)" = "Senior"
      )) + # Rename x-axis labels
      theme(plot.title = element_text(face = "bold", size = 20, color = "#000000"), # Customize plot title style
            axis.title = element_text(face = "bold", size = 16), # Customize axis title style
            legend.position = "none", # Hide legend
            panel.background = element_rect(fill = "#F0FFFF"), # Set panel background color
            plot.background = element_rect(fill = "#F0FFFF"), # Set overall plot background color
            panel.grid.major = element_line(color = "#000000"), # Customize major grid lines
            panel.grid.minor = element_line(color = "#000000")) # Customize minor grid lines
    
    # Convert the ggplot to a Plotly object for interactivity and set custom tooltips
    ggplotly(p, tooltip = "text") %>%
      layout(autosize = TRUE) # Enable autosize
  })
}

# Run the Shiny app
shinyApp(ui = ui_nsf25_4, server = server)

