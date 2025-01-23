# Learning Object: Tower Building Task ----
# Objective: To build a tower using identical blocks as fast as possible, and measure the time taken by each participant to illustrate variability in task completion speeds.
# Materials: A set of identical building blocks (e.g., LEGO bricks, wooden blocks), Stopwatch or timer, A flat surface for building, A data recording sheet or digital spreadsheet
# Procedure:
# Setup:
#. Ensure all participants have the same number of building blocks.
#. The blocks should be of uniform size and shape to ensure consistency.
# Instructions to Participants: 
#. Your goal is to build a tower as tall as possible using all the blocks in the shortest amount of time.
#. The tower must be stable and stand on its own for at least 10 seconds after completion.
#. You are not allowed to start building until the signal is given.
# Execution:
#. On the signal (e.g., "Ready, Set, Go!"), start building the tower as fast as you can.
#. Use the stopwatch to time each participant from the start signal until they declare their tower complete.
#. Record the time taken for each participant.
# Data Recording:
#. Create a table to record the times. Include columns for participant ID, time taken, and any additional notes (e.g., tower stability, number of blocks used).
# Plotting the Data: Use a spreadsheet software or statistical tool to plot the times on a graph.
# The x-axis should represent the participant IDs, and the y-axis should represent the time taken (in seconds).
# You can use a bar graph, line plot, or scatter plot to visualize the differences in task completion speeds.
# ___________________________________________________________________________________________________________________----
# Packages ----
install.packages("shiny") 
install.packages("tidyverse") 
install.packages("plotly") 
install.packages("gganimate") 
install.packages("gifski")

library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)
library(gganimate)
library(gifski)

# Create a data frame for the blocks
blocks <- data.frame(
  block = rep(1:20, each = 1),
  height = rep(1, 20),
  color = rep(c("#FFD700", "#1E90FF", "#4EBA8B", "#DE0013", "#800080", "#FFA500"), length.out = 20)
)

# Add a cumulative sum for the heights to create the stacking effect 
blocks <- blocks %>% mutate(cumulative_height = cumsum(height)) 

# Create the plot 
p <- ggplot(blocks, aes(x = factor(block), y = cumulative_height, fill = color)) + 
      geom_bar(stat = "identity", width = 0.8) + scale_fill_identity() + 
      theme_classic() + 
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) + 
      transition_states(block, transition_length = 1, 
                        state_length = 1) + 
      enter_fade() + exit_fade() # Save the animation
  
animate(p, nframes = 40, renderer = gifski_renderer("blocks_animation.gif"))

# Define UI and Custom Styles ----

# UI (User Interface) Setup
ui <- fluidPage(
  # Define custom styles/theme for app page
  tags$head(
    tags$style(HTML(" 
      body { 
          background-color: #D8F0F0;  /* Set background color of the body */
          font-family: Arial, sans-serif;  /* Set font family for the page */
      }
      .sidebar {
          background-color: #7ADCF5;  /* Set background color for sidebar */
          padding: 20px;  /* Add padding inside the sidebar */
          border: 3px solid #19CAF6;  /* Add border to sidebar */
          border-radius: 10px;  /* Round corners of the sidebar */
      }
      .title {
          color: #000000;  /* Set text color for title */
          font-size: 40px;  /* Set font size for title */
          font-weight: bold;  /* Make title bold */
          text-align: center;  /* Center-align the title text */
          border: 3px solid #19CAF6;  /* Add border around the title */
          border-radius: 10px;  /* Round corners of the title */
          padding: 10px;  /* Add padding inside the title */
          background-color: #7ADCF5;  /* Set background color for the title */
          box-shadow: 2px 2px 8px rgba(0,0,0,0.3);  /* Add shadow to the title */
      }
      h3 {
          color: #4CAF50;  /* Set text color for h3 elements */
          font-size: 20px;  /* Set font size for h3 elements */
      }
      p {
          color: #0F55A3;  /* Set text color for paragraphs */
          font-size: 20px;  /* Set font size for paragraphs */
          font-weight: bold;  /* Make paragraphs bold */
      }
      .action-button {
          background-color: #0F55A3;  /* Set background color for action buttons */
          color: white;  /* Set text color for action buttons */
          border: none;  /* Remove border for action buttons */
          border-radius: 5px;  /* Round corners of action buttons */
          padding: 10px 20px;  /* Add padding inside action buttons */
          font-size: 20px;  /* Set font size for action buttons */
      }
    "))
  ),
  
  # Title displayed at the top of the app
  div(class = "title", "60 SECOND SKYSCRAPER!"), br(), # Displays the title "60 SECOND SKYSCRAPER!" with styling
  
  # Define layout of the page with a sidebar and a main panel
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",  # Apply custom class for styling the sidebar
      numericInput("age", "How old are you?", value = 5, min = 1, max = 100),  # Numeric input for user's age
      numericInput("blocks", "How many blocks?", value = 0, min = 0),  # Corrected ID for numeric input
      selectInput("color", "What's your favourite colour?", # Pick a colour from options
                  choices = c("Yellow" = "#FFD700",  # Bright yellow, highly distinguishable
                              "Blue" = "#1E90FF",  # Bright blue, stands out well
                              "Green" = "#4EBA8B",  # Soft green with good contrast
                              "Red" = "#DE0013",  # Intense red, very distinguishable
                              "Purple" = "#800080",  # Deep purple, contrasting with other colors
                              "Orange" = "#FFA500")),  # Vibrant orange, easy to spot
      actionButton("add", "Plot Data"),  # Button to Plot Data
      br(), br(),
      img(src = "blocks_animation.gif", height = "400px", alt = "Animated stacking blocks"),  # Replace the image with the animated GIF
      br(), br(),
    ),
    
    # Main panel to display the scatter plot
    mainPanel(
      plotlyOutput("scatterPlot")  # Placeholder for the interactive scatter plot
    )
  ))

# Server logic to handle user inputs and generate outputs
server <- function(input, output, session) {
  # Create a reactive value to store the data for the plot
  data <- reactiveVal(data.frame(Age = numeric(), 
                                 Blocks = numeric(), Color = character(), stringsAsFactors = FALSE))
  
  # When the "Add to plot" button is clicked, update the data
  observeEvent(input$add, {
    # Create a new row from the user inputs
    new_row <- data.frame(Age = input$age, 
                          Blocks = input$blocks, Color = input$color, stringsAsFactors = FALSE)
    
    # Append the new row to the existing data
    data(rbind(data(), new_row))  # Update the reactive value with the new data
  })
  
  # Generate and render the scatter plot when the data changes
  output$scatterPlot <- renderPlotly({
    # Retrieve the current data
    plot_data <- data()
    
    # Create the ggplot scatter plot
    p <- ggplot(plot_data, aes(x = Blocks, y = factor(Age), color = Color, 
                               text = paste("<br>Age:", Age, "<br>Blocks:", Blocks))) +
      geom_point(size = 6, shape = 21, fill = plot_data$Color) +  # Add points to the plot with customized size and color
      scale_color_identity() +  # Use the exact color values provided (no automatic scaling)
      theme_minimal(base_size = 15) +  # Apply a minimal theme to the plot
      labs(y = "Number of Blocks", x = "Age", title = " ") +  # Set labels for axes
      theme(plot.title = element_text(face = "bold", size = 20, color = "#FF6347"),  # Customize plot title style
            axis.title = element_text(face = "bold", size = 16),  # Customize axis title style
            legend.position = "none",  # Hide legend
            panel.background = element_rect(fill = "#D8F0F0"),  # Set panel background color
            plot.background = element_rect(fill = "#D8F0F0"),  # Set overall plot background color
            panel.grid.major = element_line(color = "#18508F"),  # Customize major grid lines
            panel.grid.minor = element_line(color = "#18508F"))  # Customize minor grid lines
    
    # Convert the ggplot to a Plotly object for interactivity and set custom tooltips
    ggplotly(p, tooltip = "text")
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)






# ___________________________________________________________________________________________________________________----

# Packages ----
install.packages("shiny", "tidyverse")
install.packages("plotly")

library(shiny)
library(tidyverse)
library(plotly)
# ___________________________________________________________________________________________________________________----

# Define UI and Custom Styles ----

# UI (User Interface) Setup
ui <- fluidPage(
  # Define custom styles/theme for app page
  tags$head(
    tags$style(HTML(" 
      body { 
          background-color: #D8F0F0;  /* Set background color of the body */
          font-family: Arial, sans-serif;  /* Set font family for the page */
      }
      .sidebar {
          background-color: #7ADCF5;  /* Set background color for sidebar */
          padding: 20px;  /* Add padding inside the sidebar */
          border: 3px solid #19CAF6;  /* Add border to sidebar */
          border-radius: 10px;  /* Round corners of the sidebar */
      }
      .title {
          color: #000000;  /* Set text color for title */
          font-size: 40px;  /* Set font size for title */
          font-weight: bold;  /* Make title bold */
          text-align: center;  /* Center-align the title text */
          border: 3px solid #19CAF6;  /* Add border around the title */
          border-radius: 10px;  /* Round corners of the title */
          padding: 10px;  /* Add padding inside the title */
          background-color: #7ADCF5;  /* Set background color for the title */
          box-shadow: 2px 2px 8px rgba(0,0,0,0.3);  /* Add shadow to the title */
      }
      h3 {
          color: #4CAF50;  /* Set text color for h3 elements */
          font-size: 20px;  /* Set font size for h3 elements */
      }
      p {
          color: #0F55A3;  /* Set text color for paragraphs */
          font-size: 20px;  /* Set font size for paragraphs */
          font-weight: bold;  /* Make paragraphs bold */
      }
      .action-button {
          background-color: #0F55A3;  /* Set background color for action buttons */
          color: white;  /* Set text color for action buttons */
          border: none;  /* Remove border for action buttons */
          border-radius: 5px;  /* Round corners of action buttons */
          padding: 10px 20px;  /* Add padding inside action buttons */
          font-size: 20px;  /* Set font size for action buttons */
      }
    "))
  ),
  
  # Title displayed at the top of the app
  div(class = "title", "60 SECOND SKYSCRAPER!"), br(), # Displays the title "60 SECOND SKYSCRAPER!" with styling
  
  # Define layout of the page with a sidebar and a main panel
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",  # Apply custom class for styling the sidebar
      numericInput("age", "How old are you?", value = 5, min = 1, max = 100),  # Numeric input for user's age
      numericInput("blocks", "How many blocks?", value = 0, min = 0),  # Corrected ID for numeric input
      selectInput("color", "What's your favourite colour?", # Pick a colour from options
                  choices = c("Yellow" = "#FFD700",  # Bright yellow, highly distinguishable
                              "Blue" = "#1E90FF",  # Bright blue, stands out well
                              "Green" = "#4EBA8B",  # Soft green with good contrast
                              "Red" = "#DE0013",  # Intense red, very distinguishable
                              "Purple" = "#800080",  # Deep purple, contrasting with other colors
                              "Orange" = "#FFA500")),  # Vibrant orange, easy to spot
      actionButton("add", "Plot Data"),  # Button to Plot Data
      br(), br(),
      img(src = "https://thumbs.dreamstime.com/b/very-high-tower-1056417.jpg", height = "400px", alt = "Tumbling toy blocks"),# Image displayed in the sidebar
      br(), br(),
    ),
    
    # Main panel to display the scatter plot
    mainPanel(
      plotlyOutput("scatterPlot")  # Placeholder for the interactive scatter plot
    )
  ))

# Server logic to handle user inputs and generate outputs
server <- function(input, output, session) {
  # Create a reactive value to store the data for the plot
  data <- reactiveVal(data.frame(Age = numeric(), 
                                 Blocks = numeric(), Color = character(), stringsAsFactors = FALSE))
  
  # When the "Add to plot" button is clicked, update the data
  observeEvent(input$add, {
    # Create a new row from the user inputs
    new_row <- data.frame(Age = input$age, 
                          Blocks = input$blocks, Color = input$color, stringsAsFactors = FALSE)
    
    # Append the new row to the existing data
    data(rbind(data(), new_row))  # Update the reactive value with the new data
  })
  
  # Generate and render the scatter plot when the data changes
  output$scatterPlot <- renderPlotly({
    # Retrieve the current data
    plot_data <- data()
    
    # Create the ggplot scatter plot
    p <- ggplot(plot_data, aes(x = Blocks, y = factor(Age), color = Color, 
                               text = paste("<br>Age:", Age, "<br>Blocks:", Blocks))) +
      geom_point(size = 6, shape = 21, fill = plot_data$Color) +  # Add points to the plot with customized size and color
      scale_color_identity() +  # Use the exact color values provided (no automatic scaling)
      theme_minimal(base_size = 15) +  # Apply a minimal theme to the plot
      labs(y = "Number of Blocks", x = "Age", title = " ") +  # Set labels for axes
      theme(plot.title = element_text(face = "bold", size = 20, color = "#FF6347"),  # Customize plot title style
            axis.title = element_text(face = "bold", size = 16),  # Customize axis title style
            legend.position = "none",  # Hide legend
            panel.background = element_rect(fill = "#D8F0F0"),  # Set panel background color
            plot.background = element_rect(fill = "#D8F0F0"),  # Set overall plot background color
            panel.grid.major = element_line(color = "#18508F"),  # Customize major grid lines
            panel.grid.minor = element_line(color = "#18508F"))  # Customize minor grid lines
    
    # Convert the ggplot to a Plotly object for interactivity and set custom tooltips
    ggplotly(p, tooltip = "text")
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)

