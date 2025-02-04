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
library(shiny)
library(tidyverse)
library(plotly)
# ___________________________________________________________________________________________________________________----
# Define UI and Custom Styles ---- 
# UI (User Interface) Setup
ui_nsf25 <- fluidPage(
  # Define custom styles/theme for app page
  tags$head(
    tags$style(HTML(" 
      body { 
          background-color: #C7CED6;  /* Set background color of the body */
          font-family: 'Lucida Console', Monaco, monospace;  /* Set font family for the page */
      }
      .sidebar {
          background-color: #F8D210;  /* Set background color for sidebar */
          padding: 20px;  /* Add padding inside the sidebar */
          border: 3px solid #000000;  /* Add border to sidebar */
          border-radius: 10px;  /* Round corners of the sidebar */
      }
      .title {
          color: #000000;  /* Set text color for title */
          font-size: 40px;  /* Set font size for title */
          font-weight: bold;  /* Make title bold */
          text-align: center;  /* Center-align the title text */
          border: 3px solid #000000;  /* Add border around the title */
          border-radius: 10px;  /* Round corners of the title */
          padding: 10px;  /* Add padding inside the title */
          background-color: #F8D210;  /* Set background color for the title */
          box-shadow: 2px 2px 8px rgba(0,0,0,0.3);  /* Add shadow to the title */
      }
      h3 {
          color: #000000;  /* Set text color for h3 elements */
          font-size: 20px;  /* Set font size for h3 elements */
          font-weight: bold;  /* Set text bold */
      }
      p {
          color: #000000;  /* Set text color for paragraphs */
          font-size: 20px;  /* Set font size for paragraphs */
          font-weight: bold;  /* Make paragraphs bold */
      }
      .action-button {
          background-color: #76B947;  /* Set background color for action buttons */
          color: black;  /* Set text color for action buttons */
          border: 3px solid #000000;  /* Add border for action buttons */
          border-radius: 5px;  /* Round corners of action buttons */
          padding: 10px 20px;  /* Add padding inside action buttons */
          font-size: 20px;  /* Set font size for action buttons */
          font-weight: bold;  /* Set text bold */
      }
    "))
  ),
  
  # Title displayed at the top of the app
  div(class = "title", "60-SECOND SKYSCRAPER!"), br(), # Displays the title "60 SECOND SKYSCRAPER!" with styling
  
  # Define layout of the page with a sidebar and a main panel
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",  # Apply custom class for styling the sidebar
      numericInput("age", "How old are you?", value = 5, min = 1, max = 100), # Numeric input for user's age
      numericInput("blocks", "How many blocks?", value = 0, min = 0, max = 20), # How many blocks standing (1-20)
      selectInput("color", "What's your favourite colour?", # Pick a colour from options
                  choices = c("Pink" = "#F39",  # Bright pink, highly distinguishable
                              "Blue" = "#1E90FF",  # Bright blue, stands out well
                              "Green" = "#81B622",  # Soft green with good contrast
                              "Red" = "#DE0013",  # Intense red, very distinguishable
                              "Purple" = "#800080",  # Deep purple, contrasting with other colors
                              "Orange" = "#FFA500")),  # Vibrant orange, easy to spot
      actionButton("add", "Plot Data"),  # Button to Plot Data
      br(), br() , # Image displayed in the sidebar
      br(), br(),),
    
    # Main panel to display the scatter plot
    mainPanel(
      width = 8,  # Adjust the main panel width to fill more space
      plotlyOutput("scatterPlot")  # Placeholder for the interactive scatter plot
    )
  ))

# server_2 logic to handle user inputs and generate outputs
server_2 <- function(input, output, session) {
  # Generate and render the scatter plot when the data changes
  output$scatterPlot <- renderPlotly({
    # Retrieve the current data
    plot_data <- data()
    
    # Create the ggplot scatter plot
    p <- ggplot(plot_data, aes(y = blocks, x = factor(age), color = color, 
                               text = paste("<br>Age:", age, "<br>Blocks:", blocks))) +
      geom_point(size = 6, shape = 21, fill = plot_data$Color) +  # Add points to the plot with customized size and color
      scale_color_identity() +  # Use the exact color values provided (no automatic scaling)
      theme_minimal(base_size = 15) +  # Apply a minimal theme to the plot
      labs(y = "Number of Blocks", x = "Age") +  # Set labels for axes
      theme(plot.title = element_text(face = "bold", size = 20, color = "#000000"),  # Customize plot title style
            axis.title = element_text(face = "bold", size = 16),  # Customize axis title style
            legend.position = "none",  # Hide legend
            panel.background = element_rect(fill = "#C7CED6"),  # Set panel background color
            plot.background = element_rect(fill = "#C7CED6"),  # Set overall plot background color
            panel.grid.major = element_line(color = "#000000"),  # Customize major grid lines
            panel.grid.minor = element_line(color = "#000000"),  # Customize minor grid lines
            scale_y_continuous(breaks = c(0, 20)))  # Set y-axis breaks
    
    # Convert the ggplot to a Plotly object for interactivity and set custom tooltips
    ggplotly(p, tooltip = "text")
  })
}

# Run the Shiny app
shinyApp(ui = ui_nsf25, server = server_2)

