# Define UI and Custom Styles ---- 
ui <- fluidPage(
  # Define custom styles/theme for app page
  tags$head(
    tags$style(HTML(" 
      body { 
          background-color: #C7CED6;  /* Set background color of the body */
          font-family: 'Lucida Sans Typewriter', Monaco, monospace;  /* Set font family for the page */
      }
      .sidebar {
          background-color: #05b8d8;  /* Set background color for sidebar */
          padding: 20px;  /* Add padding inside the sidebar */
          border: 3px solid #000000;  /* Add border to sidebar */
          border-radius: 10px;  /* Round corners of the sidebar */
          font-size: 20px;  /* Set font size for sidebar text */
          font-weight: bold;  /* Make sidebar text bold */
      }
      .title {
          color: #000000;  /* Set text color for title */
          font-size: 40px;  /* Set font size for title */
          font-weight: bold;  /* Make title bold */
          text-align: center;  /* Center-align the title text */
          border: 3px solid #000000;  /* Add border around the title */
          border-radius: 10px;  /* Round corners of the title */
          padding: 10px;  /* Add padding inside the title */
          background-color: #05b8d8;  /* Set background color for the title */
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
          background-color: #FFA500;  /* Set background color for action buttons */
          color: black;  /* Set text color for action buttons */
          border: 3px solid #000000;  /* Add border for action buttons */
          border-radius: 5px;  /* Round corners of action buttons */
          padding: 10px 20px;  /* Add padding inside action buttons */
          font-size: 20px;  /* Set font size for action buttons */
          font-weight: bold;  /* Set text bold */
      }
      .checkbox-group {
          display: flex;
          flex-wrap: wrap;
      }
      .checkbox-group .checkbox-inline {
          width: 33.33%;
      }
    "))
  ),
  
  # Title displayed at the top of the app
  div(class = "title", HTML("Speedy Skyscrapers<br>Does Age Matter in Tower Building?")), br(), # Displays the updated title with styling
  
  # Define layout of the page with a sidebar and a main panel
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",  # Apply custom class for styling the sidebar
      sliderInput("blocks", "How many blocks?", min = 1, max = 10, value = 1), # Slider input for number of blocks
      div(class = "checkbox-group",
          checkboxGroupInput("age", "Age", choices = c("<=5" = "<=5", "6-10" = "6-10", "11-15" = "11-15", "16-19" = "16-19", 
                                                       "20-24" = "20-24", "25-29" = "25-29", "30-39" = "30-39", "40-49" = "40-49", 
                                                       "50-59" = "50-59", "60-69" = "60-69", "70+" = "70+"), inline = TRUE)  # Age categories
      ),
      div(class = "checkbox-group",
          checkboxGroupInput("color", "Favorite Colour", # Pick a colour from options
                             choices = c("Red" = "#FF0000",       # Bold red
                                         "Yellow" = "#FFD700",    # Vibrant yellow
                                         "Pink" = "#FF0080",      # Soft pink
                                         "Green" = "#008000",     # Bright green
                                         "Orange" = "#FFA500",    # Energetic orange
                                         "Purple" = "#800080",    # Deep purple
                                         "Blue" = "#0000FF"),      # Brilliant blue
                             inline = TRUE)
      ),
      actionButton("add", "Plot Data"),  # Button to Plot Data
      br(), br() , # Image displayed in the sidebar
      br(), br(),),
    
    # Main panel to display the scatter plot
    mainPanel(
      width = 8,  # Adjust the main panel width to fill more space
      plotlyOutput("scatterPlot")  # Placeholder for the interactive scatter plot
    )
  ))

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
    new_rows <- expand.grid(Age = input$age, Blocks = input$blocks, Color = input$color, stringsAsFactors = FALSE)
    updated_data <- rbind(data(), new_rows)
    data(updated_data)
    # Save updated data to CSV
    write.csv(updated_data, tower, row.names = FALSE)
  })
  
  # Generate and render the scatter plot when the data changes
  output$scatterPlot <- renderPlotly({
    # Retrieve the current data
    plot_data <- data()
    
    # Create the ggplot scatter plot
    p <- ggplot(plot_data, aes(y = Blocks, x = Age, color = Color, text = paste("<br>Age:", Age, "<br>Blocks:", Blocks))) +
      geom_point(size = 6, shape = 21, fill = plot_data$Color) + # Add points to the plot with customized size and color
      scale_color_identity() + # Use the exact color values provided (no automatic scaling)
      theme_minimal(base_size = 15) + # Apply a minimal theme to the plot
      labs(y = "Number of Blocks", x = "Age") + # Set labels for axes
      theme(plot.title = element_text(face = "bold", size = 20, color = "#000000"), # Customize plot title style
            axis.title = element_text(face = "bold", size = 16), # Customize axis title style
            legend.position = "none", # Hide legend
            panel.background = element_rect(fill = "#C7CED6"), # Set panel background color
            plot.background = element_rect(fill = "#C7CED6"), # Set overall plot background color
            panel.grid.major = element_line(color = "#000000"), # Customize major grid lines
            panel.grid.minor = element_line(color = "#000000")) # Customize minor grid lines
    
    # Convert the ggplot to a Plotly object for interactivity and set custom tooltips
    ggplotly(p, tooltip = "text")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

