# Define UI and Custom Styles ---------------------------------------------
ui_3 <- fluidPage(
  tags$head(
    tags$style(HTML("
            body {
                background-color: #D8F0F0; 
                font-family: Arial, sans-serif; 
            }
            .sidebar {
                background-color: #7ADCF5; 
                padding: 20px;
                border: 3px solid #19CAF6;
                border-radius: 10px; 
            }
            .title {
                color: #0F55A3; 
                font-size: 40px; 
                font-weight: bold;
                text-align: center; 
                border: 3px solid #19CAF6;  
                border-radius: 10px; 
                padding: 10px; 
                background-color: #7ADCF5;  
                box-shadow: 2px 2px 8px rgba(0,0,0,0.3);  
            }
            h3 {
                color: #4CAF50;  
                font-size: 20px; 
            }
            p {
                color: #0F55A3; 
                font-size: 18px; 
                font-weight: bold; 
            }
            .action-button {
                background-color: #0F55A3; 
                color: white;
                border: none;
                border-radius: 5px; 
                padding: 10px 20px;
                font-size: 16px; 
            }
        "))
  ),
  # Title
  div(class = "title", "60 SECOND SKYSCRAPER"), br(),
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",  
      numericInput("age", "How old are you?", value = 5, min = 1, max = 100),
      numericInput("time", "How fast were you?", value = 0, min = 0),
      selectInput("color", "What's your favourite colour?", 
                  choices = c("Yellow Team" = "#FFD700", 
                              "Blue Team" = "#1E90FF", 
                              "Green Team" = "#4EBA8B", 
                              "Red Team" = "#DE0013")),
      actionButton("add", "Add to plot"),  
      br(), br(),
      img(src = "https://s3-eu-west-1.amazonaws.com/images.linnlive.com/83485047ad86c6e2cdb6a8d751245bb9/d1866711-b5a0-4a1f-9a84-b6546bea7b79.jpg", height = "200px", alt = "Tumbling toy blocks"),  
      br(), br(),
    ),
    # Main panel to display the plot
    mainPanel(
      plotlyOutput("scatterPlot")
    )
  )
)

# Define server logic -----------------------------------------------------
server_3 <- function(input, output, session) {
  # Define the path for saving the inputs as a CSV
  tower <- "data/changethisfilename.csv"          
  #This is where the input data is stored - change the file name to get a fresh graph
  # Load existing data if it exists
  if (file.exists(tower)) {
    tower_data <- read.csv(tower)
  } else {
    tower_data <- data.frame(Age = numeric(), 
                             Time = numeric(), Color = character())
  }
  # Create a reactive dataframe to store user inputs
  data <- reactiveVal(tower_data)
  # Update the dataframe when the action button is clicked
  observeEvent(input$add, {
    if (input$name != "") {  # Check if a name is entered
      new_row <- data.frame(Age = input$age, 
                            Time = input$time, Color = input$color, stringsAsFactors = FALSE)
      updated_data <- rbind(data(), new_row)
      data(updated_data)
      # Save updated data to CSV
      write.csv(updated_data, tower, row.names = FALSE)
    } else {
      showModal(modalDialog(
        title = "Input Error",
        "Please enter your name.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  # Generate the interactive scatter plot
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

# Run the Shiny app
shinyApp(ui = ui_3, server = server_3)

