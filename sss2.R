# Packages ----
library(shiny)
library(tidyverse)
library(plotly)

# custom colours for plot ----
custom_colors <- c(
  "<6" = "#FF4500",  # OrangeRed
  "7" = "#1E90FF",   # DodgerBlue
  "8" = "#3CB371",   # MediumSeaGreen
  "9" = "#8B008B",   # DarkMagenta
  "10" = "#FFD700",  # Gold
  "11" = "#ADFF2F",  # GreenYellow
  "12" = "#8B4513",  # SaddleBrown
  "13-19" = "#2F4F4F", # DarkSlateGray
  "20>" = "#FF1493"  # DeepPink
)

# CODE ----
super2 <- fluidPage(
  # Define custom styles/theme for app page
  tags$head(
    tags$style(HTML(" 
      body { 
          background-color: #F5F5F5;  /* Set background color of the body */
          font-family: 'Lucida Sans Typewriter', Monaco, monospace;  /* Set font family for the page */
      }
      .sidebar {
          color: black; /* Set text color to white */
          background-color: #FBC740; /* lilic background */
          padding: 20px; /* Padding inside the sidebar */
          border: 5px solid #000000; /* Black border */
          border-radius: 10px; /* Rounded corners */
          font-size: 25px; /* Sidebar text size */
          font-weight: bold; /* Bold text */
          height: 100%; /* Match the height dynamically */
          display: flex; /* Optional: to align content vertically */
          flex-direction: column; /* Stack items vertically */
          justify-content: space-between; /* Adjust spacing if needed */
        }

      .title {
          color: black;  /* Set text color for title */
          font-weight: bold;  /* Make title bold */
          text-align: center;  /* Center-align the title text */
          border: 6px solid black;  /* Add border around the title */
          border-radius: 10px;  /* Round corners of the title */
          padding: 20px;  /* Add padding inside the title */
          background-color: #EEB5EB;  /* Set background color for the title */ #EEB5EB
          box-shadow: 2px 2px 8px rgba(0,0,0,0.3);  /* Add shadow to the title */
      }

      h3 {
          color: white;  /* Set text color for h3 elements */
          font-size: 30px;  /* Set font size for h3 elements */
          font-weight: bold;  /* Set text bold */
      }
      p {
          color: white;  /* Set text color for paragraphs */
          font-size: 30px;  /* Set font size for paragraphs */
          font-weight: bold;  /* Make paragraphs bold */
      }
      .action-button {
          background-color: #F652A0;  /* Set background color for action buttons */
          color: white;  /* Set text color for action buttons */
          border: 3px solid black;  /* Add border for action buttons */
          border-radius: 5px;  /* Round corners of action buttons */
          padding: 10px 20px;  /* Add padding inside action buttons */
          font-size: 25px;  /* Set font size for action buttons */
          font-weight: bold;  /* Set text bold */
          border: 2px solid black; /* Add black border to text */
      }
    "))
  ),
  
  # TITLE ----
  # alternative titles ----
  # "Speedy Skyscrapers<br><br>How many blocks can people of different ages stack in 30 seconds before the tower falls?"
  
  # "Speedy Skyscrapers<br><br>Can you beat the clock?<br><br>Stack as many blocks as you can before 30 seconds runs out—and the tower topples!"
  
  # "Speedy Skyscrapers<br><br>How many blocks can different age groups pile up in 30 Seconds before disaster strikes?"
  
  # "Speedy Skyscrapers<br>See how many blocks you can stack in 30 seconds before gravity wins."
  
  # Code to embed title ----  
  div(class = "title", 
      style = "line-height: 1.25; font-family: 'Lucida Sans Typewriter', Monaco, monospace;",
      HTML(
        "<span style='font-size: 40px; font-weight: bold;'> ⏱️ Speedy Skyscrapers 🏢 </span><br><br>
     <span style='font-size: 30px;'>🧱 See how many blocks you can stack in 30 seconds before gravity wins 🌟⬇️</span>"
      )), br(),
  
  
  # Define layout of the page with a sidebar and a main panel
  sidebarPanel(
    class = "sidebar", 
    width = 3,  # Sidebar width
    sliderInput("blocks", "🧱 Number of Blocks", min = 1, max = 10, value = 1), 
    radioButtons("age", "📆 Age️", 
                 choices = c("<6" = "<6", 
                             "7" = "7",
                             "8" = "8", 
                             "9" = "9",
                             "10" = "10",
                             "11" = "11", 
                             "12" = "12", 
                             "13-19" = "13-19", 
                             "20>" = "20>")),
    actionButton("add", "📈 Plot Data 📉"),
    br(), br()
  ),
  
  mainPanel(
    div(
      style = "height: 100%; overflow: auto; 
              border: 5px solid #000000; 
              border-radius: 10px; 
              padding: 30px; 
              margin: 10px; /* Add a margin around the plot container */
              background-color: #F0FFFF;",
      plotlyOutput("scatterPlot", height = "700px", width = "100%")
    )
  )
)

# SERVER [Render scatterplot] ----
# Server logic to handle user inputs and generate outputs 
super_server2 <- function(input, output, session) {
  # Define the path for saving the inputs as a CSV
  tower <- "data/sss2025_data.csv" # Specify data path
  
  # Load existing data if it exists
  if (file.exists(tower)) {
    tower_data <- read.csv(tower)
  } else {
    tower_data <- data.frame(Age = character(), Blocks = numeric(), stringsAsFactors = FALSE)
  }
  
  # Create a reactive dataframe to store user inputs
  data <- reactiveVal(tower_data)
  
  # Update the dataframe when the action button is clicked
  observeEvent(input$add, {
    new_row <- data.frame(Age = input$age, Blocks = input$blocks, stringsAsFactors = FALSE)
    updated_data <- rbind(data(), new_row)
    data(updated_data)
    # Save updated data to CSV
    write.csv(updated_data, tower, row.names = FALSE)
  })
  
  # Generate and render the scatter plot when the data changes
  output$scatterPlot <- renderPlotly({
    # Retrieve the current data
    plot_data <- data()
    
    # SCATTERPLOT CODE ----    
    # Check if the dataset is empty
    if (nrow(plot_data) == 0) {
      # If no data is collected, display an empty plot
      p <- ggplot(plot_data, aes(y = Blocks, x = Age, color = Age, 
                                 text = paste("<br>Age:", Age, "<br>Blocks:", Blocks))) +
        geom_jitter(size = 5) +
        scale_color_manual(values = custom_colors) +
        theme_classic(base_size = 14) +
        labs(
          title = "Relationship between age and tower heights",
          y = "Number of Blocks (HEIGHT)", 
          x = "Age"
        ) +
        scale_y_continuous(
          limits = c(0, 11), 
          breaks = 0:10, 
          expand = c(0, 0) # Prevent gaps
        ) +
        scale_x_discrete(
          limits = c("<6", "7", "8", "9", "10", "11", "12", "13-19", "20>"),
          labels = c(
            "<6" = "<6", "7" = "7", "8" = "8", "9" = "9", 
            "10" = "10", "11" = "11", "12" = "12", "13-19" = "13-19", "20>" = "20>"
          )
        ) +
        theme(
          plot.title = element_text(
            face = "bold", 
            size = 20, 
            color = "#000000", 
            margin = margin(b = 20), # Adds margin below the plot title
            hjust = 0 # Aligns the title to the left
          ),
          axis.title.x = element_text(
            face = "bold", 
            size = 16, 
            margin = margin(t = 30) # Adds margin above the x-axis title
          ),
          axis.title.y = element_text(
            face = "bold", 
            size = 16, 
            margin = margin(r = 30) # Adds margin to the right of the y-axis title
          ),
          axis.text.x = element_text(face = "bold", size = 14),
          axis.text.y = element_text(face = "bold", size = 14),
          legend.position = "none",
          panel.background = element_rect(fill = "#F0FFFF"),
          plot.background = element_rect(fill = "#F0FFFF"),
          panel.grid.major = element_line(color = "grey"),
          panel.grid.minor = element_line(color = "grey")
        )
      
    } else {
      # If data is collected, create the scatter plot
      plot_data$Age <- factor(plot_data$Age, levels = c(
        "<6", "7", "8", "9", "10", "11", "12", "13-19", "20>"
      ))
      
      p <- ggplot(plot_data, aes(y = Blocks, x = Age, color = Age, 
                                 text = paste("<br>Age:", Age, "<br>Blocks:", Blocks))) +
        geom_jitter(size = 5) +
        scale_color_manual(values = custom_colors) +
        theme_classic(base_size = 14) +
        labs(
          title = "Relationship between age and tower heights",
          y = "Number of Blocks (HEIGHT)", 
          x = "Age"
        ) +
        scale_y_continuous(
          limits = c(0, 11), 
          breaks = 0:10, 
          expand = c(0, 0) # Prevent gaps
        ) +
        scale_x_discrete(
          limits = c("<6", "7", "8", "9", "10", "11", "12", "13-19", "20>"),
          labels = c(
            "<6" = "<6", "7" = "7", "8" = "8", "9" = "9", 
            "10" = "10", "11" = "11", "12" = "12", "13-19" = "13-19", "20>" = "20>"
          )
        ) +
        theme(
          plot.title = element_text(
            face = "bold", 
            size = 20, 
            color = "#000000", 
            margin = margin(b = 20), # Adds margin below the plot title
            hjust = 0 # Aligns the title to the left
          ),
          axis.title.x = element_text(
            face = "bold", 
            size = 16, 
            margin = margin(t = 30) # Adds margin above the x-axis title
          ),
          axis.title.y = element_text(
            face = "bold", 
            size = 16, 
            margin = margin(r = 30) # Adds margin to the right of the y-axis title
          ),
          axis.text.x = element_text(face = "bold", size = 14),
          axis.text.y = element_text(face = "bold", size = 14),
          legend.position = "none",
          panel.background = element_rect(fill = "#F0FFFF"),
          plot.background = element_rect(fill = "#F0FFFF"),
          panel.grid.major = element_line(color = "grey"),
          panel.grid.minor = element_line(color = "grey")
        )
      
    }
    
    # Convert the ggplot to a Plotly object for interactivity
    ggplotly(p, tooltip = "text") %>%
      layout(autosize = TRUE)
  })
}

# SHINY APP ----
# Run the Shiny app
shinyApp(ui = super2, server = super_server2)
