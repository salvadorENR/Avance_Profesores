library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RMySQL)

# Function to create the database and table if they do not exist
initialize_database <- function(con) {
  # Create the database if it doesn't exist
  dbExecute(con, "CREATE DATABASE IF NOT EXISTS avance_profesores")
  
  # Use the newly created database
  dbExecute(con, "USE avance_profesores")
  
  # Create the table if it doesn't exist
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS page_data (
      id INT AUTO_INCREMENT PRIMARY KEY,
      Department VARCHAR(255),
      Grade VARCHAR(255),
      Page INT
    )
  ")
}

# Connect to MySQL database using RMySQL
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "",
                 host = "your-online-server-host",
                 port = 3306,
                 user = "your-username",
                 password = "your-password")

# Initialize the database and table
initialize_database(con)

# Verify the connection by executing a simple query
test_connection <- function(con) {
  tryCatch({
    result <- dbGetQuery(con, "SELECT DATABASE()")
    print(result)
    TRUE
  }, error = function(e) {
    print(paste("Error en la conexión:", e$message))
    FALSE
  })
}

if (test_connection(con)) {
  print("Conexión exitosa a la base de datos.")
} else {
  print("Fallo en la conexión a la base de datos.")
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Contenidos de matemática cubiertos"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("department", "Elegir Departamento:", choices = c("Ahuachapán", "Sonsonate", "Santa Ana", "La Libertad", "Chalatenango", "San Salvador", "Cuscatlán", "La Paz", "San Vicente", "Cabañas", "Usulután", "San Miguel", "Morazán", "La Unión")),
      selectInput("grade", "Elegir Grado:", choices = paste("Grado", 1:11)),
      uiOutput("tomo_ui"),
      numericInput("page", "Ingresar Número de Página:", value = 0, min = 0, max = 160),
      actionButton("submit", "Enviar"),
      textOutput("message")
    ),
    
    mainPanel(
      uiOutput("histograms")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Dynamically create "Tomo" UI for Grade 1 and Grade 2
  output$tomo_ui <- renderUI({
    if (input$grade %in% c("Grado 1", "Grado 2")) {
      selectInput("tomo", "Elegir Tomo:", choices = c(1, 2))
    } else {
      NULL
    }
  })
  
  # Submit page data
  observeEvent(input$submit, {
    # Validate page number input
    if (input$page < 0 || input$page > 160) {
      output$message <- renderText("El número de página debe estar entre 0 y 160.")
      return()
    }
    
    # Adjust page number for Tomo 2 in Grade 1 or Grade 2
    adjusted_page <- ifelse(input$grade %in% c("Grado 1", "Grado 2") && input$tomo == 2, input$page + 160, input$page)
    
    # Insert data into the database
    query <- sprintf("INSERT INTO page_data (Grade, Page, Department) VALUES ('%s', %d, '%s')", input$grade, adjusted_page, input$department)
    dbExecute(con, query)
    
    output$message <- renderText("Datos enviados con éxito.")
    
    # Call render_histograms with user's page number and grade
    render_histograms(input$grade, adjusted_page)
  })
  
  # Function to generate histogram plot for a specific grade
  generate_histogram <- function(grade, data, user_page) {
    # Convert Page to numeric
    data$Page <- as.numeric(data$Page)
    
    # Check if there's any missing or non-numeric values in Page
    if (any(is.na(data$Page)) || any(!is.numeric(data$Page))) {
      return(NULL)
    }
    
    avg_page <- mean(data$Page, na.rm = TRUE)
    
    plot <- ggplot(data, aes(x = Page)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
      geom_vline(aes(xintercept = avg_page), color = "red", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = 145, color = "green", linetype = "dashed", linewidth = 1) +
      labs(title = paste("Distribución de Páginas para", grade),
           x = "Número de Página",
           y = "Frecuencia") +
      theme_minimal()
    
    # Add blue line for user's page
    if (grade == input$grade) {
      plot <- plot + geom_vline(xintercept = user_page, color = "blue", linetype = "dashed", linewidth = 1)
    }
    
    return(plot)
  }
  
  # Generate and render histograms for all grades
  render_histograms <- function(selected_grade, user_page) {
    lapply(paste("Grado", 1:11), function(grade) {
      query <- sprintf("SELECT * FROM page_data WHERE Grade = '%s'", grade)
      data <- dbGetQuery(con, query)
      
      output[[paste0("hist_", grade)]] <- renderPlot({
        generate_histogram(grade, data, user_page)
      })
    })
    
    output$histograms <- renderUI({
      lapply(paste("Grado", 1:11), function(grade) {
        plotOutput(paste0("hist_", grade))
      })
    })
  }
  
  # Initial render of histograms
  render_histograms(input$grade, 0)
}

# Run the application 
shinyApp(ui = ui, server = server)
