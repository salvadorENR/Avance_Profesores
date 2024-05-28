library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RMySQL)

# Function to create the database and table if they do not exist
initialize_database <- function(con) {
  # Create the database if it doesn't exist
  #dbExecute(con, "CREATE DATABASE IF NOT EXISTS sql3709755")
  
  # Use the newly created database
  dbExecute(con, "USE sql3709755")
  
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
                 dbname = "sql3709755",
                 host = "sql3.freesqldatabase.com",
                 port = 3306,
                 user = "sql3709755",
                 password = "PPK48mhriE")

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
      selectInput("grade", "Elegir Grado:", choices = c("2° Grado", "3° Grado", "4° Grado", "5° Grado", "6° Grado", "7° Grado", "8° Grado", "9° Grado", "1° Año de Bachillerato", "2° Año de Bachillerato")),
      uiOutput("tomo_ui"),
      numericInput("page", "Ingresar Número de Página:", value = 0, min = 0, max = 160),
      actionButton("submit", "Enviar"),
      actionButton("update", "Actualizar"),
      textOutput("message")
    ),
    
    mainPanel(
      uiOutput("histograms")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Dynamically create "Tomo" UI for 2° Grado
  output$tomo_ui <- renderUI({
    if (input$grade == "2° Grado") {
      selectInput("tomo", "Elegir Tomo:", choices = c(1, 2))
    } else {
      NULL
    }
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
    
    green_line_pos <- switch(grade,
                             "2° Grado" = 137,
                             "3° Grado" = 88,
                             "4° Grado" = 97,
                             "5° Grado" = 91,
                             "6° Grado" = 84,
                             "7° Grado" = 80,
                             "8° Grado" = 86,
                             "9° Grado" = 80,
                             "1° Año de Bachillerato" = 104,
                             "2° Año de Bachillerato" = 106,
                             145) # Default value
    
    plot <- ggplot(data, aes(x = Page)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
      geom_vline(aes(xintercept = avg_page), color = "red", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = green_line_pos, color = "green", linetype = "dashed", linewidth = 1) +
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
    lapply(c("2° Grado", "3° Grado", "4° Grado", "5° Grado", "6° Grado", "7° Grado", "8° Grado", "9° Grado", "1° Año de Bachillerato", "2° Año de Bachillerato"), function(grade) {
      query <- sprintf("SELECT * FROM page_data WHERE Grade = '%s'", grade)
      data <- dbGetQuery(con, query)
      
      output[[paste0("hist_", gsub(" ", "_", gsub("°|Año_de_", "", grade)))]] <- renderPlot({
        generate_histogram(grade, data, user_page)
      })
    })
    
    output$histograms <- renderUI({
      lapply(c("2° Grado", "3° Grado", "4° Grado", "5° Grado", "6° Grado", "7° Grado", "8° Grado", "9° Grado", "1° Año de Bachillerato", "2° Año de Bachillerato"), function(grade) {
        plotOutput(paste0("hist_", gsub(" ", "_", gsub("°|Año_de_", "", grade))))
      })
    })
  }
  
  # Reactive value to store user input
  user_input <- reactiveValues(page = NULL, grade = NULL)
  
  # Function to get the maximum page number for the selected grade
  get_max_page <- function(grade) {
    switch(grade,
           "2° Grado" = 150,
           "3° Grado" = 182,
           "4° Grado" = 192,
           "5° Grado" = 188,
           "6° Grado" = 188,
           "7° Grado" = 188,
           "8° Grado" = 188,
           "9° Grado" = 180,
           "1° Año de Bachillerato" = 224,
           "2° Año de Bachillerato" = 222,
           160) # Default value
  }
  
  # Submit page data
  observeEvent(input$submit, {
    max_page <- get_max_page(input$grade)
    
    # Validate page number input
    if (input$page < 0 || input$page > max_page) {
      output$message <- renderText(sprintf("El número de página debe estar entre 0 y %d.", max_page))
      return()
    }
    
    # Adjust page number for Tomo 2 in 2° Grado
    adjusted_page <- ifelse(input$grade == "2° Grado" && input$tomo == 2, input$page + 150, input$page)
    
    # Store user input
    user_input$page <- adjusted_page
    user_input$grade <- input$grade
    
    # Insert data into the database
    query <- sprintf("INSERT INTO page_data (Grade, Page, Department) VALUES ('%s', %d, '%s')", input$grade, adjusted_page, input$department)
    dbExecute(con, query)
    
    output$message <- renderText("Datos enviados con éxito.")
    
    # Call render_histograms with user's page number and grade
    render_histograms(input$grade, adjusted_page)
  })
  
  # Update histograms with the most recent data
  observeEvent(input$update, {
    if (!is.null(user_input$page) && !is.null(user_input$grade)) {
      render_histograms(user_input$grade, user_input$page)
    } else {
      render_histograms(input$grade, 0)
    }
  })
  
  # Initial render of histograms
  render_histograms(input$grade, 0)
}

# Run the application 
shinyApp(ui = ui, server = server)
