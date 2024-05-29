library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RMySQL)

# Function to create the database and table if they do not exist
initialize_database <- function(con) {
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
      numericInput("page", "Ingresar Número de Página:", value = 1, min = 1, max = 150),
      actionButton("submit", "Enviar"),
      textOutput("message"),
      actionButton("show_histogram", "Mostrar gráfica"),
      actionButton("update_histogram", "Actualizar la gráfica")
    ),
    
    mainPanel(
      plotOutput("histogram")
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
  
  # Update the max value of the page number based on the selected grade
  observeEvent(input$grade, {
    max_page <- switch(input$grade,
                       "2° Grado" = 150,
                       "3° Grado" = 182,
                       "4° Grado" = 192,
                       "5° Grado" = 188,
                       "6° Grado" = 188,
                       "7° Grado" = 188,
                       "8° Grado" = 188,
                       "9° Grado" = 180,
                       "1° Año de Bachillerato" = 224,
                       "2° Año de Bachillerato" = 222)
    updateNumericInput(session, "page", value = 1, min = 1, max = max_page)
  })
  
  # Function to generate histogram plot for a specific grade
  generate_histogram <- function(data, grade, user_page) {
    ggplot(data, aes(x = Page)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
      geom_vline(xintercept = mean(data$Page), color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = switch(grade,
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
                                     145), color = "green", linetype = "dashed", size = 1) +
      labs(title = paste("Distribución de Páginas para", grade),
           x = "Número de Página",
           y = "Frecuencia") +
      theme_minimal() +
      geom_vline(xintercept = user_page, color = "blue", linetype = "dashed", size = 1)
  }
  
  # Reactive value to manage the visibility of the success and invalid messages
  success_count <- reactiveVal(0)
  invalid_count <- reactiveVal(0)
  
  # Submit page data
  observeEvent(input$submit, {
    max_page <- switch(input$grade,
                       "2° Grado" = 150,
                       "3° Grado" = 182,
                       "4° Grado" = 192,
                       "5° Grado" = 188,
                       "6° Grado" = 188,
                       "7° Grado" = 188,
                       "8° Grado" = 188,
                       "9° Grado" = 180,
                       "1° Año de Bachillerato" = 224,
                       "2° Año de Bachillerato" = 222)
    
    # Validate the page number input
    if (is.null(input$page) || input$page == "" || !is.numeric(input$page) || input$page %% 1 != 0 || input$page < 1 || input$page > max_page) {
      invalid_count(invalid_count() + 1)
      output$message <- renderText(paste("Cantidad de datos incorrectos ingresados:", invalid_count()))
      return()
    }
    
    # Adjust page number for Tomo 2 in 2° Grado
    adjusted_page <- ifelse(input$grade == "2° Grado" && input$tomo == 2, input$page + 150, input$page)
    
    query <- sprintf("INSERT INTO page_data (Grade, Page, Department) VALUES ('%s', %d, '%s')", input$grade, adjusted_page, input$department)
    dbExecute(con, query)
    
    success_count(success_count() + 1)
    output$message <- renderText(paste("Cantidad de datos correctos ingresados:", success_count()))
  })
  
  # Show histogram for selected grade
  observeEvent(input$show_histogram, {
    query <- sprintf("SELECT * FROM page_data WHERE Grade = '%s'", input$grade)
    data <- dbGetQuery(con, query)
    output$histogram <- renderPlot({
      generate_histogram(data, input$grade, input$page)
    })
  })
  
  # Update histogram with the most recent data
  observeEvent(input$update_histogram, {
    query <- sprintf("SELECT * FROM page_data WHERE Grade = '%s'", input$grade)
    data <- dbGetQuery(con, query)
    output$histogram <- renderPlot({
      generate_histogram(data, input$grade, input$page)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
