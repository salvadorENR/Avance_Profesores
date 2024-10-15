library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RMySQL)
library(gridExtra)

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
                 dbname = "sql3720918",
                 host = "sql3.freesqldatabase.com",
                 port = 3306,
                 user = "sql3720918",
                 password = "kHY2iadreR")

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
      selectInput("department", "Elegir Departamento:", choices = c("Seleccione un departamento", "Ahuachapán", "Sonsonate", "Santa Ana", "La Libertad", "Chalatenango", "San Salvador", "Cuscatlán", "La Paz", "San Vicente", "Cabañas", "Usulután", "San Miguel", "Morazán", "La Unión")),
      selectInput("grade", "Elegir Grado:", choices = c("2° Grado", "3° Grado", "4° Grado", "5° Grado", "6° Grado", "7° Grado", "8° Grado", "9° Grado", "1° Año de Bachillerato", "2° Año de Bachillerato")),
      uiOutput("tomo_ui"),
      numericInput("page", "Ingresar Número de Página:", value = 1, min = 1, max = 150),
      actionButton("submit", "Enviar"),
      textOutput("message"),
      actionButton("show_histogram", "Mostrar gráfica"),
      actionButton("update_histogram", "Actualizar la gráfica")
    ),
    
    mainPanel(
      plotOutput("histogram"),
      uiOutput("styled_table")  # Placeholder for showing the styled table
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
  
  # Function to generate histogram plot for a specific grade with page numbers
  generate_histogram <- function(data, grade, user_page) {
    # Mean page (average progress by all teachers)
    mean_page <- mean(data$Page)
    
    # Use the updated expected pages from the Excel file
    expected_page <- switch(grade,
                            "2° Grado" = 115,
                            "3° Grado" = 176,
                            "4° Grado" = 188,
                            "5° Grado" = 186,
                            "6° Grado" = 179,
                            "7° Grado" = 187,
                            "8° Grado" = 183,
                            "9° Grado" = 174,
                            "1° Año de Bachillerato" = 224,
                            "2° Año de Bachillerato" = 218)
    
    # Personal progress page (user input)
    personal_page <- user_page
    
    max_frequency <- max(table(data$Page))  # Max frequency for adjusting label height
    
    plot <- ggplot(data, aes(x = Page)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = NA, alpha = 0.7) +
      
      # Blue dashed line for the mean page with label inside a rectangle (split the label)
      geom_vline(xintercept = mean_page, color = "blue", linetype = "dashed", size = 1) +
      annotate("label", x = mean_page, y = max_frequency * 0.95, label = paste("P.", round(mean_page)), 
               color = "blue", fill = "white", label.size = 0.5, label.r = unit(0, "lines"), label.padding = unit(0.2, "lines"), label.color = "black") +
      annotate("label", x = mean_page, y = 0, label = "Avance promedio", 
               color = "blue", fill = "white", label.size = 0.5, label.r = unit(0, "lines"), label.padding = unit(0.2, "lines"), label.color = "black") +
      
      # Green dashed line for the expected page with label inside a rectangle (split the label)
      geom_vline(xintercept = expected_page, color = "green", linetype = "dashed", size = 1) +
      annotate("label", x = expected_page, y = max_frequency * 0.95, label = paste("P.", expected_page), 
               color = "green", fill = "white", label.size = 0.5, label.r = unit(0, "lines"), label.padding = unit(0.2, "lines"), label.color = "black") +
      annotate("label", x = expected_page, y = 0, label = "Avance esperado", 
               color = "green", fill = "white", label.size = 0.5, label.r = unit(0, "lines"), label.padding = unit(0.2, "lines"), label.color = "black") +
      
      # Red dashed line for the personal page with label inside a rectangle (split the label)
      geom_vline(xintercept = personal_page, color = "red", linetype = "dashed", size = 1) +
      annotate("label", x = personal_page, y = max_frequency * 0.95, label = paste("P.", personal_page), 
               color = "red", fill = "white", label.size = 0.5, label.r = unit(0, "lines"), label.padding = unit(0.2, "lines"), label.color = "black") +
      annotate("label", x = personal_page, y = 0, label = "Avance personal", 
               color = "red", fill = "white", label.size = 0.5, label.r = unit(0, "lines"), label.padding = unit(0.2, "lines"), label.color = "black") +
      
      labs(title = paste("Distribución de Páginas para", grade),
           x = "Número de Página",
           y = "Frecuencia") +
      theme_minimal()
    
    return(plot)
  }
  
  # Show histogram for selected grade
  observeEvent(input$show_histogram, {
    query <- sprintf("SELECT * FROM page_data WHERE Grade = '%s'", input$grade)
    data <- dbGetQuery(con, query)
    
    output$histogram <- renderPlot({
      plot <- generate_histogram(data, input$grade, input$page)
      plot  # Return the plot without the legend
    })
    
    # Prepare data for the table
    mean_page <- mean(data$Page)
    expected_page <- switch(input$grade,
                            "2° Grado" = 115,
                            "3° Grado" = 176,
                            "4° Grado" = 188,
                            "5° Grado" = 186,
                            "6° Grado" = 179,
                            "7° Grado" = 187,
                            "8° Grado" = 183,
                            "9° Grado" = 174,
                            "1° Año de Bachillerato" = 224,
                            "2° Año de Bachillerato" = 218)
    personal_page <- input$page
    
    # Create a simple HTML table without
    table_html <- paste0(
      "<div style='width:50%; text-align:left;'>",
      "<p style='color:blue'>Avance promedio: ", round(mean_page), "</p>",
      "<p style='color:green'>Avance esperado: ", expected_page, "</p>",
      "<p style='color:red'>Avance personal: ", personal_page, "</p>",
      "</div>"
    )
    
    # Render the styled table
    output$styled_table <- renderUI({
      HTML(table_html)
    })
  })
  
  # Update histogram with the most recent data
  observeEvent(input$update_histogram, {
    query <- sprintf("SELECT * FROM page_data WHERE Grade = '%s'", input$grade)
    data <- dbGetQuery(con, query)
    
    output$histogram <- renderPlot({
      plot <- generate_histogram(data, input$grade, input$page)
      plot  # Return the plot without the legend
    })
    
    # Prepare data for the table
    mean_page <- mean(data$Page)
    expected_page <- switch(input$grade,
                            "2° Grado" = 115,
                            "3° Grado" = 176,
                            "4° Grado" = 188,
                            "5° Grado" = 186,
                            "6° Grado" = 179,
                            "7° Grado" = 187,
                            "8° Grado" = 183,
                            "9° Grado" = 174,
                            "1° Año de Bachillerato" = 224,
                            "2° Año de Bachillerato" = 218)
    personal_page <- input$page
    
    # Create a simple HTML table without borders
    table_html <- paste0(
      "<div style='width:50%; text-align:left;'>",
      "<p style='color:blue'>Avance promedio: ", round(mean_page), "</p>",
      "<p style='color:green'>Avance esperado: ", expected_page, "</p>",
      "<p style='color:red'>Avance personal: ", personal_page, "</p>",
      "</div>"
    )
    
    # Render the styled table
    output$styled_table <- renderUI({
      HTML(table_html)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
    