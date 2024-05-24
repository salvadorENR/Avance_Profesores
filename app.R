library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RSQLite)

# Connect to SQLite database
con <- dbConnect(RSQLite::SQLite(), "textbook_pages.db")

# Create table if it doesn't exist
if (!dbExistsTable(con, "page_data")) {
  dbExecute(con, "CREATE TABLE page_data (
                  Grade TEXT,
                  Page INTEGER,
                  Department TEXT
                )")
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
      actionButton("update", "Actualizar Histogramas"),
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
    dbExecute(con, "INSERT INTO page_data (Grade, Page, Department) VALUES (?, ?, ?)", params = list(input$grade, adjusted_page, input$department))
    
    output$message <- renderText("Datos enviados con éxito.")
  })
  
  # Function to generate histogram plot for a specific grade
  generate_histogram <- function(grade) {
    data <- dbGetQuery(con, "SELECT * FROM page_data WHERE Grade = ?", params = list(grade))
    if (nrow(data) == 0) return(NULL)
    avg_page <- mean(data$Page, na.rm = TRUE)
    
    ggplot(data, aes(x = Page)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
      geom_vline(aes(xintercept = avg_page), color = "red", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = 145, color = "green", linetype = "dashed", linewidth = 1) +
      labs(title = paste("Distribución de Páginas para", grade),
           x = "Número de Página",
           y = "Frecuencia") +
      theme_minimal()
  }
  
  # Generate and render histograms for all grades
  render_histograms <- function() {
    lapply(paste("Grado", 1:11), function(grade) {
      output[[paste0("hist_", grade)]] <- renderPlot({
        generate_histogram(grade)
      })
    })
    
    output$histograms <- renderUI({
      lapply(paste("Grado", 1:11), function(grade) {
        plotOutput(paste0("hist_", grade))
      })
    })
  }
  
  # Update histograms on button click
  observeEvent(input$update, {
    render_histograms()
  })
  
  # Initial render of histograms
  render_histograms()
}

# Run the application 
shinyApp(ui = ui, server = server)
