library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RMySQL)

# Connect to MySQL database using RMySQL
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "sql3720918",
                 host = "sql3.freesqldatabase.com",
                 port = 3306,
                 user = "sql3720918",
                 password = "kHY2iadreR")

# Close the database connection when the app stops
onStop(function() {
  dbDisconnect(con)
})

# Function to insert valid data into the database
insert_page_data <- function(grade, department, page) {
  dbExecute(con, sprintf("INSERT INTO page_data (Grade, Department, Page) VALUES ('%s', '%s', %d)", 
                         grade, department, page))
}

# Final 2023 progress for each grade (defined once)
final_2023_progress <- reactive({
  list(
    "2° Grado" = 268, "3° Grado" = 143, "4° Grado" = 146, "5° Grado" = 143,
    "6° Grado" = 144, "7° Grado" = 138, "8° Grado" = 145, "9° Grado" = 143,
    "1° Año de Bachillerato" = 170, "2° Año de Bachillerato" = 176
  )
})

# Define UI for application
ui <- fluidPage(
  titlePanel("Contenidos de matemática cubiertos"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("department", "Elegir Departamento:", 
                  choices = c("Seleccione un departamento", "Ahuachapán", "Sonsonate", "Santa Ana", "La Libertad", 
                              "Chalatenango", "San Salvador", "Cuscatlán", "La Paz", "San Vicente", "Cabañas", 
                              "Usulután", "San Miguel", "Morazán", "La Unión")),
      selectInput("grade", "Elegir Grado:", 
                  choices = c("2° Grado", "3° Grado", "4° Grado", "5° Grado", "6° Grado", "7° Grado", 
                              "8° Grado", "9° Grado", "1° Año de Bachillerato", "2° Año de Bachillerato")),
      uiOutput("tomo_ui"),
      textOutput("validation_message"),
      numericInput("page", "Ingresar Número de Página:", value = 1, min = 1, max = 150),
      textOutput("message"),
      actionButton("submit", "Registrar Progreso"),
      actionButton("show_histogram", "Mostrar gráfica"),
      actionButton("show_2023_progress", "Mostrar la línea del avance final del 2023")
    ),
    
    mainPanel(
      plotOutput("histogram"),
      uiOutput("styled_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Dynamically create "Tomo" UI for 2° Grado
  output$tomo_ui <- renderUI({
    if (input$grade == "2° Grado") {
      selectInput("tomo", "Elegir Tomo:", choices = c(1, 2))
    }
  })
  
  # Reactive max page based on selected grade
  max_page <- reactive({
    switch(input$grade,
           "2° Grado" = 150, "3° Grado" = 182, "4° Grado" = 192, "5° Grado" = 188,
           "6° Grado" = 188, "7° Grado" = 188, "8° Grado" = 188, "9° Grado" = 180,
           "1° Año de Bachillerato" = 224, "2° Año de Bachillerato" = 222)
  })
  
  # Display a reminder message about correct page number input
  output$validation_message <- renderText({
    "Recuerde que el número de página debe ser un número entero positivo y dentro del rango del libro."
  })
  
  # Show 2023 progress toggle
  show_2023_progress <- reactiveVal(FALSE)
  observeEvent(input$show_2023_progress, {
    show_2023_progress(!show_2023_progress())
  })
  
  # Function to validate and register the progress in the database only if valid
  observeEvent(input$submit, {
    # Validate the page number
    if (input$page <= 0 || input$page %% 1 != 0 || input$page > max_page()) {
      output$message <- renderText(if (input$page <= 0 || input$page %% 1 != 0) {
        "Por favor, introduzca un número de página válido y positivo."
      } else {
        paste("El número de página no puede ser mayor a", max_page(), "para este grado.")
      })
      return()
    }
    
    # Adjust page for Tomo II if applicable
    registered_page <- if (input$grade == "2° Grado" && input$tomo == 2) input$page + 150 else input$page
    
    # Insert data if department is valid
    if (input$department == "Seleccione un departamento") {
      output$message <- renderText("Por favor, elija un departamento válido.")
    } else {
      insert_page_data(input$grade, input$department, registered_page)
      output$message <- renderText("Progreso registrado exitosamente.")
    }
  })
  
  # Show histogram for selected grade
  observeEvent(input$show_histogram, {
    data <- dbGetQuery(con, sprintf("SELECT * FROM page_data WHERE Grade = '%s'", input$grade))
    
    # Adjust user page for Tomo II if applicable
    user_page <- if (input$grade == "2° Grado" && input$tomo == 2) input$page + 150 else input$page
    
    output$histogram <- renderPlot({
      if (nrow(data) > 0) {
        generate_histogram(data, input$grade, user_page, show_2023_progress())
      } else {
        showNotification("No hay datos registrados para este grado.", type = "warning")
      }
    })
    
    # Display summary labels
    mean_page <- round(ifelse(nrow(data) > 0, mean(data$Page, na.rm = TRUE), NA))
    expected_page <- switch(input$grade, "2° Grado" = 265, "3° Grado" = 176, "4° Grado" = 188, "5° Grado" = 186, 
                            "6° Grado" = 179, "7° Grado" = 187, "8° Grado" = 183, "9° Grado" = 174, 
                            "1° Año de Bachillerato" = 224, "2° Año de Bachillerato" = 218)
    final_2023 <- final_2023_progress()[[input$grade]]
    
    # Format labels
    format_page <- function(page) if (page <= 150) paste("Tomo I, Pág.", page) else paste("Tomo II, Pág.", page - 150)
    mean_page_formatted <- if (input$grade == "2° Grado") format_page(mean_page) else paste("Pág.", mean_page)
    expected_page_formatted <- if (input$grade == "2° Grado") format_page(expected_page) else paste("Pág.", expected_page)
    final_2023_formatted <- if (input$grade == "2° Grado") "Tomo II, Pág. 118" else paste("Pág.", final_2023)
    personal_legend <- if (input$grade == "2° Grado") paste("Tomo", input$tomo, ", Pág.", input$page) else paste("Pág.", input$page)
    
    # Display label text below plot
    output$styled_table <- renderUI({
      HTML(paste0(
        "<div style='text-align:left; display: block;'>",
        "<p style='color:blue; margin: 0;'>Avance promedio: ", mean_page_formatted, "</p>",
        "<p style='color:#00FF00; margin: 0;'>Avance esperado: ", expected_page_formatted, "</p>",
        "<p style='color:red; margin: 0;'>Avance personal: ", personal_legend, "</p>",
        "<p style='color:darkorange; margin: 0;'>Avance promedio final del 2023: ", final_2023_formatted, "</p>",
        "</div>"
      ))
    })
  })
}

# Generate histogram with the vertical lines
generate_histogram <- function(data, grade, user_page, show_2023) {
  expected_page <- switch(grade,
                          "2° Grado" = 265, "3° Grado" = 176, "4° Grado" = 188, "5° Grado" = 186,
                          "6° Grado" = 179, "7° Grado" = 187, "8° Grado" = 183, "9° Grado" = 174,
                          "1° Año de Bachillerato" = 224, "2° Año de Bachillerato" = 218)
  
  final_2023 <- final_2023_progress()[[grade]]
  
  plot <- ggplot(data, aes(x = Page)) +
    geom_histogram(binwidth = 1, fill = "skyblue", alpha = 0.7) +
    geom_vline(xintercept = mean(data$Page, na.rm = TRUE), color = "blue", linetype = "dashed", size = 1) +
    geom_vline(xintercept = expected_page, color = "#00FF00", linetype = "dashed", size = 1) +
    geom_vline(xintercept = user_page, color = "red", linetype = "dashed", size = 1)
  
  # Add the final 2023 line if the button is toggled
  if (show_2023) {
    plot <- plot + geom_vline(xintercept = final_2023, color = "darkorange", linetype = "dashed", size = 1)
  }
  
  plot + labs(title = paste("Distribución de Páginas para", grade),
              x = "Número de Página", y = "Frecuencia") +
    theme_minimal()
}

# Run the application
shinyApp(ui = ui, server = server)
