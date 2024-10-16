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

# Function to insert valid data into the database
insert_page_data <- function(grade, department, page) {
  dbExecute(con, sprintf("INSERT INTO page_data (Grade, Department, Page) VALUES ('%s', '%s', %d)", 
                         grade, department, page))
}

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
      numericInput("page", "Ingresar Número de Página:", value = 1, min = 1, max = 150),
      actionButton("submit", "Registrar Progreso"),
      textOutput("message"),
      actionButton("show_histogram", "Mostrar gráfica")
    ),
    
    mainPanel(
      plotOutput("histogram"),
      uiOutput("styled_table")  # Placeholder for the text under the plot
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
  
  # Update the max value of the page number based on the selected grade
  observeEvent(input$grade, {
    max_page <- switch(input$grade,
                       "2° Grado" = 150, "3° Grado" = 182, "4° Grado" = 192, "5° Grado" = 188,
                       "6° Grado" = 188, "7° Grado" = 188, "8° Grado" = 188, "9° Grado" = 180,
                       "1° Año de Bachillerato" = 224, "2° Año de Bachillerato" = 222)
    updateNumericInput(session, "page", value = 1, min = 1, max = max_page)
  })
  
  # Function to generate histogram plot with three vertical lines
  generate_histogram <- function(data, grade, user_page) {
    mean_page <- mean(data$Page, na.rm = TRUE)  # Calculate the mean page number
    
    expected_page <- switch(grade,
                            "2° Grado" = 265,  # Corrected expected page for Segundo Grado
                            "3° Grado" = 176, "4° Grado" = 188, "5° Grado" = 186,
                            "6° Grado" = 179, "7° Grado" = 187, "8° Grado" = 183, "9° Grado" = 174,
                            "1° Año de Bachillerato" = 224, "2° Año de Bachillerato" = 218)
    
    ggplot(data, aes(x = Page)) +
      geom_histogram(binwidth = 1, fill = "skyblue", alpha = 0.7) +
      geom_vline(xintercept = mean_page, color = "blue", linetype = "dashed", size = 1) +
      geom_vline(xintercept = expected_page, color = "#00FF00", linetype = "dashed", size = 1) +
      geom_vline(xintercept = user_page, color = "red", linetype = "dashed", size = 1) +  # Ensure the adjusted page is used here
      labs(title = paste("Distribución de Páginas para", grade),
           x = "Número de Página", y = "Frecuencia") +
      theme_minimal()
  }
  
  # Function to validate and register the progress in the database only if valid
  observeEvent(input$submit, {
    # Get the max page value based on the grade
    max_page <- switch(input$grade,
                       "2° Grado" = 150, "3° Grado" = 182, "4° Grado" = 192, "5° Grado" = 188,
                       "6° Grado" = 188, "7° Grado" = 188, "8° Grado" = 188, "9° Grado" = 180,
                       "1° Año de Bachillerato" = 224, "2° Año de Bachillerato" = 222)
    
    # Validate the page number (must be a positive integer and <= max_page)
    if (input$page <= 0 || input$page %% 1 != 0) {  # Check if page is not a positive integer
      output$message <- renderText("Por favor, introduzca un número de página válido y positivo.")
      return()  # Skip registration if the page number is invalid
    } else if (input$page > max_page) {  # Check if the page number exceeds the max allowed
      output$message <- renderText(paste("El número de página no puede ser mayor a", max_page, "para este grado."))
      return()  # Skip registration if the page number exceeds the max
    }
    
    # Adjust page for Tomo II if applicable (for "Segundo Grado")
    registered_page <- input$page
    if (input$grade == "2° Grado" && input$tomo == 2) {
      registered_page <- registered_page + 150  # Add 150 for Tomo II
    }
    
    # Insert valid data into the database only if department is valid
    if (input$department == "Seleccione un departamento") {
      output$message <- renderText("Por favor, elija un departamento válido.")
    } else {
      insert_page_data(input$grade, input$department, registered_page)  # Insert the adjusted page
      output$message <- renderText("Progreso registrado exitosamente.")
    }
  })
  
  # Show histogram for selected grade
  observeEvent(input$show_histogram, {
    query <- sprintf("SELECT * FROM page_data WHERE Grade = '%s'", input$grade)
    data <- dbGetQuery(con, query)
    
    # Adjust page for Tomo II if applicable (for the vertical red line)
    user_page <- input$page
    if (input$grade == "2° Grado" && input$tomo == 2) {
      user_page <- user_page + 150  # Adjust user page before plotting the red line
    }
    
    output$histogram <- renderPlot({
      if (nrow(data) > 0) {
        generate_histogram(data, input$grade, user_page)  # Use the adjusted user page here
      } else {
        showNotification("No hay datos registrados para este grado.", type = "warning")
      }
    })
    
    # Generate the dynamic text for each line (below the plot)
    mean_page <- mean(data$Page, na.rm = TRUE)
    expected_page <- switch(input$grade,
                            "2° Grado" = 265,  # Corrected expected page for Segundo Grado
                            "3° Grado" = 176, "4° Grado" = 188, "5° Grado" = 186,
                            "6° Grado" = 179, "7° Grado" = 187, "8° Grado" = 183, "9° Grado" = 174,
                            "1° Año de Bachillerato" = 224, "2° Año de Bachillerato" = 218)
    
    # Create the HTML content for the line descriptions with the matching colors
    table_html <- paste0(
      "<div style='text-align:left;'>",
      "<p style='color:blue'>Avance promedio: ", round(mean_page), "</p>",
      "<p style='color:#00FF00'>Avance esperado: ", expected_page, "</p>",
      "<p style='color:red'>Progreso personal: ", user_page, "</p>",
      "</div>"
      
    )
    
    # Render the HTML content with the colored text below the plot
    output$styled_table <- renderUI({
      HTML(table_html)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

      