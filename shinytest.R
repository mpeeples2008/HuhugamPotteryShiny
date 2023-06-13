library(shiny)

attribute_dates <- read.csv("Hohokam_pottery_dates.csv", header = TRUE)

# Define the UI
ui <- fluidPage(
  titlePanel("Hohokam Ceramic Coding App"),
  sidebarLayout(
    sidebarPanel(
      textInput("sample_id", "Sample ID"),
      checkboxGroupInput("vesselform", "Vessel Form",
                         choices = c("12 - Bowl" = 12,
                                     "78 - Everted Jar Rim" = 78,
                                     "179 - Compressed globular body jar with short flared rim" = 179,
                                     "185 - Flare rimmed bowl" = 185,
                                     "186 - Outcurved or flat rimmed bowl, shallow or flat bottomoed" = 186,
                                     "245 - Cauldron" = 245,
                                     "246 - Cauldron with Gila Shoulder" = 246,
                                     "247 - Cauldron, vertical walls" = 247,
                                     "275 - Semi flare-rimmed hemispherical bowl" = 275)),
      
      checkboxGroupInput("incising", "Incising",
                         choices = c("1 - Deep, regular incising" = 1,
                                     "15 - Incising" = 15,
                                     "29 - Incising, discontinuous (not patterned)" = 29,
                                     "157 - Coil based incising" = 157,
                                     "290 - Coil-based incising thickest lines > 4 mm in width" = 290,
                                     "302 - Incising, imitation of coil based incising (very deep, regular, continuous)" = 302)),
      checkboxGroupInput("trailinglines", "Trailing Lines and Exterior Bowl Designs",
                         choices = c("5 - Exterior bowl design (more than trailing lines)" = 5,
                                     "24 - Trailing line spacing < 3cm at rim" = 24,
                                     "26 - Trailing Lines" = 26,
                                     "175 - Trailing lines short < 3 cm with more than 3 lines spacing < 4 cm at rim" = 175,
                                     "345 - Trailing lines present spaced > 3 cm (or unknown) and large solid and thickest line > 2.5 mm" = 345,
                                     "346 - Trailing lines present, umber of trailing lines/bowl < 6" = 346)),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("date_plot"),
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Create a reactive value to store the input data
  data <- reactiveValues(list = NULL)
  
  # Render the interest plot
  output$date_plot <- renderPlot({
    out <- c(input$vesselform, input$incising, input$trailinglines)
    dat_out <- attribute_dates[which(attribute_dates$Number %in% as.numeric(out)), ]
    dat_out[is.na(dat_out)] <- 0
    data$list <- dat_out
    if (length(out) > 0) {
      # Create a plot
      plot(colSums(dat_out[, 3:14]), main = "Date Range",
           xlab = "Date", ylab = "Sum",
           col = "skyblue")
    }
  })
  
  # Function to update the input fields
  updateData <- function() {
    # Clear the input fields
    updateTextInput(session, "sample_id", value = "")
    updateCheckboxGroupInput(session, "vesselform", choices = c("12 - Bowl",
                                                                "78 - Everted Jar Rim",
                                                                "179 - Compressed globular body jar with short flared rim",
                                                                "185 - Flare rimmed bowl",
                                                                "186 - Outcurved or flat rimmed bowl, shallow or flat bottomoed",
                                                                "245 - Cauldron",
                                                                "246 - Cauldron with Gila Shoulder",
                                                                "247 - Cauldron, vertical walls",
                                                                "275 - Semi flare-rimmed hemispherical bowl"))
    updateCheckboxGroupInput(session, "incising", choices = c("1 - Deep, regular incising" = 1,
                                                              "15 - Incising" = 15,
                                                              "29 - Incising, discontinuous (not patterned)" = 29,
                                                              "157 - Coil based incising" = 157,
                                                              "290 - Coil-based incising thickest lines > 4 mm in width" = 290,
                                                              "302 - Incising, imitation of coil based incising (very deep, regular, continuous)"))
    updateCheckboxGroupInput(session, "trailinglines", choices = c("5 - Exterior bowl design (more than trailing lines)",
                                                                   "24 - Trailing line spacing < 3cm at rim",
                                                                   "26 - Trailing Lines",
                                                                   "175 - Trailing lines short < 3 cm with more than 3 lines spacing < 4 cm at rim",
                                                                   "345 - Trailing lines present spaced > 3 cm (or unknown) and large solid and thickest line > 2.5 mm",
                                                                   "346 - Trailing lines present, number of trailing lines/bowl < 6"))
  }
  
  # Event handler for the submit button
  observeEvent(input$submit, {
    # Add the input data to the list
    data$list <- c(data$list, list(
      sample_id = isolate(input$sample_id),
      vesselform = isolate(input$vesselform),
      incising = isolate(input$incising),
      trailinglines = isolate(input$trailinglines)
    ))
    updateData()  # Update the input fields
  })
}

# Run the app
shinyApp(ui = ui, server = server)