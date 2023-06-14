library(shiny)
library(shinyWidgets)
library(ggplot2)

attribute_dates <- read.csv("Hohokam_pottery_dates.csv", header = TRUE)

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      .sidebar-panel {
        overflow-y: scroll;
        max-height: calc(100vh - 80px); /* Adjust the value based on your layout */
      }
      ")
    )
  ),
  titlePanel("Hohokam Ceramic Coding App"),
  sidebarLayout(
    sidebarPanel(
      style = "height: 90vh; overflow-y: auto;",
      textInput("sample_id", "Sample ID"),
      checkboxGroupInput("vesselform", "Vessel Form",
                         choices = c("12 - Bowl" = 12,
                                     "78 - Everted Jar Rim" = 78,
                                     "114 - Pitcher" = 114,
                                     "124 - Tall neck jar or pitcher" = 124,
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
                                     "295 - Coil-based incsing with either (1) line with < 4 mm or (2) Snaketown style design" = 295,
                                     "302 - Incising, imitation of coil based incising (very deep, regular, continuous)" = 302)),
      checkboxGroupInput("trailinglines", "Trailing Lines and Exterior Bowl Designs",
                         choices = c("5 - Exterior bowl design (more than trailing lines)" = 5,
                                     "24 - Trailing line spacing < 3cm at rim" = 24,
                                     "26 - Trailing Lines" = 26,
                                     "175 - Trailing lines short < 3 cm with more than 3 lines spacing < 4 cm at rim" = 175,
                                     "345 - Trailing lines present spaced > 3 cm (or unknown) and large solid and thickest line > 2.5 mm" = 345,
                                     "347 - Trailing lines present, number of trailing lines/bowl < 6" = 347)),
      checkboxGroupInput("designlayouts", "Design Styles and Layouts",
                         choices = c("4 - Snaketown style layout" = 4,
                                     "32 - Snaketown style design" = 32,
                                     "305 - Snaketown style layout and thickest line < 2.5 mm wide, and polished surface" = 305,
                                     "307 - Repeated negative life form and Snaketown layout" = 307,
                                     "315 - Filled in or partly filled in Snaketown style design and bowl exterior with trailing lines or no painted exterior design" = 315,
                                     "17 - Estrella style" = 17,
                                     "18 - Sweetwater style" = 18,
                                     "52 - Organizational Banding Layout" = 52,
                                     "74 - Diamond panel layout" = 74,
                                     "112 - Upper Freeline, jars only" = 112,
                                     "121 - Open panel (difficult to identify in small sherds)" = 121,
                                     "172 - All over layout spiraling elements" = 172,
                                     "220 - Jar with sectioned design" = 220,
                                     "260 - Banded layout" = 260,
                                     "270 - Design field separation from rim" = 270,
                                     "300 - Hachure sectioning negative star layout and hachure spacing > thickness of hachure lines" = 300,
                                     "324 - Full field negative design, not hachure filled" = 324,
                                     "340 - Organizational banding layout and full rim line" = 340)),
      checkboxGroupInput("shoulder", "Vessel shoulder",
                         choices = c("77 - Gila Shoulder" = 77,
                                     "92 - Gila Shoulder < 120 degrees" = 92,
                                     "93 - Gila Shoulder, knife edge" = 93,
                                     "123 - Classic (mold inset) shoulder" = 123,
                                     "348 - Gila Shoulder angle < 110 degrees" = 348)),
      checkboxGroupInput("surfacetreatment", "Surface treatment",
                         choices = c("209 - Wipe marked jar interior" = 209,
                                     "293 - Tool polishing over painted lines" = 293,
                                     "294 - Red slip or wash on bowl exterior" = 294,
                                     "310 - Polish, lusterous" = 310,
                                     "311 - Polished" = 311,
                                     "312 - Polished and gray paste" = 312,
                                     "316 - Surface coating present and Snaketown style design present" = 316,
                                     "319 - Surface coating absent and fire clouding present" = 319,
                                     "320 - Surface coating absent and sooted surface" = 320,
                                     "321 - Surface coating absent and gray paste" = 321,
                                     "330 = Surface coating absent" = 330)),
      checkboxGroupInput("lifeform", "Life forms",
                         choices = c("21 - Flying bird, Negative (Snaketown Style)" = 21,
                                     "37 - Flying bird, Negative (not Snaketown style)" = 37,
                                     "53 - Flying bird, Positive" = 53,
                                     "54 - Life forms, except birds and lizards" = 54,
                                     "55 - Quail" = 55,
                                     "207 - Large repeated life form or geometric element > 5 cm" = 207,
                                     "326 - Repeated negaive life form, not in a rim solid, not Snaketown style" = 326)),
      checkboxGroupInput("hachure", "Hachure",
                         choices = c("22 - Massed hachure" = 22,
                                     "34 - Filler space hachure" = 34,
                                     "51 - Slanted railroad tie hachure" = 51,
                                     "62 - Cunieform hatch" = 62,
                                     "165 - Hachure filled design plus thickest line width < 2.1 mm" = 165)),
      checkboxGroupInput("geoelements", "Geometric elements",
                         choices = c("23 - Key" = 23,
                                     "25 - Multiple dots" = 25,
                                     "28 - Linebird (pendant dash motif)" = 28,
                                     "35 - Motif seration" = 35,
                                     "36 - Long scroll seration" = 36,
                                     "41 - Design ticking" = 41,
                                     "58 - Large solid" = 58,
                                     "95 - Outline line and stagger" = 95,
                                     "96 - Rectilinear scroll" = 96,
                                     "101 - Lines motif" = 101,
                                     "102 - Solid Void Motif (with specific geometric design)" = 102,
                                     "111 - Tappered lines" = 111,
                                     "188 - Wavy edge solid" = 188,
                                     "195 - Small element group A" = 195,
                                     "280 - Small element group B" = 280,
                                     "284 - Small element group C" = 284,
                                     "190 - Small element group D" = 190,
                                     "250 - Small element group E" = 250,
                                     "200 - Crenulated line" = 200,
                                     "203 - Fringed curvilinear scroll" = 203,
                                     "207 - Large repeated life form or geometric element > 5 cm" = 207,
                                     "222 - Small elements used as panel centerline" = 222,
                                     "224 - Design element diversity > 4" = 224,
                                     "227 - Zipper motif" = 227,
                                     "230 - Two or more voids within a single solid element" = 230,
                                     "277 - Interlocking rectilinear fret" = 277,
                                     "333 - Serrated curvilinear scroll" = 333,
                                     "349 - Thick serrated line" = 349)),
      checkboxGroupInput("panelandfringe", "Panels and fringe",
                         choices = c("56 - Free floating fringe" = 56,
                                     "57 - Single capped fringe" = 57,
                                     "59 - Indeterminate free floating or single capped fringe" = 59,
                                     "80 - Wavy capped fringe" = 80,
                                     "78 - Panel with a centerline motif" = 78,
                                     "81 - Panel with serrated margin" = 81,
                                     "91 - Crenulated lines as centerline motif in panel" = 91,
                                     "103 - Double capped fringe as panel or panel border" = 103,
                                     "217 - Line demarcated panels" = 217,
                                     "233 - Panel, at least partly line demarcated > 1 cm motif" = 233,
                                     "235 - Panel, at least partly line demarcated with zipper, curavlinear scroll, or other elaboration" = 235,
                                     "238 - Panel, at least partly line demarcated with multiple different elements used" = 238,
                                     "255 - Panel, isolated completely line demarcated" = 255,
                                     "257 - Panel, at least partly line demarcated (does not have any parts where panel border is attached to an adjacent solid)" = 257)),
      checkboxGroupInput("linethickness", "Line thickness and other associated attributes",
                         choices = c("291 - Thickest lines > 4 mm in width, paste gray or brown, surface fire clouded" = 291,
                                     "335 - Avg. thickest line < 2.2 mm and either linebird or organizational banding" = 335,
                                     "338 - Thickest line < 2.2 mm avg width and surface coating present" = 338,
                                     "342 - Large solid (> 5cm) and thickest line < 2.5 mm avg. width; in combination with other traits that place the sherd post SC" = 342,
                                     "350 - Large solid and thickest line > 3.1 mm" = 350)),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("date_plot"),
      verbatimTextOutput("data_output")  # Output to display collected data
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Create a reactive value to store the input data
  data <- reactiveValues(list = list())  # Initialize as an empty list
  
  # Render the interest plot
  output$date_plot <- renderPlot({
    
    out <- c(input$vesselform, input$incising, 
             input$trailinglines, input$designlayouts, input$shoulder,
             input$surfacetreatment, input$lifeform, input$hachure,
             input$geoelements, input$panelandfringe, input$linethickness)
    dat_out <- attribute_dates[which(attribute_dates$Number %in% out), ]
    dat_y <<- colSums(dat_out[,3:14], na.rm=TRUE)
    
    df_plot <- as.data.frame(matrix(0,length(seq(600:1299)),2))
    colnames(df_plot) <- c("Year","CeramicAttribute")
    df_plot$Year <- seq(600:1299)+599
    
    df_plot[which(df_plot$Year==650):which(df_plot$Year==674),2] <- dat_y[1]
    df_plot[which(df_plot$Year==675):which(df_plot$Year==699),2] <- dat_y[2]
    df_plot[which(df_plot$Year==700):which(df_plot$Year==729),2] <- dat_y[3]
    df_plot[which(df_plot$Year==730):which(df_plot$Year==749),2] <- dat_y[4]
    df_plot[which(df_plot$Year==750):which(df_plot$Year==799),2] <- dat_y[5]
    df_plot[which(df_plot$Year==800):which(df_plot$Year==899),2] <- dat_y[6]
    df_plot[which(df_plot$Year==900):which(df_plot$Year==949),2] <- dat_y[7]
    df_plot[which(df_plot$Year==950):which(df_plot$Year==1019),2] <- dat_y[8]
    df_plot[which(df_plot$Year==1020):which(df_plot$Year==1079),2] <- dat_y[9]
    df_plot[which(df_plot$Year==1080):which(df_plot$Year==1099),2] <- dat_y[10]
    df_plot[which(df_plot$Year==1100):which(df_plot$Year==1149),2] <- dat_y[11]
    df_plot[which(df_plot$Year==1150):which(df_plot$Year==1299),2] <- dat_y[12]

    beg <- c(650,675,700,730,750,800,900,950,1020,1080,1100,1150)
    end <- c(674,699,729,749,799,899,949,1019,1079,1099,1149,1299)
    
    cer_min <- beg[min(which(dat_y==max(dat_y)))]
    cer_max <- end[max(which(dat_y==max(dat_y)))]
    
    cer_range <- paste("Intercepting ceramic attribute range: ", cer_min,"-", cer_max)
    
    
    if (length(out) > 0) {
      # Create a plot
      ggplot(df_plot) +
        geom_line(aes(x=Year,y=CeramicAttribute), size=3, alpha=0.5, color = "skyblue") +
        scale_x_continuous(breaks=seq(600, 1349, 50)) +
        ggtitle(cer_range) +
        theme_bw() +
        theme(
          axis.text.x = element_text(size = rel(2)),
          axis.text.y = element_text(size = rel(2)),
          axis.title.x = element_text(size = rel(2)),
          axis.title.y = element_text(size = rel(2)),
          legend.text = element_text(size = rel(1))
        )
    }
  })
  
  # Function to update the input fields
  updateData <- function() {
    # Clear the input fields
    updateTextInput(session, "sample_id", value = "")
    updateCheckboxGroupInput(session, "vesselform", choices = c("12 - Bowl" = 12,
                                                                "78 - Everted Jar Rim" = 78,
                                                                "114 - Pitcher" = 114,
                                                                "124 - Tall neck jar or pitcher" = 124,
                                                                "179 - Compressed globular body jar with short flared rim" = 179,
                                                                "185 - Flare rimmed bowl" = 185,
                                                                "186 - Outcurved or flat rimmed bowl, shallow or flat bottomoed" = 186,
                                                                "245 - Cauldron" = 245,
                                                                "246 - Cauldron with Gila Shoulder" = 246,
                                                                "247 - Cauldron, vertical walls" = 247,
                                                                "275 - Semi flare-rimmed hemispherical bowl" = 275))
    updateCheckboxGroupInput(session, "incising", choices = c("1 - Deep, regular incising" = 1,
                                                              "15 - Incising" = 15,
                                                              "29 - Incising, discontinuous (not patterned)" = 29,
                                                              "157 - Coil based incising" = 157,
                                                              "290 - Coil-based incising thickest lines > 4 mm in width" = 290,
                                                              "295 - Coil-based incsing with either (1) line with < 4 mm or (2) Snaketown style design" = 295,
                                                              "302 - Incising, imitation of coil based incising (very deep, regular, continuous)" = 302))
    updateCheckboxGroupInput(session, "trailinglines", choices = c("5 - Exterior bowl design (more than trailing lines)" = 5,
                                                                   "24 - Trailing line spacing < 3cm at rim" = 24,
                                                                   "26 - Trailing Lines" = 26,
                                                                   "175 - Trailing lines short < 3 cm with more than 3 lines spacing < 4 cm at rim" = 175,
                                                                   "345 - Trailing lines present spaced > 3 cm (or unknown) and large solid and thickest line > 2.5 mm" = 345,
                                                                   "347 - Trailing lines present, umber of trailing lines/bowl < 6" = 347))
    updateCheckboxGroupInput(session, "designlayouts", choices = c("4 - Snaketown style layout" = 4,
                                                                   "32 - Snaketown style design" = 32,
                                                                   "305 - Snaketown style layout and thickest line < 2.5 mm wide, and polished surface" = 305,
                                                                   "307 - Repeated negative life form and Snaketown layout" = 307,
                                                                   "315 - Filled in or partly filled in Snaketown style design and bowl exterior with trailing lines or no painted exterior design" = 315,
                                                                   "17 - Estrella style" = 17,
                                                                   "18 - Sweetwater style" = 18,
                                                                   "52 - Organizational Banding Layout" = 52,
                                                                   "74 - Diamond panel layout" = 74,
                                                                   "112 - Upper Freeline, jars only" = 112,
                                                                   "121 - Open panel (difficult to identify in small sherds)" = 121,
                                                                   "172 - All over layout spiraling elements" = 172,
                                                                   "220 - Jar with sectioned design" = 220,
                                                                   "260 - Banded layout" = 260,
                                                                   "270 - Design field separation from rim" = 270,
                                                                   "300 - Hachure sectioning negative star layout and hachure spacing > thickness of hachure lines" = 300,
                                                                   "324 - Full field negative design, not hachure filled" = 324,
                                                                   "340 - Organizational banding layout and full rim line" = 340))
    updateCheckboxGroupInput(session, "shoulder", choices = c("77 - Gila Shoulder" = 77,
                                                              "92 - Gila Shoulder < 120 degrees" = 92,
                                                              "93 - Gila Shoulder, knife edge" = 93,
                                                              "123 - Classic (mold inset) shoulder" = 123,
                                                              "348 - Gila Shoulder angle < 110 degrees" = 348))
    updateCheckboxGroupInput(session, "surfacetreatment", choices = c("209 - Wipe marked jar interior" = 209,
                                                              "293 - Tool polishing over painted lines" = 293,
                                                              "294 - Red slip or wash on bowl exterior" = 294,
                                                              "310 - Polish, lusterous" = 310,
                                                              "311 - Polished" = 311,
                                                              "312 - Polished and gray paste" = 312,
                                                              "316 - Surface coating present and Snaketown style design present" = 316,
                                                              "319 - Surface coating absent and fire clouding present" = 319,
                                                              "320 - Surface coating absent and sooted surface" = 320,
                                                              "321 - Surface coating absent and gray paste" = 321,
                                                              "330 = Surface coating absent" = 330))
    updateCheckboxGroupInput(session, "lifeform", choices = c("21 - Flying bird, Negative (Snaketown Style)" = 21,
                                                              "37 - Flying bird, Negative (not Snaketown style)" = 37,
                                                              "53 - Flying bird, Positive" = 53,
                                                              "54 - Life forms, except birds and lizards" = 54,
                                                              "55 - Quail" = 55,
                                                              "326 - Repeated negaive life form, not in a rim solid, not Snaketown style" = 326))
    updateCheckboxGroupInput(session, "hachure", choices = c("22 - Massed hachure" = 22,
                                                             "34 - Filler space hachure" = 34,
                                                             "51 - Slanted railroad tie hachure" = 51,
                                                             "62 - Cunieform hatch" = 62,
                                                             "165 - Hachure filled design plus thickest line width < 2.1 mm" = 165))
    updateCheckboxGroupInput(session, "geoelements", choices = c("23 - Key" = 23,
                                                                 "25 - Multiple dots" = 25,
                                                                 "28 - Linebird (pendant dash motif)" = 28,
                                                                 "35 - Motif seration" = 35,
                                                                 "36 - Long scroll seration" = 36,
                                                                 "41 - Design ticking" = 41,
                                                                 "58 - Large solid" = 58,
                                                                 "95 - Outline line and stagger" = 95,
                                                                 "96 - Rectilinear scroll" = 96,
                                                                 "101 - Lines motif" = 101,
                                                                 "102 - Solid Void Motif (with specific geometric design)" = 102,
                                                                 "111 - Tappered lines" = 111,
                                                                 "188 - Wavy edge solid" = 188,
                                                                 "195 - Small element group A" = 195,
                                                                 "280 - Small element group B" = 280,
                                                                 "284 - Small element group C" = 284,
                                                                 "190 - Small element group D" = 190,
                                                                 "250 - Small element group E" = 250,
                                                                 "200 - Crenulated line" = 200,
                                                                 "203 - Fringed curvilinear scroll" = 203,
                                                                 "207 - Large repeated life form or geometric element > 5 cm" = 207,
                                                                 "222 - Small elements used as panel centerline" = 222,
                                                                 "224 - Design element diversity > 4" = 224,
                                                                 "227 - Zipper motif" = 227,
                                                                 "230 - Two or more voids within a single solid element" = 230,
                                                                 "277 - Interlocking rectilinear fret" = 277,
                                                                 "333 - Serrated curvilinear scroll" = 333,
                                                                 "349 - Thick serrated line" = 349))
    updateCheckboxGroupInput(session, "panelandfringe", choices = c("56 - Free floating fringe" = 56,
                                                                    "57 - Single capped fringe" = 57,
                                                                    "59 - Indeterminate free floating or single capped fringe" = 59,
                                                                    "80 - Wavy capped fringe" = 80,
                                                                    "78 - Panel with a centerline motif" = 78,
                                                                    "81 - Panel with serrated margin" = 81,
                                                                    "91 - Crenulated lines as centerline motif in panel" = 91,
                                                                    "103 - Double capped fringe as panel or panel border" = 103,
                                                                    "217 - Line demarcated panels" = 217,
                                                                    "233 - Panel, at least partly line demarcated > 1 cm motif" = 233,
                                                                    "235 - Panel, at least partly line demarcated with zipper, curavlinear scroll, or other elaboration" = 235,
                                                                    "238 - Panel, at least partly line demarcated with multiple different elements used" = 238,
                                                                    "255 - Panel, isolated completely line demarcated" = 255,
                                                                    "257 - Panel, at least partly line demarcated (does not have any parts where panel border is attached to an adjacent solid)" = 257))
    updateCheckboxGroupInput(session, "linethickness", choices = c("291 - Thickest lines > 4 mm in width, paste gray or brown, surface fire clouded" = 291,
                                                                   "335 - Avg. thickest line < 2.2 mm and either linebird or organizational banding" = 335,
                                                                   "338 - Thickest line < 2.2 mm avg width and surface coating present" = 338,
                                                                   "342 - Large solid (> 5cm) and thickest line < 2.5 mm avg. width; in combination with other traits that place the sherd post SC" = 342,
                                                                   "350 - Large solid and thickest line > 3.1 mm" = 350))
    
  }
  
  # Event handler for the submit button
  observeEvent(input$submit, {
    # Check if sample_id is provided
    if (isTruthy(input$sample_id)) {
      # Add the input data to the list
      data$list[[length(data$list) + 1]] <- c(input$sample_id, input$vesselform, input$incising, 
                                              input$trailinglines, input$designlayouts, input$shoulder,
                                              input$surfacetreatment, input$lifeform, input$hachure,
                                              input$geoelements, input$panelandfringe, input$linethickness)
      
      updateData()  # Update the input fields
    } else {
      showNotification("Please enter a Sample ID", type = "warning")
    }
  })
  
  # Render the collected data
  output$data_output <- renderPrint({
    if (length(data$list) > 0) {
      cat("Collected Data:\n")
      for (i in 1:length(data$list)) {
        cat(data$list[[i]])
        cat("\n")
      }
    } else {
      cat("No data collected yet.")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
