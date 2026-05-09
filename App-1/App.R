library(shiny)
library(bslib)
library(colourpicker)
library(tigris)
library(sf)   
library(leaflet)
library(dplyr)
library(maps)
library(leaflet.extras)
library(shinydashboard)
library(patchwork)
library(markdown)
library(ggplot2)

# setwd("C:/Users/alexm/REPOS/CRANE/App-1")
ee_data <- read.csv("Exemptions_Elections.csv")
idaho_data <- read.csv("social_Idaho.csv")

# create a label vector 
var_labels <- c(
  "Density" = "Population Density (per square mile)",
  "REP.PERCENT" = "Republican Votes (%)",
  "DEM.PERCENT" = "Democrat Votes (%)",
  "UNA.PERCENT" = "Independent Votes (%)",
  "FRINGE.PERCENT" = "Other Votes (%)",
  "language.isolation.percent" = "Language Isolation (%)",
  "below.poverty.percent" = "Below Poverty (%)",
  "below.education.percent" = "Below 9th Grade (%)",
  "median.income" = "Median Income ($)"
)

# change percent column from character to numerical after removing the NA & NR values
ee_data <- ee_data %>%
  filter(!percent %in% c("NA", "NR")) %>%
  mutate(percent = as.numeric(percent))


# add all the small party votes into a new vote category "Other" and remove all those lines
other_data <- ee_data %>%
  dplyr::filter(type == "Other") %>%
  dplyr::group_by(NAME, data_type, type) %>%
  dplyr::summarise(percent = sum(percent, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(percent))

# filter data that is not for other voters
non_other <- ee_data %>%
  dplyr::filter(type != "Other")

# combine back together
ee_data <- dplyr::bind_rows(non_other, other_data)

# check resulting data to make sure there is only one row with type "Other" for each state (NAME)
ee_data %>%
  dplyr::filter(type == "Other") %>%
  head()
ee_data %>%
  dplyr::filter(type == "Other") %>%
  dplyr::count(NAME)

us_states <- tigris::states(cb = TRUE, class = "sf")
us_states <- sf::st_transform(us_states, 4326)

# remove Leading or Trailing Whitespaces in character strings
ee_data$NAME <- trimws(ee_data$NAME)
ee_data$type <- trimws(ee_data$type)
us_states$NAME  <- trimws(us_states$NAME)

# remove Alaska and Hawaii from map representation to allow proper zoom on continental US
states_continental <- subset(us_states, STUSPS %in% state.abb & !STUSPS %in% c("AK", "HI"))

# Write a single R object to a file
saveRDS(states_continental, "us_states.rds")

# Restore a single R object from a file
us_states <- readRDS("us_states.rds")

vote_vals <- ee_data %>%
  dplyr::filter(data_type == "Vote") %>%
  dplyr::group_by(NAME, type) %>%
  dplyr::summarise(percent = sum(percent, na.rm = TRUE),
                   .groups = "drop")

exempt_vals <- ee_data %>%
  dplyr::filter(data_type == "Exemption") 
# %>%
#  dplyr::group_by(NAME, type, year) %>%
#  dplyr::summarise(percent = sum(percent, na.rm = TRUE),
#                   .groups = "drop")

#merge with map with data based on State NAME for exemption and vote data
map_exempt <- left_join(us_states, exempt_vals, by = "NAME") 

map_vote <- left_join(us_states, vote_vals, by = "NAME")


# County map for Idaho

library(tigris)
idaho <- counties(state = "ID", cb = TRUE, class = "sf")

# Class conversion
# convert estimate column as  as numeric
# installing the package dplyr to use the mutate function

library(dplyr)
idaho_data$FIPS <- as.character(idaho_data$FIPS)

# Write a single R object to a file
saveRDS(idaho, "Idaho_counties.rds")


# merge with data
idaho_all <- idaho %>%
  left_join(idaho_data, by = c("GEOID" = "FIPS"))



#Shiny App #######################################################

ui <- dashboardPage(
  
  dashboardHeader(
    title = tags$div(
      style = "
      white-space: normal;
      line-height: 1.2;
      padding-top: 8px;
      font-size: 16px;
      max-width: 300px;
    ",
      "Vaccination Exemptions"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map USA", tabName = "map1", icon = icon("globe")),
      menuItem("Data Check", tabName = "check", icon = icon("exclamation-triangle")),
      menuItem("Map Idaho", tabName = "map2", icon = icon("globe")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("GitHub", tabName = "link", icon = icon ("github"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # WHOLE US MAP TAB 
      tabItem(
        tabName = "map1",
        
        div(
          style = "padding:15px;",
          
          fluidRow(
            column(6,
                   radioButtons("ex_type", "Vaccination Exemption type:",
                                choices = c("Any Exemption","Medical Exemption"),
                                inline = TRUE),
                   leafletOutput("map_data_ex", height = 600),
                   sliderInput("year", "Year:",
                               min = 2010, max = 2025,
                               value = 2024, step = 1, sep = ""),
                   div(
                     style = "font-size:16px; text-align:left; color:darkorange;",
                     "Note: Exemption data is missing for some years/states- check Data Check tab"
                   )
                   
            ), # close column loop
            
            column(6,
                   radioButtons("vote_type", "2024 Presidential Election:",
                                choices = c("Republican",
                                            "Democrat",
                                            "Other"),
                                inline = TRUE),
                   leafletOutput("map_data_vote", height = 600)
            ) # close column loop
          ) # close fluidRow loop
        ) # close div loop
      ), # close tabItem
      
      # CHECK COMPLETENESS TAB      
      tabItem(
        tabName = "check",
        
        div(
          style = "padding:15px;",
          
          h3("Data Completeness Checker"),
          
          fluidRow(
            
            column(6,
                   selectInput("check_year", "Year:",
                               choices = sort(unique(ee_data$year)),
                               selected = max(ee_data$year)),
                   
                   selectInput("check_type", "Type:",
                               choices = unique(ee_data$type))
            ),
            
            column(6,
                   verbatimTextOutput("missing_states"),
                   verbatimTextOutput("summary_missing")
            )
          )
        )
      ),
      
       # IDAHO MAPS
      tabItem(
        tabName = "map2",
        
        div(
          style = "padding:15px;",
          
          fluidRow(
            # left Idaho Map
            column(6,
                   selectInput(
                     "var1", 
                     "Variable:",
                                choices = c("Population Density (per sq.mile)"= "Density",
                                            "Republican Votes (%)"="REP.PERCENT", 
                                            "Democrat Votes (%)"="DEM.PERCENT",
                                            "Independent Votes (%)"="UNA.PERCENT", 
                                            "Other Votes (%)"="FRINGE.PERCENT",
                                            "Language Isolation (%)"="language.isolation.percent", 
                                            "Below Poverty (%)"="below.poverty.percent", 
                                            "Below 9th grade Education (%)"="below.education.percent",
                                            "Median Income ($)"="median.income")
                   ), # close selectInput
        leafletOutput("map_idaho2", height = 600)
          ), # close column loop
        
        column(6,
               selectInput(
                 "var2", 
                 "Variable:",
                            choices = c("Median Income ($)"="median.income",
                                        "Population Density (per sq.mile)"= "Density",
                                        "Republican Votes (%)"="REP.PERCENT", 
                                        "Democrat Votes (%)"="DEM.PERCENT",
                                        "Independent Votes (%)"="UNA.PERCENT", 
                                        "Other Votes (%)"="FRINGE.PERCENT",
                                        "Language Isolation (%)"="language.isolation.percent", 
                                        "Below Poverty (%)"="below.poverty.percent", 
                                        "Below 9th grade Education (%)"="below.education.percent")
                          ), # close selectInput
               leafletOutput("map_idaho1", height = 600)
          ) # close column loop
        ) # close fluidRow loop
      ) # close div loop
    ), # close tabItem

      # ANALYSIS TAB
      tabItem(
        tabName = "analysis",
        
        # 🔹 Row 1 → inputs
        
        fluidRow(
          
          column(6,
                 selectInput(
                   "xvar",
                   "X variable:",
                   choices = c("Population Density (per square mile)"= "Density",
                               "Republican Votes (%)"="REP.PERCENT", 
                               "Democrat Votes (%)"="DEM.PERCENT",
                               "Independent Votes (%)"="UNA.PERCENT", 
                               "Other Votes (%)"="FRINGE.PERCENT",
                               "Language Isolation (%)"="language.isolation.percent", 
                               "Below Poverty (%)"="below.poverty.percent", 
                               "Below 9th grade Education (%)"="below.education.percent",
                               "Median Income ($)"="median.income"
                               ) # close choices
                 ) # close SelectInput
              ), # close column
          
          column(6,
                 selectInput(
                   "yvar",
                   "Y variable:",
                   choices = c("Median Income ($)"="median.income",
                               "Population Density (per square mile)"= "Density",
                               "Republican Votes (%)"="REP.PERCENT", 
                               "Democrat Votes (%)"="DEM.PERCENT",
                               "Independent Votes (%)"="UNA.PERCENT", 
                               "Other Votes (%)"="FRINGE.PERCENT",
                               "Language Isolation (%)"="language.isolation.percent", 
                               "Below Poverty (%)"="below.poverty.percent", 
                               "Below 9th grade Education (%)"="below.education.percent"
                   ) # close choices
                 ) # close SelectInpur
          ), # close column
          
          # 🔹 Row 2 → plot (full width)
          
          column(12,
                 plotOutput("plot1", height="500px")
          ) # close column
          
        ) # close fluidRow
      ), # close tabItem
    
    # DATA FOR IDAHO TAB
    tabItem(
      tabName = "data",
      
      div(
        style = "padding:15px;",
        
        h3("Idaho Data Table"),
        
        DT::dataTableOutput("table")
      ) # close div
    ), # close tabItem
    
      # ABOUT PROJECT TAB
      tabItem(
        tabName = "about",
        h3("About this project"),
        tags$div(
          style = "padding: 15px; max-width: 900px;",
          includeMarkdown("About.md")
        ) # close tags$div loop
        ), # close tabItem loop

     # GITHUP LINK TAB    
      tabItem(
        tabName = "link",
        div(
          style = "padding:20px;",
          
          tags$a(
            href = "https://github.com/ac292/CRANE",
            target = "_blank",
            h3("View related project on GitHub")
        ) # close tags
      ) # close div
    ) # close tabItem
   ) # close tabItems
  ) # close dashboardBody
) # close dashboardPage

  ################################### dashboard end 
  
 
# SERVER ######################################################################

server <- function(input, output, session) {

  req_packages <- c("shiny","bslib","tigris","sf","leaflet",
                    "dplyr","maps","leaflet.extras","shinydashboard")
  
  invisible(lapply(req_packages, library, character.only = TRUE))
  
    sync_lock <- reactiveVal(FALSE)
  
  ex_data <- reactive({
    map_exempt %>%
      filter(year == input$year,
             type == input$ex_type) %>%
      group_by(NAME) %>%
      summarise(
        percent = mean(percent, na.rm = TRUE),
        geometry = first(geometry),
        .groups = "drop"
      ) %>%
      sf::st_as_sf()
  })
  
  vote_data <- reactive({
    map_vote %>%
      filter(type == input$vote_type)
  })
  
  
  output$missing_states <- renderPrint({
    
    req(input$check_year, input$check_type)
    
    expected <- us_states$NAME
    
    actual <- ee_data %>%
      filter(year == input$check_year,
             type == input$check_type) %>%
      distinct(NAME) %>%
      pull(NAME)
    
    missing <- setdiff(expected, actual)
    
    cat("Missing states:\n")
    print(missing)
  })
  
  idaho_data <- reactive({
    idaho_all
  })

  
  # synchronize zooming between maps


  # palettes 
  
  pal_ex <- colorNumeric("Reds", domain = c(0, 15))

#   pal_vote <- colorNumeric("Reds", domain = c(0, 100))

  
  # left map output
  
  output$map_data_ex <- renderLeaflet({
    
    data <- ex_data()

    bbox <- sf::st_bbox(data)
    
    leaflet(data) %>%
      addTiles() %>%
      fitBounds(
        as.numeric(bbox["xmin"]), as.numeric(bbox["ymin"]),
        as.numeric(bbox["xmax"]), as.numeric(bbox["ymax"])
      ) %>%
      addPolygons(
        fillColor = ~pal_ex(percent),
        fillOpacity = 0.7,
        color = "white"
      ) %>%
      addLegend(
        pal = pal_ex,
        values = ~percent,
        title = paste(input$ex_type, input$year)
      )
  })
  
  observeEvent({
    input$map_data_ex_center
    input$map_data_ex_zoom
  }, {
    
    if (sync_lock()) return()
    sync_lock(TRUE)
    
    leafletProxy("map_data_vote") %>%
      setView(
        lng = input$map_data_ex_center$lng,
        lat = input$map_data_ex_center$lat,
        zoom = input$map_data_ex_zoom
      )
    
    sync_lock(FALSE)
  })

  # right map output
  
  output$map_data_vote <- renderLeaflet({
    
    data <- vote_data()
    
    bbox <- sf::st_bbox(data)
    
    pal_vote <- colorBin(
      palette = "viridis",
      domain = data$percent,
      bins = 6,
      pretty = TRUE
    )
    
    
    leaflet(data) %>%
      addTiles() %>%
      fitBounds(
        as.numeric(bbox["xmin"]), as.numeric(bbox["ymin"]),
        as.numeric(bbox["xmax"]), as.numeric(bbox["ymax"])
      ) %>%
      addPolygons(
        fillColor = ~pal_vote(percent),
        fillOpacity = 0.7,
        color = "white",
        weight = 1
      ) %>%
      addLegend(
        pal = pal_vote,
        values = ~percent,
        title = paste(input$vote_type, "%")
        )

  })
  
  observeEvent({
    input$map_data_vote_center
    input$map_data_vote_zoom
  }, {
    
    if (sync_lock()) return()
    sync_lock(TRUE)
    
    leafletProxy("map_data_ex") %>%
      setView(
        lng = input$map_data_vote_center$lng,
        lat = input$map_data_vote_center$lat,
        zoom = input$map_data_vote_zoom
      )
    
    sync_lock(FALSE)
  })
  
  # Idaho map output left
  
  output$map_idaho2 <- renderLeaflet({
    
    data <- idaho_data()
    var <- data[[input$var1]]
    
    bbox <- sf::st_bbox(data)
    
    pal_idaho <- colorBin(
      palette = "viridis",
      domain = var,
      bins = 6,
      pretty = TRUE
    )
    
    leaflet(data) %>%
      addTiles() %>%
      fitBounds(
        as.numeric(bbox["xmin"]), as.numeric(bbox["ymin"]),
        as.numeric(bbox["xmax"]), as.numeric(bbox["ymax"])
      ) %>%
      addPolygons(
        fillColor = ~pal_idaho(var),
        fillOpacity = 0.7,
        color = "white",
        label = ~paste0(NAME, ": ", round(var, 2))
      ) %>%
      addLegend(
        pal = pal_idaho,
        values = var,
        title = input$variable
      )
  })
  
  observeEvent({
    input$map_idaho2_center
    input$map_idaho2_zoom
  }, {
    
    if (sync_lock()) return()
    sync_lock(TRUE)
    
    leafletProxy("map_idaho1") %>%
      setView(
        lng = input$map_idaho2_center$lng,
        lat = input$map_idaho2_center$lat,
        zoom = input$map_idaho2_zoom
      )
    
    sync_lock(FALSE)
  })
  
  # Idaho map output right
  output$map_idaho1 <- renderLeaflet({
    
    data <- idaho_data()
    var <- data[[input$var2]]
    
    bbox <- sf::st_bbox(data)
    
    pal_idaho <- colorBin(
      palette = "viridis",
      domain = var,
      bins = 6,
      pretty = TRUE
    )
    
    leaflet(data) %>%
      addTiles() %>%
      fitBounds(
        as.numeric(bbox["xmin"]), as.numeric(bbox["ymin"]),
        as.numeric(bbox["xmax"]), as.numeric(bbox["ymax"])
      ) %>%
      addPolygons(
        fillColor = ~pal_idaho(var),
        fillOpacity = 0.7,
        color = "white",
        label = ~paste0(NAME, ": ", round(var, 2))
      ) %>%
      addLegend(
        pal = pal_idaho,
        values = var,
        title = input$variable
      )
  })
  
  observeEvent({
    input$map_idaho1_center
    input$map_idaho1_zoom
  }, {
    
    if (sync_lock()) return()
    sync_lock(TRUE)
    
    leafletProxy("map_idaho1") %>%
      setView(
        lng = input$map_idaho1_center$lng,
        lat = input$map_idaho1_center$lat,
        zoom = input$map_idaho1_zoom
      )
    
    sync_lock(FALSE)
  })
  # scatter plots for analysis tab
  
  output$plot1 <- renderPlot({
    
    
    req(input$xvar, input$yvar)
    
    df <- idaho_data()
    
    # Extract selected variables
    df2 <- data.frame(
      x = df[[input$xvar]],
      y = df[[input$yvar]]
    ) %>%
      na.omit()
    
    # Build clean dataset
#    df2 <- data.frame(x = x, y = y) %>% na.omit()
    
    # Correlation
    r <- cor(df2$x, df2$y)
    
    # Linear model
    model <- lm(y ~ x, data = df2)
    
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    
    # extract Adjusted R-square
    adj_r2 <- summary(model)$adj.r.squared
    
    # Extract p-value
    p_value <- summary(model)$coefficients[2, 4]
    
    # 95% CI
    ci <- confint(model, level = 0.95)
    lower_ci <- ci[2, 1]
    upper_ci <- ci[2, 2]
    
    # Labels
    eq <- paste0(
      "y = ",
      round(slope, 4),
      "x + ",
      round(intercept, 2)
    )
    
    adj_r2_text <- paste0("adj. R² = ", round(adj_r2, 2))

    ci_text <- paste0(
      "95% CI slope: [",
      round(lower_ci, 4),
      ", ",
      round(upper_ci, 4),
      "]"
    )
    
    p_text <- ifelse(
      p_value < 0.001,
      "p < 0.001",
      paste0("p = ", round(p_value, 3))
    )
    
    label <- paste(eq, adj_r2_text, ci_text, p_text, sep = "\n")
    
    # calculate residuals
    
    df2$residuals <- resid(model)
    df2$fitted <- fitted(model)
    
    # Position annotation
    x_pos <- mean(range(df2$x, na.rm = TRUE))
    y_pos <- max(df2$y, na.rm = TRUE)
    
    # Plot
    p1 <- ggplot(df2, aes(x = x, y = y)) +
      
      geom_point(
        size = 3,
        color = "steelblue",
        alpha = 0.8
      ) +
      
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "red"
      ) +
      
      annotate(
        "label",
        x = x_pos,
        y = y_pos,
        label = label,
        hjust = 0.5,   # 👈 centers horizontally
        vjust = 1,     # 👈 anchors to top
        size = 4
      ) +
      
      theme_minimal() +
      
      labs(
        x = var_labels[input$xvar],
        y = var_labels[input$yvar],
        title = paste(var_labels[input$yvar], "vs", var_labels[input$xvar])
      ) +
    coord_cartesian(clip = "off")
    
    # residual plot
    
    p2 <- ggplot(df2, aes(x = fitted, y = residuals)) +
      
      geom_point(
        size = 3,
        color = "darkorange",
        alpha = 0.8
      ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "red"
      ) +
      
      theme_minimal() +
      
      labs(
        x = "Fitted values",
        y = "Residuals",
        title = "Residual Plot"
      )
    
    # Check for Normality of residuals
    
   p3 <- ggplot(data.frame(res = resid(model)), aes(sample = res)) +
      stat_qq() +
      stat_qq_line() +
   labs(
     x = "Therotical quantiles",
     y = "Residual quantiles",
     title = "QQ Plot"
   )
    
    # Combine plots vertically
    p1 / (p2 + p3)
    
  })
  
  # Table output in server
  
  output$table <- DT::renderDataTable({
    
    dftab <- idaho_data() %>%
      sf::st_drop_geometry() %>%   # 👈 THIS IS THE KEY FIX
      dplyr::select(
        County = NAME,
        `Republican (%)` = REP.PERCENT,
        `Democrat (%)` = DEM.PERCENT,
        `Independent (%)` = UNA.PERCENT,
        `Other (%)` = FRINGE.PERCENT,
        `Median Income` = median.income,
        `Population age 18-39` = population.age.18.39,
        `County area <br>(sq.mile)` = SQ_MILES,
        `Population per sq.mile` = Density,
        `Below Poverty (%) ` = below.poverty.percent,
        `Below Education (%)` = below.education.percent,
        `Language Isolation (%)` = language.isolation.percent
      )
    
    DT::datatable(
      dftab,
      escape = FALSE,   # 👈 allows HTML rendering
      options = list(
        pageLength = 45,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
}
# See above for the definitions of ui and server
# FINAL COMMAND to EXECUTE APP ##########################################
library(shiny)
shinyApp(ui = ui, server = server)
