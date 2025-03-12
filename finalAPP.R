library(shiny)
library(openmeteo)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(tidygeocoder)
library(lubridate)
library(httr)
library(jsonlite)
library(markdown) 

# Existing functions remain unchanged
get_ai_analysis <- function(data_summary) {
  url <- "https://rakshitjan-pdfproject.hf.space/chat"
  data_text <- paste0(
    "Analyze the following weather data: ", 
    "Average Temperature: ", round(data_summary$`Average Temperature (°C)`, 1), "°C, ",
    "Maximum Temperature: ", round(data_summary$`Maximum Temperature (°C)`, 1), "°C, ",
    "Minimum Temperature: ", round(data_summary$`Minimum Temperature (°C)`, 1), "°C, ",
    "Maximum Wind Speed: ", round(data_summary$`Maximum Wind Speed (m/s)`, 1), "m/s, ",
    "Total Rainfall: ", round(data_summary$`Total Rainfall (mm)`, 1), "mm. ",
    "Please provide a detailed analysis of this weather pattern."
  )
  body <- list(user_message = data_text)
  body_json <- toJSON(body, auto_unbox = TRUE)
  response <- tryCatch(
    {
      POST(
        url,
        add_headers(
          "accept" = "application/json",
          "Content-Type" = "application/json"
        ),
        body = body_json,
        encode = "json"
      )
    }, 
    error = function(e) {
      return(NULL)
    }
  )
  if (is.null(response) || http_status(response)$category != "Success") {
    return("Unable to fetch AI analysis at this time. Please try again later.")
  }
  response_content <- fromJSON(content(response, "text", encoding = "UTF-8"))
  return(response_content$response)
}

get_weather_forecast_with_analysis <- function(city, days_ahead) {
  if (days_ahead > 3) {
    stop("Error: Forecast can only be provided for up to 3 days ahead.")
  }
  location <- tryCatch({
    geo(city, method = "osm")
  }, error = function(e) {
    return(NULL)
  })
  if (is.null(location) || nrow(location) == 0) {
    stop("Error: City not found. Please check the spelling or try another city.")
  }
  start_date <- Sys.Date() - 3 
  end_date <- Sys.Date() + days_ahead 
  weather_data <- tryCatch({
    weather_forecast(
      location = c(location$lat, location$long),
      start = as.character(start_date),
      end = as.character(end_date),
      daily = c("temperature_2m_max", "temperature_2m_min", "daylight_duration", "rain_sum"),
      timezone = "auto"
    )
  }, error = function(e) {
    stop("Error: Unable to fetch weather data. Please try again later.")
  })
  if (nrow(weather_data) == 0) {
    stop("Error: No weather data retrieved. Please check the city name or API availability.")
  }
  weather_data$date <- as.Date(weather_data$date)
  forecasted_data <- weather_data %>%
    filter(date > Sys.Date()) %>% 
    rename(
      "Date" = date,
      "Max Temp (°C)" = daily_temperature_2m_max,
      "Min Temp (°C)" = daily_temperature_2m_min,
      "Daylight (hours)" = daily_daylight_duration,
      "Rainfall (mm)" = daily_rain_sum
    )
  forecast_summary <- forecasted_data %>%
    summarise(
      `Average Temperature (°C)` = mean((forecasted_data$`Max Temp (°C)` + forecasted_data$`Min Temp (°C)`) / 2, na.rm = TRUE),
      `Maximum Temperature (°C)` = max(forecasted_data$`Max Temp (°C)`, na.rm = TRUE),
      `Minimum Temperature (°C)` = min(forecasted_data$`Min Temp (°C)`, na.rm = TRUE),
      `Maximum Wind Speed (m/s)` = 0, # Not in forecast data, placeholder
      `Total Rainfall (mm)` = sum(forecasted_data$`Rainfall (mm)`, na.rm = TRUE)
    )
  ai_analysis <- get_ai_analysis(forecast_summary)
  if ("daily_daylight_duration" %in% colnames(weather_data)) {
    weather_data$daily_daylight_duration <- round(weather_data$daily_daylight_duration / 3600, 2)
  } else {
    weather_data$daily_daylight_duration <- NA
  }
  weather_long <- weather_data %>%
    select(date, daily_temperature_2m_max, daily_temperature_2m_min, daily_daylight_duration, daily_rain_sum) %>%
    pivot_longer(cols = -date, names_to = "type", values_to = "value") %>%
    mutate(value = ifelse(is.na(value), 0, value))
  weather_long$type <- recode(weather_long$type,
                              "daily_temperature_2m_max" = "Max Temperature (°C)",
                              "daily_temperature_2m_min" = "Min Temperature (°C)",
                              "daily_daylight_duration" = "Daylight Duration (hours)",
                              "daily_rain_sum" = "Total Rainfall (Normalized)"
  )
  max_rain <- max(weather_long$value[weather_long$type == "Total Rainfall (Normalized)"], na.rm = TRUE)
  if (!is.na(max_rain) && max_rain > 0) {
    weather_long <- weather_long %>%
      mutate(value = ifelse(type == "Total Rainfall (Normalized)", value / max_rain * 100, value))
  }
  fig <- plot_ly(weather_long, x = ~date, y = ~value, type = 'scatter', mode = 'lines+markers',
                 color = ~type,
                 hovertemplate = paste(
                   "<b>Date:</b> %{x}<br>",
                   "<b>%{fullData.name}:</b> %{y}<extra></extra>"
                 ),
                 source = "forecast_plot") %>%  # Add source ID
    layout(
      title = paste("Weather Forecast for", city),
      xaxis = list(title = "Date", showgrid = TRUE),
      yaxis = list(title = "Value (Temperature °C, Rainfall, Daylight Hours)", showgrid = TRUE),
      hovermode = "x unified",
      dragmode = "lasso",
      legend = list(title = list(text = "Weather Metrics"))
    )
  if ("Daylight (hours)" %in% colnames(forecasted_data)) {
    forecasted_data$`Daylight (hours)` <- round(forecasted_data$`Daylight (hours)` / 3600, 2)
  }
  
  return(list(data = forecasted_data, plot = fig, analysis = ai_analysis))
}

get_weather_summary_with_analysis <- function(city, start_date, end_date) {
  location <- tryCatch({
    geo(city, method = "osm")
  }, error = function(e) {
    return(NULL)
  })
  if (is.null(location) || nrow(location) == 0) {
    stop("Error: City not found. Please check the spelling or try another city.")
  }
  lat <- location$lat
  lon <- location$long
  weather_data <- weather_history(
    location = c(lat, lon),
    start = start_date,
    end = end_date,
    daily = c("temperature_2m_max", "temperature_2m_min", "temperature_2m_mean",
              "wind_speed_10m_max", "rain_sum"),
    timezone = "auto"
  )
  if (nrow(weather_data) == 0) {
    stop("Error: No weather data retrieved. Please check the city name or date range.")
  }
  weather_data$date <- as.Date(weather_data$date)
  summary_stats <- weather_data %>%
    summarise(
      `Average Temperature (°C)` = mean(daily_temperature_2m_mean, na.rm = TRUE),
      `Maximum Temperature (°C)` = max(daily_temperature_2m_max, na.rm = TRUE),
      `Minimum Temperature (°C)` = min(daily_temperature_2m_min, na.rm = TRUE),
      `Maximum Wind Speed (m/s)` = max(daily_wind_speed_10m_max, na.rm = TRUE),
      `Total Rainfall (mm)` = sum(daily_rain_sum, na.rm = TRUE)
    )
  ai_analysis <- get_ai_analysis(summary_stats)
  weather_data_long <- weather_data %>%
    select(date, daily_temperature_2m_max, daily_temperature_2m_min,
           daily_temperature_2m_mean, daily_wind_speed_10m_max, daily_rain_sum) %>%
    pivot_longer(cols = -date, names_to = "type", values_to = "value")
  weather_data_long$type <- recode(weather_data_long$type,
                                   "daily_temperature_2m_max" = "Max Temperature (°C)",
                                   "daily_temperature_2m_min" = "Min Temperature (°C)",
                                   "daily_temperature_2m_mean" = "Avg Temperature (°C)",
                                   "daily_wind_speed_10m_max" = "Max Wind Speed (m/s)",
                                   "daily_rain_sum" = "Total Rainfall (Normalized)"
  )
  max_rain <- max(weather_data_long$value[weather_data_long$type == "Total Rainfall (Normalized)"], na.rm = TRUE)
  if (!is.na(max_rain) && max_rain > 0) {
    weather_data_long <- weather_data_long %>%
      mutate(value = ifelse(type == "Total Rainfall (Normalized)", value / max_rain * 100, value))
  }
  fig <- plot_ly(weather_data_long, x = ~date, y = ~value, type = 'scatter', mode = 'lines+markers',
                 color = ~type,
                 hovertemplate = paste(
                   "<b>Date:</b> %{x}<br>",
                   "<b>%{fullData.name}:</b> %{y}<extra></extra>"
                 ),
                 source = "history_plot") %>%  # Add source ID
    layout(
      title = paste("Weather Summary for", city),
      xaxis = list(title = "Date", showgrid = TRUE),
      yaxis = list(title = "Value (Temperature °C, Wind Speed m/s, Rain Normalized)", showgrid = TRUE),
      hovermode = "x unified",
      dragmode = "lasso",
      legend = list(title = list(text = "Weather Metrics"))
    )
  formatted_data <- weather_data %>%
    rename(
      "Date" = date,
      "Max Temp (°C)" = daily_temperature_2m_max,
      "Min Temp (°C)" = daily_temperature_2m_min,
      "Avg Temp (°C)" = daily_temperature_2m_mean,
      "Max Wind (m/s)" = daily_wind_speed_10m_max,
      "Rainfall (mm)" = daily_rain_sum
    )
  return(list(summary_stats = summary_stats, plot = fig, data = formatted_data, analysis = ai_analysis))
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .home-container {
        background-image: url('background.jpg');
        background-size: cover;
        background-position: center;
        min-height: 100vh;
        padding: 20px;
      }
      
      .home-content {
        background-color: rgba(255, 255, 255, 0.85);
        border-radius: 10px;
        padding: 20px;
        margin-top: 50px;
      }
      .btn-lg { margin: 10px 0; }
      .shiny-plot-output { margin-top: 20px; }
      .error-message { color: red; font-weight: bold; }
      .summary-box { 
        background-color: #f8f9fa;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 20px;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
      }
      .summary-title {
        font-weight: bold;
        margin-bottom: 10px;
      }
      .ai-analysis {
        background-color: #e9f7ef;
        border-left: 4px solid #27ae60;
        padding: 15px;
        margin: 20px 0;
        border-radius: 5px;
      }
      .ai-analysis-title {
        color: #27ae60;
        font-weight: bold;
        margin-bottom: 10px;
      }
      .selected-dates {
        background-color: #f0f8ff;
        border-left: 4px solid #3498db;
        padding: 15px;
        margin: 20px 0;
        border-radius: 5px;
      }
    "))
  ),
  uiOutput("main_ui")
)
# Add these functions after your existing get_ai_analysis function

# Function to get AI analysis for selected historical dates
get_selected_history_analysis <- function(data, selected_dates) {
  # Filter the data based on selected dates
  filtered_data <- data %>%
    filter(Date %in% selected_dates)
  
  # Calculate summary statistics for the filtered data
  filtered_summary <- filtered_data %>%
    summarise(
      `Average Temperature (°C)` = mean(`Avg Temp (°C)`, na.rm = TRUE),
      `Maximum Temperature (°C)` = max(`Max Temp (°C)`, na.rm = TRUE),
      `Minimum Temperature (°C)` = min(`Min Temp (°C)`, na.rm = TRUE),
      `Maximum Wind Speed (m/s)` = max(`Max Wind (m/s)`, na.rm = TRUE),
      `Total Rainfall (mm)` = sum(`Rainfall (mm)`, na.rm = TRUE)
    )
  
  # Get AI analysis for the filtered data
  ai_analysis <- get_ai_analysis(filtered_summary)
  
  return(list(filtered_data = filtered_data, 
              filtered_summary = filtered_summary, 
              analysis = ai_analysis))
}

# Function to get AI analysis for selected forecast dates
get_selected_forecast_analysis <- function(data, selected_dates) {
  # Filter the data based on selected dates
  filtered_data <- data %>%
    filter(Date %in% selected_dates)
  
  # Calculate summary statistics for the filtered data
  filtered_summary <- filtered_data %>%
    summarise(
      `Average Temperature (°C)` = mean((filtered_data$`Max Temp (°C)` + filtered_data$`Min Temp (°C)`) / 2, na.rm = TRUE),
      `Maximum Temperature (°C)` = max(filtered_data$`Max Temp (°C)`, na.rm = TRUE),
      `Minimum Temperature (°C)` = min(filtered_data$`Min Temp (°C)`, na.rm = TRUE),
      `Maximum Wind Speed (m/s)` = 0, # Not in forecast data, placeholder
      `Total Rainfall (mm)` = sum(filtered_data$`Rainfall (mm)`, na.rm = TRUE)
    )
  
  # Get AI analysis for the filtered data
  ai_analysis <- get_ai_analysis(filtered_summary)
  
  return(list(filtered_data = filtered_data, 
              filtered_summary = filtered_summary, 
              analysis = ai_analysis))
}

# Modify the server function
# Modify the server function to add analyze buttons and change functionality
server <- function(input, output, session) {
  show_forecast_headings <- reactiveVal(FALSE)
  show_history_headings <- reactiveVal(FALSE)
  selected_history_dates <- reactiveVal(NULL)
  selected_forecast_dates <- reactiveVal(NULL)
  history_selected_analysis <- reactiveVal(NULL)
  forecast_selected_analysis <- reactiveVal(NULL)
  
  output$main_ui <- renderUI({
    # Keep existing main_ui code...
    if (!isTruthy(input$history) && !isTruthy(input$forecast)) {
      # Home screen with background image
      div(class = "home-container",
          div(class = "home-content",
              h2("Welcome to Weather Prediction App", align = "center"),
              hr(),
              fluidRow(
                column(6, actionButton("history", "Historical Weather", class = "btn-primary btn-lg", width = "100%")),
                column(6, actionButton("forecast", "Future Forecast", class = "btn-success btn-lg", width = "100%"))
              ),
              hr(),
              div(style = "text-align: center; padding: 10px; font-size: 14px; color: grey;",
                  "© 2025 Weather App | All Rights Reserved"),
              hr()
          )
      )
    } else if (isTruthy(input$history)) {
      # Historical weather UI
      fluidPage(
        if(show_history_headings()) {
          h3("Historical Weather Data", align = "center")
        },
        sidebarLayout(
          sidebarPanel(
            dateRangeInput("dateRange", "Select Date Range:", 
                           start = Sys.Date() - 7, end = Sys.Date()),
            textInput("city", "Enter City:", "New Delhi"),
            actionButton("show_history", "Show History", class = "btn-primary", width = "100%"),
            hr(),
            # Add "Back to Home" button
            actionButton("back", "Back to Home", class = "btn-secondary", width = "100%")
          ),
          mainPanel(
            uiOutput("error_history"),
            uiOutput("summary_box"),
            plotlyOutput("history_plot", height = "400px"),
            # Display selected dates count and analyze button
            uiOutput("selected_history_dates_count"),
            uiOutput("history_analyze_button"),
            # Area for analysis results (will only show after button click)
            uiOutput("selected_history_analysis"),
            if(show_history_headings()) {
              h4("Detailed Weather Data")
            },
            dataTableOutput("history_data"),
            uiOutput("ai_analysis_history")
          )
        )
      )
    } else if (isTruthy(input$forecast)) {
      # Forecast UI
      fluidPage(
        if(show_forecast_headings()) {
          h3("Weather Forecast", align = "center")
        },
        sidebarLayout(
          sidebarPanel(
            textInput("forecast_city", "Enter City:", "New Delhi"),
            radioButtons("days", "Select Forecast:",
                         choices = list("Next Day" = 1, "Next 2 Days" = 2, "Next 3 Days" = 3)),
            actionButton("show_forecast", "Show Forecast", class = "btn-success", width = "100%"),
            hr(),
            # Add "Back to Home" button
            actionButton("back", "Back to Home", class = "btn-secondary", width = "100%")
          ),
          mainPanel(
            uiOutput("error_forecast"),
            plotlyOutput("forecast_plot", height = "400px"),
            # Display selected dates count and analyze button
            uiOutput("selected_forecast_dates_count"),
            uiOutput("forecast_analyze_button"),
            # Area for analysis results (will only show after button click)
            uiOutput("selected_forecast_analysis"),
            if(show_forecast_headings()) {
              h4("Forecast Data")
            },
            dataTableOutput("forecast_data"),
            uiOutput("ai_analysis_forecast")
          )
        )
      )
    }
  })
  
  observeEvent(input$back, {
    updateActionButton(session, "history", label = "Historical Weather")
    updateActionButton(session, "forecast", label = "Future Forecast")
    show_forecast_headings(FALSE)
    show_history_headings(FALSE)
    # Reset selected dates when going back
    selected_history_dates(NULL)
    selected_forecast_dates(NULL)
    history_selected_analysis(NULL)
    forecast_selected_analysis(NULL)
  })
  
  # History data handling
  history_results <- reactiveVal(NULL)
  
  observeEvent(input$show_history, {
    output$error_history <- renderUI(NULL)
    tryCatch({
      results <- get_weather_summary_with_analysis(
        input$city,
        input$dateRange[1],
        input$dateRange[2]
      )
      history_results(results)
      show_history_headings(TRUE)
      selected_history_dates(NULL)
      history_selected_analysis(NULL)
    }, error = function(e) {
      output$error_history <- renderUI(
        div(class = "error-message", "Error: ", e$message)
      )
      history_results(NULL)
      show_history_headings(FALSE)
    })
  })
  
  # Listen for plotly_selected event on history plot
  observeEvent(event_data("plotly_selected", source = "history_plot"), {
    req(history_results())
    selected_data <- event_data("plotly_selected", source = "history_plot")
    
    if(!is.null(selected_data)) {
      # Extract unique dates
      dates <- sort(unique(as.Date(selected_data$x)))
      
      if(length(dates) > 0) {
        selected_history_dates(dates)
        # We no longer process the analysis immediately
        # Instead, we'll wait for the button click
        history_selected_analysis(NULL)
      }
    } else {
      # Clear selection if nothing is selected
      selected_history_dates(NULL)
      history_selected_analysis(NULL)
    }
  })
  
  # Display count of selected dates instead of the dates themselves
  output$selected_history_dates_count <- renderUI({
    dates <- selected_history_dates()
    
    if(!is.null(dates) && length(dates) > 0) {
      div(
        h4("Selected Data:"),
        p(paste("You've selected", length(dates), "date(s) using lasso selection."))
      )
    }
  })
  
  # Display analyze button when dates are selected
  output$history_analyze_button <- renderUI({
    dates <- selected_history_dates()
    
    if(!is.null(dates) && length(dates) > 0) {
      actionButton("analyze_history_selection", "Analyze Selected Dates", 
                   class = "btn-info", width = "100%")
    }
  })
  
  # Process analysis when Analyze button is clicked
  observeEvent(input$analyze_history_selection, {
    req(history_results(), selected_history_dates())
    dates <- selected_history_dates()
    
    tryCatch({
      selected_analysis <- get_selected_history_analysis(
        history_results()$data,
        dates
      )
      history_selected_analysis(selected_analysis)
    }, error = function(e) {
      # Handle errors silently
      history_selected_analysis(NULL)
    })
  })
  
  # Render the selected history analysis UI (only after button click)
  output$selected_history_analysis <- renderUI({
    req(history_selected_analysis())
    analysis <- history_selected_analysis()
    
    if(!is.null(analysis)) {
      div(class = "selected-dates-analysis",
          h4("Weather Summary for Selected Dates:"),
          fluidRow(
            column(6, 
                   p(strong("Average Temperature: "), 
                     round(analysis$filtered_summary$`Average Temperature (°C)`, 1), "°C"),
                   p(strong("Maximum Temperature: "), 
                     round(analysis$filtered_summary$`Maximum Temperature (°C)`, 1), "°C")
            ),
            column(6, 
                   p(strong("Minimum Temperature: "), 
                     round(analysis$filtered_summary$`Minimum Temperature (°C)`, 1), "°C"),
                   p(strong("Total Rainfall: "), 
                     round(analysis$filtered_summary$`Total Rainfall (mm)`, 1), "mm")
            )
          ),
          
          # AI analysis for selected dates
          div(class = "ai-analysis",
              div(class = "ai-analysis-title", "AI Analysis for Selected Dates:"),
              HTML(markdown::markdownToHTML(text = analysis$analysis, fragment.only = TRUE))
          ),
          
          # Table with the filtered data
          h4("Data for Selected Dates:"),
          renderDataTable({
            analysis$filtered_data
          })
      )
    }
  })
  
  # Rest of history-related outputs
  output$summary_box <- renderUI({
    req(history_results())
    stats <- history_results()$summary_stats
    div(class = "summary-box",
        div(class = "summary-title", "Weather Summary Statistics:"),
        fluidRow(
          column(6, 
                 p(strong("Average Temperature: "), round(stats$`Average Temperature (°C)`, 1), "°C"),
                 p(strong("Maximum Temperature: "), round(stats$`Maximum Temperature (°C)`, 1), "°C")
          ),
          column(6, 
                 p(strong("Minimum Temperature: "), round(stats$`Minimum Temperature (°C)`, 1), "°C"),
                 p(strong("Total Rainfall: "), round(stats$`Total Rainfall (mm)`, 1), "mm")
          )
        )
    )
  })
  
  output$history_plot <- renderPlotly({
    req(history_results())
    p <- history_results()$plot
    # Make sure lasso selection is enabled
    p %>% layout(dragmode = "lasso")
  })
  
  output$history_data <- renderDataTable({
    req(history_results())
    history_results()$data
  })
  
  output$ai_analysis_history <- renderUI({
    req(history_results())
    analysis <- history_results()$analysis
    div(class = "ai-analysis",
        div(class = "ai-analysis-title", "AI-Powered Weather Analysis:"),
        HTML(markdown::markdownToHTML(text = analysis, fragment.only = TRUE))
    )
  })
  
  # Forecast data handling
  forecast_results <- reactiveVal(NULL)
  
  observeEvent(input$show_forecast, {
    output$error_forecast <- renderUI(NULL)
    tryCatch({
      results <- get_weather_forecast_with_analysis(
        input$forecast_city,
        as.numeric(input$days)
      )
      forecast_results(results)
      show_forecast_headings(TRUE)
      selected_forecast_dates(NULL)
      forecast_selected_analysis(NULL)
    }, error = function(e) {
      output$error_forecast <- renderUI(
        div(class = "error-message", "Error: ", e$message)
      )
      forecast_results(NULL)
      show_forecast_headings(FALSE)
    })
  })
  
  # Listen for plotly_selected event on forecast plot
  observeEvent(event_data("plotly_selected", source = "forecast_plot"), {
    req(forecast_results())
    selected_data <- event_data("plotly_selected", source = "forecast_plot")
    
    if(!is.null(selected_data)) {
      # Extract unique dates
      dates <- sort(unique(as.Date(selected_data$x)))
      
      if(length(dates) > 0) {
        selected_forecast_dates(dates)
        # We no longer process the analysis immediately
        # Instead, we'll wait for the button click
        forecast_selected_analysis(NULL)
      }
    } else {
      # Clear selection if nothing is selected
      selected_forecast_dates(NULL)
      forecast_selected_analysis(NULL)
    }
  })
  
  # Display count of selected dates instead of the dates themselves
  output$selected_forecast_dates_count <- renderUI({
    dates <- selected_forecast_dates()
    
    if(!is.null(dates) && length(dates) > 0) {
      div(
        h4("Selected Data:"),
        p(paste("You've selected", length(dates), "date(s) using lasso selection."))
      )
    }
  })
  
  # Display analyze button when dates are selected
  output$forecast_analyze_button <- renderUI({
    dates <- selected_forecast_dates()
    
    if(!is.null(dates) && length(dates) > 0) {
      actionButton("analyze_forecast_selection", "Analyze Selected Dates", 
                   class = "btn-info", width = "100%")
    }
  })
  
  # Process analysis when Analyze button is clicked
  observeEvent(input$analyze_forecast_selection, {
    req(forecast_results(), selected_forecast_dates())
    dates <- selected_forecast_dates()
    
    tryCatch({
      selected_analysis <- get_selected_forecast_analysis(
        forecast_results()$data,
        dates
      )
      forecast_selected_analysis(selected_analysis)
    }, error = function(e) {
      # Handle errors silently
      forecast_selected_analysis(NULL)
    })
  })
  
  # Render the selected forecast analysis UI (only after button click)
  output$selected_forecast_analysis <- renderUI({
    req(forecast_selected_analysis())
    analysis <- forecast_selected_analysis()
    
    if(!is.null(analysis)) {
      div(class = "selected-dates-analysis",
          h4("Weather Summary for Selected Dates:"),
          fluidRow(
            column(6, 
                   p(strong("Average Temperature: "), 
                     round(analysis$filtered_summary$`Average Temperature (°C)`, 1), "°C"),
                   p(strong("Maximum Temperature: "), 
                     round(analysis$filtered_summary$`Maximum Temperature (°C)`, 1), "°C")
            ),
            column(6, 
                   p(strong("Minimum Temperature: "), 
                     round(analysis$filtered_summary$`Minimum Temperature (°C)`, 1), "°C"),
                   p(strong("Total Rainfall: "), 
                     round(analysis$filtered_summary$`Total Rainfall (mm)`, 1), "mm")
            )
          ),
          
          # AI analysis for selected dates
          div(class = "ai-analysis",
              div(class = "ai-analysis-title", "AI Analysis for Selected Dates:"),
              HTML(markdown::markdownToHTML(text = analysis$analysis, fragment.only = TRUE))
          ),
          
          # Table with the filtered data
          h4("Data for Selected Dates:"),
          renderDataTable({
            analysis$filtered_data
          })
      )
    }
  })
  
  # Rest of forecast-related outputs
  output$forecast_plot <- renderPlotly({
    req(forecast_results())
    p <- forecast_results()$plot
    # Make sure lasso selection is enabled
    p %>% layout(dragmode = "lasso")
  })
  
  output$forecast_data <- renderDataTable({
    req(forecast_results())
    forecast_results()$data
  })
  
  output$ai_analysis_forecast <- renderUI({
    req(forecast_results())
    analysis <- forecast_results()$analysis
    div(class = "ai-analysis",
        div(class = "ai-analysis-title", "AI-Powered Forecast Analysis:"),
        HTML(markdown::markdownToHTML(text = analysis, fragment.only = TRUE))
    )
  })
}
shinyApp(ui, server)