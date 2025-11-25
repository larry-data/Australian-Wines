# ...existing code...
library(shiny)
library(shinythemes)
library(tidyverse)
library(fpp3)
library(gt)
library(here)
library(zoo)
library(lubridate)

# Load & prepare data once
aus_wine <- readr::read_csv(here::here("AustralianWines.csv"), na = "*",
                            col_types = cols(Rose = col_number()),
                            show_col_types = FALSE) |>
  fill(Rose, .direction = "down") |>
  mutate(Month = mdy(str_replace(Month, '-', '-01-')) |> yearmonth())

aus_wine_ts <- aus_wine |>
  pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal) |>
  mutate(Varietal = str_trim(Varietal))

varietals <- c("All", aus_wine_ts |> distinct(Varietal) |> arrange(Varietal) |> pull())

min_date <- as.Date(min(aus_wine_ts$Month))
max_date <- as.Date(max(aus_wine_ts$Month))
default_cutoff <- as.Date(yearmonth("1993 Dec"))

ui <- navbarPage(
  title = "Australian Wines Time Series",
  theme = shinytheme("flatly"),

  tabPanel("Visualization",
    sidebarLayout(
      sidebarPanel(
        selectInput("viz_varietal", "Varietal", choices = varietals, selected = "All"),
        sliderInput("cutoff", "Training end (cutoff)", min = min_date, max = max_date,
                    value = default_cutoff, timeFormat = "%Y-%m-%d")
      ),
      mainPanel(
        plotOutput("viz_plot", height = "700px")
      )
    )
  ),

  tabPanel("Model Building",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("models", "Models to fit", choices = c("TSLM","ETS","ARIMA"),
                           selected = c("TSLM","ETS","ARIMA")),
        actionButton("fit_models", "Fit models"),
        hr(),
        checkboxInput("show_train_acc", "Show training accuracy", value = TRUE),
        checkboxInput("show_val_acc", "Show forecast (validation) accuracy", value = TRUE)
      ),
      mainPanel(
        h4("Model specs"),
        verbatimTextOutput("model_specs"),    # brief mable summary
        conditionalPanel(condition = "input.show_train_acc == true",
                         gt_output("train_acc")),
        conditionalPanel(condition = "input.show_val_acc == true",
                         gt_output("val_acc"))
      )
    )
  ),

  tabPanel("Forecast",
    sidebarLayout(
      sidebarPanel(
        selectInput("fc_model_choice", "Model for forecast table", choices = c("arima","ets","tslm"), selected = "arima"),
        numericInput("horizon", "Forecast horizon (months)", value = 12, min = 1, max = 60),
        actionButton("reforecast", "Recompute forecasts")
      ),
      mainPanel(
        plotOutput("fc_plot", height = "700px"),
        hr(),
        gt_output("fc_table")
      )
    )
  ),

  tabPanel("About",
    fluidPage(
      fluidRow(column(12,
        h4("About"),
        p("This Australian Wines Time Series Analysis Shiny provides a comprehensive overview of the sales trends of various wine varietals in Australia. 
            The analysis includes data visualization, model building, forecast accuracy evaluation, and detailed forecasting. "),
        p("The document is structured into four main tabs: Visualization, Model Building, Forecast, and About. Each tab offers insights into different aspects of the time series analysis, 
          from initial data exploration to advanced forecasting techniques."),
        p('Data Source: The dataset used in this analysis is sourced from the Australian government’s open data portal, 
        which provides monthly sales figures for different wine varietals across Australia. 
        The dataset used in this analysis is specifically from "AustralianWines.csv", which contains monthly sales data for various wine varietals.'),  
        p("Technologies Used: The analysis is conducted using R programming language, leveraging packages such as fpp3 for time series analysis, 
          ggplot2 for data visualization, and shiny for interactive web applications."),
        p("Intended Audience: This document is intended for data analysts, statisticians, and anyone interested in time series forecasting and the Australian wine market."),
        p("Developed by: Larry (Lei) Lin")
      ))
    )
  )
)

server <- function(input, output, session) {

  cutoff_ym <- reactive({
    req(input$cutoff)
    yearmonth(as.Date(input$cutoff))
  })

  # Visualization plot
  output$viz_plot <- renderPlot({
    cd <- cutoff_ym()
    df <- if (input$viz_varietal == "All") {
      aus_wine_ts
    } else {
      aus_wine_ts |> filter(Varietal == input$viz_varietal)
    }

    df |> autoplot(Sales) +
      facet_wrap(~Varietal, scales = "free_y", ncol = 2) +
      labs(title = "Australian Wine Sales by Varietal", y = "Sales Volume") +
      theme_minimal() +
      geom_vline(xintercept = as.Date(cd), linetype = "dashed", color = "black", linewidth = 0.6)
  })

  # Reactive training data (up to cutoff)
  train_data <- reactive({
    aus_wine_ts |> filter(Month <= cutoff_ym())
  })

  # Fit models when button pressed
  fitted_models <- eventReactive(input$fit_models, {
    sel <- input$models
    validate(need(length(sel) > 0, "Select at least one model to fit."))

    # build model call dynamically
    specs <- list()
    if ("TSLM" %in% sel) specs$TSLM <- TSLM(Sales ~ trend() + season())
    if ("ETS" %in% sel)  specs$ETS  <- ETS(Sales)
    if ("ARIMA" %in% sel) specs$ARIMA <- ARIMA(Sales)

    tryCatch(
      {
        train_data() |> model(!!!specs)
      },
      error = function(e) {
        showNotification(paste("Model fit error:", e$message), type = "error", duration = 6)
        NULL
      }
    )
  }, ignoreNULL = FALSE)

  output$model_specs <- renderText({
    fm <- fitted_models()
    if (is.null(fm)) return("No models fitted yet.")
    capture.output(print(fm))
  })

  # Training accuracy table
  output$train_acc <- render_gt({
    fm <- fitted_models()
    req(fm)
    tbl <- fm |> accuracy() |> select(Varietal, .model, RMSE, MAE, MAPE) |> arrange(.model, RMSE)
    tbl |> gt() |> fmt_number(decimals = 2)
  })

  # Forecasts (recompute on reforecast button or after fit)
  forecasts <- eventReactive(list(input$reforecast, fitted_models()), {
    fm <- fitted_models()
    req(fm)
    h <- paste0(input$horizon, " months")
    tryCatch(
      {
        fm |> forecast(h = h)
      },
      error = function(e) {
        showNotification(paste("Forecast error:", e$message), type = "error", duration = 6)
        NULL
      }
    )
  }, ignoreNULL = FALSE)

  # Validation accuracy table (accuracy of fc vs full data)
  output$val_acc <- render_gt({
    fc <- forecasts()
    req(fc)
    acc_tbl <- accuracy(fc, aus_wine_ts) |> select(Varietal, .model, RMSE, MAE, MAPE) |> arrange(Varietal, RMSE)
    acc_tbl |> group_by(Varietal) |> gt() |> fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2) |> tab_header(title = "Forecast Accuracy (Validation Set)")
  })

  # Forecast plots
  output$fc_plot <- renderPlot({
    fc <- forecasts()
    req(fc)
    # Plot full forecast vs data (one year prior to cutoff for context)
    trn_start <- yearmonth(as.Date(cutoff_ym()) - years(1))
    fc |> autoplot(aus_wine_ts |> filter(Month >= trn_start), level = 80) +
      facet_grid(Varietal ~ .model, scales = "free_y") +
      labs(title = "Varietal Sales forecasts", y = "Sales") +
      theme_minimal() +
      geom_vline(xintercept = as.Date(cutoff_ym()), linetype = "dashed", color = "black", linewidth = 0.6)
  })

  # Forecast table (model chosen by user)
  output$fc_table <- render_gt({
    fc <- forecasts()
    req(fc)
    model_choice <- tolower(input$fc_model_choice)
    tbl <- fc |>
      filter(tolower(.model) == model_choice) |>
      as_tibble() |>
      mutate(.mean = round(.mean, 0)) |>
      select(Varietal, Month, .mean) |>
      pivot_wider(names_from = Month, values_from = .mean)

    tbl |> gt() |> fmt_number(columns = everything(), decimals = 0)
  })
}

shinyApp(ui, server)
