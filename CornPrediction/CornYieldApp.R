#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
install.packages("bslib")

library(shiny)
library(tidyverse)
library(tidymodels)
library(bslib)

final_xgb_fit <- readRDS("../output/final_xgb_fit.rds")
final_train_reference <- readRDS("../output/final_train_reference.rds")

final_train_reference <- final_train_reference %>%
  mutate(
    previous_crop_clean = case_when(
      str_detect(str_to_lower(previous_crop), "soy") ~ "Soybean",
      str_detect(str_to_lower(previous_crop), "wheat|small grain|rye") ~ "Wheat/Small grains",
      str_detect(str_to_lower(previous_crop), "corn|maize") ~ "Corn",
      str_detect(str_to_lower(previous_crop), "peanut") ~ "Peanut",
      str_detect(str_to_lower(previous_crop), "sorghum") ~ "Sorghum",
      str_detect(str_to_lower(previous_crop), "cotton") ~ "Cotton",
      str_detect(str_to_lower(previous_crop), "clover") ~ "Clover",
      str_detect(str_to_lower(previous_crop), "fallow") ~ "Fallow",
      TRUE ~ "Other"
    )
  )

titlePanel(tagList(icon("seedling"), "Corn Yield Prediction App"))

ui <- ui <- page_sidebar(
  title = "Corn Yield Prediction App",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2C7A4B",
    base_font = font_google("Lato"),
    heading_font = font_google("Playfair Display")
  ),
  
  sidebar = sidebar(
    width = 360,
    h4("Input Conditions"),
    
    selectInput(
      "site",
      tagList(icon("map-marker-alt"), "Site:"),
      choices = sort(unique(final_train_reference$site))
    ),
    
    selectInput(
      "hybrid",
      tagList(icon("seedling"), "Hybrid:"),
      choices = sort(unique(final_train_reference$hybrid))
    ),
    
    selectInput(
      "previous_crop_clean",
      tagList(icon("leaf"), "Previous crop:"),
      choices = c("Soybean", "Corn", "Wheat/Small grains", "Peanut", "Sorghum", "Cotton", "Clover", "Fallow", "Other")
    ),
    
    numericInput("year", "Year:", value = 2024),
    
    dateInput("plant_date", tagList(icon("calendar"), "Planting date:"), value = as.Date("2024-04-15")),
    dateInput("harvest_date", tagList(icon("calendar-check"), "Harvest date:"), value = as.Date("2024-09-15")),
    
    numericInput("soilp_h", tagList(icon("flask"), "Soil pH:"), value = 6.5),
    numericInput("om_pct", tagList(icon("seedling"), "Organic matter (%):"), value = 2.5),
    numericInput("soilk_ppm", tagList(icon("vial"), "Soil K (ppm):"), value = 120),
    numericInput("soilp_ppm", tagList(icon("vial"), "Soil P (ppm):"), value = 45),
    
    numericInput("mean_temp", tagList(icon("thermometer-half"), "Mean temperature (°C):"), value = 22),
    numericInput("total_prcp", tagList(icon("cloud-rain"), "Total precipitation (mm):"), value = 500),
    
    actionButton("predict_btn", "Predict Yield", class = "btn-success")
  ),
  
  layout_columns(
    card(
      card_header("Predicted Corn Yield"),
      h2(textOutput("prediction_output"), style = "color:#2C7A4B;")
    ),
    
    card(
      card_header("Model Inputs"),
      verbatimTextOutput("input_summary")
    )
  )
)
    
    mainPanel(
      h3("Predicted Corn Yield"),
      verbatimTextOutput("prediction_output")
    )


server <- function(input, output) {
  
  prediction <- eventReactive(input$predict_btn, {
    
    new_data <- tibble(
      site = input$site,
      year = input$year,
      hybrid = input$hybrid,
      previous_crop = input$previous_crop_clean,
      latitude = input$latitude,
      longitude = input$longitude,
      soilp_h = input$soilp_h,
      om_pct = input$om_pct,
      soilk_ppm = input$soilk_ppm,
      soilp_ppm = input$soilp_ppm,
      plant_doy = lubridate::yday(input$plant_date),
      harvest_doy = lubridate::yday(input$harvest_date),
      typical_plant_doy = lubridate::yday(input$plant_date),
      typical_harvest_doy = lubridate::yday(input$harvest_date),
      mean_temp = input$mean_temp,
      max_temp = input$mean_temp,
      min_temp = input$mean_temp,
      total_prcp = input$total_prcp,
      rain_days = 30,
      hot_days = 10,
      hot_nights = 10,
      gdd = input$gdd,
      total_radiation = median(final_train_reference$total_radiation, na.rm = TRUE),
      mean_vp = median(final_train_reference$mean_vp, na.rm = TRUE),
      latitude = median(final_train_reference$latitude, na.rm = TRUE),
      longitude = median(final_train_reference$longitude, na.rm = TRUE),
      gdd = median(final_train_reference$gdd, na.rm = TRUE)
    )
    
    output$input_summary <- renderText({
      paste0(
        "Site: ", input$site, "\n",
        "Hybrid: ", input$hybrid, "\n",
        "Previous crop: ", input$previous_crop_clean, "\n",
        "Planting date: ", input$plant_date, "\n",
        "Harvest date: ", input$harvest_date
      )
    })
    
    predict(final_xgb_fit, new_data = new_data)
  })
  
  output$prediction_output <- renderText({
    pred <- prediction()
    
    paste0(
      "Predicted yield: ",
      round(pred$.pred, 2),
      " Mg/ha"
    )
  })
}

actionButton(
  "predict_btn",
  tagList(icon("chart-line"), "Predict Yield"),
  class = "btn-success"
)

shinyApp(ui = ui, server = server)