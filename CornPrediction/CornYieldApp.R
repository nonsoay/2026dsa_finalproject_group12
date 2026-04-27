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

xgb_pred_2023 <- readRDS("../output/xgb_pred_2023.rds")
xgb_metrics <- readRDS("../output/xgb_metrics.rds")

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

ui <- page_sidebar(
  title = "Corn Yield Prediction App",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2C7A4B"
  ),
  
  sidebar = sidebar(
    width = 360,
    h4("Input Conditions"),
    
    selectInput("site", tagList(icon("map-marker-alt"), "Site:"),
                choices = sort(unique(final_train_reference$site))),
    
    selectInput("hybrid", tagList(icon("seedling"), "Hybrid:"),
                choices = sort(unique(final_train_reference$hybrid))),
    
    selectInput("previous_crop_clean", tagList(icon("leaf"), "Previous crop:"),
                choices = c("Soybean","Corn","Wheat/Small grains","Peanut","Sorghum","Cotton","Clover","Fallow","Other")),
    
    numericInput("year", "Year:", value = 2024),
    
    dateInput("plant_date", tagList(icon("calendar"), "Planting date:"), value = Sys.Date()),
    dateInput("harvest_date", tagList(icon("calendar-check"), "Harvest date:"), value = Sys.Date()),
    
    numericInput("soilp_h", "Soil pH:", value = 6.5),
    numericInput("om_pct", "Organic matter (%):", value = 2.5),
    numericInput("soilk_ppm", "Soil K:", value = 120),
    numericInput("soilp_ppm", "Soil P:", value = 45),
    
    numericInput("mean_temp", "Mean temp:", value = 22),
    numericInput("total_prcp", "Precipitation:", value = 500),
    
    actionButton("predict_btn", tagList(icon("chart-line"), "Predict Yield"), class = "btn-success")
  ),
  
  layout_columns(
    
    card(
      card_header("Predicted Yield"),
      h2(textOutput("prediction_output"), style = "color:#2C7A4B;")
    ),
    
    card(
      card_header("Model Inputs"),
      verbatimTextOutput("input_summary")
    ),
    
    card(
      card_header("Model Performance"),
      plotOutput("xgb_performance_plot"),
      tableOutput("xgb_metrics_table")
    )
  )
)


server <- function(input, output) {
  
  prediction <- eventReactive(input$predict_btn, {
    
    new_data <- tibble(
      site = input$site,
      year = input$year,
      hybrid = input$hybrid,
      previous_crop = input$previous_crop_clean,
      
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
      
      gdd = median(final_train_reference$gdd, na.rm = TRUE),
      total_radiation = median(final_train_reference$total_radiation, na.rm = TRUE),
      mean_vp = median(final_train_reference$mean_vp, na.rm = TRUE),
      
      latitude = median(final_train_reference$latitude, na.rm = TRUE),
      longitude = median(final_train_reference$longitude, na.rm = TRUE)
    )
    
    predict(final_xgb_fit, new_data = new_data)
  })
  
  output$prediction_output <- renderText({
    req(prediction())
    
    pred <- prediction()
    
    paste0(
      "Predicted yield: ",
      round(pred$.pred, 2),
      " Mg/ha"
    )
  })
  
  output$input_summary <- renderText({
    paste0(
      "Site: ", input$site, "\n",
      "Hybrid: ", input$hybrid, "\n",
      "Previous crop: ", input$previous_crop_clean, "\n",
      "Planting date: ", input$plant_date, "\n",
      "Harvest date: ", input$harvest_date
    )
  })
  
  output$xgb_performance_plot <- renderPlot({
    xgb_pred_2023 %>%
      ggplot(aes(x = yield, y = .pred)) +
      geom_point(alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(
        title = "XGBoost: Predicted vs Observed Yield",
        x = "Observed Yield (Mg/ha)",
        y = "Predicted Yield (Mg/ha)"
      ) +
      theme_minimal()
  })
  
  output$xgb_metrics_table <- renderTable({
    xgb_metrics %>%
      select(.metric, .estimate) %>%
      mutate(.estimate = round(.estimate, 3))
  })
}
  


shinyApp(ui = ui, server = server)