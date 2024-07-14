library(shiny)
library(leaflet)
library(httr)

# Hàm để lấy dữ liệu thời tiết từ API
get_weather_data <- function(lat, lon) {
  api_key <- "4b69145ad81b2e5956920f3ad131e621" # Thay thế YOUR_API_KEY bằng khóa API thật của bạn từ OpenWeatherMap
  url <- paste0("https://api.openweathermap.org/data/2.5/weather?lat=", lat, "&lon=", lon, "&units=metric&appid=", api_key)
  response <- GET(url)
  content(response, "parsed")
}

# UI của ứng dụng
ui <- fluidPage(
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("Current Weather"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2/css/all.min.css"),
    tags$style(HTML("
      .container {
        width: 100%;
      }
      .title {
        font-size: 66px;
        font-weight: 900;
      }
      .title-map {
        font-weight: 900;
      }
      .weather-container {
        display: flex;
        justify-content: space-between;
        flex-wrap: wrap;
      }
      .weather-container-left {
        width: 45%;
        padding: 20px;
      }
      .weather-container-right {
        width: 50%;
        padding: 20px;
      }
      .title-small {
        font-size: 50px;
        font-weight: 900;
      }
      .date {
        font-weight: 700;
        padding: 10px 0;
      }
      .title-temperature {
        font-size: 30px;
        padding: 10px 0;
      }
      .temperature {
        font-size: 30px;
      }
      .temperature-top {
        display: flex;
        flex-wrap: wrap;
      }
      .body-item {
        flex: 1;
        margin: 10px;
        border: 1px solid #c32020;
        border-radius: 4px;
        min-width: 200px; /* Ensure minimum width for the boxes */
      }
      .title-item {
        color: #fff;
        border: 1px solid #c32020;
        border-color: transparent transparent #c32020 transparent;
        padding: 10px;
      }
      .temperature-top .title-item.title-item-1 {
        background-color: #c32020;
      }
      .temperature-top .title-item.title-item-2 {
        background-color: #20c32e;
      }
      .temperature-top .title-item.title-item-3 {
        background-color: #3a82e6;
      }
      .temperature-top .title-item.title-item-4 {
        background-color: #c715a1;
      }
      .temperature-top .title-item.title-item-5 {
        background-color: #4d90c2;
      }
      .temperature-top .title-item.title-item-6 {
        background-color: #8f1d1d;
      }
      .temperature-item {
        padding: 10px;
      }
      .weather-container-right {
        border: 1px solid #ccc;
      }
      .location-container {
        display: inline-flex;
        align-items: center;
        font-size: 35px; /* Increase the font size */
        font-weight: 900; /* Make the text bold */
      }
      .location-container i {
        margin-right: 10px;
      }
    "))
  ),
  div(class = "container",
      h1(class = "title", "Current Weather"),
      div(class = "weather-container",
          div(class = "weather-container-left",
              div(class = "location-container",
                  icon("location-crosshairs"),
                  textOutput("location")
              ),
              div(class = "date", textOutput("date")),
              div(class = "title-temperature", icon("temperature-three-quarters"), " Current Temperature"),
              div(class = "temperature", textOutput("temperature")),
              div(class = "temperature-top",
                  div(class = "body-item",
                      div(class = "title-item title-item-1", "Feels Like"),
                      div(class = "temperature-item", textOutput("feels_like"))
                  ),
                  div(class = "body-item",
                      div(class = "title-item title-item-2", "Humidity"),
                      div(class = "temperature-item", textOutput("humidity"))
                  ),
                  div(class = "body-item",
                      div(class = "title-item title-item-3", "Weather Condition"),
                      div(class = "temperature-item", textOutput("weather"))
                  ),
                  div(class = "body-item",
                      div(class = "title-item title-item-4", "Visibility"),
                      div(class = "temperature-item", textOutput("visibility"))
                  ),
                  div(class = "body-item",
                      div(class = "title-item title-item-5", "Wind Speed"),
                      div(class = "temperature-item", textOutput("wind_speed"))
                  ),
                  div(class = "body-item",
                      div(class = "title-item title-item-6", "Air Pressure"),
                      div(class = "temperature-item", textOutput("pressure"))
                  )
              )
          ),
          div(class = "weather-container-right",
              div(class = "title-map", "MAP"),
              leafletOutput("map"),
              verbatimTextOutput("weather_info")
          )
      )
  )
)

# Server của ứng dụng
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 105.85, lat = 21.02, zoom = 5) # Tọa độ trung tâm mặc định là Hà Nội
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lon <- click$lng
    
    weather_data <- get_weather_data(lat, lon)
    
    output$location <- renderText({
      if (!is.null(weather_data)) {
        weather_data$name
      } else {
        "N/A"
      }
    })
    
    output$date <- renderText({
      if (!is.null(weather_data)) {
        format(Sys.Date(), "%d-%m-%Y")
      } else {
        "N/A"
      }
    })
    
    output$temperature <- renderText({
      if (!is.null(weather_data)) {
        paste(weather_data$main$temp, "°C")
      } else {
        "N/A"
      }
    })
    
    output$feels_like <- renderText({
      if (!is.null(weather_data)) {
        paste(weather_data$main$feels_like, "°C")
      } else {
        "N/A"
      }
    })
    
    output$weather <- renderText({
      if (!is.null(weather_data)) {
        weather_data$weather[[1]]$description
      } else {
        "N/A"
      }
    })
    
    output$humidity <- renderText({
      if (!is.null(weather_data)) {
        paste(weather_data$main$humidity, "%")
      } else {
        "N/A"
      }
    })
    
    output$visibility <- renderText({
      if (!is.null(weather_data)) {
        if (!is.null(weather_data$visibility)) {
          paste(weather_data$visibility / 1000, "km")
        } else {
          "N/A"
        }
      } else {
        "N/A"
      }
    })
    
    output$wind_speed <- renderText({
      if (!is.null(weather_data)) {
        paste(weather_data$wind$speed, "m/s")
      } else {
        "N/A"
      }
    })
    
    output$pressure <- renderText({
      if (!is.null(weather_data)) {
        paste(weather_data$main$pressure, "hPa")
      } else {
        "N/A"
      }
    })
  })
}

# Chạy ứng dụng Shiny
shinyApp(ui = ui, server = server)
