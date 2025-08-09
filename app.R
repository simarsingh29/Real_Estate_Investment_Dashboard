# Load Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(leaflet)

# Sample Data
cities <- c("Bangalore", "Hyderabad", "Pune", "Mumbai", "Gurugram", "Chennai")
property_types <- c("Apartment", "Office Space", "Villa", "Commercial Complex", "Plot")
years <- 2020:2025

set.seed(123)
investment_data <- expand.grid(City = cities, Property = property_types, Year = years) %>%
  mutate(Price = round(runif(n(), 6000000, 25000000)))

# City coordinates for map with additional detail links
city_coords <- data.frame(
  City = cities,
  lat = c(12.9716, 17.3850, 18.5204, 19.0760, 28.4595, 13.0827),
  lng = c(77.5946, 78.4867, 73.8567, 72.8777, 77.0266, 80.2707),
  url = c(
    "https://www.magicbricks.com/property-for-sale/residential-real-estate-Bangalore",
    "https://www.magicbricks.com/property-for-sale/residential-real-estate-Hyderabad",
    "https://www.magicbricks.com/property-for-sale/residential-real-estate-Pune",
    "https://www.magicbricks.com/property-for-sale/residential-real-estate-Mumbai",
    "https://www.magicbricks.com/property-for-sale/residential-real-estate-Gurgaon",
    "https://www.magicbricks.com/property-for-sale/residential-real-estate-Chennai"
  )
)

# Define colors for each property type
property_colors <- c(
  "Apartment" = "#3498db",
  "Office Space" = "#e67e22",
  "Villa" = "#9b59b6",
  "Commercial Complex" = "#1abc9c",
  "Plot" = "#f39c12"
)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("body { background: url('https://images.unsplash.com/photo-1600585154340-be6161a56a0c') no-repeat center center fixed; background-size: cover; } .overview-bg { background-color: rgba(255,255,255,0.85); padding: 30px; border-radius: 15px; margin-top: 20px; } h3, h4, h2, p { color: #2c3e50; } .stat-box { background: white; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); padding: 20px; margin-bottom: 20px; } .credit { text-align: center; font-style: italic; color: #888; margin-top: 20px; font-size: 13px; }"))
  ),
  navbarPage("Real Estate Investment by Simardeep Singh",
             tabPanel("Overview",
                      div(class = "overview-bg",
                          h3("\U0001F4CA Real Estate Investment Overview"),
                          fluidRow(
                            column(4, div(class = "stat-box", h4("\U0001F3D9 Total Properties"), h2(textOutput("totalProps")), p("Analyzed across India"))),
                            column(4, div(class = "stat-box", h4("\U0001F4B0 Avg Price (INR)"), h2(textOutput("avgPrice")), p("Based on 2020–2025 data"))),
                            column(4, div(class = "stat-box", h4("\U0001F3C6 Top Investment City"), h2(textOutput("topCity")), p("With highest total investment")))
                          ),
                          br(),
                          fluidRow(
                            column(6, div(class = "stat-box", h4("\U0001F4CD City-wise Investment Share"), withSpinner(plotlyOutput("cityDonut"), type = 6))),
                            column(6, div(class = "stat-box", h4("\U0001F3D8 Property-wise Investment Share"), withSpinner(plotlyOutput("propertyDonut"), type = 6)))
                          ),
                          br(),
                          div(class = "stat-box", h4("\U0001F4C8 Investment Trend Over Years"), withSpinner(plotlyOutput("trendPlot"), type = 6)),
                          div(class = "credit", "Dashboard designed and developed by Simardeep Singh")
                      )
             ),
             tabPanel("Learn Investment",
                      div(class = "overview-bg",
                          h3("\U0001F393 Learn Real Estate Investment"),
                          p("Explore curated resources to understand the fundamentals of investing in Indian real estate."),
                          tags$ul(
                            tags$li(tags$a(href = "https://www.youtube.com/watch?v=WlqYhER9Dd8", target = "_blank", "Real Estate Investment for Beginners")),
                            tags$li(tags$a(href = "https://www.youtube.com/watch?v=Yoa0zjSLSkw", target = "_blank", "Residential vs Commercial Investment")),
                            tags$li(tags$a(href = "https://www.youtube.com/watch?v=1azmtbrch8o", target = "_blank", "Property Selection Strategy")),
                            tags$li(tags$a(href = "https://www.tn.gov/content/dam/tn/commerce/documents/securities/posts/The-Basics-of-Savings-and-Investing.pdf", target = "_blank", download = NA, "Download Real Estate Investment Guide (PDF)"))
                          )
                      )
             ),
             tabPanel("Past Statistics",
                      div(class = "overview-bg",
                          h3("\U0001F4C9 Past Investment Trends"),
                          fluidRow(
                            column(4,
                                   pickerInput("pastCity", "Choose City:", choices = cities),
                                   pickerInput("pastProperty", "Choose Property Type:", choices = property_types),
                                   actionButton("searchBtn", "Show Data", class = "btn btn-primary")
                            ),
                            column(8,
                                   div(class = "stat-box", h4("\U0001F4CA Price Over the Years"), withSpinner(plotOutput("pastPlot", height = "350px"), type = 6)),
                                   div(class = "stat-box", h4("\U0001F4C4 Price Table"), withSpinner(tableOutput("pastTable")))
                            )
                          )
                      )
             ),
             tabPanel("Present Statistics",
                      div(class = "overview-bg",
                          h3("\U0001F4CC Present Investment Snapshot"),
                          fluidRow(
                            column(4,
                                   pickerInput("presentCity", "Select City:", choices = cities),
                                   pickerInput("presentProperty", "Select Property:", choices = property_types)
                            ),
                            column(8,
                                   div(class = "stat-box", h4("\U0001F4CA 2025 Prices Comparison"), withSpinner(plotlyOutput("presentPlot", height = "350px"), type = 6)),
                                   div(class = "stat-box", h4("\U0001F4C4 Current Year Price Table"), withSpinner(tableOutput("presentTable")))
                            )
                          )
                      )
             ),
             tabPanel("Map View",
                      div(class = "overview-bg",
                          h3("\U0001F5FA Investment Locations in India"),
                          leafletOutput("map", height = 500)
                      )
             ),
             tabPanel("About",
                      div(class = "overview-bg",
                          h3("\U0001F4DA About This Dashboard"),
                          p("This real estate investment dashboard is a comprehensive tool to visualize property trends across major Indian cities. Developed by Simardeep Singh, it provides key statistics, visual insights, and educational content to help users make informed investment decisions."),
                          p("The dashboard includes historical trends, current statistics, a map view, and learning resources, all presented in a clean and interactive interface. Built using R Shiny and Plotly.")
                      )
             )
  )
)

# Server
server <- function(input, output, session) {
  output$totalProps <- renderText({ nrow(investment_data) })
  output$avgPrice <- renderText({ paste0("\u20B9 ", comma(round(mean(investment_data$Price)))) })
  output$topCity <- renderText({
    investment_data %>% group_by(City) %>% summarise(Total = sum(Price)) %>% arrange(desc(Total)) %>% slice(1) %>% pull(City)
  })
  
  output$cityDonut <- renderPlotly({
    city_data <- investment_data %>% group_by(City) %>% summarise(Total = sum(Price))
    plot_ly(city_data, labels = ~City, values = ~Total, type = 'pie', hole = 0.5) %>% layout(title = "Investment by City")
  })
  
  output$propertyDonut <- renderPlotly({
    prop_data <- investment_data %>% group_by(Property) %>% summarise(Total = sum(Price))
    plot_ly(prop_data, labels = ~Property, values = ~Total, type = 'pie', hole = 0.5) %>% layout(title = "Investment by Property")
  })
  
  output$trendPlot <- renderPlotly({
    trend <- investment_data %>% group_by(Year) %>% summarise(Total = sum(Price))
    ggplotly(ggplot(trend, aes(x = Year, y = Total)) +
               geom_line(color = "#2980b9", size = 1.5) +
               geom_point(size = 3, color = "#3498db") +
               labs(y = "Total Investment (INR)", x = "Year") +
               theme_minimal())
  })
  
  observeEvent(input$searchBtn, {
    pastData <- investment_data %>% filter(City == input$pastCity, Property == input$pastProperty)
    output$pastPlot <- renderPlot({
      ggplot(pastData, aes(x = factor(Year), y = Price, fill = Property)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = comma(Price)), vjust = -0.5, color = "white") +
        scale_fill_manual(values = property_colors) +
        labs(title = paste(input$pastCity, "-", input$pastProperty), x = "Year", y = "Price (INR)") +
        theme_minimal()
    })
    output$pastTable <- renderTable(pastData)
  })
  
  output$presentPlot <- renderPlotly({
    presData <- investment_data %>% filter(City == input$presentCity, Property == input$presentProperty, Year == 2025)
    ggplotly(ggplot(presData, aes(x = City, y = Price, fill = Property)) +
               geom_col(show.legend = FALSE) +
               scale_fill_manual(values = property_colors) +
               labs(title = paste("2025 -", input$presentCity, "-", input$presentProperty), y = "Price", x = "") +
               theme_minimal())
  })
  
  output$presentTable <- renderTable({
    investment_data %>% filter(City == input$presentCity, Property == input$presentProperty, Year == 2025)
  })
  
  output$map <- renderLeaflet({
    leaflet(city_coords) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lng, ~lat,
        label = ~City,
        color = "blue",
        radius = 6,
        fillOpacity = 0.8,
        popup = ~paste0("<b>", City, "</b><br><a href='", url, "' target='_blank'>View Properties</a>")
      )
  })
}

# Run App
shinyApp(ui, server)