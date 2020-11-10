library(shinydashboard)
library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(readxl)
library(geojsonsf)
library(osmdata)

# All data
dir <- "data/"
file <- "DVLA_FOI_LSOA_v2"
dvla <- read_csv(file = paste0(dir, file, ".csv"),
                 guess_max = 100000)

# Key
file <- "DVLA_FOI_KEY"
key <- read_csv(file = paste0(dir, file, ".csv"))

# LSOAs
dir <- "data/"
lsoa_bounds <- st_read(paste0(dir, "lsoas.shp"))

# Create list of LAs
la_list <- dvla %>%
  distinct(lan2019) %>%
  arrange(lan2019) %>%
  pull()

# Get range of years
years <- dvla %>%
  distinct(year) %>%
  pull()

nice_names <- dvla %>%
  names() %>%
  as_tibble() %>%
  left_join(key, by = c("value" = "variable")) %>%
  pull(label)

# Create CAGR function
cagr_f <- function(start, end, n_years) {
  round(((end / start) ^ (1/n_years) - 1), 4)*100
}

# Define UI for app that draws a map of a selected area, with LSOAs coloured by CAGR in car ownership per capita
ui <- fluidPage(
  titlePanel("Change in per capita car ownership by local authority"),
  sidebarLayout(
      sidebarPanel(
          selectInput("la",
                  label = h4("Choose local authority"),
                  choices = la_list,
                  selected = 1),
          sliderInput("range_of_years",
                      label = h4("Select range"),
                      min = min(years),
                      max = max(years),
                      value = c(2016, 2020),
                      ticks = FALSE,
                      sep = ""),
          checkboxGroupInput("features",
                             label = h4("Map features"),
                             choiceNames = list("Universities",
                                                "Car dealerships",
                                                "Zones with low-traffic neighbourhood (LTN)",
                                                "Place names"),
                             choiceValues = list("universities", 
                                                 "car_dealerships", 
                                                 "ltn",
                                                 "place_names")),
          br(),
          h4("Compound annual growth rate (CAGR)"),
          "CAGR is the annual rate of change that would be required for per capita car ownership to move from its value at the start of the time period to the value at the end. The graph shows the actual change, aggregated across the local authority as a whole.",
          h4("Sources"),
          "Car ownership and LTN location data from ",
          tags$a(href="https://twitter.com/Urban_Turbo/", "Scott Urban.", target="_blank"),
          "You can download the data from ",
          tags$a(href="https://drive.google.com/drive/folders/1XUJVz5UfdG7m0XDxp5EdSt2FeGik1H_G", "Google Drive.", target="_blank"),
          "Map features from Open Street Map, accessed using ", 
          tags$a(href="https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html", "osmdata() package.", target="_blank"),
          "Small area boundaries are 2011 census lower-layer super output areas (LSOAs), accessed from ", 
          tags$a(href="https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-december-2011-ew-bsc-v2", "Open Geography Portal.", target="_blank"),
          h4("Dashboard creation"),
          "This app was created by ",
          tags$a(href="https://twitter.com/chrisb_key/", "Chris C Brown", target="_blank"),
          "using ",
          tags$a(href="https://www.shinyapps.io/", "Shiny", target="_blank"),
          "in R. Any development requests or feedback warmly recieved. You can find the source code on ",
          tags$a(href="https://github.com/ccb2n19/", "GitHub.", target="_blank")
          ),
      mainPanel(
        fluidRow(
          valueBoxOutput("overall_cagr")
          ,valueBoxOutput("change_in_cars")
          ,valueBoxOutput("value3")
        ),
        tmapOutput("map"),
        br(),
        dataTableOutput("datatable"),
        h3("Local authority-wide trend"),
        plotOutput("time_series")
               )
    )
  )

server <- function(input, output) {
  start_date <- reactive({input$range_of_years[1]})
  
  end_date <- reactive({input$range_of_years[2]})
  
  averaging_period <- reactive({end_date() - start_date()})
  
  la <- reactive({input$la})
  
  features <- reactive({input$features})
  
  data <- reactive({dvla %>%
      filter(lan2019 == la()) %>%
      mutate(cars_per_capita = cars / pop18plus,
             lag_cars_per_capita = lag(cars_per_capita, averaging_period()),
             cagr = cagr_f(end = cars_per_capita, start = lag_cars_per_capita, averaging_period())) %>%
      filter(year == end_date()) %>%
      arrange(desc(cagr)) %>%
      mutate_if(is.numeric, round, 2) %>%
      left_join(lsoa_bounds, by = c("lsoan" = "LSOA11NMW")) %>%
      st_as_sf() %>%
      select(year, 
             lsoan, 
             lsoac, 
             cars_per_capita, 
             lag_cars_per_capita, 
             cagr, 
             ltn, 
             ltna, 
             ltn_time, 
             pop18plus,
             ltnpc,
             ltn_note) %>%
      st_transform(27700) %>%
      st_make_valid()
      })
  
  data_for_graph <- reactive({dvla %>%
      filter(lan2019 == la(),
             year >= start_date(),
             year <= end_date()) %>%
      group_by(year) %>%
      summarise(total_population = sum(pop18plus),
                total_cars = sum(cars),
                cars_per_capita = total_cars / total_population)
  })
  
  la_cagr <- reactive({data_for_graph() %>%
    mutate(lag_cars_per_capita_la = lag(cars_per_capita, averaging_period()),
           la_cagr = cagr_f(start = lag_cars_per_capita_la, 
                            end = cars_per_capita, 
                            n_years = averaging_period())) %>%
    filter(year == end_date()) %>%
    pull(la_cagr)})
  
  raw_cars <- reactive({data_for_graph() %>%
      mutate(lag_total_cars = lag(total_cars, averaging_period()),
             change_in_cars = total_cars - lag_total_cars) %>%
      filter(year == end_date())
      })
  
  ltns <- reactive({
    data() %>%
      filter(!is.na(ltn)) %>%
      select(lsoan, ltn, ltna, ltnpc, ltn_note)
  })
  
  data_to_present <- reactive({data() %>%
                                st_drop_geometry() %>%
                                select(year, lsoan, lsoac, cagr, pop18plus, lag_cars_per_capita, cars_per_capita, ltn, ltn_time) %>%
                                    rename("Year"                              = year,
                                           "LSOA name"                         = lsoan,
                                           "Cars per capita (start of period)" = lag_cars_per_capita,
                                           "Cars per capita (end of period)"   = cars_per_capita,
                                           "18-plus population"                = pop18plus,
                                           "LSOA code"                         = lsoac,
                                           "CAGR (%)"                          = cagr,
                                           "Zone contains LTN"                 = ltn,
                                           "Years as LTN"                      = ltn_time
                                           )})

  osm_cd <- reactive({data() %>%
      st_transform(4326) %>%
      st_bbox() %>%
      opq() %>%
      add_osm_feature(key = "shop",
                      value = "car") %>%
      osmdata_sf()
    })
    
  osm_uni <- reactive({data() %>%
      st_transform(4326) %>%
      st_bbox() %>%
      opq() %>%
      add_osm_feature(key = "amenity",
                      value = "university") %>%
      osmdata_sf()
    })
  
  osm_places <- reactive({data() %>%
      st_transform(4326) %>%
      st_bbox() %>%
      opq() %>%
      add_osm_feature(key = "place",
                      value = c("city", "town", "village")) %>%
      osmdata_sf()
  })
  
  points_cd <- reactive({osm_cd()$osm_points %>%
      st_transform(27700) %>%
      st_intersection(data())
    })
  
  polys_unis <- reactive({osm_uni()$osm_polygons %>%
      st_transform(27700) %>%
      st_intersection(data())
    })
  
  points_places <- reactive({osm_places()$osm_points %>%
      st_transform(27700) %>%
      st_intersection(data())
  })

  output$datatable <- renderDataTable({data_to_present()},
                                      options = list(pageLength = 10))
  
  output$map <- renderTmap({tm_shape(data(),
                                     name = "LSOAs") +
                              tm_polygons(col = "cagr",
                                          palette = "Spectral",
                                          alpha = 0.5,
                                          title = "CAGR (%)",
                                          popup.vars = c("Year"                              = "year",
                                                         "LSOA name"                         = "lsoan",
                                                         "Cars per capita (start of period)" = "lag_cars_per_capita",
                                                         "Cars per capita (end of period)"   = "cars_per_capita",
                                                         "18-plus population"                = "pop18plus",
                                                         "LSOA code"                         = "lsoac",
                                                         "CAGR (%)"                          = "cagr"
                                          )) +
                            {if(nrow(points_cd()) >= 1 & "car_dealerships" %in% features())
                              tm_shape(points_cd(),
                                       name = "Car dealerships") +
                              tm_dots(popup.vars = c("Dealership name" = "name") +
                              tm_add_legend(type = "fill",
                                            col = "black",
                                            lwd = 2,
                                            labels = "Car dealerships"))} +
                            {if(nrow(polys_unis()) >= 1 & "universities" %in% features())
                              tm_shape(polys_unis(),
                                       name = "Universities") +
                              tm_polygons(col = "white",
                                          alpha = 0.5,
                                          lwd = 2,
                                          border.col = "blue",
                                          popup.vars = c("University name" = "name")) +
                               tm_add_legend(type = "fill",
                                             col = "blue",
                                             lwd = 2,
                                             labels = "Universities")} +
                            {if(nrow(ltns()) >= 1 & "ltn" %in% features())
                              tm_shape(ltns(),
                                       name = "LTNs") +
                              tm_polygons(border.col = "green",
                                          lwd = 4,
                                          alpha = 0,
                                          popup.vars=c("LTN name" = "ltn",
                                                       "LTN subsection" = "ltna",
                                                       "Percent of LSOA in LTN" = "ltnpc",
                                                       "Note" = "ltn_note")) +
                                tm_add_legend(type = "fill",
                                              col = "green",
                                              lwd = 2,
                                              labels = "LTNs")} +
      {if(nrow(points_places()) >= 1 & "place_names" %in% features())
                              tm_shape(points_places()) +
                                tm_text(text = "name",
                                        auto.placement = TRUE)}
                          })
  
  output$time_series_table <- renderDataTable({data_for_graph()})
  
  output$time_series <- renderPlot({
    ggplot(data = data_for_graph(), aes(x = year, y = cars_per_capita)) +
      geom_line(col = "grey",
                lwd = 2) +
      ylim(0, NA) +
      labs(x = "Year",
           y = "Cars per capita")
  })
  
  output$la <- renderText(la())
  
  output$features <- renderText(features())
  
  output$start_date <- renderText(start_date())
  
  output$end_date <- renderText(end_date())
  
  output$averaging_period <- renderText(averaging_period())
  
  output$overall_cagr <- renderValueBox({
    valueBox(
      value = paste0(la_cagr(), "%"),
      subtitle = paste0("LA-wide CAGR"),
      icon = icon("stats",lib='glyphicon'),
      color = "purple")
  })
  
  output$change_in_cars <- renderValueBox({
    valueBox(
      value = paste0(raw_cars() %>% pull(change_in_cars)),
      subtitle = paste0("change in the total number of cars. From ", raw_cars() %>% pull(lag_total_cars), " to ", raw_cars() %>% pull(total_cars)),
      icon = icon("stats",lib='glyphicon'),
      color = "purple")
  })

}

shinyApp(ui = ui, server = server)

# deployApp("C:/Users/brown/Desktop/car_purchases_by_lsoa/application")