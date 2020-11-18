# Setup
library(dplyr)
library(readr)
library(sf)
library(tmap)
library(shiny)
library(shinydashboard)
library(osmdata)
library(tidyr)
library(RSQLite)
library(dbplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(forcats)
library(magrittr)
# setwd("C:/Users/brown/Desktop/car_purchases_by_lsoa/application")

### Initial things ###

# Create database connection
app_db_path <- "data"
app_db_file <- "app_database.sqlite"
app_database <- dbConnect(RSQLite::SQLite(), paste(app_db_path, app_db_file, sep = "/"))

# Load boundaries
path <- "data/spatial/lsoas.shp"
lsoa_shapes <- st_read(path) %>%
  select(LSOA11CD = LSOA11C)

opts <- tmap_options(basemaps = c(Canvas = "Esri.WorldGrayCanvas", Imagery = "Esri.WorldImagery"),
                     overlays = c(Labels = paste0("http://services.arcgisonline.com/arcgis/rest/services/Canvas/",
                                                  "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}")))

path <- "data/spatial/Local_Authority_Districts__December_2019__Boundaries_UK_BGC.shp"
la_shapes <- st_read(path) %>%
  rename(LAD19CD = lad19cd,
         LAD19NM = lad19nm)

# Get list of LAs
la_list_tib <- tbl(app_database, "la_list") %>%
  select(RGN11NM, LAD19NM) %>%
  as_tibble()

# This turns in into a grouped list for the drop-down menu
la_list <- split(la_list_tib$LAD19NM, la_list_tib$RGN11NM)

# Get range of years covered in data
year_range <- tbl(app_database, "time_series") %>%
  summarise(min = min(year, na.rm = TRUE),
            max = max(year, na.rm = TRUE)) %>%
  as_tibble()

# Get list of LSOA to exclude because of dealerships
excluded_zones <- tbl(app_database, "all_lsoa_data") %>%
  filter(dealer == 1 | total_business_land_use_pct > 50) %>%
  as_tibble() %>%
  pull(LSOA11CD)

# Create CAGR function
cagr_f <- function(start, end, n_years) {
  round(((end / start) ^ (1/n_years) - 1), 4)*100
}

# Defaults
default_year_range <- c(2016, 2020)

### UI ###
ui <- fluidPage(
  title = "Data dashboard of car registrations in England",
  
  br(),
  h4("Data dashboard of car registrations in England"),
  br(),
  
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("la",
                  label = h4("Choose local authority"),
                  choices = la_list,
                  selected = 1,
                  selectize = TRUE),
      sliderInput("range_of_years",
                  label = h4("Select range"),
                  min = year_range$min,
                  max = year_range$max,
                  value = default_year_range,
                  ticks = FALSE,
                  sep = ""),
      ),
    
    mainPanel(
      tabsetPanel(
      
            tabPanel("Headline trends",
                   valueBoxOutput("cpc_growth_box"),
                   valueBoxOutput("cagr_box"),
                   valueBoxOutput("change_in_cars_box"),
                   br(),
                   br(),
                   br(),
                   fluidRow(
                   column(6,
                          h4("Cars per capita time series"),
                          plotlyOutput("national_and_local_time_series_plot")),
                   column(6,
                          h4("Index of absolute number of registered cars"),
                          plotlyOutput("number_of_cars_index_plot")))),
            tabPanel("National comparison",
                   tmapOutput("local_authority_plot",
                              height = 550),
                 br(),
                 column(6,
                        h4("Biggest reducers"),
                        tableOutput("biggest_reducers")),
                 column(6,
                        h4("Biggest growers"),
                        tableOutput("biggest_growers"))
                 ),
            tabPanel("Small area comparisons",
                 tmapOutput("lsoa_map",
                            height = 550),
                 br(),
                 fluidRow(
                 column(6,
                        checkboxGroupInput("features",
                                    inline = FALSE,
                                    label = "Options",
                                    choiceNames = list("Exclude high non-residential land use zones",
                                                       "Highlight low-traffic neighbourhoods zones",
                                                       "Show neighbouring local authorities"),
                                    choiceValues = list("remove_non_res_zones",
                                                        "show_ltns",
                                                        "show_neighbours"))),
                 column(6,
                        radioButtons("stat_select_small_area", 
                                     label = "Edit statistics",
                                     c("Change in cars per capita" = "cpc_change_select",
                                       "Cars per capita at start of range" = "cpc_start_select",
                                       "Cars per capita at end of range" = "cpc_end_select")))),
                 br(),
                 column(6,
                        h4("Biggest reducers"),
                        tableOutput("small_area_biggest_reducers")),
                 column(6, 
                        h4("Biggest growers"),
                        tableOutput("small_area_biggest_growers"))),
            tabPanel("Socio-demographic breakdown",
                 br(),
                 helpText("This chart presents the overall change in",
                          "cars per capita (CPC) in each zone of your chosen local authority, ",
                          "alongside zones in the wider local government region. ",
                          "The plots are subdivided according to the 2011 census area ",
                          "classification of each lower-level super output area (LSOA). ", 
                          "The graphic highlights different levels of variance ",
                          "and average levels of change in CPC, once socio-demographic ",
                          "factors are taken into account.",
                          style="background-color:#f2f5fa"),
                 fluidRow(
                 plotlyOutput("lsoas_in_region_scatter",
                              height = 1000)),
                 br(),
                 br()
                 ),
            tabPanel("About the data",
                     shinydashboard::box(width = 9,
                                         br(),
                                         p("Car registrations data is from the Driver and Vehicle Licensing Agency (DVLA). 
                                           It was obtained via a freedom of information (FOI) request by ", 
                                           a(href = "https://twitter.com/Urban_Turbo", "Scott Urban.", target="_blank"), 
                                           "The dataset details the annual population and number of car registrations in each ", 
                                           tags$a(href="https://ocsi.uk/2019/03/18/lsoas-leps-and-lookups-a-beginners-guide-to-statistical-geographies/",
                                           "lower layer super output area", target="_blank"),
                                           "(LSOA) in England. The FOI data was supplemented with further information on low-traffic neighbourhoods (LTNs), 
                                           land use, and socio-demographic characteristics of each zone.",
                                           style="padding-bottom: 10px;"),
                                         
                                         h4("What the data says ..."),
                                         p("It provides a detailed geographic breakdown of the number of ",
                                           tags$a(href="https://www.gov.uk/vehicle-registration", "registered", target="_blank"),
                                           "cars, how this has changed since 2010, and a comparison between the number of registered cars and the 
                                           number of people in each area. This provides an aggregate perspective on whether local populations have decided 
                                           to take on car ownership, purchase additional cars, or give up car ownership. 
                                           The high spatial resolution may help detect the impact of local interventions, like LTNs 
                                           or public transport infrastructure.",
                                           style="padding-bottom: 10px;"),
                                         
                                           h4("What it doesn’t say …"),
                                           p("• Whether the cars are kept where they are registered. The cars of students, 
                                             for example, may not be registered at their term address.",
                                             br(),
                                             "• Much about local car use and travel behaviour (e.g. trip distance or purpose). 
                                                   The Department for Transport provides an ",
                                             tags$a(href="https://roadtraffic.dft.gov.uk/local-authorities", 
                                                    "annual estimate of motor vehicle traffic,", target="_blank"),
                                             "but this is only available at local authority level.",
                                         br(),
                                           "• Whether the cars are for personal use. An analysis has been conducted to provide the option to 
                                           exclude zones with high-non-residential land use and car dealerships, 
                                                   which can otherwise skew the local picture.",
                                           style="padding-bottom: 10px;")
                     )
            ),
            tabPanel("About the dashboard & author",
                     shinydashboard::box(width = 9,
                                         br(),
                                         p("This app provides a breakdown of car registrations in England."),
                                         p("It’s designed to support:"),
                                         p("• Decision-making by transport authorities and campaigners by providing 
                                           a local insight into car dependence trends",
                                         br(), "• Research into the drivers of car ownership by people like transport 
                                           planners and urban geographers", style="padding-bottom: 10px;"), 
                                         h4("Dashboard creation"),
                                         p("The dashboard was created using Shiny in R. Data processing was conducted using 
                                           the tidyverse group of R packages. Spatial analysis was conducted using the ",
                                           a(href = "https://r-spatial.github.io/sf/", "sf()", target="_blank"), "and ", 
                                           a(href ="https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html", "osmdata()", target="_blank"), 
                                           "R packages. Maps were created using ",
                                           a(href = "https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html",  "tmap()", target="_blank"), 
                                           "alongside basemaps from ",
                                           a(href = "https://stadiamaps.com/", "Stadia Maps,", target="_blank"),
                                           a(href = "https://openmaptiles.org/","OpenMapTiles", target="_blank"), "and",
                                           a(href = "https://www.openstreetmap.org/#map=15/56.1146/-3.9063","OpenStreetMap", target="_blank"), "contributors.",
                                           "Local authority (2019) and LSOA boundaries (2011) are from the Ordnance Survey ",
                                           a(href = "https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-december-2011-ew-bsc-v2", "Open Geography Portal.", target="_blank"),
                                           "Source code is available on ",
                                           a(href = "https://github.com/ccb2n19", "GitHub.", target="_blank")
                                           ,
                                           style="padding-bottom: 10px;"),
                                         h4("About the author"),
                                         p("This app was created by Christoper C Brown, a recent graduate in transport planning from the University of Southampton. 
                                          I'm now applying for roles in local government, academia, and consultancy in the UK and Switzerland. 
                                           If you know of projects or positions in which I could use geospatial analysis, mapping, marketing and transport appraisal to 
                                           help tackle car dependence - I'd love to hear from you. Get in touch via ",
                                           a(href = "mailto:brown.chrisc1@gmail.com", "email,", target="_blank"),
                                           a(href = "https://www.linkedin.com/in/christopher-c-brown-62280842/", "LinkedIn", target="_blank"), "or",
                                           a(href = "https://twitter.com/ChrisB_Key", "Twitter.", target="_blank")
                                           )
              
            )
            )
            
    ), width = 9)))
           

### SERVER ###
server <- function(input, output) {
  
  # Welcome popup
  
  query_modal <- modalDialog(
    
    # Text
    
    p("This app provides a geographic breakdown of car registrations in England,
      based on data from the Driver and Vehicle Licensing Agency (DVLA)."),
    p(
      "← Select a local authority and year range on the left"),
    p(
       "↑ See different views of the data using the tabs at the top"),
    p(
      "This is a personal project. Care has been taken during data processing and development, 
      but it hasn't been peer reviewed or rigorously quality assured. Any issues or 
      proposals for developments would be gladly recieved by the author via",
      a(href = "https://www.linkedin.com/in/christopher-c-brown-62280842/", "LinkedIn", target="_blank"), "or",
      a(href = "https://twitter.com/ChrisB_Key", "Twitter.", target="_blank")),
    
    title = "Data dashboard of car registrations in England",
    easyClose = TRUE,
    footer = modalButton("Let's go")
    )
  
  showModal(query_modal)
  
  # Set inputs as reactive values
  
  ## Years
  start_year <- reactive({input$range_of_years[1]})
  end_year <- reactive({input$range_of_years[2]})
  averaging_period <- reactive({end_year() - start_year()})
  
  ## LA
  la <- reactive({input$la})
  
  ## Map features
  
  features <- reactive({input$features})
  
  ## Shape of selected LA
  
  la_shape <- reactive({la_shapes %>%
    filter(LAD19NM == la())})
  
  # Growth by local authority during range of years
  
  growth_by_la <- reactive({
    start_year_db <- start_year()
    end_year_db <- end_year()
    
    non_spatial <- tbl(app_database, "time_series") %>%
      filter(year == start_year_db | year == end_year_db) %>%
      as_tibble() %>%
      group_by(LAD19NM, year) %>%
      summarise(total_pop = sum(pop18plus),
                total_cars = sum(cars)) %>%
      mutate(cpc = total_cars / total_pop,
             start_or_end = case_when(year == start_year() ~ "start_year",
                                      year == end_year()   ~ "end_year")) %>%
      select(-year) %>%
      mutate(cpc_start_of_period = dplyr::lag(cpc, 1),
             cars_start_of_period = dplyr::lag(total_cars, 1),
             cagr                     = cagr_f(start   = cpc_start_of_period,
                                               end     = cpc,
                                               n_years = averaging_period()),
             total_growth_in_cpc = round(((cpc - cpc_start_of_period) / cpc_start_of_period) * 100, 2),
             change_in_cars      = total_cars - cars_start_of_period,
             people_per_car      = total_pop / total_cars) %>%
      filter(start_or_end == "end_year") %>%
      ungroup() %>%
      mutate(cpc_growth_rank = row_number(total_growth_in_cpc))
    
    la_shapes %>%
      select(LAD19NM) %>%
      inner_join(non_spatial, by = "LAD19NM")
  })
  
  # Growth by LSOA during range of years
  growth_by_lsoa <- reactive({
    # Set values
    start_year_db <- start_year()
    end_year_db <- end_year()
    
    # Get shape of relevant LA
    la_shape <- la_shapes %>%
      filter(LAD19NM == la())
    
    la_selection = case_when("show_neighbours" %in% features() ~ la_shapes %>%
                                                               st_filter(la_shape, f = st_intersects) %>%
                                                               st_drop_geometry() %>%
                                                               pull(LAD19NM),
                                                       TRUE ~ la())
    
    non_spatial <- tbl(app_database, "time_series") %>%
      filter(year == start_year_db | year == end_year_db,
             LAD19NM %in% la_selection) %>%
      as_tibble() %>%
      mutate(cpc = cars / pop18plus,
             start_or_end = case_when(year == start_year() ~ "start_year",
                                      year == end_year()   ~ "end_year")) %>%
      group_by(LSOA11CD) %>%
      mutate(cpc_start_of_period  = dplyr::lag(cpc, 1),
             cars_start_of_period = dplyr::lag(cars, 1),
             cagr                 = cagr_f(start   = cpc_start_of_period,
                                           end     = cpc,
                                           n_years = averaging_period()),
             total_growth_in_cpc  = round(((cpc - cpc_start_of_period) / cpc_start_of_period) * 100, 2),
             change_in_cars       = cars - cars_start_of_period,
             people_per_car       = pop18plus / cars) %>%
      ungroup() %>%
      filter(start_or_end == "end_year") %>%
      group_by(LAD19NM) %>%
      mutate(cpc_growth_rank = row_number(total_growth_in_cpc)) %>%
      select(LSOA11CD, 
             LAD19NM, 
             cars,
             cars_start_of_period, 
             cpc, 
             cpc_start_of_period,
             total_growth_in_cpc,
             cagr,
             cpc_growth_rank)
    
    additional_data <- tbl(app_database, "all_lsoa_data") %>%
      filter(LAD19NM %in% la_selection) %>%
      select(LSOA11CD,
             ltn, 
             ltna, 
             ltnpc, 
             ltn_note, 
             ltn_time, 
             SOAC11NM,
             MSOA11HCLNM) %>%
      as_tibble()
    
    lsoa_shapes %>%
      inner_join(non_spatial, by = "LSOA11CD") %>%
      left_join(additional_data, by = "LSOA11CD")
  })
  
  # Growth in CPC for all LSOAs within region
  
  all_lsoas_in_region <- reactive({
    # Set values
    start_year_db <- start_year()
    end_year_db <- end_year()
    
    # Get region from LA
    region <- la_list_tib %>%
      filter(LAD19NM == la()) %>%
      pull(RGN11NM)
    
    all_lsoa_data <- tbl(app_database, "all_lsoa_data") %>%
      select(LSOA11CD, LAD19NM, RGN11NM, ltn, MSOA11HCLNM, SOAC11NM) %>%
      filter(!LSOA11CD %in% excluded_zones,
             RGN11NM == region) %>%
      as_tibble()
    
    lsoa_list <- all_lsoa_data$LSOA11CD
      
    time_series <- tbl(app_database, "time_series") %>%
                    select(LSOA11CD, year, cars, pop18plus) %>%
                    filter(LSOA11CD %in% lsoa_list,
                         year == start_year_db | year == end_year_db) %>%
                    as_tibble() %>%
                    group_by(LSOA11CD) %>%
                    mutate(cpc = cars / pop18plus,
                           total_growth_in_cpc = ((cpc - lag(cpc, 1)) / lag(cpc, 1)) * 100,
                           total_growth_in_cpc = round(total_growth_in_cpc, 2)) %>%
      ungroup() %>%
      filter(year == end_year_db) %>%
      select(-year)
    
    all_lsoa_data %>%
      left_join(time_series, by = "LSOA11CD") %>%
      slice(sample(1:n())) %>%
      mutate(rand = row_number(),
             facet_SOAC = str_wrap(SOAC11NM, 20),
             from_selected_la = case_when(LAD19NM == la() ~ paste0("Zone in ", la()),
                                          TRUE ~ paste0("Zone elsewhere in ", RGN11NM)),
             from_selected_la = fct_relevel(from_selected_la, la())) %>%
      arrange(rand)
  })
  
  # Values by local authority across all years
  
  time_series_by_la <- reactive({
    start_year_db <- start_year()
    end_year_db <- end_year()
    
    tbl(app_database, "time_series") %>%
      filter(year >= start_year_db &  year <= end_year_db) %>%
      as_tibble() %>%
      group_by(LAD19NM, year) %>%
      summarise(total_pop = sum(pop18plus),
                total_cars = sum(cars)) %>%
      mutate(cpc = total_cars / total_pop)
  })
  
  # National vs. local time series data
  
  national_and_local_time_series <- reactive({
    national <- time_series_by_la() %>%
      group_by(year) %>%
      summarise(total_pop = sum(total_pop),
                total_cars = sum(total_cars)) %>%
      ungroup() %>%
      mutate(cpc = total_cars / total_pop,
             type = "Nationwide")
    
    selected_la <- time_series_by_la() %>%
      filter(LAD19NM == la()) %>%
      mutate(type = la())
    
    bind_rows(national, selected_la) %>%
      mutate(type = fct_relevel(type, "Nationwide", after = 1))
  })
  
  # Index graph data
  
  national_and_local_cars_index <- reactive({
    
    local_base <- time_series_by_la() %>%
      filter(LAD19NM == la(),
             year == start_year()) %>%
      select(LAD19NM, base_cars = total_cars)
    
    local_index <- time_series_by_la() %>%
      filter(LAD19NM == la()) %>%
      left_join(local_base, by = "LAD19NM") %>%
      mutate(index = (total_cars / base_cars) * 100,
             type = la()) %>%
      select(year, type, index)
    
    national_totals <- time_series_by_la() %>%
      group_by(year) %>%
      summarise(total_cars = sum(total_cars)) %>%
      ungroup()
    
    national_base <- national_totals %>%
      filter(year == start_year()) %>%
      pull(total_cars)
    
    national_index <- national_totals %>%
      mutate(index = (total_cars / national_base) * 100,
             type = "Nationwide") %>%
      select(year, type, index)
    
    bind_rows(local_index, national_index) %>%
      mutate(type = fct_relevel(type, "Nationwide", after = 1))
  })
  
  
  # Turn reactive objects into outputs
  ## HEADLINES ##
  output$cpc_growth_box <- renderValueBox({
    number <- growth_by_la() %>% 
      filter(LAD19NM == la()) %>% 
      pull(total_growth_in_cpc)
    
    valueBox(
      value = paste0(number %>% 
                       prettyNum(big.mark = ","), "%"),
      subtitle = "Total change in cars per person"
    )
  })
  
  output$cagr_box <- renderValueBox({
    number <- growth_by_la() %>% 
      filter(LAD19NM == la()) %>% 
      pull(cagr)
    
    valueBox(
      value = paste0(number %>%
                       prettyNum(big.mark = ","), "%"),
      subtitle = "Average annual change in cars per person "
    )
  })
  
  output$change_in_cars_box <- renderValueBox({
    number <- growth_by_la() %>% 
      filter(LAD19NM == la()) %>% 
      pull(change_in_cars)

    valueBox(
      value = number %>% 
                prettyNum(big.mark = ","),
      subtitle = if_else(growth_by_la() %>% 
                           filter(LAD19NM == la()) %>% 
                           pull(change_in_cars) < 0,
                         "fewer cars registered in the LA",
                         "more cars registered in the LA")
    )
  })
  
  ## TREND GRAPHS ##
  
  output$national_and_local_time_series_plot <- renderPlotly({
    ggplotly(
      ggplot(data = national_and_local_time_series(), aes(x = year, y = cpc)) +
        geom_line(aes(linetype = type, colour = type), lwd = 1.3) +
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::number_format(accuracy = 0.1),
                           name = "Cars per capita") +
        scale_x_continuous(labels = scales::number_format(accuracy = 1,
                                                          big.mark = ""),
                           name = "") +
        theme_minimal()
    ) %>% layout(legend = list(orientation = "h", x = 0, y = -0.08))
  })

  output$number_of_cars_index_plot <- renderPlotly({
    ggplotly(
      ggplot(data = national_and_local_cars_index(), aes(x = year, y = index)) +
        geom_line(aes(linetype = type, colour = type), lwd = 1.3) +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                           name = paste0("Index (", start_year(), "= 100)")) +
        scale_x_continuous(labels = scales::number_format(accuracy = 1,
                                                          big.mark = ""),
                           name = "") +
        theme_minimal()
  ) %>% layout(legend = list(orientation = "h", x = 0, y = -0.08))
  })
  
  ## LA VIEW OUTPUTS ##
  
  output$local_authority_plot <- renderTmap({
    breaks <- seq(-50, 50, by = 10)
    
    selected_la <- growth_by_la() %>%
      filter(LAD19NM == la())
    
    tm_shape(growth_by_la(), 
             bbox = selected_la,
             name = "All LAs") +
      tm_polygons(col     = "total_growth_in_cpc",
                  palette = "-RdYlGn",
                  midpoint = 0,
                  alpha   = 0.7,
                  lwd     = 0.5,
                  style = "jenks",
                  n = 10,
                  title = "Total growth in cars per capita",
                  popup.vars = c("LA name" = "LAD19NM",
                                 "Car reduction rank" = "cpc_growth_rank",
                                 "Change in CPC over period (%)" = "total_growth_in_cpc",
                                 "Average annual change in CPC (%)" = "cagr",
                                 "Number of cars (start)" = "cars_start_of_period",
                                 "Number of cars (end)" = "total_cars",
                                 "CPC (start)" = "cpc_start_of_period",
                                 "CPC (end)" = "cpc")) +
    tm_shape(selected_la,
             name = "Selected LA") +
      tm_borders(lwd = 3,
                 col = "black")
  })
  
  output$biggest_reducers <- renderTable({
    growth_by_la() %>%
      st_drop_geometry() %>%
      arrange(cpc_growth_rank) %>%
      select("Car reduction rank" = cpc_growth_rank,
             "LA name" = LAD19NM,
             "Cars per capita at start" = cpc_start_of_period,
             "Cars per capita at end" = cpc,
             "Overall change" = total_growth_in_cpc) %>%
      head(10)
   })
  
  output$biggest_growers <- renderTable({
    growth_by_la() %>%
      st_drop_geometry() %>%
      arrange(desc(cpc_growth_rank)) %>%
      select("Car reduction rank" = cpc_growth_rank,
             "LA name" = LAD19NM,
             "Cars per capita at start" = cpc_start_of_period,
             "Cars per capita at end" = cpc,
             "Overall change" = total_growth_in_cpc) %>%
      head(10)
  })
  
  lsoas_for_plot <- reactive({
    growth_by_lsoa() %>%
     filter(if ("remove_non_res_zones" %in% features()) 
            {!LSOA11CD %in% excluded_zones}
            else {is.character(LSOA11CD)}
            ) %>%
      mutate(cpc_growth_rank = row_number(total_growth_in_cpc)) %>%
      arrange(cpc_growth_rank)
    })
  
  stat_select <- reactive({input$stat_select_small_area})
  
  output$stat_select <- renderText(stat_select())
  
  output$lsoa_map <- renderTmap({
    la_outline <- growth_by_lsoa() %>%
      filter(LAD19NM == la()) %>%
      summarise(LAD19NM = la())

                  tm_shape(lsoas_for_plot(),
                           bbox = la_outline, 
                           name = "LSOAs") +
                  {if(stat_select() == "cpc_change_select")
                    tm_polygons(col     = "total_growth_in_cpc",
                                palette = "-RdYlGn",
                                midpoint = 0,
                                alpha   = 0.7,
                                lwd     = 0.5,
                                style = "jenks",
                                n = 10,
                                title = "Total growth in cars per capita",
                                popup.vars = c("LSOA code" = "LSOA11CD",
                                               "Locality" = "MSOA11HCLNM",
                                               "Census categorisation" = "SOAC11NM",
                                               "Change in CPC over period (%)" = "total_growth_in_cpc",
                                               "Average annual change in CPC (%)" = "cagr",
                                               "Number of cars (start)" = "cars_start_of_period",
                                               "Number of cars (end)" = "cars",
                                               "CPC (start)" = "cpc_start_of_period",
                                               "CPC (end)" = "cpc"))} +
                {if(stat_select() == "cpc_start_select")
                    tm_polygons(col         = "cpc_start_of_period",
                                 palette    = "viridis",
                                 style      = "jenks",
                                 n          = 10,
                                 alpha      = 0.7,
                                 title      = paste0("Cars per capita in ", start_year()),
                                 popup.vars = c("LSOA code" = "LSOA11CD",
                                                "Locality" = "MSOA11HCLNM",
                                                "Census categorisation" = "SOAC11NM",
                                                "Change in CPC over period (%)" = "total_growth_in_cpc",
                                                "Average annual change in CPC (%)" = "cagr",
                                                "Number of cars (start)" = "cars_start_of_period",
                                                "Number of cars (end)" = "cars",
                                                "CPC (start)" = "cpc_start_of_period",
                                                "CPC (end)" = "cpc"))} +
                 {if(stat_select() == "cpc_end_select")
                   tm_polygons(col         = "cpc",
                               palette    = "viridis",
                               style      = "jenks",
                               n          = 10,
                               alpha      = 0.7,
                               title      = paste0("Cars per capita in ", end_year()),
                               popup.vars = c("LSOA code" = "LSOA11CD",
                                              "Locality" = "MSOA11HCLNM",
                                              "Census categorisation" = "SOAC11NM",
                                              "Change in CPC over period (%)" = "total_growth_in_cpc",
                                              "Average annual change in CPC (%)" = "cagr",
                                              "Number of cars (start)" = "cars_start_of_period",
                                              "Number of cars (end)" = "cars",
                                              "CPC (start)" = "cpc_start_of_period",
                                              "CPC (end)" = "cpc"))} +
                {if("show_ltns" %in% features() & nrow(lsoas_for_plot() %>% filter(!is.na(ltn))) > 0)
                  tm_shape(lsoas_for_plot() %>% filter(!is.na(ltn)),
                           name = "Low-traffic neighbourhoods") +
                    tm_polygons(lwd = 2.5,
                                alpha = 0,
                                border.col = "Yellow",
                                popup.vars = c("LTN detail" = "ltn_note",
                                               "LSOA code" = "LSOA11CD",
                                               "Locality" = "MSOA11HCLNM",
                                               "Census categorisation" = "SOAC11NM",
                                               "Change in CPC over period (%)" = "total_growth_in_cpc",
                                               "Average annual change in CPC (%)" = "cagr",
                                               "Number of cars (start)" = "cars_start_of_period",
                                               "Number of cars (end)" = "cars",
                                               "CPC (start)" = "cpc_start_of_period",
                                               "CPC (end)" = "cpc"))} +
                  tm_shape(la_outline) +
                    tm_borders(lwd = 3,
                               col = "black")
    })
  
  
  
  output$small_area_biggest_reducers <- renderTable({
    lsoas_for_plot() %>%
      head(5) %>%
      st_drop_geometry() %>%
      mutate(cars_start_of_period = as.integer(cars_start_of_period),
             cars = as.integer(cars)) %>%
      select("Rank" = cpc_growth_rank,
             "LSOA code" = LSOA11CD,
             "Locality" = MSOA11HCLNM,
             "CPC growth (%)" = total_growth_in_cpc,
             "Cars at start" = cars_start_of_period,
             "Cars at end" = cars
             )
  })
  
  output$small_area_biggest_growers <- renderTable({
    lsoas_for_plot() %>%
      tail(5) %>%
      st_drop_geometry() %>%
      mutate(cars_start_of_period = as.integer(cars_start_of_period),
             cars = as.integer(cars)) %>%
      arrange(desc(cpc_growth_rank)) %>%
      select("Rank" = cpc_growth_rank,
             "LSOA code" = LSOA11CD,
             "Locality" = MSOA11HCLNM,
             "CPC growth (%)" = total_growth_in_cpc,
             "Cars at start" = cars_start_of_period,
             "Cars at end" = cars
      )
  })
  
  output$lsoas_in_region_scatter <- renderPlotly({
    ggp <- ggplot(data = all_lsoas_in_region(), aes(x      = rand, 
                                                    y      = total_growth_in_cpc, 
                                                    label  = LSOA11CD,
                                                    label2 = MSOA11HCLNM)) +
      geom_point(aes(col = from_selected_la),
                 alpha = 0.5) +
      geom_hline(yintercept = 0,
                  lty = "dashed") +
      facet_wrap(~facet_SOAC,
                 ncol = 5) +
      scale_x_continuous(name = "",
                         labels = NULL) +
      scale_y_continuous(name = "Total change in cars per person within zone (%)",
                         breaks = seq(-500, 500, by = 20)) +
      theme_minimal() +
      theme(strip.text = element_text(size = 8))
    
    ggplty <- ggplotly(ggp,
                       tooltip = c("label",
                                   "y",
                                   "label2"))
    
    
    
    ggplty %>% layout(legend = list(orientation = "h", x = 0, y = 1.085),
                      margin = list(l = 85))
      
  })
  
  output$lsoas_in_region <- renderTable({all_lsoas_in_region()})

}

shinyApp(ui = ui, server = server)

### rsconnect::deployApp("C:/Users/brown/Desktop/car_purchases_by_lsoa/application")