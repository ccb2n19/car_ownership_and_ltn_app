# Data dashboard of car registrations in England

This app provides a breakdown of car registrations in England.

It’s designed to support:

• Decision-making by transport authorities and campaigners by providing a local insight into car dependence trends
• Research into the drivers of car ownership by people like transport planners and urban geographers

## Dashboard creation
The dashboard was created using Shiny in R. Data processing was conducted using the tidyverse group of R packages. Spatial analysis was conducted using the sf() and osmdata() R packages. Maps were created using tmap() alongside basemaps from Stadia Maps, OpenMapTiles and OpenStreetMap contributors. Local authority (2019) and LSOA boundaries (2011) are from the Ordnance Survey Open Geography Portal. Source code is available on GitHub.

## About the data
Car registrations data is from the Driver and Vehicle Licensing Agency (DVLA). It was obtained via a freedom of information (FOI) request by Scott Urban. The dataset details the annual population and number of car registrations in each lower layer super output area (LSOA) in England. The FOI data was supplemented with further information on low-traffic neighbourhoods (LTNs), land use, and socio-demographic characteristics of each zone.

You can access all sources via the 'process data' script.
