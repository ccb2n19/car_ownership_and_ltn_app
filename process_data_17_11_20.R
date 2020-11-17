library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(RSQLite)
library(dbplyr)
setwd("C:/Users/brown/Desktop/car_purchases_by_lsoa/v2/data")

all_data_list <- list()

path <- "DVLA_FOI_LSOA_v2.csv"

all_data_list[["dvla"]] <- read_csv(path,
                                    col_types = cols(
                                      .default = col_double(),
                                      lac2019 = col_character(),
                                      lan2019 = col_character(),
                                      lsoac = col_character(),
                                      lsoan = col_character(),
                                      ltn = col_character(),
                                      ltna = col_character(),
                                      ltn_note = col_character(),
                                      lac2020 = col_character(),
                                      lan2020 = col_character(),
                                      id = col_character()
                                    )) %>%
                                      rename(LSOA11CD = lsoac)

# Just time series data
all_data_list[["time_series"]] <- all_data_list[["dvla"]] %>%
  mutate(popunder18 = pop - pop18plus,
         cars_per_capita = cars / pop18plus,
         cars_ppl_per = pop18plus / cars,
         id = paste0(LSOA11CD, "_", year)) %>%
  select(id,
         year, 
         LSOA11CD,
         LAD19NM = lan2019, # Changed
         cars, 
         cars_per_capita, 
         cars_ppl_per,
         pop,
         pop18plus,
         popunder18, 
         medage,
         property_sales = propas,
         property_median_price = propmp)

# LSOA data
## List of LSOAs
path <- "https://opendata.arcgis.com/datasets/3ce71e53d9254a73b3e887a506b82f63_0.csv"
all_data_list[["list_of_lsoas"]] <- read_csv(path,
                                             col_types = cols(
                                               LSOA11CD = col_character(),
                                               LSOA11NM = col_character(),
                                               FID = col_double()
                                             ))

## Land use data
path <- "land_uses_by_lsoa.csv"
all_data_list[["land_use_data"]] <- read_csv(path)

## LSOA data from DVLA
all_data_list[["lsoa_from_scott"]] <- all_data_list[["dvla"]] %>%
                                          distinct(LSOA11CD, .keep_all = TRUE) %>%
                                          select(LSOA11CD,
                                                 LAD19NM = lan2019,
                                                 LAD19CD = lac2019,
                                                 contains("ltn"),
                                                 dealer)

## LSOA classifications
path <- "https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv"
all_data_list[["lsoa_classifications"]] <- read_csv(path) %>%
  distinct(LSOA11CD, .keep_all = TRUE) %>%
  select(LSOA11CD,
         SOAC11NM,
         MSOA11CD,
         RGN11CD,
         RGN11NM)

## MSOA names
path <- "nice_msoa_names.csv"
all_data_list[["nice_msoa_names"]] <- read_csv(path) %>%
  select(MSOA11CD = msoa11cd,
         MSOA11HCLNM = msoa11hclnm)

all_data_list[["all_lsoa_data"]] <- all_data_list$lsoa_from_scott %>%
  left_join(all_data_list$land_use_data, by = "LSOA11CD") %>%
  left_join(all_data_list$list_of_lsoas, by = "LSOA11CD") %>%
  left_join(all_data_list$lsoa_classifications, by = "LSOA11CD") %>%
  left_join(all_data_list$nice_msoa_names, by = "MSOA11CD") %>%
  select(LSOA11CD, LSOA11NM, MSOA11CD, LAD19CD, LAD19NM, RGN11CD, RGN11NM, everything())

# IMD data
## 2019
tmp <- "C:/Temp/imd_2019.xlsx"
path <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833973/File_2_-_IoD2019_Domains_of_Deprivation.xlsx"
download.file(url = path, destfile = tmp, mode="wb")

all_data_list[["imd_2019"]] <- read_xlsx(tmp, sheet = 2) %>%
  rename_with(~ tolower(gsub(".", "_", .x, fixed = TRUE))) %>%
  select(LSOA11CD = 1,
         imd_decile = 6,
         imd_rank = 5) %>%
  mutate(year = 2019)

## 2010
tmp <- "C:/Temp/imd_2010.xls"
path <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/6872/1871524.xls"
download.file(url = path, destfile = tmp, mode="wb")

all_data_list[["imd_2010"]] <- read_xls(tmp, sheet = 2) %>%
  rename_with(~ tolower(gsub(".", "_", .x, fixed = TRUE))) %>%
  select(LSOA01CD = 1,
         imd_score = 6,
         imd_rank = 7) %>%
  mutate(imd_decile = ntile(-imd_score, 10),
         year = 2010) %>%
  select(LSOA01CD, imd_decile, imd_rank, year)

## Put the two together, via the lookup between LSOAs across the two years
path <- "https://opendata.arcgis.com/datasets/cb316960f04c4a659ddf0dff6510eb3e_0.csv"
all_data_list[["lsoa_01_11_lookup"]] <- read_csv(path, col_types = cols(
                                                  LSOA01CD = col_character(),
                                                  LSOA11CD = col_character()
                                                )) %>%
  select(LSOA01CD, LSOA11CD)

# Get a LSOA11CD for each 2010 score, and then have a long table with LSOA11CD, year, imd_score, rank, decile
all_data_list[["imd_combined"]] <- all_data_list[["lsoa_01_11_lookup"]] %>%
  left_join(all_data_list[["imd_2010"]], by = "LSOA01CD") %>%
  select(-LSOA01CD) %>%
  bind_rows(all_data_list[["imd_2019"]]) %>%
  arrange(LSOA11CD, year) %>%
  group_by(LSOA11CD) %>%
  mutate(change_in_imd_decile = imd_decile - lag(imd_decile, 1)
  )

all_data_list[["la_list"]] <- all_data_list[["all_lsoa_data"]] %>%
  select(LAD19CD, LAD19NM, RGN11NM) %>%
  distinct(LAD19CD, .keep_all = TRUE) %>%
  arrange(LAD19NM)

# Create a database in which to store all of this
## From https://www.datacamp.com/community/tutorials/sqlite-in-r)

app_db_file <- "app_database.sqlite"
app_database <- dbConnect(RSQLite::SQLite(), app_db_file)
desired_tables <- c("imd_combined", "all_lsoa_data", "time_series", "la_list")

object_name <- NULL

for(i in 1:length(desired_tables)){
object_name <- desired_tables[i]
dbWriteTable(app_database,
             object_name,
             all_data_list[[object_name]],
             overwrite = TRUE
            )
print(paste0("Done with ", object_name))
}

dbDisconnect(app_database)

# dbRemoveTable(app_database, "")
# dbListTables(app_database)