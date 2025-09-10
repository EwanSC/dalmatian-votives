# filter Dalmatian data
# started 10/09/2025
# last edit 10/09/2025
# last ran 10/09/2025
library(dplyr)
library(sqldf)
library(arrow)
library(rjson)

# EDH
# downloaded from EDH
# https://edh.ub.uni-heidelberg.de/data/download/edh_data_text.csv
# as of 10/09/2025
EDH <-
  read.csv("data/EDH/edh_data_text.csv",
           na.strings = c("","NA","NULL",NULL))

EDH_Dalmatia <- filter(EDH, provinz == "Dal")

write.csv(EDH_Dalmatia,
          file = "output_data/dalmatia/EDH_Dalmatia.csv")

# EDCS
# downloaded via Ballsun-Stanton, B., Heřmánková, P., & Laurence, R. (2024).
# Latin Epigraphy Scraper (v2.0). GitHub.
# https://doi.org/10.5281/zenodo.12036540
# Data as of 2022-08-29

EDCSJSON <- fromJSON(file= "data/EDCS/2022-08-29-EDCS_via_Lat_Epig-prov_Dalmatia-10140.json")
EDCSJSON_Data <- EDCSJSON$data
EDCS_Dalmatia <- data.table::rbindlist(EDCSJSON_Data, fill = TRUE)
write.csv(EDCS_Dalmatia,
          file = "output_data/cleaning/EDCS_Dalmatia.csv")
EDCS_Dalmatia <-
  read.csv("output_data/cleaning/EDCS_Dalmatia.csv",
           na.strings = c(""," "))
write.csv(EDCS_Dalmatia,
          file = "output_data/dalmatia/EDCS_Dalmatia.csv")

# LIRE
LIRE <-
  read_parquet("data/LIRE/LIRE_v3-0.parquet")

LIRE_filtered <- select(LIRE,
                        "LIST-ID",
                        "EDCS-ID",
                        "EDH-ID",
                        "not_before",
                        "not_after",
                        "transcription",
                        "clean_text_interpretive_word",
                        "type_of_inscription_clean",
                        "type_of_inscription_auto",
                        "type_of_monument_clean",
                        "province",
                        "province_label_clean",
                        "place",
                        "findspot_ancient_clean",
                        "findspot_modern_clean",
                        "status_notation",
                        "Latitude",
                        "Longitude",
                        "urban_context",
                        "urban_context_city",
                        "people")

LIRE_Dalmatia <- filter(LIRE_filtered, province_label_clean == "Dalmatia" | province == "Dalmatia")
write.csv(LIRE_Dalmatia,
          file = "output_data/cleaning/LIRE_Dalmatia.csv")
LIRE_Dalmatia <-
  read.csv("output_data/cleaning/LIRE_Dalmatia.csv",
           na.strings = c("","NA","NULL",NULL))
write.csv(LIRE_Dalmatia,
          file = "output_data/dalmatia/LIRE_Dalmatia.csv")

# count
LIRE_Dalmatia_count <- count(LIRE_Dalmatia)
EDCS_Dalmatia_count <- count(EDCS_Dalmatia)
EDH_Dalmatia_count <- count(EDH_Dalmatia)

LIRE_Dalmatia_count$database <- "LIRE"
EDCS_Dalmatia_count$database <- "EDCS"
EDH_Dalmatia_count$database <- "EDH"

#merge all data frames together
count_list <- list(LIRE_Dalmatia_count, EDCS_Dalmatia_count, EDH_Dalmatia_count)      

database_Dalmatia_count <- Reduce(function(x, y) merge(x, y, all=TRUE), count_list) 

write.csv(database_Dalmatia_count,
          file = "output_data/dalmatia/database_Dalmatia_count.csv")
