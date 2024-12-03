# script for data cleaningfor the observer & quayside datasets - Northumberland IFCA

# Check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
observer_data_master <- readr::read_csv(file = "data/nifca/nifca_fleet.quayside.sampling_master.csv") |> 
  dplyr::glimpse()
observer_data_measure <- readr::read_csv(file = "data/nifca/nifca_fleet.quayside.sampling_measurement.csv", 
                                         col_types = readr::cols(`Vessel ID` = readr::col_double())) |> 
  dplyr::glimpse()

# merge vessel  info and measurements
observer_data <- observer_data_measure |> 
  dplyr::left_join(observer_data_master, multiple = "all",
                   by = c("Survey Type", "Date", "Year", "Month","Vessel ID", "Port", "Fleet No." = "Fleet Number"))
colnames(observer_data) <- c("date", "year", "month", "survey_type", "port", "vesselID", "species", "fleet_num", 
                             "pot_num", "carapace_width", "undersize", "sex", "abdomen_wdth", "damage", "v_notch",
                             "berried", "egg_col", "n_missing_limb", "comments_measure", "mass_g", "start_time", 
                             "end_time", "pot_type", "start_lat", "start_lon", "end_lat", "end_lon", 
                             "num_pots_per_fleet", "num_pots_sampled", "sample_mass_kg", "bait", "depth_fa", 
                             "depth_m", "soak_time", "num_lobsters_measured", "num_crabs_measured", "bycatch_species",
                             "surveyor", "comments_master") 


# data cleaning
observer_data <- observer_data |> 
  dplyr::mutate(month = dplyr::recode(month, April=4, August=8, December=12, February=2, January=1, 
                                      July=7, June=6, March=3, May=5, November=11, October=10, September=9)) |> 
  dplyr::mutate(sex = dplyr::recode(sex, F=1, M=0, m=0)) |> 
  dplyr::mutate(undersize = dplyr::recode(undersize, Size=0, Undersize=1)) |> 
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) |>  
  dplyr::mutate(soak_time = as.numeric(soak_time),
                num_pots_sampled = as.numeric(num_pots_sampled),
                num_pots_per_fleet = as.numeric(num_pots_per_fleet)) |> 
  tidyr::unite(month.yr, c(year, month), sep = "-", remove = FALSE) |> 
  dplyr::mutate(qtr = lubridate::quarter(date, with_year = FALSE)) |> 
  tidyr::unite(qrt.yr, c(year, qtr), sep = "-", remove = FALSE) |> 
  tidyr::unite(trip, c(vesselID, date), sep = "|", remove = FALSE) |> 
  tidyr::unite(lat_lon, c(start_lat, start_lon), sep = "|", remove = FALSE) |> 
  tidyr::unite(lat_lon_trip, c(start_lat, start_lon, trip), sep = "|", remove = FALSE) 

# subset data by survey type
observer_data_quayside <- observer_data |> 
  dplyr::filter(survey_type == "Quayside")
observer_data_fleet <- observer_data |> 
  dplyr::filter(survey_type == "Fleet")
observer_data_offshore <- observer_data |> 
  dplyr::filter(survey_type == "Offshore")

# export datasets
readr::write_csv(observer_data_quayside, file = "processed_data/nifca/observer_data_quayside_nifca_clean.csv") 
readr::write_csv(observer_data_fleet, file = "processed_data/nifca/observer_data_fleet_nifca_clean.csv")
readr::write_csv(observer_data_offshore, file = "processed_data/nifca/observer_data_offshore_nifca_clean.csv")
