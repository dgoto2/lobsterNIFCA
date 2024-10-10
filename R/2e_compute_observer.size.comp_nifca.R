# script to reformat size composition data as SS model input for the crab and lobster observer data - Northumberland IFCA 

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# run input data processing script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source( file = "1b_observer_dataprocessing_nifca.R" )

# read in data
observer_data_fleet <- readr::read_csv("processed_data/nifca/observer_data_fleet_nifca_clean.csv") |> 
  dplyr::glimpse()
observer_data_offshore <- readr::read_csv("processed_data/nifca/observer_data_offshore_nifca_clean.csv") |> 
  dplyr::glimpse()

# data w/habitat data
observer_data_fleet <- readr::read_csv("processed_data/nifca/observer_data_fleet_nifca_clean_env.csv") |>
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) |>
  dplyr::glimpse() 

observer_data_offshore <- readr::read_csv("processed_data/nifca/observer_data_offshore_nifca_clean_env.csv") |> 
  dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y")) |> # format as date 
  dplyr::mutate(temperature = Temperature,
                temperature_interpolation_dist = Temp_dist,
                Distance_to_shore = distance_to_shore,
                Depth = depth,
                Depth_interpolation_distance = depth_interpolation_distance,
                Distance_to_shore = distance_to_shore) |>
  dplyr::glimpse()

# subset by species
observer_data_fleet_lobster <- observer_data_fleet |> 
  dplyr::filter(species=="Lobster") |> 
  dplyr::glimpse()
observer_data_offshore_lobster <- observer_data_offshore |> 
  dplyr::filter(species=="Lobster") |> 
  dplyr::glimpse()

# reformat length composition input data (for SS)
#_N_LengthBins; then enter lower edge of each length bin
#_yr month fleet sex part Nsamp datavector(female-male) ***separate males and females*** 
# fleet data
# size.data_lobster
data <- observer_data_fleet_lobster
colnames(data)[14] <- "length" 
data$length <- data$length/10 # recorded in mm -> convert to cm for ss
#data <- data |> dplyr::filter(!is.na(sex))
size.min <- 1#round(min(data$length, na.rm = TRUE))
size.max <- 23#round(max(data$length, na.rm = TRUE))
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, 
                                      seq(size.min+width, size.max-width, by = width), size.max))))
size.dist_m <- matrix(NA, 1, n.size+6)
size.dist_f <- matrix(NA, 1, n.size+6)
size.dist_lobster <- NULL 
for (i in c(unique(data$month.yr))) {
  print(i)
  subdata <- data[data$month.yr==i,]
  subdata_m <- subdata[subdata$sex==0,]
  subdata_f <- subdata[subdata$sex==1,]
  if (nrow(subdata_m) > 0) {
    size.dist_m[1] <- unique(subdata_m$year)
    size.dist_m[2] <- unique(subdata_m$month)
    size.dist_m[3] <- 1 # fleet
    size.dist_m[4] <- unique(subdata_m$sex)
    size.dist_m[5] <- 0 #part
    size.dist_m[6] <- nrow(subdata_m)
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), 
                                                      size.max)))
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part
    size.dist_f[6] <- nrow(subdata_f)
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), 
                                                      size.max)))
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_lobster <- dplyr::bind_rows(as.data.frame(size.dist_lobster), as.data.frame(size.dist))
}
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_lobster_m <- size.dist_lobster |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::select(-year, -month, -fleet, -sex, -part, -nsample)
size.dist_lobster_f <- size.dist_lobster |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
size.dist_lobster_fleet <- size.dist_lobster_f |> dplyr::bind_cols(size.dist_lobster_m) 
colnames(size.dist_lobster_fleet) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                                       paste0("f", 1:n.size), paste0("m", 1:n.size))

# offshore data
data <- observer_data_offshore_lobster
colnames(data)[15] <- "length"
#data <- data |> dplyr::filter(!is.na(sex))
data$length <- data$length/10 # recorded in mm -> convert to cm for ss
size.min <- 1#round(min(data$length, na.rm = TRUE))
size.max <- 23#round(max(data$length, na.rm = TRUE))
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, 
                                      seq(size.min+width, size.max-width, by = width), size.max))))
size.dist_m <- matrix(NA, 1, n.size+6)
size.dist_f <- matrix(NA, 1, n.size+6)
size.dist_lobster <- NULL 
for (i in c(unique(data$month.yr))) {
  print(i)
  subdata <- data[data$month.yr==i,]
  subdata_m <- subdata[subdata$sex==0,]
  subdata_f <- subdata[subdata$sex==1,]
  if (nrow(subdata_m) > 0) {
    size.dist_m[1] <- unique(subdata_m$year)
    size.dist_m[2] <- unique(subdata_m$month)
    size.dist_m[3] <- 1 # fleet
    size.dist_m[4] <- unique(subdata_m$sex)
    size.dist_m[5] <- 0 #part
    size.dist_m[6] <- nrow(subdata_m)
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max)))
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part
    size.dist_f[6] <- nrow(subdata_f)
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max-width, by = width), size.max)))
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_lobster <- dplyr::bind_rows(as.data.frame(size.dist_lobster), as.data.frame(size.dist))
}
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_lobster_m <- size.dist_lobster |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::select(-year, -month, -fleet, -sex, -part, -nsample)
size.dist_lobster_f <- size.dist_lobster |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
size.dist_lobster_offshore <- size.dist_lobster_f |> dplyr::bind_cols(size.dist_lobster_m) 
colnames(size.dist_lobster_offshore) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                                          paste0("f", 1:n.size), paste0("m", 1:n.size))

# export data
readr::write_csv(size.dist_lobster_fleet, file = "processed_data/nifca/observer.size.comp.data_lobster_fleet_nifca_ss.csv") 
readr::write_csv(size.dist_lobster_offshore, file = "processed_data/nifca/observer.size.comp.data_lobster_offshore_nifca_ss.csv") 
