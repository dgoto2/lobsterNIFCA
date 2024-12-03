# script to reformat size composition data as SS model input for the crab and lobster quayside data - Northumberland IFCA 

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", 
              "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# run input data processing script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source(file = "1e_return_dataprocessing_nifca.R")
source(file = "R/1b_observer_dataprocessing_nifca.R")

# read in data
quayside_data_lobster <- readr::read_csv("processed_data/nifca/observer_data_quayside_nifca_clean_env.csv") |> 
  dplyr::select(-damage, -n_missing_limb, -start_time, -end_time,  -end_lat, -end_lon,  -depth_fa, -depth_m) |>
  dplyr::filter(species == "Lobster") |>
  dplyr::arrange(vesselID , year, month) |>
  dplyr::glimpse()
quayside_data_crab <- readr::read_csv("processed_data/nifca/observer_data_quayside_nifca_clean_env.csv") |> 
  dplyr::select(-damage, -n_missing_limb, -start_time, -end_time,  -end_lat, -end_lon,  -depth_fa, -depth_m) |>
  dplyr::filter(species == "Crab") |>
  dplyr::arrange(vesselID , year, month) |>
  dplyr::glimpse()

return_data_lobster <- readr::read_csv("processed_data/nifca/returns_data_all_nifca_clean.csv") |> 
  dplyr::group_by(vessel_id, year, month) |>
  dplyr::filter(nil_return == "No") |>
  dplyr::reframe(vessel_id = unique(vessel_id),
                 mon_year = unique(mon_year), 
                 qrt.yr = unique(qrt.yr), 
                 year = unique(year), 
                 month = unique(month), 
                 quarter = unique(quarter), 
                 landing_port = unique(landing_port), 
                 sector = unique(sector), 
                 nil_return = unique(nil_return), 
                 n_pots_in_sea = unique(number_of_pots_in_sea), 
                 ave_n_pots_per_day = unique(average_number_of_pots_hauled_per_day),
                 days_pots_hauled = unique(number_of_days_pots_hauled), 
                 pots_hauled_per_month = unique(pots_hauled_per_month), 
                 landings_lobster = unique(total_lobster),
                 nominal_cpue = landings_lobster/pots_hauled_per_month) |>
  dplyr::glimpse()
return_data_crab <- readr::read_csv("processed_data/nifca/returns_data_all_nifca_clean.csv") |> 
  dplyr::group_by(vessel_id, year, month) |>
  dplyr::filter(nil_return == "No") |>
  dplyr::reframe(vessel_id = unique(vessel_id),
                 mon_year = unique(mon_year), 
                 qrt.yr = unique(qrt.yr), 
                 year = unique(year), 
                 month = unique(month), 
                 quarter = unique(quarter), 
                 landing_port = unique(landing_port), 
                 sector = unique(sector), 
                 nil_return = unique(nil_return), 
                 n_pots_in_sea = unique(number_of_pots_in_sea), 
                 ave_n_pots_per_day = unique(average_number_of_pots_hauled_per_day),
                 days_pots_hauled = unique(number_of_days_pots_hauled), 
                 pots_hauled_per_month = unique(pots_hauled_per_month), 
                 landings_crab = unique(total_crab),
                 nominal_cpue = landings_crab/pots_hauled_per_month) |>
  dplyr::glimpse()

# merge size comp & cpue data
data_lobster <- quayside_data_lobster |> 
  dplyr::left_join(return_data_lobster, by = c("vesselID"="vessel_id", "year", "month", "qrt.yr")) |>
  dplyr::glimpse()
data_crab <- quayside_data_crab |> 
  dplyr::left_join(return_data_crab, by = c("vesselID"="vessel_id", "year", "month", "qrt.yr")) |> # , "port"="landing_port"
  dplyr::glimpse()


# reformat length composition input data (for SS)
# observer data (not weighted yet as some cpues are missing)
# lobster - weighted by vessel-level landings 
data <- data_lobster |>
  dplyr::filter(!is.na(sex)) |>
  dplyr::mutate(carapace_width = carapace_width/10)
colnames(data)[14] <- "length" 
size.min <- 1
size.max <- 23
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, 
                                      seq(size.min+width, size.max-width, by = width), size.max))))
size.dist_m <- matrix(NA, 1, n.size+7)
size.dist_f <- matrix(NA, 1, n.size+7)
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
                                                      seq(size.min+width, size.max-width, by = width), size.max))) #* 
    size.dist_m[(n.size+6)+1] <- unique(subdata_m$month.yr)
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
                                                      seq(size.min+width, size.max-width, by = width), size.max))) #* 
    size.dist_f[(n.size+6)+1] <- unique(subdata_f$month.yr)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_lobster <- dplyr::bind_rows(as.data.frame(size.dist_lobster), as.data.frame(size.dist))
}

# aggregate by month
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size), "month.yr")
size.dist_m2 <- matrix(0, 1, n.size+6)
size.dist_f2 <- matrix(0, 1, n.size+6)
size.dist_lobster2 <- NULL 
size.dist_lobster <- size.dist_lobster |> 
  dplyr::filter(is.finite(as.numeric(nsample)))
for (i in c(unique(data$month.yr))) {
  subdata <- size.dist_lobster[size.dist_lobster$month.yr==i,]
  subdata_m <- subdata[subdata$sex==0,1:ncol(subdata)-1]
  subdata_f <- subdata[subdata$sex==1,1:ncol(subdata)-1]
  if (nrow(subdata_m) > 0) {
    size.dist_m2[1] <- unique(subdata_m$year)
    size.dist_m2[2] <- unique(subdata_m$month)
    size.dist_m2[3] <- unique(subdata_m$fleet)
    size.dist_m2[4] <- unique(subdata_m$sex)
    size.dist_m2[5] <- unique(subdata_m$part)
    size.dist_m2[6] <- round(sum(as.numeric(subdata_m$nsample), na.rm = TRUE))
    if (nrow(subdata_m) > 1) {
      size.dist_m2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_m[7:ncol(subdata_m)], 2, as.numeric)), na.rm = TRUE), digits=1)
    } else size.dist_m2[7:(n.size+6)] <- round(as.numeric(subdata_m[7:ncol(subdata_m)]), digits = 1)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f2[1] <- unique(subdata_f$year)
    size.dist_f2[2] <- unique(subdata_f$month)
    size.dist_f2[3] <- unique(subdata_f$fleet)
    size.dist_f2[4] <- unique(subdata_f$sex)
    size.dist_f2[5] <- unique(subdata_f$part)
    size.dist_f2[6] <- round(sum(as.numeric(subdata_f$nsample), na.rm = TRUE))
    if (nrow(subdata_f) > 1) {
      size.dist_f2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_f[7:ncol(subdata_f)], 2, as.numeric)), na.rm = TRUE), digits=1)
    } else size.dist_f2[7:(n.size+6)] <- round(as.numeric(subdata_f[7:ncol(subdata_f)]), digits = 1)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m2), as.data.frame(size.dist_f2))
  size.dist_lobster2 <- dplyr::bind_rows(as.data.frame(size.dist_lobster2), as.data.frame(size.dist))
}

# reformat for ss
colnames(size.dist_lobster2) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_lobster_m <- size.dist_lobster2 |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::rename(nsample.m = nsample) |>
  dplyr::select(-year, -month, -fleet, -sex, -part)
size.dist_lobster_f <- size.dist_lobster2 |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
size.dist_lobster <- size.dist_lobster_f |> 
  dplyr::bind_cols(size.dist_lobster_m) |>
  dplyr::mutate(nsample = as.numeric(nsample)+as.numeric(nsample.m)) |>
  dplyr::select(-nsample.m)
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                                       paste0("f", 1:n.size), paste0("m", 1:n.size))

# aggregate by year
size.dist_lobster_yr <- size.dist_lobster |> 
  tidyr::gather(sizebin, value, f1:colnames(size.dist_lobster)[length(colnames(size.dist_lobster))], 
                factor_key = TRUE) |>
  dplyr::filter(is.finite(nsample)) |>
  dplyr::group_by(year, sizebin) |>
  dplyr::reframe(year = unique(year),
                 month = max(month),
                 fleet = 2,
                 sex = 3,
                 part = 0,
                 nsample = sum(nsample, na.rm = TRUE),
                 sizebin = unique(sizebin),
                 value = sum(as.numeric(value), na.rm = TRUE)/nsample) |>
  tidyr::spread(sizebin, value) |>
  dplyr::glimpse()


# crab (landings are missing in 2022 - low length sample size)
data <- data_crab |>
  dplyr::filter(!is.na(sex)) |>
  dplyr::mutate(carapace_width = carapace_width/10)
colnames(data)[14] <- "length" 
size.min <- 1
size.max <- 24
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, 
                                      seq(size.min+width, size.max-width, by = width), size.max))))
size.dist_m <- matrix(NA, 1, n.size+7)
size.dist_f <- matrix(NA, 1, n.size+7)
size.dist_crab <- NULL 
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
                                                      seq(size.min+width, size.max+width*2-width, by = width), 
                                                      size.max+width*2))) 
    size.dist_m[(n.size+6)+1] <- unique(subdata_m$month.yr)
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
                                                      seq(size.min+width, size.max+width*2-width, by = width),
                                                      size.max+width*2))) 
    size.dist_f[(n.size+6)+1] <- unique(subdata_f$month.yr)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_crab <- dplyr::bind_rows(as.data.frame(size.dist_crab), as.data.frame(size.dist))
}

# aggregate by month
colnames(size.dist_crab) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size), "month.yr")
size.dist_m2 <- matrix(0, 1, n.size+6)
size.dist_f2 <- matrix(0, 1, n.size+6)
size.dist_crab2 <- NULL 
size.dist_crab <- size.dist_crab |> 
  dplyr::filter(is.finite(as.numeric(nsample)))
for (i in c(unique(data$month.yr))) {
  subdata <- size.dist_crab[size.dist_crab$month.yr==i,]
  subdata_m <- subdata[subdata$sex==0,1:ncol(subdata)-1]
  subdata_f <- subdata[subdata$sex==1,1:ncol(subdata)-1]
  if (nrow(subdata_m) > 0) {
    size.dist_m2[1] <- unique(subdata_m$year)
    size.dist_m2[2] <- unique(subdata_m$month)
    size.dist_m2[3] <- unique(subdata_m$fleet)
    size.dist_m2[4] <- unique(subdata_m$sex)
    size.dist_m2[5] <- unique(subdata_m$part)
    size.dist_m2[6] <- round(sum(as.numeric(subdata_m$nsample), na.rm = TRUE))
    if (nrow(subdata_m) > 1) {
      size.dist_m2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_m[7:ncol(subdata_m)], 2, as.numeric)), na.rm = TRUE), digits=1)
    } else size.dist_m2[7:(n.size+6)] <- round(as.numeric(subdata_m[7:ncol(subdata_m)]), digits = 1)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f2[1] <- unique(subdata_f$year)
    size.dist_f2[2] <- unique(subdata_f$month)
    size.dist_f2[3] <- unique(subdata_f$fleet)
    size.dist_f2[4] <- unique(subdata_f$sex)
    size.dist_f2[5] <- unique(subdata_f$part)
    size.dist_f2[6] <- round(sum(as.numeric(subdata_f$nsample), na.rm = TRUE))
    if (nrow(subdata_f) > 1) {
      size.dist_f2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_f[7:ncol(subdata_f)], 2, as.numeric)), na.rm = TRUE), digits=1)
    } else size.dist_f2[7:(n.size+6)] <- round(as.numeric(subdata_f[7:ncol(subdata_f)]), digits = 1)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m2), as.data.frame(size.dist_f2))
  size.dist_crab2 <- dplyr::bind_rows(as.data.frame(size.dist_crab2), as.data.frame(size.dist))
}

# reformat for ss
colnames(size.dist_crab2) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_crab_m <- size.dist_crab2 |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::rename(nsample.m = nsample) |>
  dplyr::select(-year, -month, -fleet, -sex, -part)
size.dist_crab_f <- size.dist_crab2 |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
size.dist_crab <- size.dist_crab_f |> 
  dplyr::bind_cols(size.dist_crab_m) |> 
  dplyr::mutate(nsample = as.numeric(nsample)+as.numeric(nsample.m)) |>
  dplyr::select(-nsample.m)
colnames(size.dist_crab) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                                    paste0("f", 1:n.size), paste0("m", 1:n.size))

# aggregate by year
size.dist_crab_yr <- size.dist_crab |> 
  tidyr::gather(sizebin, value, f1:colnames(size.dist_crab)[length(colnames(size.dist_crab))], factor_key = TRUE) |>
  dplyr::group_by(year, sizebin) |>
  dplyr::reframe(year = unique(year),
                 month = max(month),
                 fleet = 2,
                 sex = 3,
                 part = 0,
                 nsample = sum(nsample, na.rm = TRUE),
                 sizebin = unique(sizebin),
                 value = sum(as.numeric(value), na.rm = TRUE)/nsample) |>
  tidyr::spread(sizebin, value) |>
  dplyr::glimpse()

# export data
readr::write_csv(size.dist_lobster, file = "processed_data/nifca/quayside.size.comp.data_lobster_nifca_ss.csv") 
readr::write_csv(size.dist_crab, file = "processed_data/nifca/quayside.size.comp.data_crab_nifca_ss.csv") 
readr::write_csv(size.dist_lobster_yr, file = "processed_data/nifca/quayside.size.comp.data_lobster_yr_nifca_ss.csv") 
readr::write_csv(size.dist_crab_yr, file = "processed_data/nifca/quayside.size.comp.data_crab_yr_nifca_ss.csv") 
