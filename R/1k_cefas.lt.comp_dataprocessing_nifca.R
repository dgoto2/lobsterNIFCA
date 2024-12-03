# script for data cleaning for the crab and lobster length composition dataset (from CEFAS port sampling) - Northumberland IFCA

# Check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "janitor")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
port.sampling_england <- readr::read_csv(file = "data/nifca/port.sampling_lobster.crab_england.csv", 
                                         col_types = readr::cols(Rectangle = readr::col_character())) |> 
  dplyr::glimpse()
port.sampling_england <- port.sampling_england |> 
  janitor::clean_names() 
port.sampling_nifca <- port.sampling_england |> 
  dplyr::filter(rectangle %in% c("39E8", "39E9", "39E0", "40E8", "40E9", "41E7", "41E8")) |> 
  dplyr::glimpse()

# data cleaning
port.sampling_nifca <- port.sampling_nifca |> 
  dplyr::mutate(quarter = dplyr::case_when(month %in% c(1,2,3) ~ 1,
                                           month %in% c(4,5,6) ~ 2,
                                           month %in% c(7,8,9) ~ 3,
                                           month %in% c(10,11,12) ~ 4)) |>  
  tidyr::unite(month.yr, c(year, month), sep = "-", remove = FALSE) |> 
  tidyr::unite(qrt.yr, c(year, quarter), sep = "-", remove = FALSE) |>
  tidyr::unite(month.yr.rect, c(year, month, rectangle), sep = "-", remove = FALSE) |> 
  dplyr::mutate(sex = dplyr::recode(sex, F=1, M=0, B=1, U=3)) 

# subset species
port.sampling_nifca.lobster <- port.sampling_nifca |> 
  dplyr::filter(species == "LBE")
port.sampling_nifca.crab <- port.sampling_nifca |> 
  dplyr::filter(species == "CRE")

# export datasets
readr::write_csv(port.sampling_nifca.lobster, file = "processed_data/nifca/lt.comp_lobster_nifca_clean.csv") 
readr::write_csv(port.sampling_nifca.crab, file = "processed_data/nifca/lt.comp_crab_nifca_clean.csv")

# read in landings data
ifish_landings_lobster <- readr::read_csv(file = "processed_data/nifca/ifish_landings_nifca_lobster_clean.csv",
                                          col_types = readr::cols(rectangle = readr::col_character())) 
ifish_landings_crab <- readr::read_csv(file = "processed_data/nifca/ifish_landings_nifca_crab_clean.csv",
                                       col_types = readr::cols(rectangle = readr::col_character()))

# merge landings and size comp data
data_lobster <- port.sampling_nifca.lobster |> 
  dplyr::left_join(ifish_landings_lobster,   
                   by = c("species", "rectangle", "month.yr"="mon_year", "qrt.yr", "year", "month", "quarter"="qtr"))
data_crab <- port.sampling_nifca.crab |> dplyr::left_join(ifish_landings_crab) |> 
  dplyr::left_join(ifish_landings_lobster,   
                   by = c("species", "rectangle", "month.yr"="mon_year", "qrt.yr", "year", "month", "quarter"="qtr"))


# reformat length composition input data (for SS) 
# lobster
data <- data_lobster
data$length <- data$length/10 
size.min <- 1
(size.max <- round(max(data$length, na.rm = TRUE)))
width <- 0.2
n.size <- length(table(cut(data$length, breaks = c(size.min, seq(size.min+width, size.max-width, by = width), 
                                                   size.max))))
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
    size.dist_m[3] <- 1 
    size.dist_m[4] <- unique(subdata_m$sex)
    size.dist_m[5] <- 0 
    size.dist_m[6] <- nrow(subdata_m) 
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, seq(size.min+width, size.max-width, by = width), 
                                                      size.max))) 
    size.dist_m[(n.size+6)+1] <- unique(subdata_m$qrt.yr)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part
    size.dist_f[6] <- nrow(subdata_f)
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, seq(size.min+width, size.max-width, by = width), 
                                                      size.max)))
    size.dist_f[(n.size+6)+1] <- unique(subdata_f$qrt.yr)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_lobster <- dplyr::bind_rows(as.data.frame(size.dist_lobster), as.data.frame(size.dist))
}

# aggregate by quarter
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size), "qrt.yr")
size.dist_m2 <- matrix(0, 1, n.size+6)
size.dist_f2 <- matrix(0, 1, n.size+6)
size.dist_lobster2 <- NULL 
for (i in c(unique(data$qrt.yr))) {
  subdata <- size.dist_lobster[size.dist_lobster$qrt.yr==i,]
  subdata_m <- subdata[subdata$sex==0,1:ncol(subdata)-1]
  subdata_f <- subdata[subdata$sex==1,1:ncol(subdata)-1]
  if (nrow(subdata_m) > 0) {
    size.dist_m2[1] <- unique(subdata_m$year)
    size.dist_m2[2] <- max(subdata_m$month)
    size.dist_m2[3] <- unique(subdata_m$fleet)
    size.dist_m2[4] <- unique(subdata_m$sex)
    size.dist_m2[5] <- unique(subdata_m$part)
    size.dist_m2[6] <- round(sum(as.numeric(subdata_m$nsample)))
    if (nrow(subdata_m) > 1) {
      size.dist_m2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_m[7:ncol(subdata_m)], 2, as.numeric))), digits=1)
    } else size.dist_m2[7:(n.size+6)] <- round(as.numeric(subdata_m[7:ncol(subdata_m)]), digits = 1)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f2[1] <- unique(subdata_f$year)
    size.dist_f2[2] <- max(subdata_f$month)
    size.dist_f2[3] <- unique(subdata_f$fleet)
    size.dist_f2[4] <- unique(subdata_f$sex)
    size.dist_f2[5] <- unique(subdata_f$part)
    size.dist_f2[6] <- round(sum(as.numeric(subdata_f$nsample)))
    if (nrow(subdata_f) > 1) {
      size.dist_f2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_f[7:ncol(subdata_f)], 2, as.numeric))), digits=1)
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
  dplyr::group_by(year, sizebin) |>
  dplyr::reframe(year = unique(year),
                 month = max(month),
                 fleet = 3,
                 sex = 3,
                 part = 0,
                 nsample = sum(nsample),
                 sizebin = unique(sizebin),
                 value = sum(as.numeric(value))/nsample) |>
  tidyr::spread(sizebin, value) |>
  dplyr::glimpse()


# crab
data <- data_crab
data$length <- data$length/10 
size.min <- 1
(size.max <- round(max(data$length, na.rm = TRUE))) 
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min,
                                      seq(size.min+width, size.max-width, by = width), 
                                      size.max))))
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
                                           breaks = c(size.min, seq(size.min+width, size.max+width*2-width, by = width), 
                                                      size.max+width*2))) 
    size.dist_m[(n.size+6)+1] <- unique(subdata_m$qrt.yr)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part    
    size.dist_f[6] <- nrow(subdata_f) 
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, seq(size.min+width,size.max+width*2-width, by = width), 
                                                      size.max+width*2))) 
    size.dist_f[(n.size+6)+1] <- unique(subdata_f$qrt.yr)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_crab <- dplyr::bind_rows(as.data.frame(size.dist_crab), as.data.frame(size.dist))
}

# aggregate by quarter
colnames(size.dist_crab) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size), "qrt.yr")
size.dist_m2 <- matrix(0, 1, n.size+6)
size.dist_f2 <- matrix(0, 1, n.size+6)
size.dist_crab2 <- NULL 
for (i in c(unique(data$qrt.yr))) {
  subdata <- size.dist_crab[size.dist_crab$qrt.yr==i,]
  subdata_m <- subdata[subdata$sex==0,1:ncol(subdata)-1]
  subdata_f <- subdata[subdata$sex==1,1:ncol(subdata)-1]
  if (nrow(subdata_m) > 0) {
    size.dist_m2[1] <- unique(subdata_m$year)
    size.dist_m2[2] <- max(subdata_m$month)
    size.dist_m2[3] <- unique(subdata_m$fleet)
    size.dist_m2[4] <- unique(subdata_m$sex)
    size.dist_m2[5] <- unique(subdata_m$part)
    size.dist_m2[6] <- round(sum(as.numeric(subdata_m$nsample)))
    if (nrow(subdata_m) > 1) {
      size.dist_m2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_m[7:ncol(subdata_m)], 2, as.numeric))), digits=1)
    } else size.dist_m2[7:(n.size+6)] <- round(as.numeric(subdata_m[7:ncol(subdata_m)]), digits = 1)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f2[1] <- unique(subdata_f$year)
    size.dist_f2[2] <- max(subdata_f$month)
    size.dist_f2[3] <- unique(subdata_f$fleet)
    size.dist_f2[4] <- unique(subdata_f$sex)
    size.dist_f2[5] <- unique(subdata_f$part)
    size.dist_f2[6] <- round(sum(as.numeric(subdata_f$nsample)))
    if (nrow(subdata_f) > 1) {
      size.dist_f2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_f[7:ncol(subdata_f)], 2, as.numeric))), digits=1)
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
  tidyr::gather(sizebin, value, f1:colnames(size.dist_crab)[length(colnames(size.dist_crab))], 
                factor_key = TRUE) |>
  dplyr::group_by(year, sizebin) |>
  dplyr::reframe(year = unique(year),
                 month = max(month),
                 fleet = 3,
                 sex = 3,
                 part = 0,
                 nsample = sum(nsample),
                 sizebin = unique(sizebin),
                 value = sum(as.numeric(value))/nsample) |>
  tidyr::spread(sizebin, value) |>
  dplyr::glimpse()

# export data
readr::write_csv(size.dist_lobster, file = "processed_data/nifca/size.comp.data_lobster_nifca_ss.csv") 
readr::write_csv(size.dist_crab, file = "processed_data/nifca/size.comp.data_crab_nifca_ss.csv") 
readr::write_csv(size.dist_lobster_yr, file = "processed_data/nifca/size.comp.data_lobster_nifca_yr_ss.csv") 
readr::write_csv(size.dist_crab_yr, file = "processed_data/nifca/size.comp.data_crab_nifca_yr_ss.csv") 
