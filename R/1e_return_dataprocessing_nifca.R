# script for data cleaning for the return (reported catch) dataset. - Northumberland IFCA
# created: 6/7/2024 by Daisuke Goto (d.goto@bangor.ac.uk)

# Check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "janitor")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
returns_data <- readr::read_csv(file = "data/nifca/nifca_returns.csv") |> 
  dplyr::glimpse()
colnames(returns_data) <- returns_data |> 
  janitor::clean_names() |> colnames()

# data cleaning
returns_data <- returns_data |> 
  dplyr::mutate(month = dplyr::recode(month, January=1, February=2, March=3, April=4, May=5, August=8, 
                                      June=6, July=7, September=9, October=10, November=11, December=12)) |>
  dplyr::mutate(month.chr = dplyr::case_when(month < 10 ~ paste0(0, month),
                                         month >= 10 ~ as.character(month))) |>
  tidyr::unite(mon_year, c(year, month.chr), sep = "-", remove = FALSE) |> 
  dplyr::mutate(date = as.Date(paste(mon_year, "-01", sep=""))) |>
  tidyr::unite(qrt.yr, c(year, quarter), sep = "-", remove = FALSE)  

# subset data with total landings by species
returns_data_total <- returns_data |> 
  dplyr::select(colnames(returns_data)[1:30])

# reformat sector data from wide to long
returns_data_sector <- returns_data |> 
  dplyr::select(colnames(returns_data)[c(1:6, 30:36)]) |> 
  tidyr::gather(key="Sector", value="value", 7:13) |>
  dplyr::mutate(Sector = stringr::str_extract(Sector, "\\d+" )) 
returns_data_pot.set <- returns_data |> 
  dplyr::select(colnames(returns_data)[c(1:6, 37:43)]) |>
  tidyr::gather(key="Sector", value="pots_set", 7:13) |>
  dplyr::mutate(Sector = stringr::str_extract(Sector, "\\d+" )) 
returns_data_pot.hauled <- returns_data |> 
  dplyr::select(colnames(returns_data)[c(1:6, 44:50)]) |>
  tidyr::gather(key="Sector", value="pots_hauled", 7:13) |>
  dplyr::mutate(Sector = stringr::str_extract(Sector, "\\d+" )) 
returns_data_lobster.landed <- returns_data |> 
  dplyr::select(colnames(returns_data)[c(1:6, 51:57)]) |>
  tidyr::gather(key="Sector", value="lobster_landed", 7:13) |>
  dplyr::mutate(Sector = stringr::str_extract(Sector, "\\d+" )) 
returns_data_crab.landed <- returns_data |> 
  dplyr::select(colnames(returns_data)[c(1:6, 58:64)]) |>
  tidyr::gather(key="Sector", value="crab_landed", 7:13) |>
  dplyr::mutate(Sector = stringr::str_extract(Sector, "\\d+" )) 

# merge all datasets
returns_data <- returns_data_total |> 
  list(returns_data_sector, returns_data_pot.set, returns_data_pot.hauled, 
       returns_data_lobster.landed, returns_data_crab.landed) |> 
  purrr::reduce(dplyr::left_join)

# export files
readr::write_csv(returns_data_total, file = "processed_data/nifca/returns_data_total_nifca_clean.csv") 
readr::write_csv(returns_data, file = "processed_data/nifca/returns_data_all_nifca_clean.csv") 