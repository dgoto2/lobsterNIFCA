# script for data cleaning after the initial check for observer & quayside datasets - Northumberland IFCA 
# created: 6/10/2024 by Daisuke Goto (d.goto@bangor.ac.uk)

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", 
              "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# run input data processing script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source( file = "1b_observer_dataprocessing_nifca.R" )
setwd("R/")
source( file = "1e_return_dataprocessing_nifca.R" )

# read in data
observer_data_fleet <- readr::read_csv("processed_data/nifca/observer_data_fleet_nifca_clean.csv") |> 
  dplyr::glimpse()
observer_data_offshore <- readr::read_csv("processed_data/nifca/observer_data_offshore_nifca_clean.csv") |> 
  dplyr::glimpse()
returns_data <- readr::read_csv(file = "processed_data/nifca/returns_data_all_nifca_clean.csv") |> 
  dplyr::mutate(nominal.lpue_returns_lobster = dplyr::case_when(pots_hauled_per_month > 0 ~ total_lobster/pots_hauled_per_month,
                                                pots_hauled_per_month == 0  ~ NA),
                nominal.lpue_returns_crab = dplyr::case_when(pots_hauled_per_month > 0 ~ total_crab/pots_hauled_per_month,
                                                        pots_hauled_per_month == 0  ~ NA)) |>
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

# (exploratory) use number of pots on fleet for number of pots samples if missing
#observer_data_offshore <- observer_data_offshore |> 
#  dplyr::mutate(num_pots_sampled = dplyr::case_when(is.na(num_pots_sampled) ~ num_pots_per_fleet,
#                                             !is.na(num_pots_sampled) ~ num_pots_sampled))
# ***number of pot data is available for the fleet observer dataset***

# subset by species
observer_data_fleet_lobster <- observer_data_fleet |> 
  dplyr::filter(species=="Lobster") |> 
  dplyr::glimpse()
observer_data_fleet_crab <- observer_data_fleet |> 
  dplyr::filter(species=="Crab") |> 
  dplyr::glimpse()
observer_data_offshore_lobster <- observer_data_offshore |> 
  dplyr::filter(species=="Lobster") |> 
  dplyr::glimpse()
observer_data_offshore_crab <- observer_data_offshore |> 
  dplyr::filter(species=="Crab") |> 
  dplyr::glimpse()

# compute mass (the same equations used for the welsh stocks)
observer_data_fleet_lobster <- observer_data_fleet_lobster |> 
  dplyr::mutate(sample_mass_kg = 4.36E-07*carapace_width^3.10753)
observer_data_offshore_lobster <- observer_data_offshore_lobster |> 
  dplyr::mutate(sample_mass_kg = 4.36E-07*carapace_width^3.10753)
observer_data_fleet_crab <- observer_data_fleet_crab |> 
  dplyr::mutate(sample_mass_kg = dplyr::case_when(sex == 0 ~ 0.0002*carapace_width^3.03/1000,  
                                                  sex == 1 ~ 0.0002*carapace_width^2.94/1000))
observer_data_offshore_crab <- observer_data_offshore_crab |> 
  dplyr::mutate(sample_mass_kg = dplyr::case_when(sex == 0 ~ 0.0002*carapace_width^3.03/1000, 
                                                  sex == 1 ~ 0.0002*carapace_width^2.94/1000))


# function to compute nominal catch, landings, cpue, and lpue per fishing trip 
compute_cpue.lpue <- function(data) {
  
  # per fishing trip
  # compute effort (number of pots lifted per trip)
  observer_data_effort <- data |> 
    dplyr::filter(!is.na(num_pots_sampled)) |>    # ***MANY NA IN POTS SAMPLED in offshore data -> USE FLEETS & RETURNS DATAFOR EFFORT****
    dplyr::group_by(date, vesselID, fleet_num) |> 
    dplyr::summarise(nominal.effort = unique(num_pots_sampled, na.rm = TRUE)) 
  
  # compute total catch (kg) per trip
  observer_data_catch <- data |> 
    dplyr::group_by(date, vesselID, fleet_num) |> 
    dplyr::summarise(nominal.catch = sum(sample_mass_kg, na.rm = TRUE)) 
  
  # compute total landings (kg) per trip
     observer_data_landing <- data |> 
       dplyr::group_by(date, vesselID, fleet_num) |> 
       dplyr::filter(undersize==0) |>    
       dplyr::summarise(nominal.landing = sum(sample_mass_kg, na.rm = TRUE))
  
  # merge datasets and add vessel info
  observer_data_trip <- observer_data_catch |> 
    list(observer_data_landing, observer_data_effort) |> 
    purrr::reduce(dplyr::left_join) |>
    dplyr::mutate(nominal.cpue = nominal.catch/nominal.effort
                  , nominal.lpue = nominal.landing/nominal.effort) |>
    tidyr::unite(trip, c(vesselID, date), sep = "|", remove = FALSE) |> # create factor per fishing trip
    dplyr::mutate(date = as.Date(date), 
                  month = lubridate::month(date), 
                  quarter = lubridate::quarter(date), 
                  year = lubridate::year(date))
  observer_data_select_trip <- data |> 
    dplyr::group_by(date, vesselID, fleet_num) |> 
    dplyr::reframe(# vessel_len=unique(vessel_len), 
      #nominal.lpue_returns_lobster = mean(nominal.lpue_returns_lobster, na.rm = TRUE),
      #nominal.lpue_returns_crab = mean(nominal.lpue_returns_crab, na.rm = TRUE),
                  num_pots_per_fleet = unique(num_pots_per_fleet),
                  port = unique(port), 
                  species = unique(species)) # trip-level info
  observer_data_trip <- observer_data_trip |> 
    dplyr::left_join(observer_data_select_trip, by = c("date", "vesselID", "fleet_num")) 
  
  
  # per pot set (w/ unique gps coordinates)
  # compute total number of pots set in each location in each trip
  observer_data_potset <- data |> 
    dplyr::filter(!is.na(num_pots_sampled)) |>
    dplyr::group_by(trip, lat_lon) |> 
    dplyr::summarise(potset = unique(num_pots_sampled))#, na.rm = TRUE)) 
  
  # compute total catch (kg) per each location in each trip
  observer_data_catch_potset <- data |> 
    dplyr::group_by(lat_lon, trip) |> 
    dplyr::summarise(nominal.catch_potset = sum(sample_mass_kg, na.rm = TRUE)) 
  
  # compute total landings (kg) per each location in each trip
  observer_data_landing_potset <- data |> 
    dplyr::group_by(lat_lon, trip) |>
    dplyr::filter(undersize==0) |>
    dplyr::summarise(nominal.landing_potset = sum(sample_mass_kg, na.rm = TRUE))

  # merge all datasets
  observer_data_potset <- observer_data_catch_potset |> 
    list(observer_data_landing_potset, observer_data_potset) |> 
    purrr::reduce(dplyr::left_join) |>
    dplyr::mutate(nominal.cpue_potset = nominal.catch_potset/potset 
                  , nominal.lpue_potset = nominal.landing_potset/potset) |>
    tidyr::separate_wider_delim(cols = lat_lon, delim = "|", names = c("lat", "lon")) |>
    tidyr::separate_wider_delim(cols = trip, delim = "|", names = c("vesselID", "date")) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::mutate(lat = as.numeric(lat), 
                  lon = as.numeric(lon), 
                  month = lubridate::month(date), 
                  quarter = lubridate::quarter(date), 
                  year = lubridate::year(date))
  
  # add covariate info
  observer_data_select_potset <- data |> 
    dplyr::group_by(vesselID, date, start_lat, start_lon) |> 
    dplyr::reframe(#vessel_len=unique(vessel_len), 
                  port = unique(port), #ices_sub_rect=unique(ices_sub_rect), 
                  species = unique(species), 
                  num_pots_per_fleet = unique(num_pots_per_fleet),
                  num_pots_per_fleet = unique(pot_type), 
                  #nominal.lpue_returns_lobster = unique(nominal.lpue_returns_lobster),
                  #nominal.lpue_returns_crab = unique(nominal.lpue_returns_crab),
                  lat = unique(start_lat), 
                  lon = unique(start_lon), 
                  vesselID = unique(as.character(vesselID)),
                   #lowest_tide=unique(lowest_tide), 
                  temp = mean(temperature, na.rm = TRUE), 
                  temp_interpol_dist = mean(temperature_interpolation_dist, na.rm = TRUE), 
                  dist_shore = mean(Distance_to_shore, na.rm = TRUE), 
                  depth = mean(Depth, na.rm = TRUE), 
                  depth_interpol_dist = mean(Depth_interpolation_distance, na.rm = TRUE), 
                  folk_16 = unique(folk_16), 
                  folk_7 = unique(folk_7)
                   ) # potset-level info
  observer_data_potset <- observer_data_potset |> 
    dplyr::left_join(observer_data_select_potset, by = c("vesselID", "date", "lat", "lon")) 
  
  return(list(observer_data_trip, observer_data_potset))
}

# apply the function to each stock
observer_data_fleet_lobster_out <- compute_cpue.lpue(observer_data_fleet_lobster) # lobster
observer_data_fleet_crab_out <- compute_cpue.lpue(observer_data_fleet_crab) # crab
observer_data_offshore_lobster_out <- compute_cpue.lpue(observer_data_offshore_lobster) # lobster
observer_data_offshore_crab_out <- compute_cpue.lpue(observer_data_offshore_crab) # crab

# REPLACE landings and LPUE before OCTOER 2017 (IMPLEMENTAITON OF BANNING OF SUBLEGAL SIZED ANIMALS)
# *** UNDERSIZED ANIMALS ARE RELEASED AFTER October 2017
replace_landing <- function(data) {
  data[[1]][data[[1]]$year < 2017, ] <- data[[1]] |>
  dplyr::filter(year < 2017) |>
  dplyr::mutate(nominal.landing = nominal.catch,
                nominal.lpue = nominal.cpue)
  data[[1]][data[[1]]$year == 2017 & data[[1]]$month < 10, ]  <- data[[1]] |>
    dplyr::filter((year == 2017) & (month < 10)) |>
    dplyr::mutate(nominal.landing = nominal.catch,
                   nominal.lpue = nominal.cpue)
  data[[2]][data[[2]]$year < 2017, ] <- data[[2]] |>
    dplyr::filter(year < 2017) |>
    dplyr::mutate(nominal.landing_potset = nominal.catch_potset,
                  nominal.lpue_potset = nominal.cpue_potset)
   data[[2]][data[[2]]$year == 2017 & data[[2]]$month < 10, ] <- data[[2]] |>
     dplyr::filter(year == 2017 & month < 10) |>
     dplyr::mutate(nominal.landing_potset = nominal.catch_potset,
                   nominal.lpue_potset = nominal.cpue_potset)
  return(list(data[[1]], data[[2]]))
}

observer_data_fleet_lobster_out <- replace_landing(observer_data_fleet_lobster_out)
observer_data_fleet_crab_out <- replace_landing(observer_data_fleet_crab_out)
observer_data_offshore_lobster_out <- replace_landing(observer_data_offshore_lobster_out)
observer_data_offshore_crab_out <- replace_landing(observer_data_offshore_crab_out)

# # returns data per month, observer data per fishing trip
# observer_data_fleet_lobster_out[[1]] <- observer_data_fleet_lobster_out[[1]] |> 
#   dplyr::right_join(returns_data, by = c("vesselID" = "vessel_id", "year", "month", "qrt.yr", "month.yr" = "mon_year"))
# observer_data_offshore <- observer_data_offshore |> 
#   dplyr::right_join(returns_data, by = c("vesselID" = "vessel_id", "year", "month", "qrt.yr", "month.yr" = "mon_year"))

returns_data_compact <- returns_data |>
  dplyr::group_by(vessel_id, year, month, sector, landing_port) |>
  dplyr::filter(nil_return == "No") |>
  dplyr::reframe(number_of_pots_in_sea = mean(number_of_pots_in_sea, na.rm = TRUE),                 
                 average_number_of_pots_hauled_per_day = mean(average_number_of_pots_hauled_per_day, na.rm = TRUE),
                 number_of_days_pots_hauled = mean(number_of_days_pots_hauled, na.rm = TRUE),         
                 pots_hauled_per_month = mean(pots_hauled_per_month, na.rm = TRUE),                 
                 total_crab = mean(total_crab, na.rm = TRUE),                            
                 total_lobster = mean(total_lobster, na.rm = TRUE),                         
                 total_nephrops = mean(total_nephrops, na.rm = TRUE),                        
                 total_velvet = mean(total_velvet, na.rm = TRUE),                         
                 total_whelk = mean(total_whelk, na.rm = TRUE),                           
                 pots_set = mean(pots_set, na.rm = TRUE),                             
                 pots_hauled = mean(pots_hauled, na.rm = TRUE),                           
                 lobster_landed = mean(lobster_landed, na.rm = TRUE),                        
                 crab_landed = mean(crab_landed, na.rm = TRUE),                          
                 nominal.lpue_returns_lobster = mean(nominal.lpue_returns_lobster, na.rm = TRUE),          
                 nominal.lpue_returns_crab = mean(nominal.lpue_returns_crab, na.rm = TRUE)
  )


# export output as rds (as a list)
readr::write_rds(observer_data_fleet_lobster_out, file = "processed_data/nifca/observer_data_fleet_lobster_nominal.cpue.rds") # lobster
readr::write_rds(observer_data_fleet_crab_out, file = "processed_data/nifca/observer_data_fleet_crab_nominal.cpue.rds") # crab
readr::write_rds(observer_data_offshore_lobster_out, file = "processed_data/nifca/observer_data_offshore_lobster_nominal.cpue.rds") # lobster
readr::write_rds(observer_data_offshore_crab_out, file = "processed_data/nifca/observer_data_offshore_crab_nominal.cpue.rds") # crab

# export output as csv
readr::write_csv(observer_data_fleet_lobster_out[[1]], file = "processed_data/nifca/observer_data_fleet_lobster_nominal.cpue_trip.csv") 
readr::write_csv(observer_data_fleet_crab_out[[1]], file = "processed_data/nifca/observer_data_fleet_crab_nominal.cpue_trip.csv")
readr::write_csv(observer_data_fleet_lobster_out[[2]], file = "processed_data/nifca/observer_data_fleet_lobster_nominal.cpue_potset.csv") 
readr::write_csv(observer_data_fleet_crab_out[[2]], file = "processed_data/nifca/observer_data_fleet_crab_nominal.cpue_potset.csv")

readr::write_csv(observer_data_offshore_lobster_out[[1]], file = "processed_data/nifca/observer_data_offshore_lobster_nominal.cpue_trip.csv") 
readr::write_csv(observer_data_offshore_crab_out[[1]], file = "processed_data/nifca/observer_data_offshore_crab_nominal.cpue_trip.csv")
readr::write_csv(observer_data_offshore_lobster_out[[2]], file = "processed_data/nifca/observer_data_offshore_lobster_nominal.cpue_potset.csv") 
readr::write_csv(observer_data_offshore_crab_out[[2]], file = "processed_data/nifca/observer_data_offshore_crab_nominal.cpue_potset.csv")

readr::write_csv(returns_data_compact, file = "processed_data/nifca/returns_data_compact.csv")


# plot output
# temporal variation
# select a dataset and a parameter
data <- observer_data_fleet_lobster_out[[1]]
response <- data$nominal.cpue
response.name <- "nominal catch rate (kg per number of pots hauled)"

mycolors <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), 
              RColorBrewer::brewer.pal(name = "Set3", n = 7))
(plot1 <- data |> ggplot2::ggplot(ggplot2::aes(lubridate::quarter(date, with_year = TRUE), response, 
                                     group = lubridate::quarter(date, with_year = TRUE))) +
  ggplot2::scale_color_manual(values = mycolors) +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::geom_jitter(size = 2., ggplot2::aes(lubridate::quarter(date, with_year = TRUE), response,
                                               group = lubridate::quarter(date, with_year = TRUE),
                                               color = as.factor(lubridate::month(date))), 
                       alpha = 0.4) +
  ggplot2::xlab("year") +
  ggplot2::ylab(response.name) +
  ggplot2::theme_classic() +
  ggplot2::theme( 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black"),
    axis.title.x = ggplot2::element_text(size=10),
    axis.title.y = ggplot2::element_text(size=10),	
    axis.text.x = ggplot2::element_text(size=8), 
    axis.text.y = ggplot2::element_text(size=8),
    legend.background = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(colour="black", size = 8),
    plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
    legend.key = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(), 
    strip.placement = "outside",
    strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))))

# export plot
ggplot2::ggsave(file=paste0("plots/nifca/", response.name, "_observer_trends_nifca.svg"), plot=plot1, width=12, height=8)

# spatial distribution
# select a dataset and a parameter
data <- observer_data_offshore_lobster_out[[2]]
response <- data$nominal.cpue_potset
response.name <- "nominal cpue (kg per number of pots hauled)"

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#eez <- sf::read_sf(dsn = "data/shapefiles/World_EEZ_v12_20231025/eez_v12.shp", stringsAsFactors = FALSE)
#eez_uk <- eez |> dplyr::filter()

xlim <- c(min(data$lon, na.rm = TRUE)*2.7, 
          max(data$lon, na.rm = TRUE)*-0.3)
ylim <- c(min(data$lat, na.rm = TRUE)*0.97, 
          max(data$lat, na.rm = TRUE)*1.02)
# icea rectangles
shp_ices.rec <- sf::read_sf(dsn = "data/ICES_Rect/ICES_Statistical_Rectangles_Eco.shp", stringsAsFactors = FALSE)

# read in ICES rectangles for wales
# subset nifca landings
ices_rec <- readr::read_delim(file = "data/nifca/ices_rectangles_england.csv") |> 
  dplyr::filter(proportion != 0)
nifca_rec <- c("39E8", "39E9", "40E8", "40E9", "41E8", "39E0",  "41E7") 
ices_rec_nifca <- ices_rec |> 
  dplyr::filter(`ICES Rectangle` %in% nifca_rec)

# subset nifca
shp_ices.rec_nifca <- shp_ices.rec |> 
  dplyr::right_join(ices_rec_nifca, by = c("ICESNAME"="ICES Rectangle")) |>
  dplyr::mutate(PERCENTAGE = PERCENTAGE*proportion)

(plot2 <- ggplot2::ggplot(data = world) +  
  ggplot2::scale_color_manual(values = mycolors) +
  ggplot2::geom_sf(data = shp_ices.rec_nifca, fill = NA, colour = "darkblue") +
  ggplot2::geom_sf() +
  ggplot2::coord_sf(xlim = xlim, 
                    ylim = ylim, expand = FALSE) +
  ggplot2::geom_point(data=data, ggplot2::aes(x=lon, y=lat, size = nominal.cpue_potset, color=as.factor(month)), 
                      alpha=I(0.3)) + 
  ggplot2::theme_bw() +
  ggplot2::theme( 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black"),
    axis.title.x = ggplot2::element_text(size=10),
    axis.title.y = ggplot2::element_text(size=10),	
    axis.text.x = ggplot2::element_text(size=8), 
    axis.text.y = ggplot2::element_text(size=8),
    legend.background = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(colour="black", size = 8),
    plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
    legend.key = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(), 
    strip.placement = "outside",
    strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))  +
  ggplot2::guides(fill=ggplot2::guide_legend(title="Month")) +
  ggplot2::facet_wrap(~ year, strip.position = "top", ncol = 3))

# export plot
ggplot2::ggsave(file=paste0("plots/nifca/", response.name, "_observer_space_nifca.svg"), 
                plot=plot2, width=12, height=8)


# size structure
# select a dataset
nominal.cpue_potset_no.na <- observer_data_fleet_lobster |> 
  dplyr::filter(!is.na(sex))
observer_data_crabb_no.na <- observer_data_offshore_crab |> 
  dplyr::filter(!is.na(sex))
sex.label <- c("male", "female")
names(sex.label) <- c(0, 1)

ggplot2::ggplot(nominal.cpue_potset_no.na, 
                ggplot2::aes(x = carapace_width, 
                             y = as.factor(year))) +
  ggridges::geom_density_ridges(scale = 2.5, 
                                alpha = 0.3, 
                                quantile_lines = TRUE, 
                                quantiles = 0.5, 
                                ggplot2::aes(fill = as.factor(sex))) +
  ggplot2::xlab("carapace width (mm)") +
  ggplot2::ylab("year") +
  ggplot2::scale_fill_manual(labels = c("male", "female"), 
                             values = c("darkblue", "darkred")) +
  ggplot2::theme_classic() + 
  ggplot2::coord_flip() +
  ggplot2::theme( 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black"),
    axis.title.x = ggplot2::element_text(size=10),
    axis.title.y = ggplot2::element_text(size=10),	
    axis.text.x = ggplot2::element_text(size=8), 
    axis.text.y = ggplot2::element_text(size=8),
    legend.background = ggplot2::element_blank(),
    legend.position = "none",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(colour="black", size = 8),
    plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
    legend.key = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(), 
    strip.placement = "outside",
    strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))  +
  ggplot2::facet_wrap(~ sex, 
                      labeller = ggplot2::labeller(sex = sex.label), 
                      strip.position = "top", 
                      ncol = 3) 

# export plot
ggplot2::ggsave(file=paste0("plots/nifca/size.comp_observer_nifca.svg"), plot=plot3, width=12, height=8)
