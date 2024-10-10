# script for extracting monthly crab and lobster landings data from the MMO IFISH dataset - Northumberland IFCA

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in data
# vessel license history
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
license.history_data <- readr::read_csv(file = "data/FISP_license_history_anon.csv")
dplyr::glimpse(license.history_data)

# ifish landings: 2008-2022 (files in each folder have different data structures)
files1 <- list.files(path="data/mmo_landings_published.data/2008_2012", pattern=".csv$")
files2 <- list.files(path="data/mmo_landings_published.data/2013_2017", pattern=".csv$")
files3 <- list.files(path="data/mmo_landings_published.data/2018_2022", pattern=".csv$")
allmyData1 <- allmyData2 <- allmyData3 <- NULL

for (i in c(files1)) {
  myData1 <- readr::read_csv(file = paste0("data/mmo_landings_published.data/2008_2012/", i))
  allmyData1 <- data.table::rbindlist(list(allmyData1, myData1)) 
}
allmyData1$fao_area <- NA
colnames(allmyData1) <- allmyData1 |> 
  janitor::clean_names() |> colnames()
allmyData1 <- allmyData1 |> 
  dplyr::select(year, month, vessel_nationality, fao_area, ices_division, rectangle, length_group, 
                gear_category, species, species_group, live_weight_tonnes, landed_weight_tonnes, value_pounds)

for (i in c(files2)) {
  myData2 <- readr::read_csv(file = paste0("data/mmo_landings_published.data/2013_2017/", i))
  allmyData2 <- data.table::rbindlist(list(allmyData2, myData2)) 
}
colnames(allmyData2) <- allmyData2 |> 
  janitor::clean_names()|> colnames()
allmyData2 <- allmyData2 |> 
  dplyr::select( -"species_code", -"species_as_shown_in_publication")
colnames(allmyData2)[9] <- "species"

for (i in c(files3)) {
  myData3 <- readr::read_csv(file = paste0("data/mmo_landings_published.data/2018_2022/", i))
  allmyData3 <- data.table::rbindlist(list(allmyData3, myData3))
}
allmyData3$ices_division <- NA
colnames(allmyData3) <- allmyData3 |> 
  janitor::clean_names()|> colnames()
allmyData3 <- allmyData3 |> 
  dplyr::select(-reported_zone, -estimated_eez_of_capture, -estimated_region_of_capture, 
                -port_of_landing, -port_nationality, -species_code, -landing_type, -tac_code)
allmyData3 <- allmyData3 |> 
  dplyr::select(year, month,  vessel_nationality, fao_division, ices_division, rectangle,   
                length_group, gear_category, species_name, species_group, live_weight_tonnes, landed_weight_tonnes, value_pounds)
colnames(allmyData3)[4] <- "fao_area"
colnames(allmyData3)[9] <- "species"

# merge all landings datasets
ifish_landings <- data.table::rbindlist(list(allmyData1, allmyData2, allmyData3))|> dplyr::glimpse()

# subset nifca landings
ices_rec <- readr::read_delim(file = "data/nifca/ices_rectangles_england.csv") |> 
  dplyr::filter(proportion != 0)
nifca_rec <- c("39E8", "39E9", "40E8", "40E9", "41E8", "39E0",  "41E7") 
ices_rec_nifca <- ices_rec |> 
  dplyr::filter(`ICES Rectangle` %in% nifca_rec)

ifish_landings_nifca <- ifish_landings |> 
  dplyr::filter(rectangle %in% nifca_rec) |>
  dplyr::left_join(ices_rec_nifca, by = c("rectangle"="ICES Rectangle")) |>
  dplyr::mutate(month.chr = dplyr::case_when(month < 10 ~ paste0(0, month), 
                                             month >= 10 ~ as.character(month))) |>
  tidyr::unite(mon_year, c(year, month.chr), sep = "-", remove = FALSE) |> 
  dplyr::mutate(date = as.Date(paste(mon_year, "-01", sep=""))) |>
  dplyr::mutate(qtr = lubridate::quarter(date, with_year = FALSE)) |> 
  tidyr::unite(qrt.yr, c(year, qtr), sep = "-", remove = FALSE) |> 
  dplyr::mutate(live_wt_t = round(live_weight_tonnes*proportion, 4), 
                landed_wt_t = round(landed_weight_tonnes*proportion, 4)) 

# convert ices rectangles to coordinates
ifish_landings_nifca <- ifish_landings_nifca |> 
  dplyr::mutate(lon = mapplots::ices.rect(rectangle)$lon,  
                lat = mapplots::ices.rect(rectangle)$lat) |> 
  dplyr::glimpse()

# subset landings by species
ifish_landings_nifca_lobster <- ifish_landings_nifca |> 
  dplyr::filter(species %in% c("Lobsters" )) |> 
  dplyr::filter((gear_category == "Pots and traps"))

# bycatch (crabs and lobsters not caught by pots and traps)
ifish_landings_nifca_lobster_bycatch <- ifish_landings_nifca |> 
  dplyr::filter(species %in% c("Lobsters" )) |> 
  dplyr::filter(!(gear_category == "Pots and traps"))

# export output as csv
readr::write_csv(ifish_landings_nifca_lobster, file = "processed_data/nifca/ifish_landings_nifca_lobster_clean.csv") 
readr::write_csv(ifish_landings_nifca_lobster_bycatch, file = "processed_data/wales/ifish_landings_nifca_lobster_bycatch_clean.csv") 

# aggregate by year
ifish_landings_nifca_lobster_annual <- ifish_landings_nifca_lobster |> 
  dplyr::group_by(species, year, lon, lat) |> 
  dplyr::summarise(landings = sum(live_weight_tonnes), 
                   econ.value = sum(value_pounds)/1000000)
ifish_landings_nifca_lobster_bycatch_annual <- ifish_landings_nifca_lobster_bycatch |> 
  dplyr::group_by(species, year, lon, lat) |> 
  dplyr::summarise(landings = sum(live_weight_tonnes), 
                   econ.value = sum(value_pounds)/1000000)

# export output as csv
readr::write_csv(ifish_landings_nifca_lobster_annual, file = "processed_data/nifca/ifish_landings_nifca_lobster_annual_clean.csv") 
readr::write_csv(ifish_landings_nifca_lobster_bycatch_annual, file = "processed_data/nifca/ifish_landings_nifca_lobster_bycatch_annual_clean.csv") 

# exploratory plotting
# temporal trends
data <- ifish_landings_nifca_lobster
response <- data$landed_weight_tonnes
response.name <- "nominal landings (t)"

mycolors = c(RColorBrewer::brewer.pal(name = "Paired", n = 12), 
             RColorBrewer::brewer.pal(name = "Set3", n = 7) )
(plot1 <- data |>
    ggplot2::ggplot(ggplot2::aes(fill = length_group, y = response, x=mon_year)) + 
    ggsci::scale_fill_jco() +
    ggplot2::geom_bar(position="stack", stat="identity") +
    ggplot2::xlab("year") +
    ggplot2::ylab(response.name) +
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
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))))

# export plot
ggplot2::ggsave(file=paste0("plots/nifca/", response.name, "_ifish_trends_nifca.svg"), plot=plot1, width=12, height=8)

# spatial variation
data <- ifish_landings_nifca_lobster_annual
response <- data$landings
response.name <- "nominal landings (t)"

path <- file.path(getwd(),"data/shapefiles")
world.coast <- rgdal::readOGR(dsn = file.path(path, "ne_110m_coastline"), layer = "ne_110m_coastline")
world.coast <- sp::spTransform(world.coast, sp::CRS("+proj=longlat +datum=WGS84"))
xlim <- c(min(data$lon, na.rm = TRUE) * 1.05, 
          max(data$lon, na.rm = TRUE) * 0.7)
ylim <- c(min(data$lat, na.rm = T) * 0.99, 
          max(data$lat, na.rm = T) * 1.05)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

(plot2 <- ggplot2::ggplot(data = world) +  
    ggplot2::scale_color_manual(values = mycolors) +
    #ggsci::scale_color_jco() +
    ggplot2::geom_sf() +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::geom_point(data = data, 
                        ggplot2::aes(x=lon, y=lat, size = landings), 
                        alpha=I(0.3), 
                        color = "darkred") + 
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
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
    ggplot2::guides(fill=ggplot2::guide_legend(title="Month")) +
    ggplot2::facet_wrap(~ year, #scales = "free", 
                        strip.position = "top", ncol = 5))

# export plot
ggplot2::ggsave(file=paste0("plots/nifca/", response.name, "_ifish_ices.rec_nifca_", unique(data$species), ".svg"), plot=plot2, width=12, height=8)

# reformat datasets for ss
# load landings data from published ifish datasets
ifish_landings_nifca_lobster <- ifish_landings_nifca_lobster |>
  dplyr::mutate(length_group = dplyr::case_when(length_group == "10m&Under" ~ "10m&Under", 
                                                length_group == "Over10m" ~ "Over10m", 
                                                length_group %in% c("10.01 - 12.00m",  "12.01 - 15.00m", "15.01 - 18.00m") ~ "Over10m",
                                                length_group %in% c("8.00m and under", "8.01 - 10.00m") ~ "10m&Under"))
ifish_landings_nifca_lobster <- ifish_landings_nifca_lobster |> 
  dplyr::group_by(year, qtr, length_group) |>
  dplyr::reframe(landing = sum(live_weight_tonnes))

# bycatch
ifish_bycatch_nifca_lobster <- ifish_landings_nifca_lobster_bycatch |>
  dplyr::mutate(length_group = dplyr::case_when(length_group == "10m&Under" ~ "10m&Under", 
                                                length_group == "Over10m" ~ "Over10m", 
                                                length_group %in% c("10.01 - 12.00m",  "12.01 - 15.00m", "15.01 - 18.00m", "24.01 - 40.00m", "18.01 - 24.00m", "Over 40.00m" ) ~ "Over10m",
                                                length_group %in% c("8.00m and under", "8.01 - 10.00m") ~ "10m&Under"))
ifish_bycatch_nifca_lobster <- ifish_bycatch_nifca_lobster |> 
  dplyr::group_by(year, qtr, length_group) |>
  dplyr::reframe(landing = sum(live_weight_tonnes))

# Catch data: yr, season, fleet, catch, catch_se
data_lobster <- ifish_landings_nifca_lobster |>
  dplyr::mutate(length_group = dplyr::case_when(length_group == "10m&Under" ~ 1, 
                                                length_group == "Over10m" ~ 2),
                catch.se = 0.05)

# bycatch
data_lobster_bycatch <- ifish_bycatch_nifca_lobster |> 
  dplyr::mutate(length_group = dplyr::case_when(length_group == "10m&Under" ~ 1, 
                                                length_group == "Over10m" ~ 2),
                catch.se = 0.05)

# export datasets
readr::write_csv(data_lobster, file = "processed_data/nifca/landing.data_lobster_nifca_ss.csv") 
readr::write_csv(data_lobster_bycatch, file = "processed_data/nifca/bycatch.data_lobster_nifca_ss.csv") 
