# to standardize cpue/lpue for crab and lobster stocks - nifca

# check if required packages are installed
required <- c("TMB", "readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
devtools::install_github("james-thorson/FishStatsUtils@main", INSTALL_opts="--no-staged-install")
devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")
library(VAST)

# run input data processing script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source(file = "2b_compute_cpue_nifca.R")

# read in data
observer_data_lobster_fleet <- readr::read_csv("processed_data/nifca/observer_data_fleet_lobster_nominal.cpue_potset.csv") |> 
  dplyr::glimpse()
observer_data_crab_fleet <- readr::read_csv("processed_data/nifca/observer_data_fleet_crab_nominal.cpue_potset.csv") |> 
  dplyr::glimpse()
observer_data_lobster_offshore <- readr::read_csv("processed_data/nifca/observer_data_offshore_lobster_nominal.cpue_potset.csv") |> 
  dplyr::glimpse()
observer_data_crab_offshore <- readr::read_csv("processed_data/nifca/observer_data_offshore_crab_nominal.cpue_potset.csv") |> 
  dplyr::glimpse()
# non-spatial data
returns_data <- readr::read_csv("processed_data/nifca/returns_data_compact.csv") |> 
  dplyr::glimpse()

# merge fleet and offshore
observer_data_lobster <- observer_data_lobster_fleet |>
  dplyr::bind_rows(observer_data_lobster_offshore) |>
  dplyr::group_by(year, vesselID) |>
  dplyr::mutate(potset = dplyr::case_when(is.na(potset) ~ mean(potset, na.rm = TRUE),
                                           !is.na(potset) ~ potset)) |>
  dplyr::ungroup() |>
  dplyr::group_by(vesselID) |>
  dplyr::mutate(potset = dplyr::case_when(is.nan(potset) ~ mean(potset, na.rm = TRUE),
                                           !is.nan(potset) ~ potset)) |>
  dplyr::ungroup() |>
  dplyr::mutate(potset = dplyr::case_when(is.nan(potset) ~ mean(potset, na.rm = TRUE),
                                            !is.nan(potset) ~ potset)) |>
  dplyr::mutate(nominal.cpue_potset = dplyr::case_when(is.na(nominal.cpue_potset) ~ nominal.catch_potset/potset,
                                           !is.nan(nominal.cpue_potset) ~ nominal.cpue_potset),
                nominal.lpue_potset = dplyr::case_when(is.na(nominal.lpue_potset) ~ nominal.landing_potset/potset,
                                                       !is.na(nominal.lpue_potset) ~ nominal.lpue_potset)) |>
  dplyr::glimpse()

observer_data_crab <- observer_data_crab_fleet |>
  dplyr::bind_rows(observer_data_crab_offshore) |>
  dplyr::group_by(year, vesselID) |>
  dplyr::mutate(potset  = dplyr::case_when(is.na(potset) ~ mean(potset, na.rm = TRUE),
                                           !is.na(potset) ~ potset)) |>
  dplyr::ungroup() |>
  dplyr::group_by(vesselID) |>
  dplyr::mutate(potset  = dplyr::case_when(is.nan(potset) ~ mean(potset, na.rm = TRUE),
                                           !is.nan(potset) ~ potset)) |>
  dplyr::ungroup() |>
  dplyr::mutate(potset  = dplyr::case_when(is.nan(potset) ~ mean(potset, na.rm = TRUE),
                                           !is.nan(potset) ~ potset)) |>
  dplyr::mutate(nominal.cpue_potset = dplyr::case_when(is.nan(nominal.cpue_potset) ~ nominal.catch_potset/potset,
                                                       !is.nan(nominal.cpue_potset) ~ nominal.cpue_potset),
                nominal.lpue_potset = dplyr::case_when(is.nan(nominal.lpue_potset) ~ nominal.landing_potset/potset,
                                                       !is.nan(nominal.lpue_potset) ~ nominal.lpue_potset)) |>
  dplyr::glimpse()

# reformat datasets
observer_data_orig <- observer_data_lobster
stock <- "lobster"

observer_data_orig$ices_rect <- mapplots::ices.rect2(observer_data_orig$lon, observer_data_orig$lat)
observer_data <- observer_data_orig |> 
  dplyr::filter(!is.na(potset)) |> dplyr::filter(ices_rect!="NANANA") |>
  dplyr::mutate(areaSwept_km2 = 1) |>
  dplyr::arrange(year, month) |>
  dplyr::select(year, quarter, lat, lon, vesselID, ices_rect, nominal.cpue_potset, areaSwept_km2) |>
  dplyr::rename(Lat = lat,
                Lon = lon,
                Year = year,
                season = quarter,
                vessel = vesselID,
                icesRect = ices_rect,
                cpue_kg_pot = nominal.cpue_potset,
                areaSwept_km2 = areaSwept_km2) 

# density covaraites
covariate_data <- observer_data_orig |>
  dplyr::mutate(areaSwept_km2 = 1) |>
  dplyr::arrange(year, month) |>
  dplyr::select(year, lat, lon, month, quarter, vesselID, nominal.cpue_potset, areaSwept_km2, 
                folk_16, temp, depth) |>
  dplyr::rename(Lat = lat,
                Lon = lon,
                month = month,
                season = quarter,
                Year = year,
                vessel = vesselID,
                cpue_kg_pot = nominal.cpue_potset,
                areaSwept_km2 = areaSwept_km2,
                depth = depth,
                temperature = temp,
                sediment = folk_16)

# catchability covariates
catchability_data <- observer_data_orig |>
  dplyr::mutate(areaSwept_km2 = 1) |>
  dplyr::arrange(year, month) |>
  dplyr::select(year, lat, lon, vesselID, month, quarter, ices_rect, nominal.cpue_potset, areaSwept_km2, 
                depth, dist_shore) |>
  dplyr::rename(Lat = lat,
                Lon = lon,
                Year = year,
                month = month,
                season = quarter,
                vessel = vesselID,
                depth = depth,
                cpue_kg_pot = nominal.cpue_potset,
                areaSwept_km2 = areaSwept_km2,
                icesRect = ices_rect,
                shoreDistance = dist_shore) 

# plot
# temporal variation
data <- observer_data_lobster
response <- data$nominal.cpue_potset 
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

# spatial distribution
# select a dataset and a parameter
data <- observer_data_lobster |>
  dplyr::mutate(lon = dplyr::case_when(lon >= -2.0 ~ lon,
                                       lon < -2.0 ~ lon+1))
response.name <- "nominal cpue (kg per number of pots hauled)"
nifca <- sf::read_sf(dsn = "data/nifca/shapefile/NIFCA District (excluding islands).shp", stringsAsFactors = FALSE)
nifca <-  sf::st_transform(nifca, "+proj=longlat +ellps=WGS84 +datum=WGS84") # convert geometry to 

# icea rectangles
shp_ices.rec <- sf::read_sf(dsn = "data/ICES_Rect/ICES_Statistical_Rectangles_Eco.shp", stringsAsFactors = FALSE)

# read in ICES rectangles for nifca
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
ggplot2::ggplot() +  
    ggplot2::scale_color_manual(values = mycolors) +
    ggplot2::geom_sf(data = nifca, ggplot2::aes(fill = as.character(Id)), alpha = 0.2,  colour = "black") +
    ggplot2::geom_sf(data = shp_ices.rec_nifca, fill = NA, colour = "darkblue") +
    ggplot2::geom_point(data=data, ggplot2::aes(x=lon, y=lat, size = nominal.cpue_potset), color="darkred", 
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
    ggplot2::facet_wrap(~ year, strip.position = "top", ncol = 3)


# standardize cpue
# run VAST
dat <- observer_data
settings <- FishStatsUtils::make_settings(n_x = 100,
                                          RhoConfig = c("Beta1" = 0, "Epsilon1" = 0, "Beta2" = 0, "Epsilon2" = 0),
                                          ObsModel = c(4, 3),
                                          FieldConfig = c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = 1, "Epsilon2" = 1),
                                          OverdispersionConfig = c("Eta1" = 0, "Eta2" = 0), 
                                          Options = c('treat_nonencounter_as_zero' = FALSE,
                                                      "report_additional_variables" = TRUE), 
                                          Region = 'User',
                                          purpose = "index2", 
                                          use_anisotropy = TRUE,
                                          bias.correct = FALSE,
                                          fine_scale = TRUE)
user_region <- readRDS('processed_data/nifca/user_region.rds')
run_dir <- paste0(getwd(),"/vast/nifca/", stock, "/base")
dir.create(run_dir,recursive=TRUE)
fit <- NULL
try(fit <- FishStatsUtils::fit_model(settings = settings,
                                     Lat_i = dat$Lat, 
                                     Lon_i = dat$Lon,
                                     t_i = dat$Year, 
                                     b_i = dat$cpue_kg_pot,
                                     a_i = dat$areaSwept_km2,
                                     v_i = dat$vessel,
                                     #Aniso=FALSE,
                                     #getsd = FALSE,
                                     #lower=Lower,
                                     input_grid = user_region,
                                     working_dir = run_dir), silent = TRUE)
try(plot(fit,
         plot_set = c(1:21),
         TmbData = fit$data_list,
         settings = settings,
         n_samples = 0), 
    silent = TRUE)


# add density & catchability covariates
covariate_data_sub <- covariate_data |> 
  dplyr::select(Year, month, Lat, Lon, vessel, cpue_kg_pot, depth, temperature, areaSwept_km2) |>
  na.omit()
catchability_data_sub <- catchability_data |>
  dplyr::select(Year, month, Lat, Lon, vessel, cpue_kg_pot, shoreDistance, depth, areaSwept_km2) |>
  na.omit()

# Define formula
X1_formula = ~ poly( log(temperature), degree=2 ) 
Q1_formula = ~ depth 
X1config_cp = array(3, dim=c(1, 2))
Q1config_k = c(3)
X2_formula = ~ poly( log(temperature), degree=2 ) 
Q2_formula = ~ depth 
X2config_cp = array(3, dim=c(1, 2))
Q2config_k = c(3)
covariate_data_sub[,'temperature'] = covariate_data_sub[,'temperature'] / 100
covariate_data_sub[,'depth'] = covariate_data_sub[,'depth'] / 100
catchability_data_sub[,'shoreDistance'] = catchability_data_sub[,'shoreDistance'] / 100
catchability_data_sub[,'depth'] = catchability_data_sub[,'depth'] / 100

# make settings
dat <- catchability_data_sub 
  settings <- FishStatsUtils::make_settings(n_x = i, 
                                            FieldConfig = c("Omega1" = 0, "Epsilon1" = 0, "Omega2" = 1, "Epsilon2" = 1),  
                                            RhoConfig = c("Beta1" = 0, "Epsilon1" = 0, "Beta2" = 0, "Epsilon2" = 4),
                                            ObsModel = c(4, 3),
                                            OverdispersionConfig = c("Eta1" = 0, "Eta2" = 0), 
                                            Options = c('treat_nonencounter_as_zero' = FALSE,
                                                        "report_additional_variables" = TRUE), 
                                            Region='User',
                                            purpose="index2", 
                                            use_anisotropy=TRUE,
                                            bias.correct=FALSE,
                                            fine_scale=TRUE)
  user_region <- readRDS('processed_data/nifca/user_region.rds')
  run_dir <- paste0(getwd(),"/vast/nifca/", stock, "/cov")
  dir.create(run_dir,recursive=TRUE)
  
  # Run model
  try(fit <- 
        FishStatsUtils::fit_model(settings = settings,
                                  Lat_i = dat$Lat, 
                                  Lon_i = dat$Lon,
                                  t_i = dat$Year, 
                                  b_i = dat$cpue_kg_pot,
                                  a_i = dat$areaSwept_km2,
                                  v_i = dat$vessel,
                                  covariate_data = covariate_data_sub,
                                  #X1_formula = X1_formula,
                                  #X1config_cp = X1config_cp,
                                  X2_formula = X2_formula,
                                  X2config_cp = X2config_cp,
                                  catchability_data = catchability_data_sub,
                                  #Q1_formula = Q1_formula,
                                  #Q1config_k = Q1config_k,
                                  Q2_formula = Q2_formula,
                                  Q2config_k = Q2config_k,
                                  #Aniso = FALSE,
                                  #getsd = FALSE,
                                  input_grid = user_region,
                                  working_dir = run_dir), silent = TRUE)
try(plot(fit,
       plot_set = c(1:21),
       TmbData = fit$data_list,
       settings = settings,
       n_samples = 0), silent = TRUE)

# Calculate percent-deviance-explained
1 - fit1$Report$deviance/fit0$Report$deviance


# reformat cpue data for ss
abundance_index <- readr::read_csv("nifca/lobster/base/index.csv") 
abundance_index <- abundance_index |>
  dplyr::select(Time, Estimate, "Std. Error for Estimate") |>
  dplyr::rename(year = Time,
                obs = Estimate,
                stdrr = "Std. Error for Estimate") |>
  dplyr::filter(obs != 0) |>
  dplyr::mutate(year = year,
                month = 12,
                fleet = 1, 
                obs = obs/mean(obs),
                stdrr = stdrr/mean(stdrr)) |>
  dplyr::arrange(year, month, fleet, obs, stdrr)

# export the file
readr::write_csv(abundance_index, file = "processed_data/nifca/observer_data_lobster_nifca_abundance.index_ss.csv") 
