# script for data cleaning for the crab and lobster length composition dataset (from CEFAS port sampling) - Northumberland IFCA
# created: 6/10/2024 by Daisuke Goto (d.goto@bangor.ac.uk)

# Check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "janitor")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
port.sampling_england <- readr::read_csv(file = "data/nifca/port.sampling_lobster.crab_england.csv", 
                                         col_types = readr::cols(Rectangle = readr::col_character())) |> dplyr::glimpse()
port.sampling_england <- port.sampling_england |> janitor::clean_names() 

# subset northumbland ifca
port.sampling_nifca <- port.sampling_england |> 
  dplyr::filter(rectangle %in% c("39E8", "39E9", "39E0", "40E8", "40E9", "41E7", "41E8")) |> dplyr::glimpse()

# data cleaning
port.sampling_nifca <- port.sampling_nifca |> 
  tidyr::unite(month.yr, c(year, month), sep = "-", remove = FALSE) |> 
  dplyr::mutate(sex = dplyr::recode(sex, F=1, M=0, B=1, U=3)) 

# subset species
port.sampling_nifca.lobster <- port.sampling_nifca |> 
  dplyr::filter(species == "LBE")
port.sampling_nifca.crab <- port.sampling_nifca |> 
  dplyr::filter(species == "CRE")

# # subset port sampling 
# port.sampling_nifca.lobster_port <- port.sampling_nifca |> 
#   dplyr::filter(species == "LBE" & data_source == "Port")
# port.sampling_nifca.crab_port <- port.sampling_nifca |> 
#   dplyr::filter(species == "CRE" & data_source == "Port")
# 
# # subset boat sampling
# port.sampling_nifca.lobster_boat <- port.sampling_nifca |> 
#   dplyr::filter(species == "LBE" & data_source == "Boat")
# port.sampling_nifca.crab_boat <- port.sampling_nifca |> 
#   dplyr::filter(species == "CRE" & data_source == "Boat")

# export datasets
readr::write_csv(port.sampling_nifca.lobster, file = "processed_data/nifca/lt.comp_lobster_nifca_clean.csv") 
readr::write_csv(port.sampling_nifca.crab, file = "processed_data/nifca/lt.comp_crab_nifca_clean.csv")

# readr::write_csv(port.sampling_nifca.lobster_port, file = "processed_data/nifca/lt.comp_lobster_nifca_port_clean.csv") 
# readr::write_csv(port.sampling_nifca.crab_port, file = "processed_data/nifca/lt.comp_crab_nifca_port_clean.csv")
# readr::write_csv(port.sampling_nifca.lobster_boat, file = "processed_data/nifca/lt.comp_lobster_nifca_boat_clean.csv") 
# readr::write_csv(port.sampling_nifca.crab_boat, file = "processed_data/nifca/lt.comp_crab_nifca_boat_clean.csv")


# reformat length composition input data (for SS)
# set up population length bin structure (note - irrelevant if not using size data and using empirical wtatage
#_N_LengthBins; then enter lower edge of each length bin

#_yr month fleet sex part Nsamp datavector(female-male) ***separate males and females*** 
# lobster
data <- port.sampling_nifca.lobster
data$length <- data$length/10 # recorded in mm -> convert to cm for ss
size.min <- 1#round(min(data$length, na.rm = TRUE))
(size.max <- round(max(data$length, na.rm = TRUE)))
width <- 0.2
n.size <- length(table(cut(data$length, breaks = c(size.min, seq(size.min+width, size.max-width, by = width), 
                                                   size.max))))
size.dist_m <- matrix(NA, 1, n.size+6)
size.dist_f <- matrix(NA, 1, n.size+6)
size.dist_lobster <- NULL 
for (i in c(unique(data$month.yr))) {
  print(i)
  #print(data[data$month.yr==i,])
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
                                           breaks = c(size.min, seq(size.min+width, size.max-width, by = width), 
                                                      size.max)))
    #print(size.dist_m[7:(n.size+6)])
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
size.dist_lobster <- size.dist_lobster_f |> dplyr::bind_cols(size.dist_lobster_m) 
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("f", 1:n.size), paste0("m", 1:n.size))

# crab
data <- port.sampling_nifca.crab
data$length <- data$length/10 # recorded in mm -> convert to cm for ss
size.min <- 1#round(min(data$length, na.rm = TRUE))
(size.max <- round(max(data$length, na.rm = TRUE)))
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min,
                                      seq(size.min+width, size.max+width*2-width, by = width), 
                                      size.max+width*2))))
size.dist_m <- matrix(NA, 1, n.size+6)
size.dist_f <- matrix(NA, 1, n.size+6)
size.dist_crab <- NULL 
for (i in c(unique(data$month.yr))) {
  print(i)
  #print(data[data$month.yr==i,])
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
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_crab <- dplyr::bind_rows(as.data.frame(size.dist_crab), as.data.frame(size.dist))
}
colnames(size.dist_crab) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_crab_m <- size.dist_crab |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::rename(nsample.m = nsample) |>
  dplyr::select(-year, -month, -fleet, -sex, -part)
size.dist_crab_f <- size.dist_crab |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
size.dist_crab <- size.dist_crab_f |> 
  dplyr::bind_cols(size.dist_crab_m) |> 
  dplyr::mutate(nsample = nsample+nsample.m) |>
  dplyr::select(-nsample.m)
colnames(size.dist_crab) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("f", 1:n.size), paste0("m", 1:n.size))

# export data
readr::write_csv(size.dist_lobster, file = "processed_data/nifca/size.comp.data_lobster_nifca_ss.csv") 
readr::write_csv(size.dist_crab, file = "processed_data/nifca/size.comp.data_crab_nifca_ss.csv") 


# data exploration
# lobster
data1 <- port.sampling_nifca.lobster

(plot <- data1 |> ggplot2::ggplot(ggplot2::aes(x = length, y = as.factor(year))) +
    ggridges::geom_density_ridges(scale = 2.5, 
                                  alpha = 0.3, 
                                  quantile_lines = TRUE, 
                                  quantiles = 0.5, 
                                  fill = "darkred") +
    ggplot2::xlab("carapace width (mm)") +
    ggplot2::ylab("year") +
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
ggplot2::ggsave(file="plots/nifca/size.comp_lobster_nifca.svg", plot=plot1, width=12, height=8)

# crab
data2 <- port.sampling_nifca.crab

(plot2 <- data2 |> ggplot2::ggplot(ggplot2::aes(x = length, y = as.factor(year))) +
    ggridges::geom_density_ridges(scale = 2.5, 
                                  alpha = 0.3, 
                                  quantile_lines = TRUE, 
                                  quantiles = 0.5, 
                                  fill = "darkred") +
    ggplot2::xlab("carapace width (mm)") +
    ggplot2::ylab("year") +
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
ggplot2::ggsave(file="plots/nifca/size.comp_crab_nifca.svg", plot=plot2, width=12, height=8)
