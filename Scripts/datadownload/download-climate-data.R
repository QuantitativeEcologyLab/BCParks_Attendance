library('climatenaR')   # to download climate data and projections

#' if necessary, install the `climatenaR` package with
#' `remotes::install_github('burnett-m/climatenaR', build_vignettes = TRUE)`
#' *NOTE:* the `climatenaR` package requires the ClimateNA software.
#'         See https://register.climatena.ca/ to download it

#' change the working directory as required by `climatenaR`
setwd('~/GitHub/BCParks_Attendance/climate-na/')

#' download historical climate data
for(y in 2010:2022) {
  cat('Downloading ', y, '...\n', sep = '') # to track progress
  histClimateNA(
    file = 'parks-dem.csv',
    dateR = as.character(y),
    tFrame = 'M', # monthly averages
    exe = 'ClimateNA_v7.31.exe',
    outdir = 'dayna-s-park-climate-data') # exe location (in working directory)
}

#' download climate data projections
for(y in 2021:2100) { # 2021-2040 to 2081-2100
  cat('Downloading ', y, '...\n', sep = '') # to track progress
  projClimateNA(
    file = 'area-dem-2021-11-27-low-res.csv',
    tFrame = 'M', # monthly averages
    exe = 'ClimateNA_v7.31.exe', # exe location (in working directory)
    scen = '8GCM', # 8GCMs_ensemble General Circulation Model
    ssp = c('S1', 'S2', 'S3', 'S5'), # Shared Socioeconomic Pathway scenarios
    years = as.character(y)) # can only extract data for two decades as a time
}

# convert csv to a TIFF (necessary for spatial cropping)
csv_file <- read.csv('area-dem-2021-11-27-low-res/8GCMs_ensemble_ssp126_2021-2040.csv')
CSVtoTIFF(Longitude = csv_file$Longitude,
          Latitude = csv_file$Latitude,
          Value = csv_file$Tmin01,
          filename = 'test.tif',
          outdir = '../data/weather-data')

terra::plot(terra::rast('../data/weather-data/test.tif'))
