library('purrr')
library('dplyr')
library('tidyr')
library('stringi')

d <-
  # list all files, and import each of the CSVs
  map_dfr(
    list.files(
      'Data/climate-projections/parks-dem',
      full.names = TRUE,
      pattern = '@'),
    \(.fname) {
      readr::read_csv(.fname, col_types = '?') %>%
        # add a column of the filename
        mutate(file = .fname)
    }) %>%
  # extract scenario and year from filename column
  mutate(scenario =
           substr(file,
                  start = stri_locate_last(file, regex = '/')[1] + 1,
                  stop = stri_locate_first(file, regex = '@')[1] - 1),
         year = substr(file,
                       start = stri_locate_first(file, regex = '@')[1] + 1,
                       stop = nchar(file) - nchar('.csv'))) %>%
  # only keep relevant columns
  select(scenario, year, Latitude, Longitude, Elevation, Tave01, Tave02,
         Tave03, Tave04, Tave05, Tave06, Tave07, Tave08, Tave09, Tave10,
         Tave11, Tave12, PPT01, PPT02, PPT03, PPT04, PPT05, PPT06, PPT07,
         PPT08, PPT09, PPT10, PPT11, PPT12) %>%
  # pivot from wide to long format (only one column of precip and temp)
  pivot_longer(-c(scenario, year, Latitude, Longitude, Elevation),
               names_to = 'parameter', values_to = 'value') %>%
  # extract month and out of parameters
  mutate(month = map_chr(parameter,
                         \(.chr) substr(.chr, nchar(.chr) - 1, nchar(.chr))),
         month = as.numeric(month),
         year = as.numeric(year),
         parameter = map_chr(parameter,
                             \(.chr) substr(.chr, 1, nchar(.chr) - 2))) %>%
  # pivot wider to make separate columns of temperature and precipitation
  pivot_wider(names_from = parameter, values_from = value) %>%
  # convert monthly total precip to average daily precip
  mutate(first_day = as.Date(paste(year, month, '01', sep = '-')),
         next_month = if_else(month != '12', as.numeric(month + 1), 1),
         next_year = if_else(month != '12', year, year + 1),
         last_day = as.Date(paste(next_year, next_month, '01', sep = '-')),
         samples = as.numeric((last_day - first_day)),
         avgprecip = PPT / samples) %>% # convert to millimeters per day
  # change to names used in the models
  rename(temperature = Tave,
         tot_precip = PPT,
         latitude = Latitude,
         longitude = Longitude,
         elevation = Elevation) %>%
  # move month column to after the year one
  relocate(month, .after = year)

saveRDS(d, 'Data/climate-projections/monthly-climate-projections.rds')
