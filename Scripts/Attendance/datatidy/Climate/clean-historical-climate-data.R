library('purrr')
library('dplyr')
library('tidyr')
library('stringi')
library('lubridate')

#' *needs comments*
historicaldata <-
  map_dfr(
    list.files('Data/Attendance/Climate/historical-climate', full.names = TRUE),
    \(.fname) {
      readr::read_csv(.fname, col_types = '?') %>%
        mutate(file = .fname)
    }) %>%
  mutate(year = substr(file,
                       start = stri_locate_first(file, regex = 'dem_')[1] + 4,
                       stop = nchar(file) - nchar('.csv'))) %>%
  select(year, Latitude, Longitude, Elevation, Tave01, Tave02, Tave03,
         Tave04, Tave05, Tave06, Tave07, Tave08, Tave09, Tave10, Tave11, Tave12,
         PPT01, PPT02, PPT03, PPT04, PPT05, PPT06, PPT07, PPT08, PPT09, PPT10,
         PPT11, PPT12) %>%
  pivot_longer(-c(year, Latitude, Longitude, Elevation),
               names_to = 'parameter', values_to = 'value') %>%
  mutate(month = map_chr(parameter,
                         \(.chr) substr(.chr, nchar(.chr) - 1, nchar(.chr))),
         dec_date = decimal_date(date(paste(year, month, '15', sep = '-'))),
         month = as.numeric(month),
         year = as.numeric(year),
         parameter = map_chr(parameter,
                             \(.chr) substr(.chr, 1, nchar(.chr) - 2))) %>%
  pivot_wider(names_from = parameter, values_from = value) %>%
  # convert monthly total precip to average daily precip
  mutate(first_day = as.Date(paste(year, month, '01', sep = '-')),
         next_month = if_else(month != '12', as.numeric(month + 1), 1),
         next_year = if_else(month != '12', year, year + 1),
         last_day = as.Date(paste(next_year, next_month, '01', sep = '-')),
         samples = as.numeric((last_day - first_day)),
         avgprecip = PPT / samples) %>% # convert to millimeters per day
  # drop temporary columns
  select(-c(first_day, next_month, next_year, last_day, samples, PPT)) %>%
  # change to names used in the models
  rename(avgtemp = Tave,
         latitude = Latitude,
         longitude = Longitude,
         elevation = Elevation) %>%
  relocate(c(month, dec_date), .after = year)

saveRDS(historicaldata, 'Data/Attendance/Climate/historical-climate-data.rds')
