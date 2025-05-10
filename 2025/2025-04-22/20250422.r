
library(haven)
library(tidyverse)
library(fs)
library(janitor)


# Crash data (from Harper and Palayew)
dl_path <- withr::local_tempfile(fileext = ".zip")
download.file("https://osf.io/kj7ub/download", dl_path, mode = "wb")
unzip_path <- withr::local_tempdir()
unzip(dl_path, exdir = unzip_path)

dta_files <- fs::dir_ls(unzip_path, glob = "*.dta")

fars <- purrr::map(dta_files, haven::read_dta) |> 
  purrr::list_rbind(names_to = "id") |> 
  dplyr::mutate(id = basename(id))

geog <- readr::read_csv(
  "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
  col_names = c(
    "state_name",
    "state_code",
    "county_code",
    "county_name",
    "FIPS_class_code"
  )
) |> 
  dplyr::mutate(
    state = as.numeric(state_code),
    count = as.numeric(county_code),
    FIPS = paste0(state_code, county_code)
  )


all_accidents <- fars %>%
  dplyr::mutate(
    dplyr::across(
      c("month", "day", "hour", "minute"),
      ~ na_if(.x, 99)
    ),
    year = readr::parse_number(id)
  ) %>%
  dplyr::filter(
    .data$per_typ == 1,
    !is.na(.data$year),
    !is.na(.data$month),
    !is.na(.data$day)
  )


motorcycle_deaths <- all_accidents %>% 
  filter(body_typ == "80") %>% 
  filter(rest_use %in% c("0", "5", "15", "16", "17", "19", "99")) %>% 
  mutate(rest_use = case_when(
    rest_use == "0" ~ "Not Applicable",
    rest_use == "5" ~ "DOT-Compliant Motorcycle Helmet",
    rest_use == "15" ~ "Helmets Used Improperly",
    rest_use == "16" ~ "Helmet, Other than DOT-Compliant Motorcycle Helmet",
    rest_use == "17" ~ "No Helmet",
    rest_use == "19" ~ "Helmet, Unknown if DOT-Compliant",
    rest_use == "99" ~ "Reported as Unknown",
    TRUE ~ NA
  )) %>% 
  filter(death_yr != 0.00) %>% 
  filter(death_yr != 8888)


motorcycle_survive <- all_accidents %>% 
  filter(body_typ == "80") %>% 
  filter(rest_use %in% c("0", "5", "15", "16", "17", "19", "99")) %>% 
  mutate(rest_use = case_when(
    rest_use == "0" ~ "Not Applicable",
    rest_use == "5" ~ "DOT-Compliant Motorcycle Helmet",
    rest_use == "15" ~ "Helmets Used Improperly",
    rest_use == "16" ~ "Helmet, Other than DOT-Compliant Motorcycle Helmet",
    rest_use == "17" ~ "No Helmet",
    rest_use == "19" ~ "Helmet, Unknown if DOT-Compliant",
    rest_use == "99" ~ "Reported as Unknown",
    TRUE ~ NA
  )) %>% 
  filter(death_yr == 0.00 | death_yr == 8888)

motorcycle_survive %>% 
  ggplot(aes(rest_use)) +
  geom_bar()

motorcycle_deaths %>% 
  ggplot(aes(rest_use)) +
  geom_bar()
