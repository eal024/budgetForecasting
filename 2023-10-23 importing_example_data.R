

Sys.setlocale("LC_CTYPE")
library(openxlsx)
library(httr)
library(tidyverse)


## Importing test data from Nav.no ()

# 1) Creating new enviroment for organizing data

nav_data <- rlang::new_environment()


# Data at nav.no is given as excel-data. Saving the excel-link at the web and downloading the data
# directly to R
nav_data$url1 <- "https://www.nav.no/_/attachment/download/a83b738d-711c-40b7-a5c0-c0c49ea9d953:89d3a5cbf42beadfd19654dbe2f237b2596977a1/PST311%20Nye%20Mottakere%20av%20uf%C3%B8retrygd.%20Alder.%20Kj%C3%B8nn_2023_08"
nav_data$url2 <- "https://www.nav.no/_/attachment/download/edc9253b-ee64-4f97-9ecc-10b96b7d2976:9bcb65d662e43cd867e31c8cd98911954de6acd7/PST311_Nye_Mottakere_av_uf%C3%B8retrygd._Alder._Kj%C3%B8nn._2022_12"
nav_data$url3 <- "https://www.nav.no/_/attachment/download/10f21377-9951-4741-8f0e-ac4de460300a:dbcc8da57bf68539a38ef280ccf1cd0206b25fa8/PST311_Nye_Mottakere_av_uf%C3%B8retrygd._Alder._Kj%C3%B8nn._2021_12"

# Reading all the data 
nav_data$data_list <- lapply( 
    list( nav_data$url1, nav_data$url2, nav_data$url3),
    function(x) openxlsx::read.xlsx( paste0(x, ".xlsx"), sheet = 2, startRow = 8)) |>
    set_names( c(2023, 2022, 2021)
               )
# 
nav_data$fun_wrangle <- function(df){
    Sys.setlocale("LC_CTYPE")
    df |> 
        rename( kjonn = X1, alder = X2) |> 
        fill( kjonn, .direction = "down") |> 
        mutate( kjonn = ifelse( is.na(kjonn), "samlet", kjonn),
                alder = str_remove(alder, "år") |> str_trim(side = "both")
        ) |> 
        filter( !is.na(alder), !str_detect(alder, "alt") ) |>
        pivot_longer( -c(kjonn,alder)) 
}


## Data data in long format
nav_data$df <- map( nav_data$data_list, function(x) nav_data$fun_wrangle(x)) |> 
    bind_rows( .id = "id") 

# Give english name to month, based on Norwegian names
nav_data$df_month <- 
    tibble( name = unique(nav_data$df$name),
        month = 1:12,
        mnd = month(month, label = T)
        )


# Last cleaning process
nav_data$df1 <- nav_data$df |> 
    left_join(nav_data$df_month, join_by(name) ) |> 
    mutate( date = paste0(id,"-",mnd, "-01") |> ymd(),
            value = as.numeric(value)
            ) |> 
    select( date, sex = kjonn, age = alder, value) |> 
    arrange( age, sex, date)


