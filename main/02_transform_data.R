
# initialize ----
rm(list = ls())
library(magrittr)

# load data ----
## market-level raw data ----
container_freight_rate_each_route <-
  readRDS(file = "cleaned/container_freight_rate_each_route.rds")
container_shipping_quantity_each_route <-
  readRDS(file = "cleaned/container_shipping_quantity_each_route.rds")
GDP_of_destination_route_level <-
  readRDS(file = "cleaned/GDP_of_destination_route_level.rds")
crude_oil_prices_route_level <-
  readRDS(file = "cleaned/crude_oil_prices_route_level.rds")

# merge market-level dataset -----
## merge p, q, demand shifter, and cost shifter IV ----
route_demand_data <-
  container_shipping_quantity_each_route %>% 
  dplyr::rename(q_TEU1000 = value) %>% 
  dplyr::left_join(
    container_freight_rate_each_route,
    by = c("year" = "year",
           "route" = "route")
    ) %>% 
  dplyr::rename(
    route = route,
    p_dollars_per_TEU = value
    ) %>% 
  ## add IV and demand shifter
  dplyr::left_join(
    GDP_of_destination_route_level,
    by = c("year" = "year",
           "route" = "route")
    ) %>% 
  dplyr::left_join(
    crude_oil_prices_route_level,
    by = c("year" = "year",
           "route" = "route")
    ) %>% 
  dplyr::select(
    - GDP,
    - liner_freight_rate,
    - liner_freight_rate_CPI_adjusted_1995
    ) 
route_demand_data$route_id <-
  dplyr::group_indices(
    route_demand_data,
    route
    )
route_demand_data <- 
  route_demand_data %>% 
  dplyr::filter(q_TEU1000 > 0) %>% 
  dplyr::mutate(
    after_1973_dummy = 
      ifelse(
        year > 1973, 1, 0),
    after_1980_dummy = 
      ifelse(
        year >= 1980, 1, 0),
    after_1984_dummy = 
      ifelse(
        year > 1984, 1, 0)
    ) %>% 
  dplyr::mutate(
    transpacific_or_transatlantic_route_dummy = 
      ifelse(
        route != "europe_to_asia"&
          route != "asia_to_europe", 1, 0)
    ) #%>% 
  # dplyr::mutate(transatlantic_eastbound_dummy =
  #                 ifelse(route == "transatlantic_eastbound",
  #                        1, 0))
route_demand_data <-
  route_demand_data %>% 
  dplyr::mutate(
    market = 
      dplyr::case_when(
        route == "transpacific_eastbound"|
          route == "transpacific_westbound"
        ~ "transpacific",
        route == "transatlantic_eastbound"|
          route == "transatlantic_westbound"
        ~ "transatlantic",
        route == "asia_to_europe"|
          route == "europe_to_asia"
        ~ "europe_and_asia"
        )
    )

# rename market ----
unique(route_demand_data$market)
route_demand_data <-
  route_demand_data %>% 
  dplyr::mutate(
    market = 
      ifelse(market == "europe_and_asia",
             "Asia and Eur",
             ifelse(market == "transatlantic",
                    "Transatlantic",
                    ifelse(market == "transpacific",
                           "Transpacific",
                           "Multiple routes")
             )
      )
  )
route_demand_data <-
  route_demand_data %>% 
  dplyr::mutate(
    route = ifelse(
      route == "europe_to_asia",
      "Eur to Asia",
      ifelse(
        route == "asia_to_europe",
        "Asia to Eur",
        ifelse(
          route == "transatlantic_westbound",
          "Transatlantic WB",
          ifelse(
            route == "transatlantic_eastbound",
            "Transatlantic EB",
            ifelse(
              route == "transpacific_eastbound",
              "Transpacific EB",
              ifelse(
                route == "transpacific_westbound",
                "Transpacific WB",
                0
                )
              )
            )
          )
        )
      )
    )


# save data ----
saveRDS(route_demand_data,
        file = "output/route_demand_data.rds")

