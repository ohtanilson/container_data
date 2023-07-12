# import package ------
rm(list = ls())
library(ggplot2)

traffic_amount_container_transportation_cost_and_profitability <-
  readxl::read_excel(
    "input/traffic_amount_container_transportation_cost_and_profitability.xlsx") %>%
  tidyr::tibble()
traffic_amount_the_container_crisis_1982 <-
  readxl::read_excel(
    "input/traffic_amount_the_container_crisis_1982.xlsx") %>%
  tidyr::tibble()
traffic_amount_world_sea_trades <-
  readxl::read_excel(
    "input/traffic_amount_world_sea_trades.xlsx") %>%
  tidyr::tibble()
traffic_amount_review_of_maritime_transport <-
  readxl::read_excel(
    "input/traffic_amount_review_of_maritime_transport.xlsx") %>%
  tidyr::tibble()
containerization_international_1973 <-
  readxl::read_excel(
    "input/containerization_international_1973.xlsx") %>%
  tidyr::tibble()

# set constant ----

conversion_ratio_tons_to_TEU = 
  20 # 20 tons = 1 TEU
conversion_ratio_million_tons_to_1000TEU =
  1000000/
  (conversion_ratio_tons_to_TEU*1000)
# construct shipping quantity dataset ----
ratio_trade_1985 <-
  traffic_amount_world_sea_trades %>%
  dplyr::filter(
    year == 1985) %>%
  dplyr::mutate(
    ratio_transpacific_eastbound_to_westbound =
      transpacific_eastbound/
      (transpacific_eastbound +
         transpacific_westbound),
    ratio_transatlantic_eastbound_to_westbound =
      transatlantic_eastbound/
      (transatlantic_eastbound +
         transatlantic_westbound),
    ratio_europe_to_asia_to_asia_to_europe =
      europe_to_asia/
      (europe_to_asia + 
         asia_to_europe)
    )

## create data between 1966 and 1973 ----------
traffic_amount_the_container_crisis_1982 <-
  traffic_amount_the_container_crisis_1982 %>%
  dplyr::filter(
    is.na(far_east) == 0) %>%
  # rescale measure from million tons to 1000TEU (24 tons = 1 TEU)
  dplyr::mutate(
    unit = 
      "1000TEU",
    north_america = 
      north_america*
      conversion_ratio_million_tons_to_1000TEU,
    west_europe = 
      west_europe*
      conversion_ratio_million_tons_to_1000TEU,
    far_east =
      far_east*
      conversion_ratio_million_tons_to_1000TEU
    ) %>%
  dplyr::select(
    - Source, 
    - unit
    )

containerization_international_1973_before_1970 <-
  containerization_international_1973 %>%
  dplyr::filter(
    built_year <= 73) %>%
  dplyr::mutate(
    route =
      dplyr::case_when(
        service == "Europe/ECNA" ~ "transatlantic",
        service == "Europe/USEC" ~ "transatlantic",
        service == "Europe/USGC" ~ "transatlantic",
        service == "Europe/WCNA" ~ "transatlantic",
        service == "Europe/WCNA" ~ "transatlantic",
        service == "USEC/Europe" ~ "transatlantic",
        service == "USEC/Med." ~ "transatlantic",
        service == "USGC/Europe" ~ "transatlantic",
        service == "USPC/Europe" ~ "transatlantic",
        service == "USSAC/Europe" ~ "transatlantic",
        service == "USSAX/Europe" ~ "transatlantic",
        service == "Med./ECNA" ~ "transatlantic",
        service == "Europe/Canada" ~ "transatlantic",
        service == "UK/Canada" ~ "transatlantic",
        service == "Europe/Canada" ~ "transatlantic",
        # transpacific
        service == "Japan/ECNA" ~ "transpacific",
        service == "EWCUS/JFE" ~ "transpacific",
        service == "PNW/Japan" ~ "transpacific",
        service == "PNW/JFE" ~ "transpacific",
        service == "PNW/SEA" ~ "transpacific",
        service == "PSW/Japan" ~ "transpacific",
        service == "PSW/JFE" ~ "transpacific",
        service == "PSW/SEA" ~ "transpacific",
        service == "USEC/JFE" ~ "transpacific",
        service == "USPC/JFE" ~ "transpacific",
        service == "WCNA/JFE" ~ "transpacific",
        service == "PSW/Hawaii" ~ "transpacific",
        # asia_to_europe
        service == "Europe/JFE" ~ "asia_and_europe",
        service == "Med./EWCUS/JFE" ~ "asia_and_europe",
        service == "Med./JFE" ~ "asia_and_europe"
        )
  )
first_asia_to_europe_route <-
  containerization_international_1973_before_1970 %>%
  dplyr::filter(
    route ==
      "asia_and_europe") %>%
  dplyr::arrange(
    built_year
    )

containerization_international_1973_before_1970 <-
  containerization_international_1973_before_1970 %>%
  dplyr::group_by(
    built_year,
    route) %>%
  dplyr::summarise(
    route_level_capacity_TEU =
      sum(capacity_TEU)) %>%
  dplyr::ungroup()


year_list <-
  c(62:73) %>%
  tibble::as.tibble()
colnames(year_list) <- 
  "built_year"
containerization_international_1973_before_1970_transatlantic <-
  containerization_international_1973_before_1970 %>%
  dplyr::filter(
    route == 
      "transatlantic"
    ) %>%
  dplyr::right_join(
    year_list,
    by =
      c(
        "built_year" = 
          "built_year"
        )
    ) %>%
  dplyr::arrange(
    built_year) %>%
  dplyr::mutate(
    route_level_capacity_TEU =
      ifelse(
        is.na(route_level_capacity_TEU) ==
          1,
        0,
        route_level_capacity_TEU
        )
    ) %>%
  dplyr::mutate(
    route = 
      "transatlantic"
    ) %>%
  dplyr::mutate(
    cumulative_route_level_capacity_TEU =
      cumsum(
        route_level_capacity_TEU)
    )

containerization_international_1973_before_1970_transpacific <-
  containerization_international_1973_before_1970 %>%
  dplyr::filter(
    route == 
      "transpacific"
    ) %>%
  dplyr::right_join(
    year_list,
    by = 
      c(
        "built_year" =
          "built_year"
        )
    ) %>%
  dplyr::arrange(
    built_year) %>%
  dplyr::mutate(
    route_level_capacity_TEU = 
      ifelse(
        is.na(
          route_level_capacity_TEU) == 
          1,
        0,
        route_level_capacity_TEU
        )
    ) %>%
  dplyr::mutate(
    route =
      "transpacific"
    ) %>%
  dplyr::mutate(
    cumulative_route_level_capacity_TEU =
      cumsum(
        route_level_capacity_TEU
        )
    )

containerization_international_1973_before_1970_asia_and_europe <-
  containerization_international_1973_before_1970 %>%
  dplyr::filter(
    route == "asia_and_europe") %>%
  dplyr::right_join(
    year_list,
    by = 
      c(
        "built_year" =
          "built_year"
        )
    ) %>%
  dplyr::arrange(
    built_year
    ) %>%
  dplyr::mutate(
    route_level_capacity_TEU =
      ifelse(
        is.na(route_level_capacity_TEU) == 1,
        0,
        route_level_capacity_TEU
        )
    ) %>%
  dplyr::mutate(
    route = 
      "asia_and_europe"
    ) %>%
  dplyr::mutate(
    cumulative_route_level_capacity_TEU =
      cumsum(
        route_level_capacity_TEU
        )
    )

## merge datasets
containerization_international_1973_before_1970 <-
  rbind(
    containerization_international_1973_before_1970_transatlantic,
    containerization_international_1973_before_1970_transpacific,
    containerization_international_1973_before_1970_asia_and_europe) %>%
  dplyr::filter(
    built_year >= 66) %>%
  dplyr::mutate(
    capacity_1000TEU = 
      cumulative_route_level_capacity_TEU/1000) %>%
  dplyr::group_by(
    built_year) %>%
  dplyr::mutate(
    year_level_total_capacity_1000TEU =
      sum(capacity_1000TEU)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    total_capacity_1000TEU_share =
      capacity_1000TEU/
      year_level_total_capacity_1000TEU) %>%
  dplyr::mutate(
    built_year = 
      built_year +
      1900
    ) %>%
  dplyr::select(
    built_year,
    route,
    capacity_1000TEU
    ) %>%
  tidyr::pivot_wider(
    names_from = 
      route,
    values_from = 
      capacity_1000TEU
    )

traffic_amount <-
  containerization_international_1973_before_1970 %>%
  dplyr::left_join(
    traffic_amount_the_container_crisis_1982,
    by = 
      c(
        "built_year" =
          "year"
        )
    ) %>%
  # assume that eastbound and westbound has equally shipping quantity (1/2)
  dplyr::mutate(
    transpacific_eastbound =
      north_america*
      (transpacific/
         (transatlantic +
            transpacific))*
      ratio_trade_1985$ratio_transpacific_eastbound_to_westbound,
    transpacific_westbound =
      north_america*
      (transpacific/
         (transatlantic + 
            transpacific))*
      (1 - ratio_trade_1985$ratio_transpacific_eastbound_to_westbound),
    transatlantic_eastbound =
      west_europe*
      (transatlantic/
         (transatlantic + 
            transpacific))*
      (ratio_trade_1985$ratio_transatlantic_eastbound_to_westbound),
    transatlantic_westbound =
      west_europe*
      (transatlantic/
         (transatlantic +
            transpacific))*
      (1 - ratio_trade_1985$ratio_transatlantic_eastbound_to_westbound),
    europe_to_asia =
      far_east*
      (asia_and_europe/
         (asia_and_europe +
            transpacific))*
      (ratio_trade_1985$ratio_europe_to_asia_to_asia_to_europe),
    asia_to_europe =
      far_east*
      (asia_and_europe/
         (asia_and_europe + 
            transpacific))*
      (1 - ratio_trade_1985$ratio_europe_to_asia_to_asia_to_europe)
  ) %>%
  tidyr::replace_na(
    list(europe_to_asia = 0)
    )
traffic_amount$transpacific_eastbound[1:4] <-
  traffic_amount$transpacific[1:4]/
  traffic_amount$transpacific[5]*
  traffic_amount$transpacific_eastbound[5]
traffic_amount$transpacific_westbound[1:4] <-
  traffic_amount$transpacific[1:4]/
  traffic_amount$transpacific[5]*
  traffic_amount$transpacific_westbound[5]
traffic_amount$transatlantic_eastbound[1:4] <-
  traffic_amount$transatlantic[1:4]/
  traffic_amount$transatlantic[5]*
  traffic_amount$transatlantic_eastbound[5]
traffic_amount$transatlantic_westbound[1:4] <-
  traffic_amount$transatlantic[1:4]/
  traffic_amount$transatlantic[5]*
  traffic_amount$transatlantic_westbound[5]
traffic_amount$asia_to_europe[1:4] <-
  traffic_amount$asia_and_europe[1:4]/
  traffic_amount$asia_and_europe[5]*
  traffic_amount$asia_to_europe[5]
traffic_amount$europe_to_asia[1:4] <-
  traffic_amount$asia_and_europe[1:4]/
  traffic_amount$asia_and_europe[5]*
  traffic_amount$europe_to_asia[5]
# 1971 - 1973
traffic_amount$transpacific_eastbound[6:8] <-
  traffic_amount$transpacific[6:8]/
  traffic_amount$transpacific[5]*
  traffic_amount$transpacific_eastbound[5]
traffic_amount$transpacific_westbound[6:8] <-
  traffic_amount$transpacific[6:8]/
  traffic_amount$transpacific[5]*
  traffic_amount$transpacific_westbound[5]
traffic_amount$transatlantic_eastbound[6:8] <-
  traffic_amount$transatlantic[6:8]/
  traffic_amount$transatlantic[5]*
  traffic_amount$transatlantic_eastbound[5]
traffic_amount$transatlantic_westbound[6:8] <-
  traffic_amount$transatlantic[6:8]/
  traffic_amount$transatlantic[5]*
  traffic_amount$transatlantic_westbound[5]
traffic_amount$asia_to_europe[6:8] <-
  traffic_amount$asia_and_europe[6:8]/
  traffic_amount$asia_and_europe[5]*
  traffic_amount$asia_to_europe[5]
traffic_amount$europe_to_asia[6:8] <-
  traffic_amount$asia_and_europe[6:8]/
  traffic_amount$asia_and_europe[5]*
  traffic_amount$europe_to_asia[5]

traffic_amount_1966_1973 <-
  traffic_amount %>%
  dplyr::rename(
    year = built_year) %>%
  dplyr::select(
    year,
    transpacific_eastbound,
    transpacific_westbound,
    transatlantic_eastbound,
    transatlantic_westbound,
    europe_to_asia,
    asia_to_europe
    )


## create data between 1975 and 1989 ----------
traffic_amount_container_transportation_cost_and_profitability <-
  traffic_amount_container_transportation_cost_and_profitability %>%
  dplyr::filter(
    is.na(asia_and_europe) == 0) %>%
  dplyr::mutate(
    transpacific_eastbound =
      transpacific*
      ratio_trade_1985$ratio_transpacific_eastbound_to_westbound,
    transpacific_westbound =
      transpacific*
      (1 - ratio_trade_1985$ratio_transpacific_eastbound_to_westbound),
    transatlantic_eastbound =
      transatlantic*
      (ratio_trade_1985$ratio_transatlantic_eastbound_to_westbound),
    transatlantic_westbound =
      transatlantic*
      (1 - ratio_trade_1985$ratio_transatlantic_eastbound_to_westbound),
    europe_to_asia =
      asia_and_europe*
      (ratio_trade_1985$ratio_europe_to_asia_to_asia_to_europe),
    asia_to_europe =
      asia_and_europe*
      (1 - ratio_trade_1985$ratio_europe_to_asia_to_asia_to_europe)
  ) %>%
  dplyr::select(
    - Source,
    - unit,
    - transpacific,
    - transatlantic,
    - asia_and_europe
    )

### linearly interpolate missing years ----
year_list_1975_1990 <-
  c(1975:1990) %>%
  tibble::as.tibble() %>%
  dplyr::rename(
    year = value)
traffic_amount_container_transportation_cost_and_profitability <-
  year_list_1975_1990 %>%
  dplyr::left_join(
    traffic_amount_container_transportation_cost_and_profitability,
    by = 
      c(
        "year" =
          "year"
        )
    )
traffic_amount_container_transportation_cost_and_profitability[,2:7] <-
  FreqProf::approxm(
    traffic_amount_container_transportation_cost_and_profitability[,2:7],
    length(
      traffic_amount_container_transportation_cost_and_profitability$year
      )
    )

## create data between 1985 and 1997 ----------
traffic_amount_world_sea_trades <-
  traffic_amount_world_sea_trades %>%
  dplyr::filter(
    is.na(europe_to_asia) == 0) %>%
  dplyr::select(
    - Source,
    - unit
    )
## conversion rate from traffic_amount_container_transportation_cost_and_profitability to traffic_amount_world_sea_trades
traffic_amount_world_sea_trades_1987 <-
  traffic_amount_world_sea_trades %>%
  dplyr::filter(year == 1987)
traffic_amount_container_transportation_cost_and_profitability_1987 <-
  traffic_amount_container_transportation_cost_and_profitability %>%
  dplyr::filter(year == 1987)
# set traffic_amount_world_sea_trades_1987 as a key data
convertion_rate_1987 <-
  traffic_amount_world_sea_trades_1987/
  traffic_amount_container_transportation_cost_and_profitability_1987
conversion_rate_list <-
  rbind(
    convertion_rate_1987,
    convertion_rate_1987,
    convertion_rate_1987,
    convertion_rate_1987,
    convertion_rate_1987,
    convertion_rate_1987,
    convertion_rate_1987,
    convertion_rate_1987,
    convertion_rate_1987,
    convertion_rate_1987,
    convertion_rate_1987,
    convertion_rate_1987
    )

traffic_amount_container_transportation_cost_and_profitability[1:12,2:7] <-
  traffic_amount_container_transportation_cost_and_profitability[1:12,2:7]*
  conversion_rate_list[1:12,2:7]

traffic_amount_container_transportation_cost_and_profitability <-
  traffic_amount_container_transportation_cost_and_profitability %>%
  dplyr::filter(year < 1987) # drop year duplicated across datasets

### interpolate missing 1974 ----
traffic_amount_1966_1984 <-
  rbind(
    traffic_amount_1966_1973,
    traffic_amount_container_transportation_cost_and_profitability
    )
year_list_1973_1975 <-
  c(1973:1975) %>%
  tibble::as.tibble() %>%
  dplyr::rename(year = value)
traffic_amount_1973_1975 <-
  year_list_1973_1975 %>%
  dplyr::left_join(
    traffic_amount_1966_1984,
    by = c("year" = "year")
  )
traffic_amount_1973_1975[,2:7] <-
  FreqProf::approxm(
    traffic_amount_1973_1975[,2:7],
    length(
      traffic_amount_1973_1975$year
      )
    ) 
traffic_amount_1974 <-
  traffic_amount_1973_1975 %>% 
  dplyr::filter(year == 1974)
traffic_amount_1966_1997 <-
  rbind(
    traffic_amount_1966_1973,
    traffic_amount_1974,
    traffic_amount_container_transportation_cost_and_profitability,
    traffic_amount_world_sea_trades %>% 
      dplyr::filter(year >= 1987))

## create data between 1995 and 2007 ----------
traffic_amount_review_of_maritime_transport <-
  traffic_amount_review_of_maritime_transport %>%
  dplyr::filter(
    is.na(europe_to_asia) == 0
    ) %>%
  dplyr::select(
    - Source,
    - unit
    )
traffic_amount_review_of_maritime_transport_in_1995 <-
  traffic_amount_review_of_maritime_transport %>%
  dplyr::filter(year == 1995)
traffic_amount_1966_1997_in_1995 <-
  traffic_amount_1966_1997 %>%
  dplyr::filter(year == 1995)
conversion_ratio_in_1995 <-
  traffic_amount_review_of_maritime_transport_in_1995/
  traffic_amount_1966_1997_in_1995
traffic_amount_1966_1997_converted <-
  traffic_amount_1966_1997 %>%
  dplyr::mutate(
    transpacific_eastbound =
      transpacific_eastbound*
      conversion_ratio_in_1995$transpacific_eastbound,
    transpacific_westbound =
      transpacific_westbound*
      conversion_ratio_in_1995$transpacific_westbound,
    transatlantic_eastbound =
      transatlantic_eastbound*
      conversion_ratio_in_1995$transatlantic_eastbound,
    transatlantic_westbound =
      transatlantic_westbound*
      conversion_ratio_in_1995$transatlantic_westbound,
    europe_to_asia =
      europe_to_asia*
      conversion_ratio_in_1995$europe_to_asia,
    asia_to_europe =
      asia_to_europe*
      conversion_ratio_in_1995$asia_to_europe
  ) %>%
  dplyr::filter(
    year < 1995
    ) # drop years duplicated across datasets
## merge quantity data  ----

d <-
  rbind(
    traffic_amount_1966_1997_converted,
    traffic_amount_review_of_maritime_transport) %>%
  dplyr::arrange(year) %>%
  tidyr::gather(
    key = "route",
    value = "value",
    - year)
x <- ggplot(d,
            aes(x = year, y = value)) +
  geom_line(aes(color = route), alpha = 0.6) +
  geom_point(aes(shape = route, color = route)) +
  #geom_point(aes(shape = route, color = route)) +
  theme_minimal() +
  theme(legend.position = 'top') +
  ggtitle("Container shipping quantity (1000 TEU)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("year") + ylab("Container shipping quantity (1000 TEU)") +
  #expand_limits(y=0, x=2000) +
  geom_vline(xintercept = 1966, linetype = "longdash") +
  annotate("text", x = 1970, y = 10000, label = "Containerization\n international 1973") +
  geom_vline(xintercept = 1975, linetype = "longdash") +
  annotate("text", x = 1980, y = 16000,
           label = "Container transportation cost\n and profitability") +
  geom_vline(xintercept = 1985, linetype = "longdash") +
  annotate("text", x = 1990, y = 18000, label = "World sea trades") +
  geom_vline(xintercept = 1995, linetype = "longdash") +
  annotate("text", x = 2003, y = 20000, label = "Review of \nMaritime Transport") +
  labs(shape = "Route", color = "Route")
x

container_shipping_quantity_each_route <-
  d
# drop the pre-period before global containerization
container_shipping_quantity_each_route <-
  container_shipping_quantity_each_route %>% 
  dplyr::filter(
    (route == "transpacific_eastbound"&year >= 1967)|
      (route == "transpacific_westbound"&year >= 1967)|
      (route == "transatlantic_eastbound"&year >= 1966)|
      (route == "transatlantic_westbound"&year >= 1966)|
      (route == "europe_to_asia"&year >= 1971)|
      (route == "asia_to_europe"&year >= 1971))

# save preprocessed data ----
saveRDS(
  container_shipping_quantity_each_route,
  file = 
    "cleaned/container_shipping_quantity_each_route.rds"
  )
write.csv(
  container_shipping_quantity_each_route, 
  "cleaned/container_shipping_quantity_each_route.rds"
)