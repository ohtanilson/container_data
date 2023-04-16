# import package ------
rm(list = ls())
library(ggplot2)
library(acs) # for getting CPI

# import raw freight data ----

freight_rate_global_container_markets <-
  readxl::read_excel(
    "input/freight_rate_global_container_markets.xlsx") %>%
  tidyr::tibble()
freight_rate_main_routes_review_of_maritime_transport <-
  readxl::read_excel(
    "input/freight_rate_main_routes_review_of_maritime_transport.xlsx") %>%
  tidyr::tibble()
freight_rate_global_review_of_maritime_transport <-
  readxl::read_excel(
    "input/freight_rate_global_review_of_maritime_transport.xlsx") %>%
  tidyr::tibble()
freight_rate_issues_of_our_ocean_shipping <-
  readxl::read_excel(
    "input/freight_rate_issues_of_our_ocean_shipping.xlsx") %>%
  tidyr::tibble()
freight_revenue_current_status_of_marine_transportation <-
  readxl::read_excel(
    "input/freight_revenue_current_status_of_marine_transportation.xlsx") %>%
  tidyr::tibble()
freight_rate_EUROFE19781989 <-
  readxl::read_excel(
    "input/EUROFE19781989.xlsx") %>%
  tidyr::tibble()
traffic_amount_current_status_of_marine_transportation <-
  readxl::read_excel(
    "input/traffic_amount_current_status_of_marine_transportation.xlsx") %>%
  tidyr::tibble()
freight_rate_current_status_of_marine_transportation <-
  freight_revenue_current_status_of_marine_transportation[,1:5]

conversion_ratio_tons_to_TEU <-
  20 # 20 tons = 1 TEU
# compute freight rate from revenue and quantity
freight_rate_current_status_of_marine_transportation <- 
  cbind(
    freight_rate_current_status_of_marine_transportation,
    # 291.84 yen = 1 U.S. dollars in 1974
    # 1mil yen per 1000 metric tons => 1000 yen per 1 metric ton
    ((freight_revenue_current_status_of_marine_transportation[,6:11]*1000000/
        (traffic_amount_current_status_of_marine_transportation[,6:11]*1000))*
       conversion_ratio_tons_to_TEU/291.84)
  ) %>% 
  # 1dollars per TEU
  dplyr::mutate(
    unit = "dollars per TEU") %>% 
  dplyr::select(
    year,
    `transpacific_eastbound(current_dollars)`,
    `transpacific_westbound(current_dollars)`,
    `transatlantic_eastbound(current_dollars)`,
    `transatlantic_westbound(current_dollars)`,
    `europe_to_asia(current_dollars)`,
    `asia_to_europe(current_dollars)`
    ) %>% 
  dplyr::filter(year >= 1973)
freight_rate_global_container_markets <-
  freight_rate_global_container_markets %>%
  dplyr::filter(is.na(Source) == 0) %>%
  dplyr::select(
    year,
    `transpacific_eastbound(current_dollars)`,
    `transpacific_westbound(current_dollars)`,
    `transatlantic_eastbound(current_dollars)`,
    `transatlantic_westbound(current_dollars)`,
    `europe_to_asia(current_dollars)`,
    `asia_to_europe(current_dollars)`
    )
freight_rate_main_routes_review_of_maritime_transport <-
  freight_rate_main_routes_review_of_maritime_transport %>%
  dplyr::filter(is.na(Source) == 0) %>%
  dplyr::select(
    year,
    `transpacific_eastbound(current_dollars)`,
    `transpacific_westbound(current_dollars)`,
    `transatlantic_eastbound(current_dollars)`,
    `transatlantic_westbound(current_dollars)`,
    `europe_to_asia(current_dollars)`,
    `asia_to_europe(current_dollars)`
    )
colnames_data_set <-
  c(
    "year",
    "transpacific_eastbound",
    "transpacific_westbound",
    "transatlantic_eastbound",
    "transatlantic_westbound",
    "europe_to_asia",
    "asia_to_europe"
    )
colnames(
  freight_rate_main_routes_review_of_maritime_transport
  ) <-
  colnames_data_set
colnames(
  freight_rate_global_container_markets
  ) <-
  colnames_data_set
colnames(
  freight_rate_current_status_of_marine_transportation
  ) <-
  colnames_data_set
## import CPI data ----
data(cpi)
cpi_based_on_1995 <-
  cpi/cpi[83]
cpi_1965_2010_based_on_1995 <-
  cpi_based_on_1995[53:98]
cpi_1965_2010_based_on_1995 <-
  cbind(
    as.numeric(1965:2010),
    cpi_1965_2010_based_on_1995
    ) %>%
  tibble::as_tibble()
colnames(
  cpi_1965_2010_based_on_1995
  ) <-
  c(
    "year",
    "cpi_based_on_1995"
    )
# construct freight rate dataset----

## construct global container freight rate data ----
container_freight_rate <-
  freight_rate_global_review_of_maritime_transport %>%
  dplyr::filter(
    is.na(`global container index(1995=100)`) == 0 |
      is.na(`global container index(1990=100)`) == 0)
container_freight_rate_1995 <-
  container_freight_rate %>%
  dplyr::filter(year == 1995)
conversion_ratio_container_freight_rate_1995 <-
  as.numeric(
    container_freight_rate_1995[5]/
      container_freight_rate_1995[6]
    )
container_freight_rate <-
  container_freight_rate %>%
  dplyr::mutate(
    merged_container_freight_rate =
      ifelse(
        year >= 1997,
        `global container index(1995=100)`,
        `global container index(1990=100)`*
          conversion_ratio_container_freight_rate_1995
        )
    ) %>%
  dplyr::select(
    year,
    merged_container_freight_rate
    )

## construct global liner freight rate data ----
freight_rate_global_review_of_maritime_transport_1965 <-
  freight_rate_global_review_of_maritime_transport %>%
  dplyr::filter(year == 1965)
freight_rate_global_review_of_maritime_transport_1980 <-
  freight_rate_global_review_of_maritime_transport %>%
  dplyr::filter(year == 1980)
freight_rate_global_review_of_maritime_transport_1985 <-
  freight_rate_global_review_of_maritime_transport %>%
  dplyr::filter(year == 1985)
freight_rate_global_review_of_maritime_transport_1990 <-
  freight_rate_global_review_of_maritime_transport %>%
  dplyr::filter(year == 1990)
freight_rate_global_review_of_maritime_transport_1995 <-
  freight_rate_global_review_of_maritime_transport %>%
  dplyr::filter(year == 1995)

# different series of RMT have different benchmark year, so I calculate
# conversion rate based on matched years.
conversion_rate_in_1965 <-
  as.numeric(
    freight_rate_global_review_of_maritime_transport_1965[11]/
      freight_rate_global_review_of_maritime_transport_1965[12]
    )
conversion_rate_in_1980 <-
  as.numeric(
    freight_rate_global_review_of_maritime_transport_1980[10]/
      freight_rate_global_review_of_maritime_transport_1980[11]
    )
conversion_rate_in_1985 <-
  as.numeric(
    freight_rate_global_review_of_maritime_transport_1985[9]/
      freight_rate_global_review_of_maritime_transport_1985[10]
    )
conversion_rate_in_1990 <-
  as.numeric(
    freight_rate_global_review_of_maritime_transport_1990[8]/
      freight_rate_global_review_of_maritime_transport_1990[9]
    )
conversion_rate_in_1995 <-
  as.numeric(
    freight_rate_global_review_of_maritime_transport_1995[7]/
      freight_rate_global_review_of_maritime_transport_1995[8]
    )
freight_rate_global_review_of_maritime_transport <-
  freight_rate_global_review_of_maritime_transport %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(
    global_liner_freight_rate =
      dplyr::case_when(
        year < 1968 ~
          `global rate(1959=100)`*
          conversion_rate_in_1965*
          conversion_rate_in_1980*
          conversion_rate_in_1985*
          conversion_rate_in_1990*
          conversion_rate_in_1995,
        year >= 1968 && year < 1983 ~
          `global rate(1965=100)`*
          conversion_rate_in_1980*
          conversion_rate_in_1985*
          conversion_rate_in_1990*
          conversion_rate_in_1995,
        year >= 1983 && year < 1990 ~
          `global rate(1980=100)`*
          conversion_rate_in_1985*
          conversion_rate_in_1990*
          conversion_rate_in_1995,
        year >= 1990 && year < 1994 ~
          `global rate(1985=100)`*
          conversion_rate_in_1990*
          conversion_rate_in_1995,
        year >= 1994 && year < 1997 ~
          `global rate(1990=100)`*
          conversion_rate_in_1995,
        year >= 1997 ~
          `global rate(1995=100)`
        )
  ) %>%
  dplyr::select(
    year,
    global_liner_freight_rate
    )

## merge liner and container freight rate data ----
### not deflated price based on year 1995=100 ----
merged_freight_rate_global_review_of_maritime_transport <-
  freight_rate_global_review_of_maritime_transport %>%
  dplyr::left_join(
    container_freight_rate,
    by = 
      c(
        "year" = 
          "year"
        )
    ) %>%
  tidyr::gather(
    "route",
    "value",
    - year
    ) %>%
  dplyr::mutate(
    route =
      ifelse(
        route == 
          "global_liner_freight_rate",
        "liner freight rate",
        "container freight rate"
        )
    )
### deflated price based on year 1995=100 ----
merged_freight_rate_global_review_of_maritime_transport_deflated <-
  freight_rate_global_review_of_maritime_transport %>%
  dplyr::left_join(
    container_freight_rate,
    by =
      c(
        "year" = 
          "year"
        )
    ) %>%
  dplyr::left_join(
    cpi_1965_2010_based_on_1995,
    by = 
      c(
        "year" = 
          "year"
        )
    ) %>%
  dplyr::mutate(
    global_liner_freight_rate_deflated =
      global_liner_freight_rate/
      cpi_based_on_1995,
    global_merged_container_freight_rate_deflated =
      merged_container_freight_rate/
      cpi_based_on_1995
  ) %>%
  dplyr::select(
    - global_liner_freight_rate,
    - merged_container_freight_rate,
    - cpi_based_on_1995
    ) %>%
  tidyr::gather(
    "route",
    "value",
    - year
    ) %>%
  dplyr::mutate(
    route =
      ifelse(
        route ==
          "global_liner_freight_rate_deflated",
        "liner freight rate (CPI adjusted to 1995)",
        "container freight rate (CPI adjusted to 1995)"
        )
    )
d <-
  rbind(
    merged_freight_rate_global_review_of_maritime_transport,
    merged_freight_rate_global_review_of_maritime_transport_deflated
    )
d <-
  merged_freight_rate_global_review_of_maritime_transport_deflated
x <-
  ggplot(d,
         aes(x = year,
             y = value)) +
  geom_line(aes(linetype = route)) +
  geom_point(aes(shape = route)) +
  ylab("liner freight rate (year 1995 = 100)") +
  geom_vline(xintercept = 1990, 
             linetype = "longdash") +
  annotate("text", x = 1990, y = 40,
           label = "container freight rate opened") +
  theme_minimal() +
  theme(legend.position = 'top')
figure_name <- "figuretable/liner_freight_rate.png"
ggsave(filename = figure_name,
       plot = x,
       device = "png",
       width = 10,
       height = 7)

## create 1994-2009 route-level data between ----------
freight_rate_main_routes_review_of_maritime_transport <-
  freight_rate_main_routes_review_of_maritime_transport %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    transpacific_eastbound =
      mean(transpacific_eastbound,
           na.rm = T),
    transpacific_westbound =
      mean(transpacific_westbound,
           na.rm = T),
    transatlantic_eastbound =
      mean(transatlantic_eastbound,
           na.rm = T),
    transatlantic_westbound =
      mean(transatlantic_westbound,
           na.rm = T),
    europe_to_asia =
      mean(europe_to_asia,
           na.rm = T),
    asia_to_europe =
      mean(asia_to_europe,
           na.rm = T)
    ) %>%
  dplyr::left_join(
    freight_rate_global_review_of_maritime_transport,
    by = 
      c(
        "year" = 
          "year"
        )
    )
## create 1976-1994 route-level data ----------
freight_rate_main_routes_review_of_maritime_transport_year1994 <-
  freight_rate_main_routes_review_of_maritime_transport %>%
  dplyr::filter(year == 1994) %>%
  dplyr::summarise(
    mean(transpacific_eastbound)
    )
freight_rate_global_container_markets_year1994 <-
  freight_rate_global_container_markets %>%
  dplyr::filter(
    year == 1994) %>%
  dplyr::summarise(
    mean(transpacific_eastbound)
    )

### compute conversion_ratio of TEU-FEU from two datasets ----
conversion_ratio_from_FEU_to_TEU <-
  as.numeric(
    freight_rate_main_routes_review_of_maritime_transport_year1994/
      freight_rate_global_container_markets_year1994
    )
freight_rate_global_container_markets <-
  freight_rate_global_container_markets %>%
  dplyr::mutate(
    transpacific_eastbound =
      transpacific_eastbound*
      conversion_ratio_from_FEU_to_TEU,
    transpacific_westbound =
      transpacific_westbound*
      conversion_ratio_from_FEU_to_TEU,
    transatlantic_eastbound =
      transatlantic_eastbound*
      conversion_ratio_from_FEU_to_TEU,
    transatlantic_westbound =
      transatlantic_westbound*
      conversion_ratio_from_FEU_to_TEU,
    europe_to_asia =
      europe_to_asia*
      conversion_ratio_from_FEU_to_TEU,
    asia_to_europe =
      asia_to_europe*
      conversion_ratio_from_FEU_to_TEU
    ) %>%
  dplyr::left_join(
    freight_rate_global_review_of_maritime_transport,
    by = 
      c(
        "year" = 
          "year"
        )
    )
conversion_ratio_from_liner_freight_rate_to_route_level_1990 <-
  freight_rate_global_container_markets %>%
  dplyr::filter(year == 1990) %>%
  dplyr::mutate(
    europe_to_asia_conversion_rate =
      europe_to_asia/
      global_liner_freight_rate,
    asia_to_europe_conversion_rate =
      asia_to_europe/
      global_liner_freight_rate
    )
conversion_ratio_from_liner_freight_rate_to_route_level_1978 <-
  freight_rate_global_container_markets %>%
  dplyr::filter(year == 1978) %>%
  dplyr::mutate(
    transatlantic_eastbound_conversion_rate =
      transatlantic_eastbound/
      global_liner_freight_rate,
    transatlantic_westbound_conversion_rate =
      transatlantic_westbound/
      global_liner_freight_rate
    )
conversion_ratio_from_liner_freight_rate_to_route_level_1990 <-
  freight_rate_global_container_markets %>%
  dplyr::filter(year == 1990) %>%
  dplyr::mutate(
    europe_to_asia_conversion_rate =
      europe_to_asia/
      global_liner_freight_rate,
    asia_to_europe_conversion_rate =
      asia_to_europe/
      global_liner_freight_rate
    )
### insert europe_to_asia data from Lloyd's Shipping Economists ----
EUROFE19781989 <-
  readxl::read_excel("input/EUROFE19781989.xlsx") %>%
  tidyr::tibble() %>% 
  dplyr::mutate(
    europe_to_asia = 
      `Europe-FE Eastbound`
    )
# compute conversion rate at 1990
EUROFE19781989_conversion_rate_1990 <-
  as.numeric(
    freight_rate_global_container_markets[
    freight_rate_global_container_markets$year==1990,6]/
      EUROFE19781989[EUROFE19781989$year == 1990,5])
freight_rate_global_container_markets[3:14,6] <-
  EUROFE19781989$europe_to_asia[1:12]*
  EUROFE19781989_conversion_rate_1990


## create 1973-1976 route-level data between ----------
# compute conversion rate at 1976
freight_rate_current_status_of_marine_transportation_1976 <-
  freight_rate_current_status_of_marine_transportation %>% 
  dplyr::filter(year == 1976)
freight_rate_issues_of_our_ocean_shipping_1976 <-
  freight_rate_issues_of_our_ocean_shipping %>% 
  dplyr::filter(year == 1976)
freight_rate_global_container_markets_1976 <-
  freight_rate_global_container_markets %>%
  dplyr::filter(year == 1976)
### calculate conversion rate based on average of rates of overlapped two routes ----
conversion_rate_1976_from_current_status_of_marine_transportation_to_global_container_markets <-
  # separate eastbound and westbound
  c(
    freight_rate_global_container_markets_1976$transpacific_eastbound/
      freight_rate_current_status_of_marine_transportation_1976$transpacific_eastbound,
    freight_rate_global_container_markets_1976$transpacific_westbound/
      freight_rate_current_status_of_marine_transportation_1976$transpacific_westbound
    )

# interpolate 1976
freight_rate_global_container_markets[1,6:7] <-
  freight_rate_current_status_of_marine_transportation_1976[,6:7]*
  conversion_rate_1976_from_current_status_of_marine_transportation_to_global_container_markets
freight_rate_current_status_of_marine_transportation_1973_1975 <- 
  freight_rate_current_status_of_marine_transportation[1:3,2:7] %>% 
  dplyr::mutate(
    transpacific_eastbound = 
      transpacific_eastbound*
      conversion_rate_1976_from_current_status_of_marine_transportation_to_global_container_markets[1],
    transpacific_westbound = 
      transpacific_westbound*
      conversion_rate_1976_from_current_status_of_marine_transportation_to_global_container_markets[2],
    europe_to_asia = 
      europe_to_asia*
      conversion_rate_1976_from_current_status_of_marine_transportation_to_global_container_markets[1],
    asia_to_europe = 
      asia_to_europe*
      conversion_rate_1976_from_current_status_of_marine_transportation_to_global_container_markets[2]
    )
freight_rate_current_status_of_marine_transportation[1:3,2:7] <-
  freight_rate_current_status_of_marine_transportation_1973_1975
freight_rate_current_status_of_marine_transportation <-
  freight_rate_current_status_of_marine_transportation %>% 
  # drop duplicated year
  dplyr::filter(year < 1976)

d_1973_1994 <-
  rbind(
    freight_rate_current_status_of_marine_transportation,
    freight_rate_global_container_markets[,1:7]
    )


## create 1966-1973 route-level data ----------
# miles for each route in freight_rate_issues_of_our_ocean_shipping
transpacific_distance_mile <- 
  4540
asia_to_europe_distance_mile <- 
  11474
transatlantic_distance_mile <- 
  #(12656+4063)/2 # average of Hamburg-San Francisco and Hamburg-Halifax
  4063 #Hamburg-Halifax

### compute conversion ratio from issues_of_our_ocean_shipping to liner freight rate ----
freight_rate_issues_of_our_ocean_shipping_1970 <-
  freight_rate_issues_of_our_ocean_shipping %>%
  dplyr::filter(
    year == 1970) %>%
  dplyr::left_join(
    freight_rate_global_review_of_maritime_transport,
    by =
      c(
        "year" =
          "year"
        )
    ) %>%
  dplyr::mutate(
    conversion_rate_1970 = 
      `asia_to_europe(constant_1979)`/
      global_liner_freight_rate
  )

## Imputation step ----

### impute asia_to_europe in 1965 from liner freight rate based on the proportion in 1970 ----
freight_rate_issues_of_our_ocean_shipping$`asia_to_europe(constant_1979)`[1:3] <-
  freight_rate_issues_of_our_ocean_shipping_1970$conversion_rate_1970[1:3]*
  freight_rate_global_review_of_maritime_transport$global_liner_freight_rate[1]
### convert dollars in 100 tons per mile into 1 TEU ----
freight_rate_issues_of_our_ocean_shipping_converted <-
  freight_rate_issues_of_our_ocean_shipping %>%
  dplyr::filter(
    is.na(
      `transpacific(constant_1979)`) == 0
    ) %>%
  dplyr::select(
    year,
    `transpacific(constant_1979)`,
    `transatlantic(constant_1979)`,
    `asia_to_europe(constant_1979)`
    ) %>%
  dplyr::mutate(
    `asia_to_europe(constant_1979)` =
      ifelse(
        is.na(
          `asia_to_europe(constant_1979)`) ==
          1,
        `transpacific(constant_1979)`,
        `asia_to_europe(constant_1979)`
        )
    ) %>%
  dplyr::group_by(
    year) %>%
  # take the mean of three main types of cargo.
  dplyr::summarise(
    transpacific =
      mean(`transpacific(constant_1979)`),
    # assume transatlantic type-level freight rate to be the mean of other two routes
    transatlantic =
      (mean(`transpacific(constant_1979)`) +
         mean(`asia_to_europe(constant_1979)`))/2,
    asia_to_europe =
      mean(`asia_to_europe(constant_1979)`)
    ) %>%
  dplyr::ungroup() %>%
  # compute route-level freight rate per mile-distance
  dplyr::mutate(
    transpacific =
      (transpacific/100)*
      transpacific_distance_mile*
      conversion_ratio_tons_to_TEU,
    transatlantic =
      (transatlantic/100)*
      transatlantic_distance_mile*
      conversion_ratio_tons_to_TEU,
    asia_to_europe =
      (asia_to_europe/100)*
      asia_to_europe_distance_mile*
      conversion_ratio_tons_to_TEU
    )

### linearly interpolate missing years ----
year_list_1965_1979 <-
  c(1965:1979) %>%
  tibble::as_tibble() %>%
  dplyr::rename(year = value)
freight_rate_issues_of_our_ocean_shipping <-
  year_list_1965_1979 %>%
  dplyr::left_join(
    freight_rate_issues_of_our_ocean_shipping_converted,
    by = 
      c(
        "year" = 
          "year"
        )
    )
freight_rate_issues_of_our_ocean_shipping[,2:4] <-
  FreqProf::approxm(
    freight_rate_issues_of_our_ocean_shipping[,2:4],
    n = 
      length(freight_rate_issues_of_our_ocean_shipping$year)
    )

### impute missing transatlantic using liner freight rate and yearly level proportional rates ----
conversion_ratio_from_liner_freight_rate_to_route_level_1976 <-
  freight_rate_global_container_markets %>%
  dplyr::filter(year == 1976) %>%
  dplyr::mutate(
    transatlantic_eastbound_conversion_rate =
      transatlantic_eastbound/
      global_liner_freight_rate,
    transatlantic_westbound_conversion_rate =
      transatlantic_westbound/
      global_liner_freight_rate)
conversion_ratio_to_transatlantic_eastbound_with_transatlantic_westbound_at_1976 <-
  freight_rate_global_container_markets[3,4]/
  freight_rate_global_container_markets[3,5]
freight_rate_global_container_markets[1:2,4] <-
  freight_rate_global_container_markets[1:2,5]*
  as.numeric(
    conversion_ratio_to_transatlantic_eastbound_with_transatlantic_westbound_at_1976
    )

### compute conversion_ratio of "each bound - aggregate" from two datasets ----
# 1976 year observes irregular swings of freight rate
# so that we calculate conversion ratio in 1975 of freight_rate_global_review_of_maritime_transport
# and 1976 of freight_rate_global_container_markets
freight_rate_issues_of_our_ocean_shipping_1976 <-
  freight_rate_issues_of_our_ocean_shipping %>%
  dplyr::filter(year == 1976) %>%
  dplyr::left_join(
    freight_rate_global_review_of_maritime_transport,
    by = 
      c(
        "year" = 
          "year"
        )
    )
freight_rate_global_container_markets_year1976 <-
  freight_rate_global_container_markets %>%
  dplyr::filter(year == 1976)
conversion_ratio_aggregate_to_each_bound <-
  freight_rate_global_container_markets_year1976 %>%
  dplyr::mutate(
    transpacific_eastbound_rate =
      transpacific_eastbound/
      freight_rate_issues_of_our_ocean_shipping_1976$transpacific,
    transpacific_westbound_rate =
      transpacific_westbound/
      freight_rate_issues_of_our_ocean_shipping_1976$transpacific,
    transatlantic_eastbound_rate =
      transatlantic_eastbound/
      freight_rate_issues_of_our_ocean_shipping_1976$transatlantic,
    transatlantic_westbound_rate =
      transatlantic_westbound/
      freight_rate_issues_of_our_ocean_shipping_1976$transatlantic,
    europe_to_asia_rate =
      europe_to_asia/
      freight_rate_issues_of_our_ocean_shipping_1976$asia_to_europe,
    asia_to_europe_rate =
      asia_to_europe/
      freight_rate_issues_of_our_ocean_shipping_1976$asia_to_europe
    ) %>%
  dplyr::select(
    year,
    transpacific_eastbound_rate,
    transpacific_westbound_rate,
    transatlantic_eastbound_rate,
    transatlantic_westbound_rate,
    europe_to_asia_rate,
    asia_to_europe_rate
    )
route_level_freight_rate_issues_of_our_ocean_shipping <-
  freight_rate_issues_of_our_ocean_shipping %>%
  dplyr::mutate(
    transpacific_eastbound =
      transpacific*
      conversion_ratio_aggregate_to_each_bound$transpacific_eastbound_rate,
    transpacific_westbound =
      transpacific*
      conversion_ratio_aggregate_to_each_bound$transpacific_westbound_rate,
    transatlantic_eastbound =
      transatlantic*
      conversion_ratio_aggregate_to_each_bound$transatlantic_eastbound_rate,
    transatlantic_westbound =
      transatlantic*
      conversion_ratio_aggregate_to_each_bound$transatlantic_westbound_rate,
    europe_to_asia =
      asia_to_europe*
      conversion_ratio_aggregate_to_each_bound$europe_to_asia_rate,
    asia_to_europe =
      asia_to_europe*
      conversion_ratio_aggregate_to_each_bound$asia_to_europe_rate
    ) %>%
  dplyr::select(
    year,
    transpacific_eastbound,
    transpacific_westbound,
    transatlantic_eastbound,
    transatlantic_westbound,
    europe_to_asia,
    asia_to_europe
    ) %>%
  dplyr::left_join(
    freight_rate_global_review_of_maritime_transport,
    by = 
      c(
        "year" = 
          "year"
        )
    )

## merge dataset between 1965 and 1994 ----
route_level_freight_rate_issues_of_our_ocean_shipping_1976 <-
  route_level_freight_rate_issues_of_our_ocean_shipping %>% 
  dplyr::filter(year == 1976)
d_1973_1994 <-
  rbind(
    freight_rate_current_status_of_marine_transportation,
    freight_rate_global_container_markets[,1:7]
    )
d_1973_1994_at_1976 <-
  d_1973_1994 %>% 
  dplyr::filter(year == 1976)
d_1965_1994 <-
  rbind(
    route_level_freight_rate_issues_of_our_ocean_shipping[1:8,1:7],
    d_1973_1994) %>%
  dplyr::distinct(
    year,
    .keep_all = TRUE
    ) %>%
  dplyr::arrange(year) 
## impute missing asia_to_europe ----
fixed_rate_europe_and_asia_at_1976 <-
  (d_1965_1994$asia_to_europe/
     d_1965_1994$europe_to_asia)[12]
d_1965_1994 <-
  d_1965_1994 %>% 
  dplyr::mutate(
    asia_to_europe = 
      ifelse(
        is.na(asia_to_europe) == 1,
        europe_to_asia*fixed_rate_europe_and_asia_at_1976,
        asia_to_europe)
    )
## impute missing routes by linear interpolation ----
d_1965_1994[8:13,4] <-
  FreqProf::approxm(
    d_1965_1994[8:13,4],
    n = 6)
d_1965_1994[8:13,5] <-
  FreqProf::approxm(
    d_1965_1994[8:13,5],
    n = 6)
d_1965_1994[12:14,6] <-
  FreqProf::approxm(
    d_1965_1994[12:14,6],
    n = 3)
d_1965_1994[12:14,7] <-
  FreqProf::approxm(
    d_1965_1994[12:14,7],
    n = 3)

## merge datasets ----
# adjust the values at 1994 across two datasets
convertion_rate_1994 <- 
  freight_rate_main_routes_review_of_maritime_transport[2,2:7]/
  d_1965_1994[30,2:7]
d_1965_1994[,2] <- 
  d_1965_1994[,2]*
  as.numeric(
    convertion_rate_1994[1])
d_1965_1994[,3] <- 
  d_1965_1994[,3]*
  as.numeric(
    convertion_rate_1994[2])
d_1965_1994[,4] <- 
  d_1965_1994[,4]*
  as.numeric(
    convertion_rate_1994[3])
d_1965_1994[,5] <- 
  d_1965_1994[,5]*
  as.numeric(
    convertion_rate_1994[4])
d_1965_1994[,6] <- 
  d_1965_1994[,6]*
  as.numeric(
    convertion_rate_1994[5])
d_1965_1994[,7] <- 
  d_1965_1994[,7]*
  as.numeric(
    convertion_rate_1994[6])
d_1965_1994 <-
  d_1965_1994 %>% 
  dplyr::filter(year < 1993) # drop duplicate year 1994 from RMT

d_1965_1994_price_rawdata <-
  d_1965_1994

d_1965_1994_for_plots <-
  d_1965_1994 %>%
  tidyr::gather(
    key = "route",
    value = "value",
    - year
    )
x <- 
  ggplot(
    d_1965_1994_for_plots,
    aes(x = year, y = value)) +
  geom_line(aes(color = route), alpha = 0.6) +
  geom_point(aes(shape = route, color = route)) +
  #geom_point(aes(shape = route, color = route)) +
  theme_minimal() +
  theme(legend.position = 'top')
x

d_1965_2009 <-
  rbind(
    d_1965_1994,
    freight_rate_main_routes_review_of_maritime_transport[,1:7]
    )

colnames(
  d_1965_2009
  ) <-
  c(
    "year",
    "transpacific_eastbound",
    "transatlantic_eastbound",
    "transpacific_westbound",
    "transatlantic_westbound",
    "europe_to_asia",
    "asia_to_europe"
    )
### nondeflated price based on 1994 U.S. dollars ----
d_1965_2009_nondeflated <-
  d_1965_2009 %>%
  tidyr::gather(
    key = "route",
    value = "value",
    - year
    )
x <- ggplot(
  d_1965_2009_nondeflated,
  aes(x = year, y = value)) +
  geom_line(aes(color = route), alpha = 0.6) +
  geom_point(aes(shape = route, color = route)) +
  # geom_line(aes(x = year, y = liner_freight_rate*30),
  #           alpha = 0.3) +
  # scale_y_continuous(
  #   sec.axis = sec_axis(~(./30),
  #                       name = "liner freight rate (year 1995 = 100)")) +
  theme_minimal() +
  theme(legend.position = 'top') +
  ggtitle("Freight rate (CPI unadjusted)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("year") + ylab("freight rate (dollars per TEU)") +
  #expand_limits(y=0, x=2000) +
  geom_vline(xintercept = 1966, linetype = "longdash") +
  annotate("text", x = 1970, y = 7000, label = "Issues of --") +
  geom_vline(xintercept = 1976, linetype = "longdash") +
  annotate("text", x = 1985, y = 6500, label = "Global container markets") +
  geom_vline(xintercept = 1993, linetype = "longdash") +
  annotate("text", x = 2003, y = 6000, label = "Review of\n Maritime Transport") +
  labs(shape = "Route", color = "Route")

figure_name <- "figuretable/nondeflated_container_freight_rate_each_route.png"
ggsave(filename = figure_name,
       plot = x,
       device = "png",
       width = 10,
       height = 7)
### deflated price based on 1994 U.S. dollars ----
d_1965_2009 <-
  d_1965_2009 %>%
  dplyr::left_join(
    cpi_1965_2010_based_on_1995,
    by =
      c(
        "year" = 
          "year"
        )
    ) %>%
  dplyr::mutate(
    transpacific_eastbound = 
      transpacific_eastbound/
      cpi_based_on_1995,
    transatlantic_eastbound =
      transatlantic_eastbound/
      cpi_based_on_1995,
    transpacific_westbound = 
      transpacific_westbound/
      cpi_based_on_1995,
    transatlantic_westbound =
      transatlantic_westbound/
      cpi_based_on_1995,
    europe_to_asia = 
      europe_to_asia/
      cpi_based_on_1995,
    asia_to_europe = 
      asia_to_europe/
      cpi_based_on_1995
  ) %>%
  dplyr::select(- cpi_based_on_1995) %>%
  tidyr::gather(
    key = "route",
    value = "value",
    - year
    )


temp_merged_freight_rate_global_review_of_maritime_transport_deflated <-
  merged_freight_rate_global_review_of_maritime_transport_deflated %>%
  dplyr::rename(
    route2 = 
      route,
    liner_freight_rate =
      value
    ) %>%
  dplyr::filter(
    route2 != 
      "container freight rate (CPI adjusted to 1995)"
    )
d_1965_2009 <-
  d_1965_2009 %>%
  dplyr::left_join(
    temp_merged_freight_rate_global_review_of_maritime_transport_deflated,
    by = 
      c(
        "year" = 
          "year"
        )
    )

x <- ggplot(d_1965_2009,
            aes(x = year, y = value)) +
  geom_line(aes(color = route), alpha = 0.6) +
  geom_point(aes(shape = route, color = route)) +
  geom_line(aes(x = year, y = liner_freight_rate*30),
            alpha = 0.3) +
  scale_y_continuous(
    sec.axis = sec_axis(~(./30),
                        name = "liner freight rate (year 1995 = 100)")) +
  theme_minimal() +
  theme(legend.position = 'top') +
  ggtitle("Freight rate (CPI adjusted to 1995)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("year") + ylab("freight rate (dollars per TEU)") +
  #expand_limits(y=0, x=2000) +
  geom_vline(xintercept = 1966, linetype = "longdash") +
  annotate("text", x = 1970, y = 7000, label = "Issues of --") +
  geom_vline(xintercept = 1976, linetype = "longdash") +
  annotate("text", x = 1985, y = 6500, label = "Global container markets") +
  geom_vline(xintercept = 1993, linetype = "longdash") +
  annotate("text", x = 2003, y = 6000, label = "Review of\n Maritime Transport") +
  labs(shape = "Route", color = "Route")

figure_name <- "figuretable/container_freight_rate_each_route.png"
ggsave(filename = figure_name,
       plot = x,
       device = "png",
       width = 10,
       height = 7)
container_freight_rate_each_route <-
  d_1965_2009 %>%
  dplyr::rename(
    route = 
      route,
    liner_freight_rate_CPI_adjusted_1995 =
      route2
    )
# drop the pre-period before global containerization
container_freight_rate_each_route <-
  container_freight_rate_each_route %>% 
  dplyr::filter(
    (route == "transpacific_eastbound"&year >= 1967)|
      (route == "transpacific_westbound"&year >= 1967)|
      (route == "transatlantic_eastbound"&year >= 1966)|
      (route == "transatlantic_westbound"&year >= 1966)|
      (route == "europe_to_asia"&year >= 1971)|
      (route == "asia_to_europe"&year >= 1971)
    )

# construct route level demand data ----

## construct demand-shifter markets data -----
Data_Extract_From_World_Development_Indicators <-
  readxl::read_excel(
    "input/Data_Extract_From_World_Development_Indicators.xlsx") %>%
  tidyr::tibble()
GDP_table <-
  t(Data_Extract_From_World_Development_Indicators[1:3,5:51]) %>%
  tibble::as_tibble() %>%
  dplyr::rename(east_asia = V1,
                united_state = V2,
                euro_area = V3) %>%
  dplyr::mutate(year = c(1964:2010)) %>%
  dplyr::select(year, everything()) %>%
  tidyr::gather(
    key = "route",
    value = "value",
    -year
    ) %>%
  dplyr::mutate(log_GDP = log(value))
ggplot(
  GDP_table,
  aes(x = year, y = log_GDP)
  ) +
  geom_point(aes(color = route)) +
  geom_line(aes(color = route))
### assign route-level routes ----
GDP_table_eastbound <-
  GDP_table %>%
  dplyr::mutate(
    route =
      dplyr::case_when(
        route ==
          "east_asia" ~
          "europe_to_asia",
        route ==
          "united_state" ~ 
          "transpacific_eastbound",
        route == 
          "euro_area" ~ 
          "transatlantic_eastbound"
        )
    )
quantity_eastbound <-
  d_1965_2009 %>%
  dplyr::select(
    year,
    route) %>%
  dplyr::filter(
    route == 
      "transpacific_eastbound"|
      route ==
      "transatlantic_eastbound"|
      route ==
      "europe_to_asia"
    ) %>%
  dplyr::left_join(
    GDP_table_eastbound,
    by = 
      c(
        "year" = 
          "year",
        "route" = 
          "route")
    )
GDP_table_westbound <-
  GDP_table %>%
  dplyr::mutate(
    route =
      dplyr::case_when(
        route == 
          "east_asia" ~
          "transpacific_westbound",
        route == 
          "united_state" ~ 
          "transatlantic_westbound",
        route == 
          "euro_area" ~
          "asia_to_europe"
        )
    )
quantity_westbound <-
  d_1965_2009 %>%
  dplyr::select(
    year,
    route
    ) %>%
  dplyr::filter(
    route == 
      "transpacific_westbound"|
      route == 
      "transatlantic_westbound"|
      route == 
      "asia_to_europe"
    ) %>%
  dplyr::left_join(
    GDP_table_westbound,
    by = 
      c(
        "year" = 
          "year",
        "route" = 
          "route"
        )
    )

GDP_of_destination_route_level <-
  rbind(
    quantity_eastbound,
    quantity_westbound
    ) %>%
  dplyr::arrange(year) %>%
  dplyr::rename(
    route = route,
    GDP = value
    )


## construct price instruments data -----

### construct fuel price data

#https://ourworldindata.org/grapher/crude-oil-prices?time=1960..latest

crude_oil_prices <-
  read.csv("input/crude-oil-prices.csv") %>%
  tidyr::tibble()
crude_oil_prices <-
  crude_oil_prices[,3:4]
colnames(
  crude_oil_prices
  ) <-
  c(
    "year",
    "crude_oil_price"
    )
crude_oil_prices <-
  crude_oil_prices %>%
  dplyr::filter(
    year >= 1965) %>%
  dplyr::left_join(
    cpi_1965_2010_based_on_1995,
    by = 
      c(
        "year" = 
          "year"
        )
    ) %>%
  dplyr::mutate(
    crude_oil_prices_cpi_adjusted_1995 =
      crude_oil_price/
      cpi_based_on_1995
    )
  
ggplot(
  crude_oil_prices,
  aes(x = year,
      y = crude_oil_prices_cpi_adjusted_1995)) +
  geom_line()

### assign route-level routes ----

crude_oil_prices_route_level <-
  d_1965_2009 %>%
  dplyr::select(
    year,
    route
    ) %>%
  dplyr::left_join(
    crude_oil_prices,
    by = 
      c(
        "year" =
          "year"
        )
    ) %>%
  dplyr::mutate(
    crude_oil_price_weighted_mile =
      dplyr::case_when(
        route == 
          "transpacific_westbound" ~
          crude_oil_prices_cpi_adjusted_1995*
          transpacific_distance_mile/
          transpacific_distance_mile,
        route == 
          "transatlantic_westbound" ~
          crude_oil_prices_cpi_adjusted_1995*
          transatlantic_distance_mile/
          transpacific_distance_mile,
        route == 
          "asia_to_europe" ~
          crude_oil_prices_cpi_adjusted_1995*
          asia_to_europe_distance_mile/
          transpacific_distance_mile,
        route == 
          "transpacific_eastbound" ~
          crude_oil_prices_cpi_adjusted_1995*
          transpacific_distance_mile/
          transpacific_distance_mile,
        route == 
          "transatlantic_eastbound" ~
          crude_oil_prices_cpi_adjusted_1995*
          transatlantic_distance_mile/
          transpacific_distance_mile,
        route ==
          "europe_to_asia" ~
          crude_oil_prices_cpi_adjusted_1995*
          asia_to_europe_distance_mile/
          transpacific_distance_mile
        )
  ) %>%
  dplyr::rename(route = route)

# save preprocessed data ----
saveRDS(
  container_freight_rate_each_route,
  file = 
    "cleaned/container_freight_rate_each_route.rds"
  )
saveRDS(
  d_1965_1994_price_rawdata,
  file = 
    "cleaned/d_1965_1994_price_rawdata.rds"
  )
saveRDS(
  GDP_of_destination_route_level,
  file = 
    "cleaned/GDP_of_destination_route_level.rds"
  )
saveRDS(
  crude_oil_prices_route_level,
  file = 
    "cleaned/crude_oil_prices_route_level.rds"
  )
