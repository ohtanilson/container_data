# import package ------
rm(list = ls())
library(ggplot2)
library(acs) # for getting CPI

# import raw freight data ----
Review1971_1998 <-
  readxl::read_excel("input/Review1971_1998.xlsx") %>%
  tidyr::tibble()
Lloyd_shipping_economist_1980_1998 <-
  readxl::read_excel("input/Lloyd_shipping_economist_1980_1998.xlsx") %>%
  tidyr::tibble()
Lloyd_shipping_economist_1980_1998 <-
  Lloyd_shipping_economist_1980_1998 %>% 
  dplyr::mutate(
    price_per_TEU =
      price_main*10
  )

# import CPI data ----
data(cpi)
cpi_based_on_1995 <-
  cpi/cpi[83]
cpi_1965_2010_based_on_1995 <-
  cpi_based_on_1995[53:98]
cpi_1965_2010_based_on_1995 <-
  cbind(as.numeric(1965:2010),
        cpi_1965_2010_based_on_1995) %>%
  tibble::as.tibble()
colnames(cpi_1965_2010_based_on_1995) <-
  c("year",
    "cpi_based_on_1995")
# merge ----
## newbuilding price ----
converted_12000_to_18000 <-
  Review1971_1998 %>% 
  dplyr::filter(year == 1968&
                  type == "newbuilding_price_12000dwt_bulk")
conversion_rate_12000_to_18000 <-
  converted_12000_to_18000$price_main/
  converted_12000_to_18000$price_sub
converted_newbuilding <-
  Review1971_1998[1:36,] %>% 
  dplyr::mutate(
    converted_price = 
      ifelse(is.na(price_sub) == 1,
             price_main,
             price_sub * conversion_rate_12000_to_18000),
    converted_unit =
      "newbuilding_price_12000dwt_bulk")



## secondhand price ----
converted_secondhand_1989 <-
  Review1971_1998  %>% 
  dplyr::filter(year == 1989&
                  type == "second_hand_dry_cargo_price_12000dwt_5years")
conversion_rate_secondhand <-
  converted_secondhand_1989$price_sub/
  converted_secondhand_1989$price_sub2

converted_secondhand2 <-
  Review1971_1998 %>% 
  dplyr::filter(year == 1981&
                  type == "second_hand_dry_cargo_price_16000dwt_built_1963")
converted_secondhand_per_age_to_5years <-
  converted_secondhand2$price_sub/
  converted_secondhand2$price_main
converted_secondhand <-
  Review1971_1998[37:72,] %>% 
  dplyr::mutate(
    converted_price = 
      ifelse(is.na(price_sub2) == 1,
             price_sub,
             price_sub2 * conversion_rate_secondhand)) %>% 
  dplyr::mutate(
    converted_price = 
      ifelse(is.na(converted_price) == 1,
             price_main * converted_secondhand_per_age_to_5years,
             converted_price),
    converted_unit =
      "second_hand_dry_cargo_price_12000dwt_5years")

## scrap price ----
converted_scrap <-
  Review1971_1998[73:dim(Review1971_1998)[1],] %>% 
  dplyr::mutate(
    converted_price = 
      price_main,
    converted_unit =
      "breaking_price_per_LTD_far_east")



# convert to price per TEU ----

## newbuilding ----
converted_newbuilding <-
  converted_newbuilding %>% 
  dplyr::mutate(
    converted_price = 
      (converted_price*1000000/10)/1200,
    converted_unit =
      "newbuilding_price_per_TEU")
## secondhand ----
#2.7+2X=16a
#0.8+ 0X=11a
#then, a=0.8/11=4/55
#X=(148.5/55-64/55)/2=74.5/110
a = 0.8/11
X = 74.5/110
converted_secondhand <-
  converted_secondhand %>% 
  dplyr::mutate(
    undepreciated_year = 1983 - year 
  ) %>% 
  dplyr::mutate(
    converted_price = converted_price + X*undepreciated_year
  ) %>% 
  dplyr::mutate(
    converted_price = 
      (converted_price*1000000*(12000/16000)/10)/1200,
    converted_unit =
      "second_hand_dry_cargo_price_per_TEU_5years") %>% 
  dplyr::select(- undepreciated_year)
## scrap ----
converted_scrap <-
  converted_scrap %>% 
  dplyr::mutate(
    converted_price = 
      (converted_price*10/4),
    converted_unit =
      "breaking_price_per_TEU_far_east")

# convert bulk into container ----
## newbuilding ----
converted_newbuilding_1980 <-
  converted_newbuilding %>% 
  dplyr::filter(year == 1980)
Lloyd_shipping_economist_1980_1998_newbuilding <-
  Lloyd_shipping_economist_1980_1998 %>% 
  dplyr::filter(type == "newbuilding_price_per_dwt_1600teu_fullcon") %>% 
  dplyr::select(year,
                price_per_TEU)
Lloyd_shipping_economist_1980 <-
  Lloyd_shipping_economist_1980_1998_newbuilding %>% 
  dplyr::filter(year == 1980)
conversion_rate_1980_newbuilding <-
  Lloyd_shipping_economist_1980$price_per_TEU/
  converted_newbuilding_1980$converted_price
converted_newbuilding <-
  converted_newbuilding %>% 
  dplyr::left_join(
    Lloyd_shipping_economist_1980_1998_newbuilding,
    by = c("year" = "year")
  ) %>% 
  dplyr::mutate(
    converted_price =
      ifelse(
        is.na(price_per_TEU),
        converted_price * conversion_rate_1980_newbuilding,
        price_per_TEU
      )
  ) %>% 
  dplyr::select(- price_per_TEU)
## secondhand ----
converted_secondhand_1980 <-
  converted_secondhand %>% 
  dplyr::filter(year == 1980)
Lloyd_shipping_economist_1980_1998_secondhand <-
  Lloyd_shipping_economist_1980_1998 %>% 
  dplyr::filter(type == "second_hand_per_dwt_1600teu_fullcon_5years") %>% 
  dplyr::select(year,
                price_per_TEU)
Lloyd_shipping_economist_1980 <-
  Lloyd_shipping_economist_1980_1998_secondhand %>% 
  dplyr::filter(year == 1980)
conversion_rate_1980_secondhand <-
  Lloyd_shipping_economist_1980$price_per_TEU/
  converted_secondhand_1980$converted_price
converted_secondhand <-
  converted_secondhand %>% 
  dplyr::left_join(
    Lloyd_shipping_economist_1980_1998_secondhand,
    by = c("year" = "year")
  ) %>% 
  dplyr::mutate(
    converted_price =
      ifelse(
        is.na(price_per_TEU),
        converted_price * conversion_rate_1980_secondhand,
        price_per_TEU
      )
  ) %>% 
  dplyr::select(- price_per_TEU)

# merge and cpi adjustment ----
price_newbuilding_secondhand_scrap <-
  rbind(converted_newbuilding,
        converted_secondhand,
        converted_scrap) %>% 
  dplyr::select(
    year,
    type,
    converted_unit,
    converted_price)
price_newbuilding_secondhand_scrap <-
  price_newbuilding_secondhand_scrap %>%
  dplyr::left_join(cpi_1965_2010_based_on_1995,
                   by = c("year" = "year")) %>%
  dplyr::mutate(
    converted_price_cpi_adjusted = converted_price/
      cpi_based_on_1995
  ) %>%
  dplyr::select(-cpi_based_on_1995) 

# save preprocessed data
saveRDS(price_newbuilding_secondhand_scrap,
        file = "cleaned/price_newbuilding_secondhand_scrap.rds")
