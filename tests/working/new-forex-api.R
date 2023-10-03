


library(jsonlite)


# If we look at one currency AUD

a <- fromJSON("http://api.exchangerate.host/timeframe?start_date=2015-01-01&end_date=2015-01-14&currencies=AUD&access_key=9f7337ae140cc2ad5704fb042125fe29")
# It assumes you want the rate between that provided currency and the USD

b <- fromJSON("http://api.exchangerate.host/timeframe?start_date=2015-01-01&end_date=2015-01-14&currencies=AUD,USD&access_key=9f7337ae140cc2ad5704fb042125fe29")

identical(a, b)
# TRUE

# Swapping the order of the currencies doesn't do anything
c <-  fromJSON("http://api.exchangerate.host/timeframe?start_date=2015-01-01&end_date=2015-01-14&currencies=USD,AUD&access_key=9f7337ae140cc2ad5704fb042125fe29")
identical(a, c)
# TRUE


# When requesting 2 currencies that aren't the USD, it gives a different data structure
d <- fromJSON("http://api.exchangerate.host/timeframe?start_date=2015-01-01&end_date=2015-01-14&currencies=AUD,JPY&access_key=9f7337ae140cc2ad5704fb042125fe29")

# For reproducibility, here's d without needing to make the API call

dd <- list(success = TRUE, terms = "https://currencylayer.com/terms",
    privacy = "https://currencylayer.com/privacy", timeframe = TRUE,
    start_date = "2015-01-01", end_date = "2015-01-14", source = "USD",
    quotes = list(`2015-01-01` = list(USDAUD = 1.227915, USDJPY = 120.2679),
        `2015-01-02` = list(USDAUD = 1.228102, USDJPY = 120.2839),
        `2015-01-03` = list(USDAUD = 1.236018, USDJPY = 120.4524),
        `2015-01-04` = list(USDAUD = 1.236932, USDJPY = 120.487899),
        `2015-01-05` = list(USDAUD = 1.236622, USDJPY = 119.66),
        `2015-01-06` = list(USDAUD = 1.238168, USDJPY = 118.868801),
        `2015-01-07` = list(USDAUD = 1.238158, USDJPY = 119.2345),
        `2015-01-08` = list(USDAUD = 1.232187, USDJPY = 119.6296),
        `2015-01-09` = list(USDAUD = 1.21894, USDJPY = 118.7298),
        `2015-01-10` = list(USDAUD = 1.219055, USDJPY = 118.6659),
        `2015-01-11` = list(USDAUD = 1.216915, USDJPY = 118.3745),
        `2015-01-12` = list(USDAUD = 1.22612, USDJPY = 118.3876),
        `2015-01-13` = list(USDAUD = 1.224683, USDJPY = 118.0138),
        `2015-01-14` = list(USDAUD = 1.226384, USDJPY = 117.4253)))

# Note that the order of the rate returned *is* dependent on the order they're provided in the URL
e <- fromJSON("http://api.exchangerate.host/timeframe?start_date=2015-01-01&end_date=2015-01-14&currencies=JPY,AUD&access_key=9f7337ae140cc2ad5704fb042125fe29")



# So, when requesting 2 currencies, instead of returning the rate between the two, it will give the USD rate of each,
# from which it's possible to triangulate between the two to get the rate you're after

# An exception is when one of them is USD, in which case it just returns the rate, hence that must be handled differently










get_values <- function(response, index) {
  response[[8]] %>%
    purrr::map_dbl( ~ {.x[[index]] }) %>% unname
}


one_usd_is_x_cur1 <- get_values(d, 1)
one_usd_is_x_cur2 <- get_values(d, 2)

rate <- one_usd_is_x_cur2 / one_usd_is_x_cur1

# [1] 97.94481 97.94292 97.45198 97.40867 96.76360 96.00377 96.29991 97.08721 97.40414 97.34253 97.27425 96.55466 96.36273 95.74921

# i.e. one AUD is ^^ many Yen




from = "AUD"
to = "USD"





get_values <- function(response, index) {
  response[[8]] %>%
    purrr::map_dbl( ~ {.x[[index]] }) %>% unname
}

# There are 3 possibilities to handle for
# 1. Convert from USD to a non-USD currency.
# 2. Convert from a non-USD currency to USD
# 3. Concert between two non-USD currencies

values <- if (from == "USD") {

  dat %>% get_values(1)

  } else if (to == "USD") {

  dat %>% get_values(1) %>% `/`(1, .)

  } else {
  # 3. Concert between two non-USD currencies

  one_usd_is_x_cur1 <- get_values(dat, 1)
  one_usd_is_x_cur2 <- get_values(dat, 2)
  one_usd_is_x_cur2 / one_usd_is_x_cur1

  }





# testing historical_exchange_rates()

historical_exchange_rates("AUD", to = "USD",
                          start_date = "2017-01-01", end_date = "2020-06-30")




# Works when dates are recent
au <- historical_exchange_rates("AUD", to = "USD",
                          start_date = "2017-01-01", end_date = "2020-06-30")

ae <- historical_exchange_rates("AUD", to = "EUR",
                          start_date = "2017-01-01", end_date = "2020-06-30")

cur <- au %>% left_join(ae, by = "date")



# But due to missing data on weekends in older data, errors

au <- historical_exchange_rates("AUD", to = "USD",
                          start_date = "2010-01-01", end_date = "2020-06-30")

ae <- historical_exchange_rates("AUD", to = "EUR",
                          start_date = "2010-01-01", end_date = "2020-06-30")

cur <- au %>% left_join(ae, by = "date")




# Testing how far back we can go without an error

historical_exchange_rates("AUD", to = "USD",
                          start_date = "2013-01-01", end_date = "2023-06-30")









from = "AUD"
to = "USD"
start_date = "2022-01-01"
end_date = "2022-06-30"
























# Exploring how to handle for missing weekends


# First examine an example that fails





















# Possible more advanced version of get_values() that could be used to handle nulls if API doesn't return weekends

get_values <- function(response, index) {
  dat[[8]] %>%
    .[[index]] %>%
    purrr::map_dbl( ~ {
      if (length(.x) > 0) {
        first_value <- .x[[1]]
      } else {
        first_value <- NA_real_
      }
      first_value
    }) %>% unname
}




get_values(a, 1)
















# Getting live rates

live <- fromJSON("http://api.exchangerate.host/live?access_key=9f7337ae140cc2ad5704fb042125fe29")


exchange_rate_latest <- function(currency = "USD") {


}












