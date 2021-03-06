% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjust_for_inflation.R
\name{adjust_for_inflation}
\alias{adjust_for_inflation}
\alias{afi}
\title{Convert nominal prices into real prices}
\usage{
adjust_for_inflation(price, from_date, country, to_date, inflation_dataframe,
countries_dataframe, extrapolate_future_method, future_averaging_period, future_rate,
extrapolate_past_method, past_averaging_period, past_rate)

afi(
  price,
  from_date,
  country,
  to_date,
  inflation_dataframe,
  countries_dataframe,
  extrapolate_future_method,
  future_averaging_period,
  future_rate,
  extrapolate_past_method,
  past_averaging_period,
  past_rate
)
}
\arguments{
\item{price}{A price (or prices).}

\item{from_date}{A date(s) from which the prices will be converted.}

\item{country}{A country or region in whose currency the prices are denominated.}

\item{to_date}{A date(s) to which the prices will be converted.}

\item{inflation_dataframe}{The R object (list) representing the JSON retrieved by calling retrieve_inflation_data().}

\item{countries_dataframe}{The R object (data.frame) representing the JSON retreived by calling show_countries().}

\item{extrapolate_future_method}{The extrapolation method that shall be used if extrapolation into the future is required. Options are 'average' or 'rate'.}

\item{future_averaging_period}{The number of recent periods to average in order to extrapolate forward (if 'average' is method being used).}

\item{future_rate}{An assumed rate of inflation to use for extrapolating forward (if 'rate' is method being used).}

\item{extrapolate_past_method}{The extrapolation method that shall be used if extrapolation from the earliest available data to some even earlier period is required.}

\item{past_averaging_period}{The number of periods back from the earliest available inflation data for a given country to average in order to extrapolate into the past (if 'average' is method being used).}

\item{past_rate}{An assumed rate of inflation to use for extrapolating from the earliest available data to some even earlier period (if 'rate' is method being used).}
}
\value{
A vector of inflation-adjusted prices
}
\description{
Inflate/deflate prices from any year to any year, using World Bank inflation data and assumptions only where necessary.
Typically used for converting past (nominal) values into current (real) values. This uses World Bank inflation data where available,
but allows for both historical and future assumptions in extrapolation.
}
\examples{
\dontrun{
# Assign these variables once
country <- "AU"
inflation_dataframe <- retrieve_inflation_data(country)
countries_dataframe <- show_countries()

# Convert $100 from 2005 into 2017 dollars

adjust_for_inflation(100, 2005, country, to_date = 2017,
inflation_dataframe = inflation_dataframe,
countries_dataframe = countries_dataframe)

# [1] 133.9861 # i.e. $100 in 2005 had the same purchasing power as $133.99 in 2017
}

}
