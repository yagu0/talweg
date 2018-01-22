#' Sample dataset to run the algorithm
#'
#' Original data from AirNormand was removed, and replaced with the following:
#' \itemize{
#'   \item{daily_exogenous.csv}{a CSV file containing dates in the first column, and then n columns for measurements, and finally n columns for predictions (same day)}
#'   \item{intraday_measures.csv}{a CSV file with two columns: the first with datetimes, and the second with measurements}
#' }
#' -----\cr
#' Here is the code that generated these files: (not any physical sense)\cr
#' #dts = timestamps from 1970-01-01 01:00 to 1972-01-01 00:00\cr
#' dts = seq(0,17519*3600,3600)\cr
#' intraday = data.frame(\cr
#'   Time=as.POSIXct(dts,tz="GMT",origin="1970-01-01 01:00"),\cr
#'   Measure=rgamma(length(dts),10,.7) )\cr
#' dates = seq(as.Date("1970-01-01"), as.Date("1972-01-01"), "day")\cr
#' m1 = cos(seq_along(dates))\cr
#' m2 = log(seq_along(dates)+1)\cr
#' daily = data.frame(\cr
#'   Date=dates,\cr
#'   m1=m1,\cr
#'   m2=m2,\cr
#'   m1_pred=m1+rnorm(length(m1),sd=.1),\cr
#'   m2_pred=m2+rnorm(length(m2),sd=.1) )\cr
#' write.csv(intraday, file="intraday_measures.csv", row.names=F)\cr
#' write.csv(daily, file="daily_exogenous.csv", row.names=F)
#'
#' @name sample
#' @docType data
#' @usage data(talweg::sample)
#' @format Two dataframes: intraday with 17519 rows and 2 columns (Time,Measure), hourly;
#'   and daily with 731 rows, 5 columns (Date,m1,m2,m1_pred,m2_pred)
NULL
