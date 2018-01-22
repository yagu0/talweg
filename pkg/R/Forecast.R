#' Forecast
#'
#' Forecast encapsulation as a list (days where prediction occur) of lists (components).
#'
#' The private field .pred is a list where each cell contains the predicted variables for
#' a period of time of H-P+1<=24 hours, from hour P until H, where P == predict_from.
#' \code{forecast$getForecast(i)} output forecasts for
#' \code{data$getSerie(forecast$getIndexInData(i))}.
#'
#' @usage # Forecast$new(dates)
#'
#' @field .pred List with
#' \itemize{
#'   \item serie: the forecasted serie
#'   \item params: corresponding list of parameters (weights, neighbors...)
#'   \item index_in_data: corresponding index in data object
#' }
#' @field .dates vector of (integer) day indices where forecast occurs
#'
#' @section Methods:
#' \describe{
#' \item{\code{initialize(dates)}}{
#'   Initialize a Forecast object with a series of date indices.}
#' \item{\code{getSize()}}{
#'   Return number of individual forecasts.}
#' \item{\code{append(forecast, params, index_in_data)}}{
#'   Acquire an individual forecast, with its (optimized) parameters and the
#'   corresponding index in the dataset.}
#' \item{\code{getDates()}}{
#'   Get dates where forecast occurs.}
#' \item{\code{getForecast(index)}}{
#'   Get forecasted serie at specified index.}
#' \item{\code{getParams(index)}}{
#'   Get parameters at specified index (for 'Neighbors' method).}
#' \item{\code{getIndexInData(index)}}{
#'   Get index in data which corresponds to current forecast.}
#' }
#'
#' @docType class
#' @format R6 class
#'
Forecast = R6::R6Class("Forecast",
	private = list(
		.pred = list(),
		.dates = integer(0) #store dates as integers (from 1970-01-01)
	),
	public = list(
		initialize = function(dates)
		{
			private$.dates <- dates
			invisible(self)
		},
		getSize = function()
			length(private$.pred)
		,
		append = function(forecast, params, index_in_data)
		{
			private$.pred[[length(private$.pred)+1]] <-
				list("forecast"=forecast, "params"=params, "index_in_data"=index_in_data)
		},
		getDates = function()
			as.Date( private$.dates, origin="1970-01-01" )
		,
		getForecast = function(index)
		{
			if (is(index,"Date"))
				index = match(index, private$.dates)
			private$.pred[[index]]$forecast
		},
		getParams = function(index)
		{
			if (is(index,"Date"))
				index = match(index, private$.dates)
			private$.pred[[index]]$params
		},
		getIndexInData = function(index)
		{
			if (is(index,"Date"))
				index = match(index, private$.dates)
			private$.pred[[index]]$index_in_data
		}
	)
)
