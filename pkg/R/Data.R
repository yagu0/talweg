#' Data
#'
#' Data encapsulation, in the form of a few lists (time-series + exogenous variables).
#'
#' The private field .tvp is a list where each cell contains the hourly variables for a
#' period of time of 24 hours, from 1am to next midnight. The other lists contain
#' informations on series' levels and exogenous variables (both measured and predicted).
#'
#' @usage # Data$new()
#'
#' @field .tvp List of "time-values"; in each cell:
#' \itemize{
#'   \item time: vector of times
#'   \item serie: (measured) serie
#'   \item level_hat: predicted level for current day
#' }
#' @field .level Vector of measured levels
#' @field .exo List of measured exogenous variables, cell = numerical vector.
#' @field .exo_hat List of predicted exogenous variables, cell = numerical vector.
#'
#' @section Methods:
#' \describe{
#' \item{\code{getSize()}}{
#'   Number of series in dataset.}
#' \item{\code{append(time, value, level_hat, exo, exo_hat)}}{
#'   Measured data for given vector of times + exogenous predictions from
#'   last midgnight.}
#' \item{\code{getTime(index)}}{
#'   Times (vector) at specified index.}
#' \item{\code{getSerie(index)}}{
#'   Serie (centered+level) at specified index.}
#' \item{\code{getSeries(indices)}}{
#'   Series at specified indices (in columns).}
#' \item{\code{getLevel(index)}}{
#'   Measured level at specified index.}
#' \item{\code{getLevelHat(index)}}{
#'   Predicted level vector at specified index (by hour).}
#' \item{\code{getCenteredSerie(index)}}{
#'   Centered serie at specified index.}
#' \item{\code{getCenteredSeries(indices)}}{
#'   Centered series at specified indices (in columns).}
#' \item{\code{getExo(index)}}{
#'   Measured exogenous variables at specified index.}
#' \item{\code{getExoHat(index)}}{
#'   Predicted exogenous variables at specified index.}
#' }
#'
#' @docType class
#' @format R6 class
#'
Data = R6::R6Class("Data",
	private = list(
		.tvp = list(),
		.level = vector("double",0),
		.exo = list(),
		.exo_hat = list()
	),
	public = list(
		getSize = function()
			length(private$.tvp)
		,
		append = function(time=NULL, value=NULL, level_hat=NULL, exo=NULL, exo_hat=NULL)
		{
			if (!is.null(time) && !is.null(value) && !is.null(level_hat))
			{
				L = length(private$.tvp)
				if (L == 0 || strftime( tail(private$.tvp[[L]]$time,1),
					format="%H:%M:%S", tz="GMT" ) == "00:00:00")
				{
					# Append a new cell
					private$.tvp[[L+1]] <- list("time"=time, "serie"=value, "level_hat"=level_hat)
				}
				else
				{
					# Complete current cell
					private$.tvp[[L]]$time <- c(private$.tvp[[L]]$time, time)
					private$.tvp[[L]]$serie <- c(private$.tvp[[L]]$serie, value)
					private$.tvp[[L]]$level_hat <- c(private$.tvp[[L]]$level_hat, level_hat)
				}
			}
			if (strftime( tail(private$.tvp[[length(private$.tvp)]]$time,1),
				format="%H:%M:%S", tz="GMT" ) == "00:00:00")
			{
				private$.level = c(private$.level,
					mean(private$.tvp[[length(private$.tvp)]]$serie, na.rm=TRUE))
			}
			if (!is.null(exo))
				private$.exo[[length(private$.exo)+1]] = exo
			if (!is.null(exo_hat))
				private$.exo_hat[[length(private$.exo_hat)+1]] = exo_hat
		},
		getTime = function(index)
		{
			index = dateIndexToInteger(index, self)
			private$.tvp[[index]]$time
		},
		getSerie = function(index)
		{
			index = dateIndexToInteger(index, self)
			private$.tvp[[index]]$serie
		},
		getSeries = function(indices)
			sapply(indices, function(i) self$getSerie(i))
		,
		getLevel = function(index)
		{
			index = dateIndexToInteger(index, self)
			private$.level[index]
		},
		getLevelHat = function(index)
		{
			index = dateIndexToInteger(index, self)
			private$.tvp[[index]]$level_hat
		},
		getCenteredSerie = function(index)
		{
			index = dateIndexToInteger(index, self)
			private$.tvp[[index]]$serie - private$.level[index]
		},
		getCenteredSeries = function(indices)
			sapply(indices, function(i) self$getCenteredSerie(i))
		,
		getExo = function(index)
		{
			index = dateIndexToInteger(index, self)
			private$.exo[[index]]
		},
		getExoHat = function(index)
		{
			index = dateIndexToInteger(index, self)
			private$.exo_hat[[index]]
		}
	)
)
