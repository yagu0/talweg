#' Compute forecast
#'
#' Predict time-series curves ("today" from predict_from to horizon) at the selected days
#' indices ("today" from 1am to predict_from-1). This function just runs a loop over all
#' requested indices, and stores the individual forecasts into a Forecast object.
#' Note: in training stage ts_hat(day+1) = f(ts(day), exo(day+1)),
#' and in production ts_hat(day+1) = f(ts(day), exo_hat(day+1))
#'
#' @param data Object of class Data, output of \code{getData()}.
#' @param indices Indices where to forecast (the day after); integers relative to the
#'   beginning of data, or (convertible to) Date objects.
#' @param forecaster Name of the main forecaster; more details: ?F_<forecastername>
#'   \itemize{
#'     \item Persistence : use last (similar) day
#'     \item Neighbors : weighted similar days
#'     \item Average : average curve of all same day-in-week
#'     \item Zero : just output 0 (benchmarking purpose)
#'   }
#' @param pjump Function to predict the jump at the interface between two days;
#'   more details: ?J_<functionname>
#'   \itemize{
#'     \item Persistence : use last (similar) day
#'     \item Neighbors: re-use the weights from F_Neighbors
#'     \item LastValue: start serie with last observed value
#'     \item Zero: no adjustment => use shape prediction only
#'   }
#' @param predict_from First time step to predict.
#' @param memory Data depth (in days) to be used for prediction.
#' @param horizon Last time step to predict.
#' @param ncores Number of cores for parallel execution (1 to disable).
#' @param verbose TRUE to print basic traces (runs beginnings)
#' @param ... Additional parameters for the forecasting models.
#'
#' @return An object of class Forecast
#'
#' @examples
#' ts_data <- system.file("extdata","intraday_measures.csv",package="talweg")
#' exo_data <- system.file("extdata","daily_exogenous.csv",package="talweg")
#' data <- getData(ts_data, exo_data, date_format="%Y-%m-%d %H:%M:%S", limit=200)
#' pred <- computeForecast(data, 100:130, "Persistence", "LastValue",
#'   predict_from=8, memory=50, horizon=12, ncores=1)
#' \dontrun{
#' #Sketch for real-time mode:
#' data <- Data$new()
#' forecaster <- MyForecaster$new(myJumpPredictFunc)
#' repeat {
#'   # As soon as daily predictions are available:
#'   data$append(
#'     level_hat=predicted_level,
#'     exo_hat=predicted_exogenous)
#'   # When a day ends:
#'   data$append(
#'     level=observed_level,
#'     exo=observed_exogenous)
#'   # And, at every hour:
#'   data$append(
#'     time=current_hour,
#'     value=current_PM10)
#'   # Finally, a bit before predict_from hour:
#'   pred <- forecaster$predictSerie(data, data$getSize(), ...)
#'   #do_something_with_pred
#' } }
#' @export
computeForecast = function(data, indices, forecaster, pjump, predict_from,
	memory=Inf, horizon=length(data$getSerie(1)), ncores=3, verbose=FALSE, ...)
{
	# (basic) Arguments sanity checks
	predict_from = as.integer(predict_from)[1]
	if (! predict_from %in% 1:length(data$getSerie(1)))
		stop("predict_from in [1,24] (hours)")
	if (hasArg("opera") && !list(...)$opera && memory < Inf)
		memory <- Inf #finite memory in training mode makes no sense
	horizon = as.integer(horizon)[1]
	if (horizon<=predict_from || horizon>length(data$getSerie(1)))
		stop("Horizon in [predict_from+1,24] (hours)")
	integer_indices = sapply(indices, function(i) dateIndexToInteger(i,data))
	if (any(integer_indices<=0 | integer_indices>data$getSize()))
		stop("Indices out of range")
	if (!is.character(forecaster))
		stop("forecaster (name): character")
	if (!is.character(pjump))
		stop("pjump (function): character")

	pred = Forecast$new( sapply(indices, function(i) integerIndexToDate(i,data)) )
	forecaster_class_name = getFromNamespace(
		paste(forecaster,"Forecaster",sep=""), "talweg")

	pjump <- getFromNamespace(paste("get",pjump,"JumpPredict",sep=""), "talweg")
	forecaster = forecaster_class_name$new(pjump)

	computeOneForecast <- function(i)
	{
		if (verbose)
			print(paste("Index",i))
		list(
			"forecast" = forecaster$predictSerie(data,i,memory,predict_from,horizon,...),
			"params" = forecaster$getParameters(),
			"index" = i )
	}

	p <-
		if (ncores > 1 && requireNamespace("parallel",quietly=TRUE))
			parallel::mclapply(integer_indices, computeOneForecast, mc.cores=ncores)
		else
			lapply(integer_indices, computeOneForecast)

	# TODO: find a way to fill pred in //...
	for (i in seq_along(integer_indices))
	{
		pred$append(
			forecast = p[[i]]$forecast,
			params = p[[i]]$params,
			index_in_data = p[[i]]$index
		)
	}
	pred
}
