#' Zero Forecaster
#'
#' Flat prediction: always forecast a serie of zeros (adjusted by 'pjump' function).
#'
#' @usage # ZeroForecaster$new(pjump)
#'
#' @docType class
#' @format R6 class, inherits Forecaster
#' @aliases F_Zero
#'
ZeroForecaster = R6::R6Class("ZeroForecaster",
	inherit = Forecaster,

	public = list(
		predictShape = function(data, today, memory, predict_from, horizon, ...)
			rep(0, (horizon-predict_from+1))
	)
)
