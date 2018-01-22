#' getZeroJumpPredict
#'
#' "Reset level to 0": jump by -A where A is the last observed value.
#'
#' @inheritParams computeForecast
#' @param today Index of the current day (predict tomorrow)
#' @param params Optional parameters computed by the main forecaster
#'
#' @aliases J_Zero
#'
getZeroJumpPredict = function(data, today, memory, predict_from, horizon, params, ...)
{
	list(...)$first_pred - ifelse( predict_from >= 2,
		data$getSerie(today)[predict_from-1], tail(data$getSerie(today-1),1) )
}
