#' getNeighborsJumpPredict
#'
#' Apply optimized weights on gaps observed on selected neighbors.
#' This jump prediction method can only be used in conjunction with the Neighbors
#' Forecaster, because it makes use of the optimized parameters to re-apply the weights
#' on the jumps observed at days interfaces of the past neighbors.
#'
#' @inheritParams computeForecast
#' @inheritParams getZeroJumpPredict
#'
#' @aliases J_Neighbors
#'
getNeighborsJumpPredict = function(data, today, memory, predict_from, horizon,
	params, ...)
{
	first_day = max(1, today-memory)
	filter = (params$indices >= first_day)
	indices = params$indices[filter]
	weights = params$weights[filter]

	if (is.na(indices[1]))
		return (NA)

	gaps = sapply(indices, function(i) {
		if (predict_from >= 2)
			data$getSerie(i)[predict_from] - data$getSerie(i)[predict_from-1]
		else
			head(data$getSerie(i),1) - tail(data$getSerie(i-1),1)
	})
	scal_product = weights * gaps
	norm_fact = sum( weights[!is.na(scal_product)] )
	sum(scal_product, na.rm=TRUE) / norm_fact
}
