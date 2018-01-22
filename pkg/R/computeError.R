#' Compute error
#'
#' Compute the errors between forecasted and measured series.
#'
#' @param data Object of class \code{Data} output of \code{getData}
#' @param pred Object of class \code{Forecast} output of \code{computeForecast}
#' @param predict_from First time step to consider (>= predict_from used in
#'   \code{computeForecast()})
#' @param horizon Horizon where to compute the error (<= horizon used in
#'   \code{computeForecast})
#'
#' @return A list (abs,MAPE) of lists (day,indices). The "indices" slots contain series
#'   of size L where L is the number of predicted days; i-th value is the averaged error
#'   (absolute or MAPE) on day i. The "day" slots contain curves of errors, for each time
#'   step, averaged on the L forecasting days.
#'
#' @export
computeError = function(data, pred, predict_from, horizon=length(data$getSerie(1)))
{
	L = pred$getSize()
	mape_day = rep(0, horizon-predict_from+1)
	abs_day = rep(0, horizon-predict_from+1)
	mape_indices = rep(NA, L)
	abs_indices = rep(NA, L)

	nb_no_NA_data = 0
	for (i in seq_len(L))
	{
		index = pred$getIndexInData(i)
		serie = data$getSerie(index)[predict_from:horizon]
		forecast = pred$getForecast(i)[predict_from:horizon]
		if (!any(is.na(serie)) && !any(is.na(forecast)))
		{
			nb_no_NA_data = nb_no_NA_data + 1
			mape_increment = abs(serie - forecast) / serie
			mape_increment[is.nan(mape_increment)] = 0. # 0 / 0
			mape_increment[!is.finite(mape_increment)] = 1. # >0 / 0
			mape_day = mape_day + mape_increment
			abs_increment = abs(serie - forecast)
			abs_day = abs_day + abs_increment
			mape_indices[i] = mean(mape_increment)
			abs_indices[i] = mean(abs_increment)
		}
	}

	list(
		"abs" = list(
			"day" = abs_day / nb_no_NA_data,
			"indices" = abs_indices),
		"MAPE" = list(
			"day" = mape_day / nb_no_NA_data,
			"indices" = mape_indices) )
}
