#' Average Forecaster
#'
#' Pointwise average of all the series of the same day of week in the past.
#'
#' For example, if the current day (argument "today") is a tuesday, then all series
#' corresponding to tuesday in the past (until the beginning or memory limit) -- and in
#' the future if 'opera' is FALSE -- are averaged to provide a smooth prediction. This
#' forecast will most of the time be wrong, but will also look plausible enough.
#'
#' @usage # AverageForecaster$new(pjump)
#'
#' @docType class
#' @format R6 class, inherits Forecaster
#' @aliases F_Average
#'
AverageForecaster = R6::R6Class("AverageForecaster",
	inherit = Forecaster,

	public = list(
		predictShape = function(data, today, memory, predict_from, horizon, ...)
		{
			avg = rep(0., (horizon-predict_from+1))
			first_day = max(1, today-memory)
			index <- today
			nb_no_na_series = 0
			opera = ifelse(hasArg("opera"), list(...)$opera, FALSE)
			repeat
			{
				index = index - 7
				if (index < first_day)
					break
				serie_on_horizon = data$getCenteredSerie(index)[predict_from:horizon]
				if (!any(is.na(serie_on_horizon)))
				{
					avg = avg + serie_on_horizon
					nb_no_na_series = nb_no_na_series + 1
				}
			}
			if (!opera)
			{
				# The same, in the future
				index <- today
				repeat
				{
					index = index + 7
					if (index > data$getSize())
						break
					serie_on_horizon = data$getCenteredSerie(index)[predict_from:horizon]
					if (!any(is.na(serie_on_horizon)))
					{
						avg = avg + serie_on_horizon
						nb_no_na_series = nb_no_na_series + 1
					}
				}
			}
			avg / nb_no_na_series
		}
	)
)
