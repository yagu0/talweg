#' Persistence Forecaster
#'
#' Look for the most recent similar day in the past, and return its corresponding curve.
#'
#' There are two variations, depending whether "similar day" means "same day in the week"
#' or "most recent day" (regardless of day type). The corresponding argument is named
#' "same_day": a value of TRUE implies the former interpretation (same day in week).
#' If the last similar day has missing values, the next one is searched, and so on until
#' one full serie is found (if no one is found, NA is returned).
#'
#' @usage # PersistenceForecaster$new(pjump)
#'
#' @docType class
#' @format R6 class, inherits Forecaster
#' @aliases F_Persistence
#'
PersistenceForecaster = R6::R6Class("PersistenceForecaster",
	inherit = Forecaster,

	public = list(
		predictShape = function(data, today, memory, predict_from, horizon, ...)
		{
			# Return centered last (similar) day curve, avoiding NAs until memory is run
			first_day = max(1, today-memory)
			same_day = ifelse(hasArg("same_day"), list(...)$same_day, TRUE)
			index <- today
			repeat
			{
				# If 'same_day', get the last known future of similar day
				index = index - ifelse(same_day,7,1)
				if (index < first_day)
					return (NA)
				last_serie = data$getCenteredSerie(index)[predict_from:horizon]
				if (!any(is.na(last_serie)))
					return (last_serie)
			}
		}
	)
)
