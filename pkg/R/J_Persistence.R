#' getPersistenceJumpPredict
#'
#' Analog of the PersistenceForecaster: predict the jump after last observed value either
#' by re-applying the last jump between similar day and its follower (if argument
#' "same_day" is TRUE), or by re-using the very last observed jump (when "same_day" =
#' FALSE).
#'
#' @inheritParams computeForecast
#' @inheritParams getZeroJumpPredict
#'
#' @aliases J_Persistence
#'
getPersistenceJumpPredict = function(data, today, memory, predict_from,
	horizon, params, ...)
{
	#return gap between end of similar day curve and first day of tomorrow (in the past)
	first_day = max(1, today-memory)
	same_day = ifelse(hasArg("same_day"), list(...)$same_day, TRUE)
	index <- today
	repeat
	{
		# If 'same_day', get the last known future of similar day
		index = index - ifelse(same_day,7,1)
		if (index < first_day)
			return (NA)
		gap <-
			if (predict_from >= 2)
				data$getSerie(index)[predict_from] - data$getSerie(index)[predict_from-1]
			else
				head(data$getSerie(index),1) - tail(data$getSerie(index-1),1)
		if (!is.na(gap))
			return (gap)
	}
}
