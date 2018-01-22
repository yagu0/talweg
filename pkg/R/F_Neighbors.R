#' Neighbors Forecaster
#'
#' Predict next serie as a weighted combination of curves observed on "similar" days in
#' the past (and future if 'opera'=FALSE); the nature of the similarity is controlled by
#' the options 'simtype' and 'local' (see below).
#'
#' Optional arguments:
#' \itemize{
#'   \item local: TRUE (default) to constrain neighbors to be "same days in same season"
#'   \item simtype: 'endo' for a similarity based on the series only,<cr>
#'             'exo' for a similarity based on exogenous variables only,<cr>
#'             'mix' for the product of 'endo' and 'exo',<cr>
#'             'none' (default) to apply a simple average: no computed weights
#'   \item window: A window for similarities computations; override cross-validation
#'     window estimation.
#' }
#' The method is summarized as follows:
#' \enumerate{
#'   \item Determine N (=20) recent days without missing values, and preceded by a
#'     curve also without missing values.
#'   \item Optimize the window parameters (if relevant) on the N chosen days.
#'   \item Considering the optimized window, compute the neighbors (with locality
#'     constraint or not), compute their similarities -- using a gaussian kernel if
#'     simtype != "none" -- and average accordingly the "tomorrows of neigbors" to
#'     obtain the final prediction.
#' }
#'
#' @usage # NeighborsForecaster$new(pjump)
#'
#' @docType class
#' @format R6 class, inherits Forecaster
#' @aliases F_Neighbors
#'
NeighborsForecaster = R6::R6Class("NeighborsForecaster",
	inherit = Forecaster,

	public = list(
		predictShape = function(data, today, memory, predict_from, horizon, ...)
		{
			# (re)initialize computed parameters
			private$.params <- list("weights"=NA, "indices"=NA, "window"=NA)

			# Do not forecast on days with NAs (TODO: softer condition...)
			if (any(is.na(data$getSerie(today-1))) ||
				(predict_from>=2 && any(is.na(data$getSerie(today)[1:(predict_from-1)]))))
			{
				return (NA)
			}

			# Get optional args
			local = ifelse(hasArg("local"), list(...)$local, TRUE) #same level + season?
			simtype = ifelse(hasArg("simtype"), list(...)$simtype, "none") #or "endo", or "exo"
			opera = ifelse(hasArg("opera"), list(...)$opera, FALSE) #operational mode?

			# Determine indices of no-NAs days preceded by no-NAs yerstedays
			tdays = .getNoNA2(data, max(today-memory,2), ifelse(opera,today-1,data$getSize()))
			if (!opera)
				tdays = setdiff(tdays, today) #always exclude current day

			# Shortcut if window is known
			if (hasArg("window"))
			{
				return ( private$.predictShapeAux(data, tdays, today, predict_from, horizon,
					local, list(...)$window, simtype, opera, TRUE) )
			}

			# Indices of similar days for cross-validation; TODO: 20 = magic number
			cv_days = getSimilarDaysIndices(today, data, limit=20, same_season=FALSE,
				days_in=tdays, operational=opera)

			# Optimize h : h |--> sum of prediction errors on last N "similar" days
			errorOnLastNdays = function(window, simtype)
			{
				error = 0
				nb_jours = 0
				for (i in seq_along(cv_days))
				{
					# mix_strategy is never used here (simtype != "mix"), therefore left blank
					prediction = private$.predictShapeAux(data, tdays, cv_days[i], predict_from,
						horizon, local, window, simtype, opera, FALSE)
					if (!is.na(prediction[1]))
					{
						nb_jours = nb_jours + 1
						error = error +
							mean((data$getSerie(cv_days[i])[predict_from:horizon] - prediction)^2)
					}
				}
				return (error / nb_jours)
			}

			# TODO: 7 == magic number
			if (simtype=="endo" || simtype=="mix")
			{
				best_window_endo = optimize(
					errorOnLastNdays, c(0,7), simtype="endo")$minimum
			}
			if (simtype=="exo" || simtype=="mix")
			{
				best_window_exo = optimize(
					errorOnLastNdays, c(0,7), simtype="exo")$minimum
			}
			if (local)
			{
				best_window_local = optimize(
					errorOnLastNdays, c(3,30), simtype="none")$minimum
			}

			best_window =
				if (simtype == "endo")
					best_window_endo
				else if (simtype == "exo")
					best_window_exo
				else if (simtype == "mix")
					c(best_window_endo,best_window_exo)
				else #none: no value
					NULL
			if (local)
				best_window = c(best_window, best_window_local)

			return( private$.predictShapeAux(data, tdays, today, predict_from, horizon, local,
				best_window, simtype, opera, TRUE) )
		}
	),
	private = list(
		# Precondition: "yersteday until predict_from-1" is full (no NAs)
		.predictShapeAux = function(data, tdays, today, predict_from, horizon, local, window,
			simtype, opera, final_call)
		{
			tdays_cut = tdays[ tdays != today ]
			if (length(tdays_cut) == 0)
				return (NA)

			if (local)
			{
				# limit=Inf to not censor any day (TODO: finite limit? 60?)
				tdays <- getSimilarDaysIndices(today, data, limit=Inf, same_season=TRUE,
					days_in=tdays_cut, operational=opera)
				nb_neighbs <- round( window[length(window)] )
				# TODO: 10 == magic number
				tdays <- .getConstrainedNeighbs(today, data, tdays, nb_neighbs, opera)
				if (length(tdays) == 1)
				{
					if (final_call)
					{
						private$.params$weights <- 1
						private$.params$indices <- tdays
						private$.params$window <- window
					}
					return ( data$getSerie(tdays[1])[predict_from:horizon] )
				}
				max_neighbs = nb_neighbs #TODO: something else?
				if (length(tdays) > max_neighbs)
				{
					distances2 <- .computeDistsEndo(data, today, tdays, predict_from)
					ordering <- order(distances2)
					tdays <- tdays[ ordering[1:max_neighbs] ]
				}
			}
			else
				tdays = tdays_cut #no conditioning

			if (simtype == "endo" || simtype == "mix")
			{
				# Distances from last observed day to selected days in the past
				# TODO: redundant computation if local==TRUE
				distances2 <- .computeDistsEndo(data, today, tdays, predict_from)

				# Compute endogen similarities using the given window
				simils_endo <- .computeSimils(distances2, window[1])
			}

			if (simtype == "exo" || simtype == "mix")
			{
				distances2 <- .computeDistsExo(data, today, tdays, opera)

				# Compute exogen similarities using the given window
				window_exo = ifelse(simtype=="mix", window[2], window[1])
				simils_exo <- .computeSimils(distances2, window_exo)
			}

			similarities =
				if (simtype == "exo")
					simils_exo
				else if (simtype == "endo")
					simils_endo
				else if (simtype == "mix")
					simils_endo * simils_exo
				else #none
					rep(1, length(tdays))
			similarities = similarities / sum(similarities)

			prediction = rep(0, horizon-predict_from+1)
			for (i in seq_along(tdays))
			{
				prediction = prediction +
					similarities[i] * data$getSerie(tdays[i])[predict_from:horizon]
			}

			if (final_call)
			{
				private$.params$weights <- similarities
				private$.params$indices <- tdays
				private$.params$window <- window
			}

			return (prediction)
		}
	)
)

# getConstrainedNeighbs
#
# Get indices of neighbors of similar pollution level (among same season + day type).
#
# @param today Index of current day
# @param data Object of class Data
# @param tdays Current set of "second days" (no-NA pairs)
# @param min_neighbs Minimum number of points in a neighborhood
# @param max_neighbs Maximum number of points in a neighborhood
#
.getConstrainedNeighbs = function(today, data, tdays, min_neighbs, opera)
{
	levelToday = ifelse(opera, tail(data$getLevelHat(today),1), data$getLevel(today))
	distances = sapply( tdays, function(i) abs(data$getLevel(i) - levelToday) )
	#TODO: 1, +1, +3 : magic numbers
	dist_thresh = 1
	min_neighbs = min(min_neighbs,length(tdays))
	repeat
	{
		same_pollution = (distances <= dist_thresh)
		nb_neighbs = sum(same_pollution)
		if (nb_neighbs >= min_neighbs) #will eventually happen
			break
		dist_thresh = dist_thresh + ifelse(dist_thresh>1,3,1)
	}
	tdays[same_pollution]
}

# compute similarities
#
# Apply the gaussian kernel on computed squared distances.
#
# @param distances2 Squared distances
# @param window Window parameter for the kernel
#
.computeSimils <- function(distances2, window)
{
	sd_dist = sd(distances2)
	if (sd_dist < .25 * sqrt(.Machine$double.eps))
	{
#		warning("All computed distances are very close: stdev too small")
		sd_dist = 1 #mostly for tests... FIXME:
	}
	exp(-distances2/(sd_dist*window^2))
}

.computeDistsEndo <- function(data, today, tdays, predict_from)
{
	lastSerie = c( data$getSerie(today-1),
		data$getSerie(today)[if (predict_from>=2) 1:(predict_from-1) else c()] )
	sapply(tdays, function(i) {
		delta = lastSerie - c(data$getSerie(i-1),
			data$getSerie(i)[if (predict_from>=2) 1:(predict_from-1) else c()])
		sqrt(mean(delta^2))
	})
}

.computeDistsExo <- function(data, today, tdays, opera)
{
	M = matrix( ncol=1+length(tdays), nrow=1+length(data$getExo(1)) )
	if (opera)
		M[,1] = c( tail(data$getLevelHat(today),1), as.double(data$getExoHat(today)) )
	else
		M[,1] = c( data$getLevel(today), as.double(data$getExo(today)) )
	for (i in seq_along(tdays))
		M[,i+1] = c( data$getLevel(tdays[i]), as.double(data$getExo(tdays[i])) )

	sigma = cov(t(M)) #NOTE: robust covariance is way too slow
	# TODO: 10 == magic number; more robust way == det, or always ginv()
	sigma_inv =
		if (length(tdays) > 10)
			solve(sigma)
		else
			MASS::ginv(sigma)

	# Distances from last observed day to days in the past
	sapply(seq_along(tdays), function(i) {
		delta = M[,1] - M[,i+1]
		delta %*% sigma_inv %*% delta
	})
}
