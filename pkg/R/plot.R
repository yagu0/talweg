#' Plot curves
#'
#' Plot a range of curves in data.
#'
#' @inheritParams computeError
#' @param indices Range of indices (integers or dates)
#'
#' @export
plotCurves <- function(data, indices=seq_len(data$getSize()))
{
	series = data$getSeries(indices)
	yrange = quantile(series, probs=c(0.025,0.975), na.rm=TRUE)
	par(mar=c(4.7,5,1,1), cex.axis=1.5, cex.lab=1.5)
	matplot(series, type="l", ylim=yrange, xlab="Time (hours)", ylab="PM10")
}

#' Plot error
#'
#' Draw error graphs, potentially from several runs of \code{computeForecast()}.
#'
#' @param err Error as returned by \code{computeError()}
#' @param cols Colors for each error (default: 1,2,3,...)
#' @param agg Aggregation level ("day", "week" or "month")
#'
#' @seealso \code{\link{plotCurves}}, \code{\link{plotPredReal}},
#'   \code{\link{plotSimils}}, \code{\link{plotFbox}}, \code{\link{computeFilaments}},
#'   \code{\link{plotFilamentsBox}}, \code{\link{plotRelVar}}
#'
#' @export
plotError <- function(err, cols=seq_along(err), agg="day")
{
	if (!is.null(err$abs))
		err = list(err)
	par(mfrow=c(2,2), mar=c(4.7,5,1,1), cex.axis=1.5, cex.lab=1.5)
	L = length(err)

	yrange = range( sapply(1:L, function(i) err[[i]]$abs$day), na.rm=TRUE )
	matplot(sapply( seq_len(L), function(i) err[[i]]$abs$day ), type="l",
		xlab="Time (hours)", ylab="Mean |y - y_hat|", ylim=yrange, col=cols, lwd=2, lty=1)

	agg_curves <- sapply( seq_len(L), function(i) {
		curve <- err[[i]]$abs$indices
		delta <- if (agg=="day") 1 else if (agg=="week") 7 else if (agg=="month") 30
		vapply( seq(1,length(curve),delta), function(i) {
			mean(curve[i:(i+delta-1)], na.rm=TRUE)
		}, vector("double",1), USE.NAMES=FALSE )
	})
	yrange = range(agg_curves, na.rm=TRUE)
	matplot(agg_curves, type="l", xlab=paste("Time (",agg,"s)", sep=""),
		ylab="Mean |y - y_hat|", ylim=yrange, col=cols, lwd=2, lty=1)

	yrange = range( sapply(1:L, function(i) err[[i]]$MAPE$day), na.rm=TRUE )
	matplot(sapply( seq_len(L), function(i) err[[i]]$MAPE$day ), type="l",
		xlab="Time (hours)", ylab="Mean MAPE", ylim=yrange, col=cols, lwd=2, lty=1)

	agg_curves <- sapply( seq_len(L), function(i) {
		curve <- err[[i]]$MAPE$indices
		delta <- if (agg=="day") 1 else if (agg=="week") 7 else if (agg=="month") 30
		vapply( seq(1,length(curve),delta), function(i) {
			mean(curve[i:(i+delta-1)], na.rm=TRUE)
		}, vector("double",1), USE.NAMES=FALSE )
	})
	yrange = range(agg_curves, na.rm=TRUE)
	matplot(agg_curves, type="l", xlab=paste("Time (",agg,"s)", sep=""),
		ylab="Mean MAPE", ylim=yrange, col=cols, lwd=2, lty=1)
}

#' Plot measured / predicted
#'
#' Plot measured curve (in black) and predicted curve (in blue).
#'
#' @inheritParams computeError
#' @param index Index in forecasts (integer or date)
#'
#' @export
plotPredReal <- function(data, pred, index)
{
	prediction = pred$getForecast(index)
	measure = data$getSerie( pred$getIndexInData(index) )[1:length(pred$getForecast(1))]

	# Remove the common part, where prediction == measure
	dot_mark <- ifelse(prediction[1]==measure[1],
		which.max(seq_along(prediction)[prediction==measure]), 0)
	prediction = prediction[(dot_mark+1):length(prediction)]
	measure = measure[(dot_mark+1):length(measure)]

	yrange = range(measure, prediction)
	par(mar=c(4.7,5,1,1), cex.axis=1.5, cex.lab=1.5, lwd=3)
	plot(measure, type="l", ylim=yrange, xlab="Time (hours)", ylab="PM10")
	par(new=TRUE)
	plot(prediction, type="l", col="#0000FF", ylim=yrange, xlab="", ylab="")
}

#' Plot similarities
#'
#' Plot histogram of similarities (weights), for 'Neighbors' method.
#'
#' @inheritParams computeError
#' @param index Index in forecasts (integer or date)
#'
#' @export
plotSimils <- function(pred, index)
{
	weights = pred$getParams(index)$weights
	if (is.null(weights))
		stop("plotSimils only works on 'Neighbors' forecasts")
	par(mfrow=c(1,2), mar=c(4.7,5,1,1), cex.axis=1.5, cex.lab=1.5)
	small_weights = weights[ weights < 1/length(weights) ]
	large_weights = weights[ weights >= 1/length(weights) ]
	hist(small_weights, nclass=25, main="", xlab="Weight < 1/N", ylab="Count")
	hist(large_weights, nclass=25, main="", xlab="Weight >= 1/N", ylab="Count")
}

#' Functional boxplot
#'
#' Draw the functional boxplot on the left, and bivariate plot on the right.
#'
#' @inheritParams computeError
#' @inheritParams plotCurves
#'
#' @export
plotFbox <- function(data, indices=seq_len(data$getSize()))
{
	if (!requireNamespace("rainbow", quietly=TRUE))
		stop("Functional boxplot requires the rainbow package")

	series_matrix = data$getSeries(indices)
	# Remove series with NAs
	no_NAs_indices = sapply( 1:ncol(series_matrix),
		function(i) all(!is.na(series_matrix[,i])) )
	series_matrix = series_matrix[,no_NAs_indices]

	series_fds = rainbow::fds(seq_len(nrow(series_matrix)), series_matrix)
	par(mar=c(4.7,5,1,1), cex.axis=1.5, cex.lab=1.5)
	rainbow::fboxplot(series_fds, "functional", "hdr", xlab="Time (hours)", ylab="PM10",
		plotlegend=FALSE, lwd=2)
	rainbow::fboxplot(series_fds, "bivariate", "hdr", plotlegend=FALSE)
}

#' Compute filaments
#'
#' Obtain similar days in the past, and (optionally) plot them -- as black as distances
#' are small.
#'
#' @inheritParams computeError
#' @param index Index in forecast (integer or date)
#' @param limit Number of neighbors to consider
#' @param plot Should the result be plotted?
#'
#' @return A list with
#' \itemize{
#'   \item index : index of the current serie ('today')
#'   \item neighb_indices : indices of its neighbors
#'   \item colors : colors of neighbors curves (shades of gray)
#' }
#'
#' @export
computeFilaments <- function(data, pred, index, limit=60, plot=TRUE)
{
	weights <- pred$getParams(index)$weights
	if (is.null(weights) || is.na(pred$getParams(index)$weights[1]))
		stop("computeFilaments requires a serie without NAs")

	nn <- min(limit, length(weights))
	sorted_dists = sort(-log(weights), index.return=TRUE)
	# Compute colors for each neighbor (from darkest to lightest), if weights differ
	if ( any( weights != weights[1] ) )
	{
		min_dist = min(sorted_dists$x[1:nn])
		max_dist = max(sorted_dists$x[1:nn])
		color_values = floor(19.5*(sorted_dists$x[1:nn]-min_dist)/(max_dist-min_dist)) + 1
		colors = gray.colors(20,0.1,0.9)[color_values] #TODO: 20 == magic number
	}
	else
		colors <- rep(colors()[17], length(weights))

	if (plot)
	{
		# Complete series with (past and present) tomorrows
		ref_serie = c( data$getCenteredSerie( pred$getIndexInData(index)-1 ),
			data$getCenteredSerie( pred$getIndexInData(index) ) )
		centered_series = rbind(
			data$getCenteredSeries( pred$getParams(index)$indices-1 ),
			data$getCenteredSeries( pred$getParams(index)$indices ) )
		yrange = range( ref_serie,
			quantile(centered_series, probs=c(0.025,0.975), na.rm=TRUE) )
		par(mar=c(4.7,5,1,1), cex.axis=1.5, cex.lab=1.5, lwd=2)
		for (i in nn:1)
		{
			plot(centered_series[,sorted_dists$ix[i]], ylim=yrange, type="l", col=colors[i],
				xlab=ifelse(i==1,"Time (hours)",""), ylab=ifelse(i==1,"Centered PM10",""))
			par(new=TRUE)
		}
		# Also plot ref curve, in red
		plot(ref_serie, ylim=yrange, type="l", col="#FF0000", xlab="", ylab="")
		dot_mark <- 0.5 + which.max( pred$getForecast(1) ==
			data$getSerie( pred$getIndexInData(1) )[1:length(pred$getForecast(1))] )
		abline(v=24+dot_mark, lty=2, col=colors()[56], lwd=1)
	}

	list(
		"index"=pred$getIndexInData(index),
		"neighb_indices"=pred$getParams(index)$indices[sorted_dists$ix[1:nn]],
		"colors"=colors)
}

#' Functional boxplot on filaments
#'
#' Draw the functional boxplot on filaments obtained by \code{computeFilaments()}.
#'
#' @inheritParams computeError
#' @param fil Output of \code{computeFilaments}
#' @param predict_from First predicted time step
#'
#' @export
plotFilamentsBox = function(data, fil, predict_from)
{
	if (!requireNamespace("rainbow", quietly=TRUE))
		stop("Functional boxplot requires the rainbow package")

	series_matrix = rbind(
		data$getSeries(fil$neighb_indices-1), data$getSeries(fil$neighb_indices) )
	series_fds = rainbow::fds(seq_len(nrow(series_matrix)), series_matrix)

	par(mar=c(4.7,5,1,1), cex.axis=1.5, cex.lab=1.5)
	rainbow::fboxplot(series_fds, "functional", "hdr", xlab="Time (hours)", ylab="PM10",
		plotlegend=FALSE, lwd=2)

	# "Magic": http://stackoverflow.com/questions/13842560/get-xlim-from-a-plot-in-r
	usr <- par("usr")
	yr <- (usr[4] - usr[3]) / 27
	par(new=TRUE)
	plot(c(data$getSerie(fil$index-1),data$getSerie(fil$index)), type="l", lwd=2, lty=2,
		ylim=c(usr[3] + yr, usr[4] - yr), xlab="", ylab="")
	abline(v=24+predict_from-0.5, lty=2, col=colors()[56])
}

#' Plot relative conditional variability / absolute variability
#'
#' Draw the relative conditional variability / absolute variability based on filaments
#' obtained by \code{computeFilaments()}.
#'
#' @inheritParams computeError
#' @inheritParams plotFilamentsBox
#'
#' @export
plotRelVar = function(data, fil, predict_from)
{
	ref_var = c( apply(data$getSeries(fil$neighb_indices-1),1,sd),
		apply(data$getSeries(fil$neighb_indices),1,sd) )
	tdays = .getNoNA2(data, 2, fil$index)
	global_var = c(
		apply(data$getSeries(tdays-1),1,sd),
		apply(data$getSeries(tdays),1,sd) )

	yrange = range(ref_var, global_var)
	par(mar=c(4.7,5,1,1), cex.axis=1.5, cex.lab=1.5)
	plot(ref_var, type="l", col=1, lwd=3, ylim=yrange,
		xlab="Time (hours)", ylab="Standard deviation")
	par(new=TRUE)
	plot(global_var, type="l", col=2, lwd=3, ylim=yrange, xlab="", ylab="")
	abline(v=24+predict_from-0.5, lty=2, col=colors()[56])
}
