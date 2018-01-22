#' Forecaster
#'
#' Forecaster (abstract class, implemented by all forecasters).
#'
#' A Forecaster object encapsulates parameters (which can be of various kinds, for
#' example "Neighbors" method stores informations about the considered neighborhood for
#' the current prediction task) and one main function: \code{predictSerie()}. This last
#' function (by default) calls \code{predictShape()} to get a forecast of a centered
#' serie, and then calls the "jump prediction" function if it's provided -- see "field"
#' section -- to adjust it based on the last observed values. The main method in derived
#' forecasters is \code{predictShape()}; see 'Methods' section.
#'
#' @usage # Forecaster$new(pjump) #warning: predictShape() is unimplemented
#'
#' @field .params List of computed parameters (if applicable).
#' @field .pjump Function: how to predict the jump at day interface? The arguments of
#'   this function are -- in this order:
#'   \itemize{
#'     \item data: object output of \code{getData()},
#'     \item today: index of the current day in data (known until predict_from-1),
#'     \item memory: number of days to use in the past (including today),
#'     \item predict_from: first time step to predict (in [1,24])
#'     \item horizon: last time step to predict (in [predict_from,24]),
#'     \item params: optimized parameters in the main method \code{predictShape()},
#'     \item ...: additional arguments.
#'   }
#'   .pjump returns an estimation of the jump after the last observed value.
#'
#' @section Methods:
#' \describe{
#' \item{\code{initialize(pjump)}}{
#'   Initialize a Forecaster object with a jump prediction function.}
#' \item{\code{predictSerie(data,today,memory,predict_from,horizon,...)}}{
#'   Predict the next curve (at index today) from predict_from to horizon (hours), using
#'   \code{memory} days in the past.}
#' \item{\code{predictShape(data,today,memory,predict_from,horizon,...)}}{
#'   Predict the shape of the next curve (at index today) from predict_from to horizon
#'   (hours), using \code{memory} days in the past.}
#' \item{\code{getParameters()}}{
#'   Return (internal) parameters.}
#' }
#'
#' @docType class
#' @format R6 class
#'
Forecaster = R6::R6Class("Forecaster",
	private = list(
		.params = list(),
		.pjump = NULL
	),
	public = list(
		initialize = function(pjump)
		{
			private$.pjump <- pjump
			invisible(self)
		},
		predictSerie = function(data, today, memory, predict_from, horizon, ...)
		{
			# Parameters (potentially) computed during shape prediction stage
			predicted_shape <- self$predictShape(data,today,memory,predict_from,horizon,...)

			if (is.na(predicted_shape[1]))
				return (NA)

			predicted_delta <- private$.pjump(data, today, memory, predict_from,
					horizon, private$.params, first_pred=predicted_shape[1], ...)

			# Predicted shape is aligned on the end of current day + jump
			c( data$getSerie(today)[if (predict_from>=2) 1:(predict_from-1) else c()],
				(predicted_shape - predicted_shape[1]) + #shape with first_pred = 0
				ifelse(predict_from>=2, #last observed value
					data$getSerie(today)[predict_from-1], tail(data$getSerie(today-1),1)) +
				predicted_delta ) #jump
		},
		predictShape = function(data, today, memory, predict_from, horizon, ...)
			NULL #empty default implementation: to implement in inherited classes
		,
		getParameters = function()
			private$.params
	)
)
