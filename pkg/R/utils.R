#' dateIndexToInteger
#'
#' Transform a (potential) date index into an integer (relative to data beginning).
#'
#' @param index Date (or integer) index
#' @param data Object of class Data, output of \code{getData()}
#'
#' @export
dateIndexToInteger = function(index, data)
{
	# Works on integers too: trust input
	if (is.numeric(index))
		index = as.integer(index)
	if (is.integer(index))
		return (index)

	if (inherits(index, "Date") || is.character(index))
	{
		tryCatch(indexAsDate <- as.Date(index),
			error=function(e) stop("Unrecognized index format"))
		#TODO: tz arg to difftime ?
		integerIndex <- round( as.numeric(
			difftime(indexAsDate, as.Date(data$getTime(1)[1])) ) ) + 1
		if (integerIndex >= 1 && integerIndex <= data$getSize())
			return (integerIndex)
		stop("Date outside data range")
	}
	stop("Unrecognized index format")
}

#' integerIndexToDate
#'
#' Transform an integer index (relative to data beginning) into a date index.
#'
#' @inheritParams dateIndexToInteger
#'
#' @export
integerIndexToDate = function(index, data)
{
	# Works on dates too: trust input
	if (is.character(index))
		index = as.Date(index)
	if (is(index,"Date"))
		return (index)

	index = index[1]
	if (is.numeric(index))
		index = as.integer(index)
	if (!is.integer(index))
		stop("'index' should be a date or integer")
	as.Date( data$getTime(index)[1] )
}

#' getSimilarDaysIndices
#'
#' Find similar days indices in the past; at least same type of day in the week:
#' monday=tuesday=wednesday=thursday != friday != saturday != sunday.
#'
#' @param index Day index (numeric or date)
#' @param data Reference dataset, object output of \code{getData}
#' @param limit Maximum number of indices to return
#' @param same_season Should the indices correspond to day in same season?
#' @param days_in Optional set to intersect with results (NULL to discard)
#' @param operational If TRUE: do not look for days after index (operational context)
#'
#' @export
getSimilarDaysIndices = function(index, data, limit, same_season,
	days_in=NULL, operational=TRUE)
{
	index = dateIndexToInteger(index, data)

	# Look for similar days (optionally in same season)
	days = c()
	dt_ref = as.POSIXlt(data$getTime(index)[1]) #first date-time of current day
	day_ref = dt_ref$wday #1=monday, ..., 6=saturday, 0=sunday
	month_ref = as.POSIXlt(data$getTime(index)[1])$mon+1 #month in 1...12
	i = index - 1
	if (!operational)
		j = index + 1
	while (length(days) < min( limit, ifelse(is.null(days_in),Inf,length(days_in)) ))
	{
		if (i < 1 && j > data$getSize())
			break
		if (i >= 1)
		{
			dt = as.POSIXlt(data$getTime(i)[1])
			if ((is.null(days_in) || i %in% days_in) && .isSameDay(dt$wday, day_ref))
			{
				if (!same_season || .isSameSeason(dt$mon+1, month_ref))
					days = c(days, i)
			}
			i = i - 1
		}
		if (!operational && j <= data$getSize())
		{
			dt = as.POSIXlt(data$getTime(j)[1])
			if ((is.null(days_in) || j %in% days_in) && .isSameDay(dt$wday, day_ref))
			{
				if (!same_season || .isSameSeason(dt$mon+1, month_ref))
					days = c(days, j)
			}
			j = j + 1
		}
	}
	return ( days )
}

# isSameSeason
#
# Check if two months fall in the same "season" (defined by estimated pollution rate).
#
# @param month Month index to test
# @param month_ref Month to compare to
#
.isSameSeason = function(month, month_ref)
{
#	if (month_ref == 3) #TODO: same as Bruno (but weird)
#		return (month %in% c(2,3,4,9,10))
	if (month_ref %in% c(11,12,1,2)) #~= mid-polluted
		return (month %in% c(11,12,1,2))
	if (month_ref %in% c(3,4,9,10)) #~= high-polluted
		return (month %in% c(3,4,9,10))
	return (month %in% c(5,6,7,8)) #~= non polluted
}

# isSameDay
#
# Monday=Tuesday=Wednesday=Thursday ; Friday, Saturday, Sunday: specials.
#
# @param day Day index to test
# @param day_ref Day index to compare to
#
.isSameDay = function(day, day_ref)
{
	if (day_ref %in% 1:4)
		return (day %in% 1:4)
	return (day == day_ref)
}

# getNoNA2
#
# Get indices in data of no-NA series preceded by no-NA, within [first,last] range.
#
# @inheritParams dateIndexToInteger
# @param first First index (included)
# @param last Last index (included)
#
.getNoNA2 = function(data, first, last)
{
	(first:last)[ sapply(first:last, function(i)
		!any( is.na(data$getSerie(i-1)) | is.na(data$getSerie(i)) )
	) ]
}
