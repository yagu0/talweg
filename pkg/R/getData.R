#' getData
#'
#' Acquire data as a Data object; see ?Data.
#'
#' Since series are given in columns (database format), this function builds series one
#' by one and incrementally grows a Data object which is finally returned.
#'
#' @param ts_data Time-series, as a data frame (DB style: 2 columns, first is date/time,
#'   second is value) or a CSV file.
#' @param exo_data Exogenous variables, as a data frame or a CSV file; first column is
#'   dates, next block are measurements for the day, and final block are exogenous
#'   forecasts (for the same day).
#' @param date_format How date/time are stored (e.g. year/month/day hour:minutes;
#'   see ?strptime)
#' @param limit Number of days to extract (default: Inf, for "all")
#'
#' @return An object of class Data
#'
#' @examples
#' ts_data = read.csv(system.file("extdata","pm10_mesures_H_loc.csv",package="talweg"))
#' exo_data = read.csv(system.file("extdata","meteo_extra_noNAs.csv",package="talweg"))
#' data = getData(ts_data, exo_data, limit=120)
#' @export
getData = function(ts_data, exo_data, date_format="%d/%m/%Y %H:%M", limit=Inf)
{
	# Sanity checks (not full, but sufficient at this stage)
	if ( (!is.data.frame(ts_data) && !is.character(ts_data)) ||
			(!is.data.frame(exo_data) && !is.character(exo_data)) )
		stop("Bad time-series / exogenous input (data frame or CSV file)")
	if (is.character(ts_data))
		ts_data = ts_data[1]
	if (is.character(exo_data))
		exo_data = exo_data[1]
	if (!is.character(date_format))
		stop("Bad date_format (character)")
	date_format = date_format[1]
	if (!is.numeric(limit) || limit < 0)
		stop("limit: positive integer")

	ts_df =
		if (is.character(ts_data))
			read.csv(ts_data)
		else
			ts_data
	# Convert to GMT (pretend it's GMT; no impact)
	dates_POSIXlt = strptime(as.character(ts_df[,1]), date_format, tz="GMT")
	ts_df[,1] = format(as.POSIXct(dates_POSIXlt, tz="GMT"), tz="GMT", usetz=TRUE)

	exo_df =
		if (is.character(exo_data))
			read.csv(exo_data)
		else
			exo_data
	# Times in exogenous variables file are ignored: no conversions required

	line = 1 #index in PM10 file (24 lines for 1 cell)
	nb_lines = nrow(ts_df)
	nb_exos = ( ncol(exo_df) - 1 ) / 2
	data = Data$new()
	i = 1 #index of a cell in data
	while (line <= nb_lines)
	{
		time = c()
		serie = c()
		level_hat = c()
		repeat
		{
			{
				time = c(time, ts_df[line,1])
				serie = c(serie, ts_df[line,2])
				level_hat = c(level_hat, #if data file is incomplete...
					ifelse(ncol(ts_df) > 2, ts_df[line,3], mean(serie,na.rm=TRUE)))
				line = line + 1
			};
			if (line >= nb_lines + 1
				|| as.POSIXlt(ts_df[line-1,1],tz="GMT")$hour == 0)
			{
				break
			}
		}

		data$append(time=time, value=serie, level_hat=level_hat,
			exo=exo_df[i,2:(1+nb_exos)], exo_hat=exo_df[i,(1+nb_exos+1):(1+2*nb_exos)])
		if (i >= limit)
			break
		i = i + 1
	}
	data
}
