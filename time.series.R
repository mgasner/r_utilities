# February 26, 2011
# Max Gasner <max@naviasystems.com>
#
# R utilities to convert between time series representations and to shape tables
# appropriately for time series analysis with Veritable.
#
# as.return.series converts a time series vector from the form (..., p_{t-1},
# p_t, ...) to the form (..., (p_{t-1} - p_{t-2})/p_{t-2}, (p_t -
# p_{t-1})/p_{t-1}, ...). This function drops the first datapoint. If NA values
# appear in the series, it will calculate the return of each datapoint based on
# the last observed value.
#
# as.log.return.series converts a time series vector to the form (...,
# ln{p_{t-1}/p_{t-2}}, ln{p_t/p_{t-1}}, ...). Thus function drops the first
# datapoint and handles NAs like as.return.series
#
# as.diff.series converts a time series vector to the form (..., p_{t-1} -
# p_{t-2}, p_t - p_{t-1}, ...)
#
# as.raw.series reconverts a time series from any of these forms to the original
# series. The start argument determines the initial value. Set type = "log" to
# convert from log returns, and type = "diff" to convert from raw differences;
# by default, type = "return".
#
# convert.ts.df converts the time series contained in selected columns of a
# between representations. By default, convert.ts.df will change all of the
# columns of its argument from raw series to return series. Set the arguments
# raw.to.return, raw.to.log, raw.to.diff, return.to.raw, log.to.raw, and
# diff.to.raw to specify which columns to convert. The first three of these
# arguments expect a vector of column names or indices; the second three expect
# a two-column data.frame, the first column of which is a vector of column names
# or indices, and the second column of which is a vector of initial values.

as.return.series <- function(ts) {
	convert.ts(ts, function(ts, last) {(ts[i] - last)/last})
}

as.log.ratio.series <- function(ts) {
	convert.ts(ts, function(ts, last) {log(ts[i]/last)})
}

as.diff.series <- function(ts) {
	convert.ts(ts, function(ts, last) {ts[i] - last})
}

as.raw.series <- function(ts, start) {
	convert.ts(ts, function(ts, last) {ts[i] - last}, start)

	rs <- vector(mode = "numeric", length = length(ts))
	rs[1] <- start
	for (i in seq_along(ts)) {
		if (i > 1) {
		
		}
	}
}

convert.ts <- function(ts, fun, start = NULL) {
	rs <- vector(mode = "numeric", length = length(ts))
	last <- NA
	if (is.null(start)) {
		for(i in seq_along(ts)) {
			if (! is.na(ts[i])) {
				if (is.na(last)) {
					rs[i] <- NA
					last <- rs[i]
				} else {
					rs[i] <- fun(ts[i], last)
					last <- ts[i]
				}
			}
		}
	} else {
	}
	rs
}

convert.ts.df <- function(df,
					      raw.to.return = NULL, 
					      raw.to.log = NULL,
					      raw.to.diff = NULL,
					      return.to.raw = NULL,
					      log.to.raw = NULL,
						  diff.to.raw = NULL) {
	if (is.null(raw.to.return) && is.null(raw.to.return) &&
	    is.null(raw.to.return) && is.null(raw.to.return) &&
	    is.null(raw.to.return) && is.null(raw.to.return)) {
		for (i in seq_along(names(df))) {
			df[,i] <- as.return.series(df[,i])
		}
	} else {
		for (i in seq_along(raw.to.return) {
		}
		for (i in seq_along(raw.to.return) {
		}
		for (i in seq_along(raw.to.return) {
		}
		for (i in seq_along(raw.to.return) {
		}
		for (i in seq_along(raw.to.return) {
		}
		for (i in seq_along(raw.to.return) {
		}
	}
	df
}
