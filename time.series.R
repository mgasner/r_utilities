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
# convert.ts.df converts the time series contained in selected columns of a data
# frame between representations. By default, convert.ts.df will change all the
# columns of its argument from raw series to return series. Set the arguments
# raw.to.return, raw.to.log, raw.to.diff, return.to.raw, log.to.raw, and
# diff.to.raw to specify which columns to convert. The first three of these
# arguments expect a vector of column names or indices; the second three expect
# a two-column data.frame, the first column of which is a vector of column names
# or indices, and the second column of which is a vector of initial values.
#
# windowize.ts converts a time series vector to an (n + 1)-column data frame,
# where column m represents the (n + 1 - m) lag of the time series, padded with
# NA values. Setting the name parameter will create appropriate column names for
# the resulting data frame.
#
# windowize.df converts a data frame of time series into windowized form. If the
# vector n is of length 1, it will use the same lags for each time series;
# otherwise, it will recycle the vector. A column in the original data frame 
# named "mydata" will be transformed into n columns of the windowized data
# frame named "mydata_lag0"... "mydata_lagn". 
#
# roll.forward runs predictions on a windowized time series dataset in order 
# to forward simulate the evolution of the time series. This function expects
# column names in the "name_lag2" format. It supports joint predictions on the
# basis of any number of lags of the predicted or fixed variables, and expects 
# the variable names and lags to be specified separately. Specifically, it
# expects a character vector pf of predicted variable names, a vector pf.lags
# of the number of lags to take into account for each predicted variable, as 
# well as the arguments n, the number of predicted joint evolutions to return,
# t, the number of periods to roll predictions forward for each evolution, and 
# r, the name or number of the row of the dataset on which to base predictions, 
# in addition to the handle of the analysis on which to base predictions and the 
# original dataset. The parameter timeout may optionally be specified to control # the length of time to wait for predictions before erroring (default is 3600 
# seconds).

as.return.series <- function(ts) {
	convert.ts(ts, function(ts, last) {(ts - last)/last})
}

as.log.return.series <- function(ts) {
	convert.ts(ts, function(ts, last) {log(ts/last)})
}

as.diff.series <- function(ts) {
	convert.ts(ts, function(ts, last) {ts - last})
}

as.raw.series <- function(ts, start, type = "return") {
	if (type == "log") {
		convert.ts(ts, function(ts, last) {exp(ts + log(last))}, start)
	} else if (type == "diff") {
		convert.ts(ts, function(ts, last) {ts + last}, start)
	} else if (type == "return") {
		convert.ts(ts, function(ts, last) {last * ts + last}, start)
	} else stop(paste("Didn't recognize type", type))
}

convert.ts <- function(ts, fun, start = NULL) {
	rs <- vector(mode = "numeric", length = length(ts))
	last <- NA
	if (is.null(start)) {
		for(i in seq_along(ts)) {
			if (! is.na(ts[i])) {
				if (is.na(last)) {
					rs[i] <- NA
				} else {
					rs[i] <- fun(ts[i], last)
				}		
				last <- ts[i]
			} else {
				rs[i] <- NA
			}
		}
	} else {
		for(i in seq_along(ts)) {
			if (is.na(last)) {
				if (! is.na(ts[i])) {
					rs[i] <- fun(ts[i], start)
					last <- rs[i]
				} else {
					if (! is.na(ts[i+1])) {
						rs[i] <- start
						last <- start
					} else {
						rs[i] <- NA
					}
				}
			} else {
				if (! is.na(ts[i])) {
					rs[i] <- fun(ts[i], last)
					last <- rs[i]
				} else {
					rs[i] <- NA
				}
			}
		}
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
	if (is.null(raw.to.return) && is.null(raw.to.log) &&
	    is.null(raw.to.diff) && is.null(return.to.raw) &&
	    is.null(log.to.raw) && is.null(diff.to.raw)) {
		for (i in seq_along(names(df))) {
			df[,i] <- as.return.series(df[,i])
		}
	} else {
		for (i in seq_along(raw.to.return)) {
			df[,i] <- as.return.series(df[,i])
		}
		for (i in seq_along(raw.to.log)) {
			df[,i] <- as.log.return.series(df[,i])
		}
		for (i in seq_along(raw.to.diff)) {
			df[,i] <- as.diff.series(df[,i])
		}
		if (! is.null(return.to.raw)) for (i in 1:dim(return.to.raw)[1]) {
			df[, return.to.raw[i,1]] <- as.raw.series(df[, return.to.raw[i,1]], return.to.raw[i,2])
		}
		if (! is.null(log.to.raw)) for (i in 1:dim(log.to.raw)[1]) {
			df[, log.to.raw[i,1]] <- as.raw.series(df[, log.to.raw[i,1]], log.to.raw[i,2], type = "log")
		}
		if (! is.null(diff.to.raw)) for (i in 1:dim(diff.to.raw)[1]) {
			df[, diff.to.raw[i,1]] <- as.raw.series(df[, diff.to.raw[i,1]], diff.to.raw[i,2], type = "diff")
		}
	}
	df
}

windowize.ts <- function(ts, n, name = NULL) {
	if (n > 0) {
		ts.pad <- c(rep(NA, n), ts, rep(NA, n))
		wiz <- matrix(nrow = length(ts), ncol = (n + 1))
		colnames <- vector(mode = "character", length = (n + 1))
	
		i <- seq_along(ts)
		j <- i + n
		
		for (k in 1:(n+1))
			wiz[i,k] <- ts.pad[j + k - n - 1]
		
		wiz <- as.data.frame(wiz)
	
		l <- n:0
		colnames <- paste(name, "_lag", l, sep = "")
		names(wiz) <- colnames
		wiz
	} else {
		wiz <- data.frame(ts)
		names(wiz) <- paste(name, "_lag0")
		wiz
	}
}

windowize.df <- function(df, n) {
	n <- rep(n, length.out = dim(df)[2])
	wf <- windowize.ts(df[,1], n[1], names(df)[1])
	if (dim(df)[2] > 1) {
		for (i in 2:dim(df)[2]) {
			wf <- cbind(wf, windowize.ts(df[,i], n[i], names(df)[i]))
		}
	}
	wf
}

roll.forward <- function(handle, dataset, pf, pf.lags, n, t, r,
						 timeout = 3600,
						 fixed.rows.limit = 5000,
						 fixed.features.limit = 5000,
						 predicted.features.limit = 5000,
						 fixed.cells.limit = 1000000,
						 predicted.cells.limit = 2000000) {

	pf.lags <- rep(pf.lags, length.out = length(pf))
	
	get.predicted <- function(dataset, pf) {
		predicted <- c()
		for (i in pf) {
			if (paste(i, "_lag0", sep = "") %in% features(dataset)) {
				predicted <- c(predicted, paste(i, "_lag0", sep = ""))
			} else stop(paste("Couldn't find predicted feature", i))
		}
		predicted
	}
	
	get.fixed <- function(dataset, pf, pf.lags, ff, ff.lags) {
		fixed <- c()
		for (i in seq_along(pf)) {
			if (pf.lags[i] > 0) {
				for (j in 1:pf.lags[i]) {
					fixed <- c(fixed, paste(pf[i], "_lag", j, sep = ""))
				}
			}
		}
		fixed <- dataset[r, fixed]
		fixed
	}
	
	shift.back <- function(dataset, pf, pf.lags, ff, ff.lags) {
		df <- dataset@data
		for (i in seq_along(pf)) {
			if (pf.lags[i] > 0) {
				for (j in pf.lags[i]:1) {
					df[, paste(pf[i], "_lag", j, sep = "")] <- df[, paste(pf[i], "_lag", j - 1, sep = "")]
				}
			} 
			df[,paste(pf[i], "_lag", 0, sep = "")] <- NA
		}
		shift <- as.veritable.dataset(df)
		datatypes(shift) <- datatypes(dataset)
		shift
	}

	predicted <- get.predicted(dataset, pf)
	fixed <- get.fixed(dataset, pf, pf.lags, ff, ff.lags)
	
	# right now this doesn't work to predict off of NA values
	handles <- list()
	results <- list()
	result.series <- list()
	
	handles[[1]] <- start.predictions(handle, dataset, n, fixed, predicted)
	results[[1]] <- wait.for.predictions(handles[[1]], dataset, timeout)
	for (i in predicted) {
		result.series[[i]] <- list()
		for (j in 1:n)
		result.series[[i]][[j]] <- c(results[[1]][j,i])
	}

	for (i in 2:t) {
		handles[[1]] <- start.predictions(handle, dataset, n, fixed, predicted)
	}
}