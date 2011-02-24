# February 22, 2011
# Max Gasner <max@naviasystems.com>

as.ratio.series <- function(ts) {
	rs <- c()
	for(i in seq_along(ts)) {
		if (i>1) {
			r <- (ts[i]-ts[i-1])/ts[i-1]
			rs <- c(rs, r)
		}
		else rs <- c(rs, NA)
	}
	rs
}

as.log.ratio.series <- function(ts) {
}
as.diff.series <- function(ts) {
}

as.log.diff.series <- function(ts) {
}

as.ratio.df <- function(df) {
	# note this function should allow us to exclude columns from conversion
	for (i in seq_along(names(df))) {
		df[,i] <- as.ratio.series(df[,i])
	}
	df
}
