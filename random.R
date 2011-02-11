make.test.dataset <- function(nrows, ncols, type, ...) {
	dataset <- make.test.df(nrows, ncols, type, ...)
	dataset <- as.veritable.dataset(dataset)
	datatypes(dataset) <- type
	dataset
}

make.test.df <- function(nrows, ncols, type, ...) {
	dataset <- cbind(make.test.column(nrows, type, ...))
	if (ncols > 1)
		for (i in 2:ncols)
			dataset <- cbind(dataset, make.test.column(nrows, type, ...))
	
	dataset <- as.data.frame(dataset)
	dataset
}

make.test.column <- function(nrows, type, ...) {
	if (identical(type, "binary")) make.test.column.binary(nrows, ...)
	else if (identical(type, "continuous")) make.test.column.continuous(nrows, ...)
	else if (identical(type, "ordinal")) make.test.column.ordinal(nrows, ...)
	else if (identical(type, "categorical")) make.test.column.categorical(nrows, ...)
	else stop(paste("Error: Did not recognize requested column type", type))
}

make.test.column.binary <- function(nrows, weight = 0.5) {
	as.logical(rbinom(nrows, 1, weight))
}

make.test.column.continuous <- function(nrows, distribution = "uniform", ...) {
	if (identical(distribution, "uniform")) make.test.column.continuous.uniform(nrows, ...)
	else stop(paste("Error: Did not recognize requested continuous distribution", distribution))
}

make.test.column.continuous.uniform <- function(nrows, interval = c(0,1)) {
	runif(nrows, min = interval[1], max = interval[2])
}

make.test.column.ordinal <- function(nrows, draw = c(1, 2, 3, 4, 5), weight = NULL) {
	sample(draw, nrows, prob = weight, replace = TRUE)
}

make.test.column.categorical <- function(nrows, draw = c("A", "B", "C", "D", "E"), weight = NULL) {
	sample(draw, nrows, prob = weight, replace = TRUE)
}