scrub.test.dataset <- function(test.dataset, train.dataset) {
	test.df <- test.dataset@data
	train.df <- train.dataset@data
	
	incompatible.rows <- c()
	for (i in 1:dim(test.df)[1]) {
		incompatible <- FALSE
		for (j in 1:dim(test.df)[2]) {
			if (datatypes(test.dataset)[[j]]$type == "categorical") {
				if (! test.df[i,j] %in% levels(train.df[,j])) incompatible <- TRUE
			}
		}
		if (incompatible == TRUE) incompatible.rows <- c(incompatible.rows, i)
	}
	
	scrubbed.df <- test.df[-incompatible.rows,]
	scrubbed.dataset <- as.veritable.dataset(scrubbed.df)
	datatypes(scrubbed.dataset) <- datatypes(test.dataset)
	scrubbed.dataset
}