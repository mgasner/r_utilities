mean.scatterplot <- function (results, dataset, target, n, pch = 3, col = "#00000022", xlab = "True Values", ylab = "Predicted Values", ...) {
	
	true.values <- get.true.values(dataset, target)
	mean.predicted.values <- get.mean.predicted.values(results, dataset, target, n)
	
	plot(mean.predicted.values ~ true.values, pch = pch, col = col, xlab = xlab, ylab = ylab, ...)
}


get.true.values <- function(dataset, target) {
	dataset[,target]
}

get.mean.predicted.values <- function(results, dataset, target, n) {
	fixed.rows.limit <- 5000
	fixed.features.limit <- 5000
	predicted.features.limit <- 5000
	fixed.cells.limit <- 1000000
	predicted.cells.limit <- 2000000
		
	rows.per.batch <- floor(fixed.cells.limit / (length(features(dataset)) - 1) / n) * n
	num.batches <- ceiling(dim(dataset@data)[1] / rows.per.batch)
	
	mean.predicted.values <- vector(mode = "numeric", length = length(true.values))
	
	for (i in 1:length(true.values)) {
		j <- ((i - 1) * n) +1
		k <- i * n
		
		start.batch <- ((j - 1) %/% rows.per.batch) + 1
		end.batch <- ((k - 1) %/% rows.per.batch) + 1
		
		p <- ((j - 1) %% rows.per.batch) + 1
		q <- ((k - 1) %% rows.per.batch) + 1
		
		#print(paste(i, j, k, start.batch, end.batch, p, q, "\n"))
		if (start.batch == end.batch) {
			mean.predicted.values[i] <- mean(results[[start.batch]][p:q, target])
		} else {
			mean.predicted.values[i] <- mean(c(results[[start.batch]][p:rows.per.batch, target], results[[end.batch]][1:q, target]))
		}
	}
	mean.predicted.values
}