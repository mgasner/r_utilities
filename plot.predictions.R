# February 14, 2011
# Max Gasner <max@naviasystems.com>
#
# R utilities that wrap R plotting functions in order to visualize the results
# of Veritable predictions.
#
# Set type = "ROC" for ROC plots of binary or multinomial classifiers. Set
# type = "scatter" for scatterplots of the predicted values against the true
# values, type = "mpv.scatter" for scatterplots of mean predicted values against
# true values, and type = "hist" for histograms of predicted values.
#
# These functions require the libraries lattice and reshape, which you can
# install from the R command line by running: install.packages(c("lattice",
# "reshape"))
#
# To do: allow aggregation by a separate column

plot.predictions <- function(dataset, predictions, target, n, type = NULL, ...) {
	library(lattice)
	library(reshape)
	if (is.null(type)) {
		if (datatypes(dataset)[target] == "binary") {
			type <- "ROC"
		} else if (datatypes(dataset)[target] == "continuous") {
			type <- "scatter"
		} else stop("Error: unable to infer plot type.") 
	}
	if (type == "ROC") {
		pct.correct <- percent.correct(dataset, predictions, target, n)
		plot.roc(pct.correct, ...)
	} else if (type == "scatter") {
		predicted.scatterplot(dataset, predictions, target, n, ...)
	} else if (type == "mpv.scatter") {
		mean.scatterplot(dataset, predictions, target, n, ...)
	} else if (type == "hist") {
		benchmark.hist(dataset, predictions, target, n, ...)
	}
}

predicted.scatterplot <- function(dataset, predictions, target, n, rgb = "000000", alpha = "33", ...) {
	true.values <- get.true.values(dataset, target)
	predicted.values <- get.predicted.values(predictions, target)
}

mean.scatterplot <- function (results, dataset, target, n, pch = 3, col = "#00000022", xlab = "True Values", ylab = "Predicted Values", ...) {
	
	true.values <- get.true.values(dataset, target)
	mean.predicted.values <- get.mean.predicted.values(results, dataset, target, n)
	
	plot(mean.predicted.values ~ true.values, pch = pch, col = col, xlab = xlab, ylab = ylab, ...)
}


get.true.values <- function(dataset, target) {
	dataset[,target]
}

get.predicted.values <- function(dataset, predictions, target, n,
							 		  fixed.rows.limit = 5000,
							 		  fixed.features.limit = 5000,
							 		  predicted.features.limit = 5000,
							 		  fixed.cells.limit = 1000000,
							 		  predicted.cells.limit = 2000000) {
	if (class(predictions) == "veritable.predictions") {
		predictions[,target]
	} else {
	
	}
}

get.mean.predicted.values <- function(results, dataset, target, n,
							 		  fixed.rows.limit = 5000,
							 		  fixed.features.limit = 5000,
							 		  predicted.features.limit = 5000,
							 		  fixed.cells.limit = 1000000,
							 		  predicted.cells.limit = 2000000) {
		
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

plot_benchmark <- function(analysis.handle, dataset, target, num.predictions, entities, predictions.df) {
    df = cbind(predictions.df, entity_names = row.names(predictions.df))
    histdf = melt(df[,2:length(names(df))], id="entity_names")

    print(histogram(~ value | entity_names,
              data = histdf, 
              xlab = paste("Predicted ", target),
              ylab = "Relative Probability",
	      nint = 40,
              panel = function(...) {
                          panel.histogram(...)
			  panel.abline(v = predictions.df[panel.number(), target], col="red")
              }))
}

# ROC plotting functions.
#
# To do: allow the specification of a simple cost function

percent.correct <- function(dataset, predictions, target, n) {
	if (class(predictions) == "veritable.predictions")
		df <- predictions@data
	else df <- predictions[[1]]@data
	# handling batched results
	if (length(predictions) > 1)
		for (i in 2:length(predictions))
			df <- rbind(df, predictions[[i]]@data)
	
	numrows <- dim(dataset@data)[1]
	correct <- vector(mode = "numeric", length = numrows)
	
	for (i in 1:numrows) {
		range <- (((i - 1) * n) + 1):(i * n)
		correct[i] <- sum(as.numeric(df[range, target] == dataset[i, target]))
		correct[i] <- (correct[i] / n)
	}
	
	correct
}

plot.roc <- function(correct, parameters = (0:100)/100, show.reference = TRUE, main = "ROC Curve", ylab = "True Classification Rate", xlab = "Misclassification Rate", xlim = c(-0.005, 1.005), ylim = c(-0.005, 1.005), ...) {
	incorrect <- 1 - correct
	true_positives <- c()
	false_positives <- c()
	for (i in parameters) {
		false_positives <- c(false_positives, length(incorrect[incorrect>i])/length(incorrect))
		true_positives <- c(true_positives, length(correct[correct>i])/length(correct))
	}
	plot(true_positives ~ false_positives, type = "l", main = main, ylab = ylab, xlab = xlab, xlim = xlim, ylim = ylim, ...)
	if (show.reference == TRUE) {
		lines(c(0,1), c(0,1), type = "l", lty = 3) # random guesses
	}
}
