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
# Predicted histograms require the libraries lattice and reshape, which you can
# install from the R command line by running: install.packages(c("lattice",
# "reshape"))
#
# For the predicted histogram, it is presently possible to aggregate predicted
# values by a column of names of length (n * length(entities(dataset))), by
# passing it to the by parameter of plot.predictions.

plot.predictions <- function(dataset, predictions, target, n, type = NULL, by = NULL, ...) {
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
		predicted.hist(dataset, predictions, target, n, by, ...)
	} else stop(paste("Didn't recognize plot type,", type))
}

predicted.scatterplot <- function(dataset, predictions, target, n, rgb = "000000", alpha = "33", col = NULL, xlab = "True Values", ylab = "Predicted Values", pch = 3, ...) {
	true.values <- get.true.values(dataset, predictions, target, n)
	predicted.values <- get.predicted.values(dataset, predictions, target, n)
	
	if (is.null(col)) {
		col <- paste(rgb, alpha, sep = "")
	}
	
	plot(predicted.values ~ rep(true.values, each = n), col = col, xlab = xlab, ylab = ylab, pch = pch, ...)
}

mean.scatterplot <- function (dataset, predictions, target, n, pch = 3, col = "#00000022", xlab = "True Values", ylab = "Predicted Values", ...) {
	
	true.values <- get.true.values(dataset, predictions, target, n)
	mean.predicted.values <- get.mean.predicted.values(dataset, predictions, target, n)
	
	plot(mean.predicted.values ~ true.values, pch = pch, col = col, xlab = xlab, ylab = ylab, ...)
}

predicted.hist <- function(dataset, predictions, target, n, by = NULL, ground.truth = NULL, xlab = NULL, ylab = "Relative Probability", nint = 40, col = "red", ...) {
	library(lattice)
	library(reshape)
	
	predicted.values <- get.predicted.values(dataset, predictions, target, n)
	
	if (is.null(by)) {
		df <- cbind(predicted.values, entity_names = rep(entities(dataset), each = n))
		true.values <- get.true.values(dataset, predictions, target, n)
	} else {
		df <- cbind(predicted.values, entity_names = by)
		true.values <- ground.truth
	}
	
	hist.df = melt(df[,2:length(names(df))], id="entity_names")

	if (is.null(xlab)) {
		xlab <- paste("Predicted", target)
    }
    grid::grid.prompt(TRUE)
	print(hist.plot <- histogram(~ value | entity_names,
						   data = hist.df,
						   xlab = xlab,
						   ylab = ylab,
						   nint = nint,
						   panel = function(...) {
						   		panel.histogram(...)
						   		panel.abline(v = true.values[panel.number()], col = col)
						   },
						   layout = c(3,4)))
	grid::grid.prompt(FALSE)
}

# ROC plotting functions.
#
# To do: allow the specification of a simple cost function

calculate.auc <- function(dataset, predictions, target, n) {
  if (class(predictions) == "veritable.predictions")
    df <- predictions@data
  else df <- predictions[[1]]@data
  if (length(predictions) > 1) 
    for (i in 2:length(predictions))
      df <- rbind(df, predictions[[1]]@data)
  
  numrows <- dim(dataset@data)[1]
  correct <- 0
  
  for (i in 1:numrows) {
    range <- (((i - 1) * n) + 1):(i * n)
    correct <- correct + sum(as.numeric(df[range, target] == as.numeric(as.data.frame(dataset[i, target]))))
  }
  
  correct / (numrows * n)
}

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
		correct[i] <- sum(as.numeric(df[range, target] == dataset@data[i, target]))
		#correct[i] <- sum(as.numeric(df[range, target] == as.numeric(as.character(dataset@data[i, target]))))
		correct[i] <- (correct[i] / n)
	}
	
	correct
}

plot.roc <- function(correct, parameters = (0:100)/100, show.reference = TRUE, main = "ROC Curve", ylab = "True Classification Rate", xlab = "Misclassification Rate", xlim = c(-0.005, 1.005), ylim = c(-0.005, 1.005), add = FALSE, ...) {
	incorrect <- 1 - correct
	true_positives <- c()
	false_positives <- c()
	for (i in parameters) {
		false_positives <- c(false_positives, length(incorrect[incorrect>i])/length(incorrect))
		true_positives <- c(true_positives, length(correct[correct>i])/length(correct))
	}
	if (add == FALSE) {
	  plot(true_positives ~ false_positives, type = "l", main = main, ylab = ylab, xlab = xlab, xlim = xlim, ylim = ylim, ...)
	} else {
	  lines(true_positives ~ false_positives, ...)
	}
	if (show.reference == TRUE) {
		lines(c(0,1), c(0,1), type = "l", lty = 3) # random guesses
	}
}

# Utility functions for scatterplots
get.true.values <- function(dataset, predictions, target, n) {
  l <- dim(predictions)[1]/n
  gt <- vector(mode = "numeric", length = l)
  
  for (i in 1:l) {
    gt[i] <- dataset@data[(((i - 1) * n) + 1), target]
  }
  
  gt
  
}

get.predicted.values <- function(dataset, predictions, target, n,
							 		  fixed.rows.limit = 5000,
							 		  fixed.features.limit = 5000,
							 		  predicted.features.limit = 5000,
							 		  fixed.cells.limit = 1000000,
							 		  predicted.cells.limit = 2000000) {
	if (class(predictions) == "veritable.predictions") {
		predictions@data[,target]
	} else if (length(predictions) == 1) {
		predictions[[1]][,target]
	} else {
		rows.per.batch <- floor(fixed.cells.limit / (length(features(dataset)) - 1) / n) * n
		num.batches <- ceiling(dim(dataset@data)[1] / rows.per.batch)
		
		if (datatypes(dataset)[target]$type == "binary") {
			predicted.values <- vector(mode = "logical", length = ((rows.per.batch * (num.batches - 1)) + dim(predictions[[num.batches]])[1]))
		} else if (datatypes(dataset)[target]$type %in% c("continuous", "ordinal")) {
			predicted.values <- vector(mode = "numeric", length = ((rows.per.batch * (num.batches - 1)) + dim(predictions[[num.batches]])[1]))
		} else {
			predicted.values <- vector(mode = "character", length = ((rows.per.batch * (num.batches - 1)) + dim(predictions[[num.batches]])[1]))
		}
		for (i in 1:(num.batches - 1)) {
			j <- (((i - 1) * rows.per.batch) + 1)
			k <- (i * rows.per.batch)
			predicted.values[j:k] <- predictions[[i]][,target]
		}
		j <- (((num.batches - 1) * rows.per.batch) + 1)
		k <- (((num.batches - 1) * rows.per.batch) + dim(predictions[[num.batches]])[1])
		predicted.values[j:k] <- predictions(num.batches)[,target]
		predicted.values
	}
}

get.mean.predicted.values <- function(dataset, predictions, target, n,
							 		  fixed.rows.limit = 5000,
							 		  fixed.features.limit = 5000,
							 		  predicted.features.limit = 5000,
							 		  fixed.cells.limit = 1000000,
							 		  predicted.cells.limit = 2000000) {
		
	rows.per.batch <- floor(fixed.cells.limit / (length(features(dataset)) - 1) / n) * n
	num.batches <- ceiling(dim(dataset@data)[1] / rows.per.batch)
	
	mean.predicted.values <- c()
	
	for (i in 1:length(true.values)) {
		j <- ((i - 1) * n) + 1
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
