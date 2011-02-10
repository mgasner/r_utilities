percent.correct <- function(dataset, predictions, target, n) {
	correct <- c()
	numrows <- dim(dataset@data)[1]
	
	for (i in 1:numrows) {
		range <- (((i - 1) * n) + 1):(i * n)
		correct[i] <- sum(as.numeric(predictions[range, target] == dataset[i, target]))
		correct[i] <- (correct[i] / n)
		cat(paste(i, ""))
	}
}

plot.roc <- function(correct, main = "ROC Curve", ylab = "True Classification Rate", xlab = "Misclassification Rate", xlim = c(-0.005, 1.005), ylim = c(-0.005, 1.005), ...) {
	incorrect <- 1 - correct
	parameters <- (0:100)/100
	true_positives <- c()
	false_positives <- c()
	for (i in parameters) {
		false_positives <- c(false_positives, length(incorrect[incorrect>i])/length(incorrect))
		true_positives <- c(true_positives, length(correct[correct>i])/length(correct))
	}
	plot(true_positives ~ false_positives, type = "l", main = main, ylab = ylab, xlab = xlab, xlim = xlim, ylim = ylim, ...)
	lines(c(0,1), c(0,1), type = "l", lty = 3) # random guesses
}