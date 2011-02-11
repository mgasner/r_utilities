start_benchmark <- function(analysis.handle, dataset, target, num.predictions, entities) {
    pred.handles <- list()
    pf <- match(target, names(dataset@data))

    for (i in seq_along(entities)) {
        entity <- entities[i]

	# NOTE: This has only been tested in the case where the full data frame is observed. When performing a similar
        #       procedure on data frames with varying patterns of missing values per row, if start.predictions does not
        #       behave as expected, then filter the NAs when assembling the fixed.features object.
        fixed <- dataset@data[entity, -pf]

        predicted <- features(dataset)[pf]

        pred_handle <- start.predictions(analysis.handle, dataset, num.predictions, fixed.features = fixed, predicted.features = predicted)

        pred.handles[[i]] <- pred_handle
    }

    pred.handles
}

wait_for_benchmark <- function(dataset, target, num.predictions, entities, prediction.handles) {
    done <- FALSE
    predictions.matrix = matrix(nrow=length(entities), ncol=(1 + num.predictions))
    target.id = match(target, names(dataset@data))
    while (done == FALSE) {
        done <- TRUE
        for (i in seq_along(prediction.handles)) {
	    predictions <- wait.for.predictions(prediction.handles[[i]], dataset, 1)
            if (!identical(predictions,"timeout")) {
               predictions.matrix[i,1] = dataset@data[i, target.id]
               predictions.matrix[i,2:(1 + num.predictions)] = predictions[,target]
            } else {
               done <- FALSE
            }
	}
    }
    predictions.df = as.data.frame(predictions.matrix, row.names=entities)
    names(predictions.df) <- c(c(target), seq(num.predictions))
    predictions.df
}

library(lattice)
library(reshape)
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