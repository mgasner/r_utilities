# February 8, 2011
# max@naviasystems.com

# right now this only works for a single target feature
start.batch.predictions <- function (handle, dataset, target, n) {
	
	fixed.rows.limit <- 5000
	fixed.features.limit <- 5000
	predicted.features.limit <- 5000
	fixed.cells.limit <- 1000000
	predicted.cells.limit <- 2000000
	
	df <- dataset@data
	tgt <- which(features(dataset) == target)
	
	if (length(features(dataset)) > predicted.features.limit) stop("Too many predicted features.")
	
	batch.rows <- floor(fixed.cells.limit / (length(features(dataset)) - 1) / n)
	num.batches <- ceiling(dim(df)[1] / batch.rows)
	
	ok <- readline(prompt = (paste("Will batch by", batch.rows, "rows, for", num.batches, "total batches. OK?\n")))
	if (! ok %in% c("Y", "Yes", "OK", "yes", "y", "YES", "ok")) stop("Stopped by user.")
	
	r <- 1
	handles <- vector("list", num.batches)
	
	cat("Batching...")
	for (i in 1:num.batches) {
		if (i == num.batches) {
			ff <- df[(((i - 1) * batch.rows) + 1):dim(df)[1], -tgt]
		} else {
			ff <- df[(((i - 1) * batch.rows) + 1):(i * batch.rows), -tgt]
		}
		cat(paste(i, " "))
		handles[[i]] <- tryCatch(start.predictions(handle, dataset, n, fixed.features = ff, predicted.features = target, description = paste("Predictions for", target, "from", substitute(dataset), "batch", i, "of", num.batches)), error = function(e) cat(e))
	}
	
	handles
}

get.batch.predictions <- function(handles, dataset) {
	results <- vector("list", length(handles))
	
	for (i in seq_along(handles)) {
		results[[i]] <- tryCatch(wait.for.predictions(handles[[i]], dataset), error = function(e) cat(e))
	}
	
	results
}

# this shit is totally broken by the seatbelts
reconstitute.batch.predictions <- function(results, dataset) {
	reconstituted <- results[[1]]@data
	for (i in 2:length(results)) {
		reconstituted <- rbind(reconstituted, results[[i]]@data)
	}
	reconstituted <- as.veritable.dataset(reconstituted)
	datatypes(reconstituted) <- datatypes(dataset)
	reconstituted
}