start.predictions.by.row <- function (handle, dataset, target, n) {
	
	fixed.rows.limit <- 5000
	fixed.features.limit <- 5000
	predicted.features.limit <- 5000
	fixed.cells.limit <- 1000000
	predicted.cells.limit <- 2000000

	if (length(features(dataset)) > predicted.features.limit) stop("Too many predicted features.")
	
	if ((n * length(features(dataset))) > predicted.cells.limit) stop ("Too many predictions requested.")
	
	ok <- readline(prompt = (paste("Will make", length(entities(dataset)), "separate predictions requests. OK?\n")))
	if (! ok %in% c("Y", "Yes", "OK", "yes", "y", "YES", "ok")) stop("Stopped by user.")

	df <- dataset@data[,-tgt]
	tgt <- which(features(dataset) == target)
	
	handles <- vector("list", length(entities(dataset)))
	
	for (i in entities(dataset)) {
		ff <- df[i, (! is.na(df[i,]))]
		handles[[i]] <- tryCatch(start.predictions(handle, dataset, n, fixed.features = ff, predicted.features = target, description = paste("Predictions for", target, "from", substitute(dataset), "batch", i, "of", length(entities(dataset)))), error = function(e) cat(e))
	}		
	handles
}