# February 13, 2011
# Max Gasner <max@naviasystems.com>
#
# Simple R utilities that wrap the veritable.r predictions functionality to
# allow for larger jobs than would otherwise be possible within the 2mn
# predicted cell limit. It should go without saying that these functions should
# be used with caution:
#
# -	 Never start a batch of predictions without knowing how many batches are 
#    going to be generated, and approximately how long each will run. (Run a
#    test job first!)
# -  Never start a batch of predictions without storing the predictions handles
#    somewhere. In other words, always run:
#         > handles <- start.batch.predictions(....)
#    and not:
#         > start.batch.predictions(....)
#

# Run start.batch.predictions in either of the following cases:
#
# -  You want to make more predictions than can fit in a single job request.
#         > start.batch.predictions(..., type = "maximal")
# -  You want to make predictions from fixed values that include NAs.
#         > start.batch,predictions(..., type = "by_row")
#
# In both cases, start.batch.predictions will return a list of predictions
# handles. You can pass this list to get.batch,predictions or to
# cancel.batch.predictions, and it should be easy to adapt this code to produce
# functions of your own to manipulate the list of handles.
#
# Set mode = "batch" if you want to run start.batch.predictions from a script.
# By default, mode = "interactive" and predictions will not be performed without
# explicit confirmation from a user within an R session. This is intended to
# make it harder to start predictions requests that will accidentally swamp an
# inference server.

start.batch.predictions <- function (handle,
									 dataset,
									 target,
									 n,
									 fixed.rows.limit = 5000,
									 fixed.features.limit = 5000,
									 predicted.features.limit = 5000,
									 fixed.cells.limit = 1000000,
									 predicted.cells.limit = 2000000,
									 mode = "interactive",
									 type = "maximal",
									 rows = 1:10) {

	df <- dataset@data
	if (length(target) == 1) {
		tgt <- which(features(dataset) == target)
	} else if (length(target) < (length(features(dataset)) - 1)) {
		tgt <- which(features(dataset) %in% target)
	} else {
		stop("Error: Too many predicted features.")
	}
	
	if (length(features(dataset)) > predicted.features.limit) {
		stop("Error: Too many predicted features.")
	}
	
	if (type == "maximal") {
		rows.per.batch <- floor(fixed.cells.limit / (length(features(dataset)) - 1) / n)
		num.batches <- ceiling(dim(df)[1] / rows.per.batch)
		
		if (mode == "interactive") {
			ok <- readline(prompt = (paste("Will batch by", batch.rows, "rows, for", num.batches, "total batches. OK?\n")))
			if (! ok %in% c("Y", "Yes", "OK", "yes", "y", "YES", "ok")) {
				stop("Stopped by user.")
			}
		} else {
			print(paste("Batching by", batch.rows, "rows, for", num.batches, "total batches.\n"))
		}
		
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
	} else if (type == "by_row") {
		num.batches <- length(rows)
		if (num.batches > 10) {
			if (mode == "interactive") {
				ok <- readline(prompt = (paste("Will make", num.batches, "total predictions requests. OK?\n")))
				if (! ok %in% c("Y", "Yes", "OK", "yes", "y", "YES", "ok")) {
					stop("Stopped by user.")
				}
			} else {
				print(paste("Will make", num.batches, "total predictions requests."))
			}
		}
		cat("Batching...")
		for (i in 1:num.batches) {
			row <- rows[i]
			ff <- df[row, (! is.na(df[row,]))]
			cat(paste(i, " "))
			handles[[i]] <- tryCatch(start.predictions(handle, dataset, n, fixed.features = ff, predicted.features = target, description = paste("Predictions for", target, "from", substitute(dataset), "row", row)), error = function(e) cat(e))

		}
		
		
		
		handles
	} else {
		stop (paste("Error: Didn't recognize requested batch predictions type: ", type, ". Supported values are 'maximal' and 'by_row'.", sep = ""))
	}
}

get.batch.predictions <- function(handles, dataset) {
	results <- vector("list", length(handles))
	
	for (i in seq_along(handles)) {
		results[[i]] <- tryCatch(wait.for.predictions(handles[[i]], dataset), error = function(e) cat(e))
	}
	
	results
}

cancel.batch.predictions <- function(handles) {
	for (i in seq_along(handles)) {
		cancel(handles[[i]])
	}
}