###########################################################
# plotting utilities

open.graphics.device <- function(name, frame, pad = 4) {
	padding <- paste("%0", pad, "d", sep="")
	png(filename = paste(name, sprintf(padding, frame), ".png", sep = ""), width = 1200, height = 1200, units = "px", bg = "white", res = 72)
}

close.graphics.device <- function() {
	dev.off()
}

plot.frame <- function(dataset, sample = NULL, frame, nframes, name, ...) {
	for (i in 1:nframes) {
		if (is.null(sample)) {
			open.graphics.device(name, frame)
			plot.dataset(dataset, scale = 1, center = FALSE, ...)
			close.graphics.device()
		}
		else {
			open.graphics.device(name, frame)
			plot.sample(sample, dataset, scale = 1, center = FALSE, ...)
			close.graphics.device()
		}
		frame <- frame + 1
	}
	frame
}

plot.results <- function(results, name, frame = 1, nframes = 1) {
	type <- results[[1]]
	dataset <- results[[2]]
	result <- results[[3]]
	
	if (type == "kernel_control") frame <- plot.kernel.results(results, name, frame = frame, nframes = nframes)
	else for (i in 1:length(result)) {
		frame <- plot.frame(dataset, result[[i]][[1]], frame = frame, nframes = nframes, name = name)
	}
	frame
}

plot.kernel.results <- function(results, name, frame = 1, nframes = 1) {
	type <- results[[1]]
	dataset <- results[[2]]
	result <- results[[3]]
	
	i <- 0
	while(i < length(result)) {
		for (j in entities(dataset)) {
			i <- i + 1
			frame <- plot.frame(dataset, result[[i]][[1]], frame = frame, nframes = nframes, name = name, highlight.entities = j)
		}
		i <- i + 1
		for (j in features(dataset)) {
			i <- i + 1
			frame <- plot.frame(dataset, result[[i]][[1]], frame = frame, nframes = nframes, name = name, highlight.features = j)
		}
	}
	
	frame
}

highlight.categories <- function(dataset, sample, view = 1, frame = 1, nframes = 1, name) {
	frame <- plot.frame(dataset, sample, frame = frame, nframes = nframes, name = name)
	
	v <- views(sample)[[view]]
	cats <- categories(v)
	l <- sapply(cats, length)
	cats.o <- order(l, decreasing = TRUE)
	cats <- cats[cats.o]
	ncats <- length(cats)
	colors <- c("darkgoldenrod", "darkred", "darkolivegreen", "violet", "darkkhaki", "firebrick", "deeppink", "deepskyblue")
	
	for (i in 1:ncats) {
		es <- cats[[i]]
		frame <- plot.frame(dataset, sample, frame = frame, nframes = nframes, name = name, highlight.entities = es, highlight.entities.colors = colors[i])
	}
	frame
}

plot.predictions <- function(predictions, sample = NULL, name, frame = 1, nframes = 1, by.row = FALSE) {
	if (by.row == TRUE) plot.row.predictions(predictions, sample, name, frame, nframes)
	else {
		dataset <- predictions[[1]]
		results <- predictions[[2]]
		n <- predictions[[3]]
		
		for (i in 1:n) {
			predicted.dataset <- dataset
			for (j in entities(dataset)){
				if (typeof(results[[j]]) == "S4") {
					repl <- features(dataset)[is.na(dataset[j, ]) == TRUE]
					predicted.dataset@data[j, repl] <- results[[j]]@data[i,repl]
				}
			}
			frame <- plot.frame(predicted.dataset, sample = sample, frame = frame, nframes = nframes, name = name)
		}
		frame
	}
}

plot.row.predictions <- function(predictions, sample, name, frame = 1, nframes = 1) {
	dataset <- predictions[[1]]
	results <- predictions[[2]]
	n <- predictions[[3]]
	
	v <- views(sample)[[1]]
	cats <- categories(v)
	l <- sapply(cats, length)
	cats.o <- order(l, decreasing = TRUE)
	cats <- cats[cats.o]
	ncats <- length(cats)
	colors <- c("darkgoldenrod", "darkred", "darkolivegreen", "violet", "darkkhaki", "firebrick", "deeppink", "deepskyblue")
	
	for (i in 1:n) {
		predicted.dataset <- dataset
		for (k in cats) {
			for (j in sort(k)){
				if (typeof(results[[j]]) == "S4") {
					repl <- features(dataset)[is.na(dataset[j, ]) == TRUE]
					predicted.dataset@data[j, repl] <- results[[j]]@data[i,repl]
				}
				frame <- plot.frame(predicted.dataset, sample = sample, frame = frame, nframes = nframes, name = name, highlight.entities = j)
			}
		}
	}
	frame
	
}
###########################################################
# dataset utilities
omit <- function(df, p) {
	ncols <- ncol(df)
	nrows <- nrow(df)
	n <- nrow(df) * ncol(df)
	m <- floor(n * p / 100)
	
	omit.vals <- sample(1:n, m)
	
	for(i in omit.vals) {
		j <- (i %/% ncols) + 1
		k <- (i %% ncols) + 1
		df[j,k] <- NA
	}
	df
}

###########################################################
# iterations
make.iterations <- function(dataset, n, name, by = 1, kernel.control = FALSE, extends = NULL, ...) {
	if (kernel.control == TRUE)
		make.kernel.iterations(dataset, n, name, extends, ...)
	else {
		handles <- list()
		if (is.null(extends)) {
			handles[[1]] <- start.analysis(dataset, description = paste(name, "iteration 1"), samples = 1, iterations = 1, ...)
		}
		else {
			handles[[1]] <- resume(extends, description = paste(name, "iteration 1"), iterations = by, ...)
		}
		for (i in 2:n) {
			while(status(handles[[i-1]]) != "finished") {
				Sys.sleep(0.1)
			}
			handles[[i]] <- resume(handles[[i-1]], description = paste(name, "iteration", i), iterations = by, ...)
		}
	list("full_dataset", dataset, handles)
	}
}
								
make.kernel.iterations <- function(dataset, n, name, extends = NULL, ...) {
	num.entities <- length(entities(dataset))
	num.features <- length(entities(dataset))
	
	handles <- list()
	
	print("kernel control is ON")
	
	i <- 1
	if(is.null(extends)) {
		handles[[i]] <- category.kernel(dataset, entities(dataset)[i], name, i)
	}
	else handles[[i]] <- category.kernel(dataset, entities(dataset)[i], name, i, extends = extends)
	
	if(length(entities(dataset)) > 1) for (j in 2:length(entities(dataset))) {
		i <- i + 1
		while(status(handles[[i - 1]]) != "finished") {
			Sys.sleep(0.1)
		}		
		handles[[i]] <- category.kernel(dataset, entities(dataset)[j], name, i, extends = handles[[i - 1]])
	}
	
	i <- i + 1
	while(status(handles[[i - 1]]) != "finished") {
		Sys.sleep(0.1)
	}			
	handles[[i]] <- hyper.kernel(handles[[i - 1]], name, i)
	
	for (j in 1:length(features(dataset))) {
		i <- i + 1
		while(status(handles[[i - 1]]) != "finished") {
			Sys.sleep(0.1)
		}		
		handles[[i]] <- kind.kernel(handles[[i - 1]], features(dataset)[j], name, i)
	}
	
	if(n > 1) for (k in 2:n) {
		for (j in 1:length(entities(dataset))) {
			i <- i + 1
			while(status(handles[[i - 1]]) != "finished") {
				Sys.sleep(0.1)
			}		
			handles[[i]] <- category.kernel(dataset, entities(dataset)[j], name, i, extends = handles[[i - 1]])
		}
		
		i <- i + 1
		handles[[i]] <- hyper.kernel(handles[[i - 1]], name, i)
		
		for (j in 1:length(features(dataset))) {
			i <- i + 1
			while(status(handles[[i - 1]]) != "finished") {
				Sys.sleep(0.1)
			}		
			handles[[i]] <- kind.kernel(handles[[i - 1]], features(dataset)[j], name, i)
		}
	}
	list("kernel_control", dataset, handles)
}

category.kernel <- function(dataset, entity, name, i, extends = NULL) {	
	if (is.null(extends)) start.analysis(dataset, samples = 1, iterations = 1, description = paste(name, "iteration", i), kernel.config =
				   veritable.r:::jsondict(
						class = "cycle",
						cycle = veritable.r:::jsonlist(
							veritable.r:::jsonlist(
								"kind_to_cat", veritable.r:::jsondict()
							),
							veritable.r:::jsonlist(
								"single_object", veritable.r:::jsondict(
										object_name = entity
								)
							),
							veritable.r:::jsonlist(
								"cat_to_kind", veritable.r:::jsondict()
							)
						)
					)
	)				
	else resume(extends, samples = 1, iterations = 1, description = paste(name, "iteration", i), kernel.config =
				veritable.r:::jsondict(
					class = "cycle",
					cycle = veritable.r:::jsonlist(
						veritable.r:::jsonlist(
							"kind_to_cat", veritable.r:::jsondict()
						),
						veritable.r:::jsonlist(
							"single_object", veritable.r:::jsondict(
								object_name = entity
							)
						),
						veritable.r:::jsonlist(
							"cat_to_kind", veritable.r:::jsondict()
						)
					)
				)
	)
}

hyper.kernel <- function(extends, name, i) {
	resume(extends, description = paste(name, "iteration", i), samples = 1, iterations = 1, kernel.config =
			veritable.r:::jsondict(
				class = "cycle",
				cycle = veritable.r:::jsonlist(
					veritable.r:::jsonlist(
						"kind_to_cat", veritable.r:::jsondict()
					),
					veritable.r:::jsonlist(
						"grid_hypers", veritable.r:::jsondict()
					),
					veritable.r:::jsonlist(
						"cat_to_kind", veritable.r:::jsondict()
					)
				)
			)
	)																					
}

kind.kernel <- function(extends, feature, name, i) {
	resume(extends, description = paste(name, "iteration", i), samples = 1, iterations = 1, kernel.config =
		   veritable.r:::jsondict(
				class = "cycle",
				cycle = veritable.r:::jsonlist(
					veritable.r:::jsonlist(
						"single_feature", veritable.r:::jsondict(
							feature_name = feature
						)
					)
				)
			)
	)
}

get.iterations <- function(iterations, ...) {
	type <- iterations[[1]]
	dataset <- iterations[[2]]
	handles <- iterations[[3]]
	
	results <- list()
		for (i in 1:length(handles)) {
			results[[i]] <- wait.for.analysis(handles[[i]], dataset, ...)
		}
	list(type, dataset, results)
}
	

###########################################################
# predictions
make.predictions <- function(dataset, extends, n, ...) {
	handles <- list()
	for (i in entities(dataset)) {
		f <- features(dataset)[is.na(dataset@data[i, ]) == FALSE]
		ff <- dataset[i, f]
		pf <- features(dataset)[is.na(dataset@data[i,]) == TRUE]
		if (length(pf) > 0) handles[[i]] <- start.predictions(extends, dataset, n = n, fixed.features = ff, predicted.features = pf, batch = FALSE)
		else handles[[i]] <- "SKIP"
	}
	list(dataset, handles, n)
}

get.predictions <- function(predictions, ...) {
	dataset <- predictions[[1]]
	handles <- predictions[[2]]
	n <- predictions[[3]]
	
	results <- list()
	for (i in entities(dataset)) {
		if (typeof(handles[[i]]) != "S4") results[[i]] <- "SKIP"
		else results[[i]] <- wait.for.predictions(handles[[i]], dataset, ...)
	}
	list(dataset, results, n)
}