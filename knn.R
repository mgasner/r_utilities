get.nearest.neighbor <- function(df, i, except = i) {
  rows <- 1:dim(df)[1]
  missing <- which(is.na(df[i,]))
  if (length(missing) == 0) {
    i
  } else if (length(missing) == dim(df)[2]) {
    sample(rows, 1)
  } else {
    purify <- which(is.na(df[rows,missing]) & (! is.na(df[rows,-missing])))
    dists <- vector(mode = "numeric", length = length(purify))
    for (j in seq_along(purify)) {
      if (! purify[j] %in% except) {
        dists[j] <- sqrt(sum((df[i,-missing] - df[purify[j], -missing]) ** 2))
      } else {
        dists[j] <- NA
      }
    }
    m <- which(dists == min(dists, na.rm = TRUE))
    m <- purify[m]
    m
  }
}

get.k.nearest.neighbors <- function(df, i, k) {
  nns <- c()
  except <- i
  for (j in 1:k) {
    nn <- get.nearest.neighbor(df, i, except = except)
    except <- c(except, nn)
    nns <- c(nns, nn)
  }
  nns
}