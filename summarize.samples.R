# March 9, 2011
# Max Gasner <max@naviasystems.com>
#
# R utilities to summarize the distribution of #views over Veritable samples,
# and #categories (conditioned on a target feature) over views in Veritable
# samples. 
#
# summary.samples(result) returns a named list; num.views is a vector of size
# length(samples(result)) whose elements are the number of views in each sample
# of the result; num.features is a vector whose elements are the number of
# features per view for each view in each sample of the result. Set verbose =
# FALSE to omit printing summary statistics.
#
# summary.sample(result, feature = "target_feature") returns a named list;
# num.features is a vector of size length(samples(result) whose elements are
# the number of features in the view containing "target_feature" for each
# sample of the result; num.categories is a vector of the same size whose
# elements are the number of categories in each of those views; num.entities
# counts the number of entities in each category. Set verbose = FALSE to
# omit printing summary statistics
summary.samples <- function (result, feature = NULL, verbose = TRUE) {
  if (is.null(feature)) {
    num.views <- c()
    num.features <- c()
    for (i in seq_along(samples(result))) {
      num.views <- c(num.views, length(views(samples(result)[[i]])))
      for (j in seq_along(views(samples(result)[[i]]))) {
        num.features <- c(num.features, length(features(views(samples(result)[[i]])[[j]])))
      }
    }
    if (verbose == TRUE) {
      print(paste(length(samples(result)), "samples found"))
      print(paste(max(num.views), "views in sample with most views"))
      print(paste(min(num.views), "views in sample with fewest views"))
      print(paste(mean(num.views), "views on average"))
      print(paste(max(num.features), "features in largest view"))
      print(paste(min(num.features), "features in smallest view"))
      print(paste(mean(num.features), "features per view on average"))
    }
    list(num.views = num.views, num.features = num.features)
  } else {
    num.features <- c()
    num.categories <- c()
    num.entities <- c()
    for (i in seq_along(samples(result))) {
      for (j in seq_along(views(samples(result)[[i]]))) {
        if (feature %in% features(views(samples(result)[[i]])[[j]])) {
          num.features <- c(num.features, length(features(views(samples(result)[[i]])[[j]])))
          num.categories <- c(num.categories, length(categories(views(samples(result)[[i]])[[j]])))
          for (k in seq_along(categories(views(samples(result)[[i]])[[j]]))) {
            num.entities <- c(num.entities, length(categories(views(samples(result)[[i]])[[j]])[[k]]))
          }
        }
      }
    }
    if (verbose == TRUE) {
      print(paste(length(samples(result)), "samples found"))
      print(paste(max(num.features), "features in largest view containing target feature", feature))
      print(paste(min(num.features), "features in smallest view containing target feature", feature))
      print(paste(mean(num.views), "features in average view containing target feature", feature))
      print(paste(max(num.categories), "categories maximum in views containing target feature", feature))
      print(paste(min(num.categories), "categories minimum in views containing target feature", feature))
      print(paste(mean(num.categories), "categories in average view containing target feature", feature))
      print(paste(max(num.entities), "entities in largest category in views containing target feature", feature))
      print(paste(min(num.entities), "entities in smallest category in views containing target feature", feature))
      print(paste(mean(num.entities), "entities in average category in views containing target feature", feature))

    }
    list(num.features = num.features, num.categories = num.features, num.entities = num.entities)
}