# March 21, 2011
# max@naviasystems.com


change.ip <- function(ip, objects = ls(.GlobalEnv)) {
  lapply(objects, function (x) {
    if ((class(get(x)) == "veritable.analysis.handle") || (class(get(x)) == "veritable.predictions.handle")) {
      y <- get(x)
      server(y) <- ip
      assign(x, y)
    } else if (is.list(x)) {
      change.ip(ip, x)
    }
  })
}