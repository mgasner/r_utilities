# February 8, 2011
# max@naviasystems.com

startup <- function(username = "fastmax", server = "dev.naviasystems.com", wd = "~/") {
	Sys.setenv(SWEAVE_STYLEPATH_DEFAULT=TRUE)
	
	library(veritable.r)
	
	setwd(wd)
	
	options(veritable.username = username, veritable.server = server)
}