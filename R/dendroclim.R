dendroclim <- function(chrono, clim, method = "response", start = -6, end = 9, timespan = NULL, vnames = NULL, sb = TRUE) {
	month.ids <- c(-1:-12, 1:12)
	errormsg1 <- "start and end have to define an interval in [-1, -2, ..., -12, 1, 2, ..., 12]."
	if (!is.element(start, month.ids) || !is.element(end, month.ids) || which(month.ids == start) > which(month.ids == end)) { # check start and end dates
		stop(errormsg1)
	}
	chrono.years <- as.numeric(row.names(chrono)) # get timespan of chrono
	clim.years <- sort(unique(clim[, 1])) # get timespan of climate data
	if (chrono.years[1] <= clim.years[1]) {
		overlap <- na.omit(clim.years[match(chrono.years, clim.years)]) # get overlap of timespans
	} else {
		overlap <- na.omit(chrono.years[match(clim.years, chrono.years)])
	}
	if (is.null(timespan)) {
		start.year <- overlap[1]
		end.year <- tail(overlap, 1)
	} else {
		if (start > 0) {
			if (!is.element(timespan[1], overlap) || !is.element(timespan[2], overlap)) {
				errormsg3 <- paste("timespan has to be between ", overlap[1], " and ", tail(overlap, 1), " for start dates in current year.", sep = "")
				stop(errormsg3)
			} else {
				start.year <- timespan[1]
				end.year <- timespan[2]
			}
		} else {
			if (!is.element(timespan[1], overlap) || !is.element(timespan[2], overlap)) {
				errormsg4 <- paste("timespan has to be between ", overlap[1] + 1, " and ", tail(overlap, 1), " for start dates in previous year.", sep = "")
				stop(errormsg4)
			} else {
				start.year <- timespan[1]
				end.year <- timespan[2]
			}
		}
	}
	if (start < 0 && is.na(match((start.year - 1), clim.years))) { # check if a previous year is available in climatic data; otherwise set start.year + 1
		offset <- 1
	} else {
		offset <- 0
	}
	if (start < 0) { # make sure that data get truncated properly	
		interval.clim <-(start.year - 1 + offset):end.year
		interval.chrono <- (start.year + offset):end.year
	} else {
		interval.clim <-(start.year + offset):end.year
		interval.chrono <- (start.year + 1 + offset):end.year
	}
	a <- as.numeric(rownames(chrono)) %in% interval.chrono
	b <- clim[, 1] %in% interval.clim
	chrono.trunc <- chrono[a, 1] # truncate data 
	clim.trunc <- clim[b, ]
	p <- pmat(clim.trunc, start, end, vnames) # call pmat to obtain variable matrix
	METHOD <- match.arg(method, c("response", "correlation"))  # match method argument
	if (METHOD == "response") {
		dc <- brf(chrono.trunc, p, sb = sb) # call brf to calculate bootstrapped response function
	}
	if (METHOD == "correlation") {
		dc <- bcf(chrono.trunc, p, sb = sb) # call bcf to calculate bootstrapped correlation function
	}
	cat("time span considered:", start.year, "-", end.year, "\n")
	dc
}