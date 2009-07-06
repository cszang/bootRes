pmat <-
function(x, start = -6, end = 9, vnames = NULL) {
	years <- unique(x[, 1])
	n <- length(years)
	no.vars <- dim(x)[2] - 2 # number of variables
	months <- paste(c(rep("prev.", 12), rep("curr.", 12)), rep(c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 2), sep = "")
	month.ids <- c(-1:-12, 1:12)
	used.months <- months[which(month.ids == start):which(month.ids == end)]
	no.months <- length(used.months)
	# check for specified variable names, else default to V1, V2 etc.
	if (is.null(vnames)) {
		vnames <- paste(rep("V", no.vars), 1:no.vars, sep = "")
	} else { # check if number of specified variable names is equal to actual number of variables
		if (length(unique(vnames)) != no.vars) {
			vnames <- paste(rep("V", no.vars), 1:no.vars, sep = "")
		}
	}
	vnames.mat <- matrix(NA, nrow = no.months, ncol = no.vars) # create unique names for variables
	for (i in 1:no.vars) {
		vnames.mat[, i] <- paste(vnames[i], ".", used.months, sep = "")
	}
	m <- matrix(NA, nrow = no.months*2, ncol = n - 1)
	colnames(m) <- years[-1]
	rownames(m) <- as.vector(vnames.mat)
	for (i in 2:n) {
		if (start < 0) {
			start.with <- which(x[, 1] == years[i - 1])[abs(start)] # start month in previous year
		} else {
			start.with <- which(x[, 1] == years[i])[start] # start month in current year
		}
		for (j in 1:no.months) { # monthly temperature data
			m[j, (i - 1)] <- x[(start.with + j - 1), 3]
		}
		for (j in 1:no.months) { # monthly precipitation sums
			m[(j + no.months), (i - 1)] <- x[(start.with + j - 1), 4]
		}
	}	
	as.data.frame(t(m))
}

