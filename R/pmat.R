pmat <-
function(x, start = -6, end = 9) {
	years <- unique(x[, 1])
	n <- length(years)
	months <- paste(c(rep("prev.", 12), rep("curr.", 12)), rep(c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"), 2), sep = "")
	month.ids <- c(-1:-12, 1:12)
	used.months <- months[which(month.ids == start):which(month.ids == end)]
	temp.vars <- paste("temp.", used.months, sep = "")
	prec.vars <- paste("prec.", used.months, sep = "")
	no.months <- length(used.months)
	m <- matrix(NA, nrow = no.months*2, ncol = n - 1)
	colnames(m) <- years[-1]
	rownames(m) <- c(temp.vars, prec.vars)
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

