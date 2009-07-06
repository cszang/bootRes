dcplot <-
function(x, ci = TRUE, sig = TRUE, labels = NULL, labstyle = 1) {
	op <- par(no.readonly = TRUE)
	if (labstyle == 1) {
		par(xpd = TRUE, oma = c(3, 0.5, 0.5, 0.5))
	} else {
		par(xpd = TRUE)
		labels <- rownames(x)
	}	
	if (is.null(labels))
		labels <- rownames(x)
	n <- dim(x)[1]
	vnames <- character(n)
	for (i in 1:n) {
		vnames[i] <- strsplit(rownames(x)[i], "\\.")[[1]][1]
	}
	no.vars <- length(unique(vnames)) 
	if (ci) {
		plot.range <- range(c(x$ci.upper, x$ci.lower))
	} else {
		plot.range <- range(x$coef)
	}
	if (no.vars == 2) {
		pch.1 <- rep(1, n/2)
		pch.2 <- rep(2, n/2)
		if (sig) {
			pch.1[which(x$significant[1:(n/2)] == 1)] <- 19
			pch.2[which(x$significant[(n/2 + 1):n] == 1)] <- 17
		}
		pchs <- c(pch.1, pch.2)
	} else {
		if (no.vars == 3) {
			pch.1 <- rep(1, n/3)
			pch.2 <- rep(2, n/3)
			pch.3 <- rep(0, n/3)
			if (sig) {
				pch.1[which(x$significant[1:(n/3)] == 1)] <- 19
				pch.2[which(x$significant[(n/3 + 1):(2*(n/3))] == 1)] <- 17
				pch.3[which(x$significant[(2*(n/3) + 1):n] == 1)] <- 15
			}
			pchs <- c(pch.1, pch.2, pch.3)
		} else {
			pchs <- rep(1, n)
			if (sig) {
				pchs[which(x$significant == 1)] <- 19
			}
		}
	}
	plot(1:n, x$coef, ylim = plot.range, xlab = "", ylab = "Coefficients", xaxt = "n", type = "n")
	lines(c(0.5, (n + 0.5)), c(0, 0), lty = 2, col = "grey")
	points(1:n, x$coef, pch = pchs)
	if (ci)
		segments(1:n, x$ci.lower, 1:n, x$ci.upper)
	if (labstyle == 1)
		text((1:n)+0.01*n, rep(plot.range[1]*1.2, n), labels, srt = 90, pos = 2, cex = 0.7)
	if (labstyle == 2) {
		parse.lab <- function(str) {
			split <- strsplit(str, "\\.")
			first.letter <- substring(split[[1]][3], 1, 1)
			if (split[[1]][2] == "prev")
				first.letter <- toupper(first.letter)
			first.letter
		}
		text((1:n)+0.02*n, rep(plot.range[1]*1.2, n), sapply(labels, parse.lab), pos = 2, cex = 1)
	}
	par(op)
}