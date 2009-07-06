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
	if (ci) {
		plot.range <- range(c(x$ci.upper, x$ci.lower))
	} else {
		plot.range <- range(x$coef)
	}
	pch.temp <- rep(21, n/2)
	pch.prec <- rep(24, n/2)
	if (sig) {
		pch.temp[which(x$significant[1:(n/2)] == 1)] <- 19
		pch.prec[which(x$significant[(n/2 + 1):n] == 1)] <- 17
	}	
	plot(1:n, x$coef, ylim = plot.range, xlab = "", ylab = "Coefficients", xaxt = "n", type = "n")
	lines(c(0.5, (n + 0.5)), c(0, 0), lty = 2, col = "grey")
	points(1:n, x$coef, pch = c(pch.temp, pch.prec)) # 24 + 17,  19 + 21
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