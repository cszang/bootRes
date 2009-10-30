dcplot <-
function(x, ci = TRUE, sig = TRUE, labels = NULL, col = NULL, density = NULL, annot = TRUE) {
	op <- par(no.readonly = TRUE)
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
	# TODO: set color/shading according to varnames
	pb <- barplot(x$coef)
	plot(1:n, x$coef, ylim = plot.range, xlab = "", ylab = "Coefficients", xaxt = "n", type = "n")
	lines(c(0.5, (n + 0.5)), c(0, 0), lty = 2, col = "grey")
	points(1:n, x$coef, pch = pchs)
	if (ci)
		segments(1:n, x$ci.lower, 1:n, x$ci.upper)
		text((1:n)+0.01*n, rep(plot.range[1]*1.2, n), labels, srt = 90, pos = 2, cex = 0.7)
	par(op)
}