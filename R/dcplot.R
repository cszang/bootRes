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
	if (is.null(density)) { # check, if density is set, if not use colors, otherwise override colors
		if (is.null(col)) { # check if custom colors are specified; if not use default colors
			# TODO: define default colors
		} else {
			# TODO: check defined colors for consistence and apply
		}
		if (sig) {
			# TODO: set different colors for significant values
		}
		pb <- barplot(x$coef, ylim = plot.range, col = col) # draw plot
	} else {
		# TODO: check defined densities and apply
		if (sig) {
			# TODO: set different densities for significant values
		}
		pb <- barplot(x$coef, ylim = plot.range, density = density) # draw plot
	}
	if (ci) { # check if confidence intervals should be plotted
		# TODO: add confidence intervals
	}
	
	plot(1:n, x$coef, ylim = plot.range, xlab = "", ylab = "Coefficients", xaxt = "n", type = "n")
	lines(c(0.5, (n + 0.5)), c(0, 0), lty = 2, col = "grey")
	points(1:n, x$coef, pch = pchs)
	if (ci)
		segments(1:n, x$ci.lower, 1:n, x$ci.upper)
		text((1:n)+0.01*n, rep(plot.range[1]*1.2, n), labels, srt = 90, pos = 2, cex = 0.7)
	par(op)
}