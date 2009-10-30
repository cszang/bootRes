dcplot <-
function(x, ci = TRUE, sig = TRUE, labels = NULL, col = NULL, density = NULL, annot = TRUE) {
	op <- par(no.readonly = TRUE) 
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
	if (annot) { # check whether "nice" annotations should be made
		# TODO: extract monthnames from labels and pass to some variable
	} else {
		# TODO: overwrite o.m. variable with original labels
	}
	# TODO: set color/shading according to number of varnames
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
	par(op)
}