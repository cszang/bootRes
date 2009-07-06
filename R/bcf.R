bcf <-
function(g, p, sb) {
	require(utils)
	n <- length(g)
	m <- dim(p)[2]
	param.matrix <- matrix(NA, nrow = m, ncol = 1000)
	if (sb) { # initialize status bar (if TRUE)
		require(utils)
		pb <- txtProgressBar(min = 1,  max = 1000, style = 3)
	}
	for (i in 1:1000) {
		boot.sample <- sample(1:n, n, replace = TRUE)
		boot.g <- g[boot.sample]
		boot.p <- p[boot.sample, ]
		boot.g <- (boot.g - mean(boot.g))/sd(boot.g) # standardize
		boot.p <- apply(boot.p, 2, function(x) { (x - mean(x))/sd(x) }) # standardize
		for (j in 1:m) {
			param.matrix[j, i] <- qr.solve(boot.p[, j], boot.g) # get parameter via singular value decomp
		}
		if (sb) # update status bar (if TRUE)
			setTxtProgressBar(pb, i)
	}
	bcf.coef <- apply(param.matrix, 1, median)
	ci.lower <- apply(param.matrix, 1, function(x) { sort(x)[25] })
	ci.upper <- apply(param.matrix, 1, function(x) { sort(x)[975] }) 
	is.sig <- ifelse(abs(bcf.coef) > (ci.upper - ci.lower)/2, TRUE, FALSE)
	out <- cbind(coef = bcf.coef, significant = is.sig, ci.lower = ci.lower, ci.upper = ci.upper)
	rownames(out) <- colnames(p)
	if (sb) # close status bar (if TRUE)
		close(pb)
	as.data.frame(out)
}

