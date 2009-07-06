brf <-
function(g, p, sb) {
	n <- length(g)
	param.matrix <- matrix(NA, nrow = dim(p)[2], ncol = 1000)
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
		cor.mat <- cor(boot.p) # correlation matrix X'X (q*q)
		eigen.decomp <- eigen(cor.mat) # eigenvector decomposition
		eigenvectors <- eigen.decomp$vectors # normalized eigenvectors
		eigenvalues <- eigen.decomp$values
		cumprods <- cumprod(eigenvalues) # PVP criterion: calculate cumulative eigenvalues until value < 1
		reduced.eigenvectors <- eigenvectors[, cumprods > 1] # matrix of reduced eigenvectors (q*m)
		pc.scores <- boot.p %*% reduced.eigenvectors # calculate princ comp scores (n*m)
		k <- qr.solve(pc.scores, boot.g) # calculate solution for Z*K = Y (coefficients) (m*1)
		zeros <- rep(0, length(which(cumprods < 1))) # pad K with zero so that Kq*1
		k <- c(k, zeros) # (q*1)
		b <- eigenvectors %*% k # response coefficients (q*1)
		param.matrix[, i] <- b
		if (sb) # update status bar (if TRUE)
			setTxtProgressBar(pb, i)
	}
	brf.coef <- apply(param.matrix, 1, median)
	ci.lower <- apply(param.matrix, 1, function(x) { sort(x)[25] })
	ci.upper <- apply(param.matrix, 1, function(x) { sort(x)[975] }) 
	is.sig <- ifelse(abs(brf.coef) > (ci.upper - ci.lower)/2, TRUE, FALSE)
	out <- cbind(coef = brf.coef, significant = is.sig, ci.lower = ci.lower, ci.upper = ci.upper)
	rownames(out) <- colnames(p)
	if (sb) # close status bar (if TRUE)
		close(pb)
	as.data.frame(out)
}

