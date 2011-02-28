## non-bootstrapped version of brf
rf <- function(g, p, vnames) {
  n <- length(g)
  param.matrix <- matrix(NA, nrow = dim(p)[2], ncol = 1000)
  boot.sample <- sample(1:n, n, replace = TRUE)
  g <- (g - mean(g))/sd(g) # standardize
  p <- apply(p, 2, function(x) { (x - mean(x))/sd(x) }) # standardize
  cor.mat <- cor(p) # correlation matrix X'X (q*q)
  eigen.decomp <- eigen(cor.mat) # eigenvector decomposition
  eigenvectors <- eigen.decomp$vectors # normalized eigenvectors
  eigenvalues <- eigen.decomp$values
  cumprods <- cumprod(eigenvalues) # PVP criterion: calculate cumulative eigenvalues until value < 1
  reduced.eigenvectors <- eigenvectors[, cumprods > 1] # matrix of reduced eigenvectors (q*m)
  pc.scores <- p %*% reduced.eigenvectors # calculate princ comp scores (n*m)
  k <- qr.solve(pc.scores, g) # calculate solution for Z*K = Y (coefficients) (m*1)
  zeros <- rep(0, length(which(cumprods < 1))) # pad K with zero so that Kq*1
  k <- c(k, zeros) # (q*1)
  b <- eigenvectors %*% k # response coefficients (q*1)
  rf.coef <- b
  ci.lower <- NA
  ci.upper <- NA
  is.sig <- NA
  out <- cbind(coef = rf.coef, significant = is.sig, ci.lower = ci.lower, ci.upper = ci.upper)
  rownames(out) <- colnames(p)
  out <- as.data.frame(out)
  attributes(out)$npar <- attributes(p)$npar
  attributes(out)$vnames <- vnames
  out
}
