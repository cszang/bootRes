dcbarplot <- function(x, labels, ci = TRUE, sig = TRUE, col =
                      "grey75", sig.col = "grey25") {
  if (is.null(labels))
    labels <- rownames(x)
  n <- dim(x)[1]
  bp <- barplot(x$coef)
}
