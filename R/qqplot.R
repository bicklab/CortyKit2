#' @title qqunif
#'
#' @description
#' simple qq plotting function that assumes theoretical distribution is uniform
#'
#' @param ps the p-values (between 0 and 1)
#'
#' @return a QQ plot
#' @export
qqunif = function(ps) {
	ps = na.omit(ps)
	n = length(ps)
	xs = -log10((1:n)/n)
	ys = -log10(sort(ps))
	themax = max(c(xs, ys))
	plot(x = xs, y = ys,
			 xlim = c(0, themax),
			 ylim = c(0, themax),
			 xlab = '-log10(uniform)',
			 ylab = '-log10(sample)')
	abline(a = 0, b = 1)
}
