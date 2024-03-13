#' @title qqunif
#'
#' @description
#' simple qq plotting function that assumes theoretical distribution is uniform
#'
#'
#' @param ps the p-values
#'
#' @return a QQ plot
#' @export
qqunif = function(ps) {
	ps = na.omit(ps)
	n = length(ps)
	xs = (1:n)/n
	ys = sort(ps)
	plot(-log10(xs), -log10(ys), asp = 1)
	abline(a = 0, b = 1)
}
