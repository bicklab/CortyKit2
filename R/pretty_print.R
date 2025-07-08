#' @title pretty print
#'
#' @param x the object to print
#'
#' @returns a string version of the object to print
#' @export
pp = function(x) {
	if (is.numeric(x)) {
		return(formatC(x, format = "f", digits = 0, big.mark = "_"))
	}
	return(x)
}
