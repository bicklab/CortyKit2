#' @title onLoad
#'
#' @param libname not used
#' @param pkgname not used
#'
#' @return invisible
#' @export
#'
.onLoad = function(libname, pkgname) {
	GOOGLE_PROJECT <<- Sys.getenv('GOOGLE_PROJECT')
	GOOGLE_BUCKET <<- Sys.getenv('WORKSPACE_BUCKET')

	# assign(
	# 	"GOOGLE_PROJECT",
	# 	Sys.getenv('GOOGLE_PROJECT'),
	# 	envir = parent.env(environment()))
	#
	# assign(
	# 	"GOOGLE_BUCKET",
	# 	Sys.getenv('WORKSPACE_BUCKET'),
	# 	envir = parent.env(environment()))

	invisible()
}
