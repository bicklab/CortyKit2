#' @title onLoad
#'
#' @param libname not used
#' @param pkgname not used
#'
#' @return invisible
#' @export
#'
.onLoad = function(libname, pkgname) {
	GOOGLE_PROJECT = Sys.getenv('GOOGLE_PROJECT')
	GOOGLE_BUCKET = Sys.getenv('WORKSPACE_BUCKET')

	invisible()
}
