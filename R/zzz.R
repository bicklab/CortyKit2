#' @title onLoad
#'
#' @param libname not used
#' @param pkgname not used
#'
#' @return invisible
#' @export
.onLoad = function(libname, pkgname) {

	# on terra.bio
	if (Sys.getenv('TERRA_DEPLOYMENT_ENV') == 'prod') {
		WORKSPACE_BUCKET <<- Sys.getenv('WORKSPACE_BUCKET')
		WORKSPACE_NAME <<- Sys.getenv('WORKSPACE_NAME')
		WORKSPACE_NAMESPACE <<- Sys.getenv('WORKSPACE_NAMESPACE')
	}

	# on allofus
	if (Sys.getenv('AOU_DOCKER_VERSION') != '') {
		WORKSPACE_BUCKET <<- Sys.getenv('WORKSPACE_BUCKET')
		WORKSPACE_NAME <<- Sys.getenv('WORKSPACE_NAME')
		WORKSPACE_NAMESPACE <<- Sys.getenv('WORKSPACE_NAMESPACE')
	}


	invisible()
}
