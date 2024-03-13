#' @title checkpoint saves/loads data from local disk or google bucket
#'
#' @param x the object in R
#' @param dir the save directory
#' @param local_disk binary indicating whether to save to local disk
#' @param google_bucket binary indicating whether to save to google bucket
#'
#' @return TRUE
#' @export
checkpoint = function(x, dir, local_disk = TRUE, google_bucket = TRUE) {

	var_name = deparse(substitute(x))
	pd_file_path = glue::glue('~/{dir}/{var_name}.csv')
	gcs_file_path = glue::glue('{GOOGLE_BUCKET}/{dir}/{var_name}.csv')

	# if the variable is in the R session, save it to PD and/or GCS
	if (var_name %in% ls(parent.env(environment()))) {
		if (local_disk) { arrow::write_csv_arrow(x = x, file = pd_file_path) }
		if (google_bucket) {
			gsutil('-m cp {pd_file_path} {GOOGLE_BUCKET}/{dir}/')
			if (rlang::is_empty(system(glue::glue('gsutil ls {gcs_file_path}'), intern = TRUE))) {
				stop('failed to save to GCS')
			}
			return(x)
		}

		# if the variable is not in the R session we need to find it saved somewhere...
	} else {

		stop('using checkpoint to read is currently dis-implemented')

		# # if it's on the persistent disk, read it in from there
		# if (file.exists(pd_file_path)) {
		# 	return(arrow::read_csv_arrow(pd_file_path))
		# }
		#
		# # otherwise check for it on google cloud storage
		# if (!rlang::is_empty(system(glue('gsutil ls {gcs_file_path}'), intern = TRUE))) {
		# 	# if we find it, transfer it to PD then read it in and return
		# 	gsutil('-m cp {gcs_file_path} {pd_file_path}')
		# 	return(arrow::read_csv_arrow(pd_file_path))
		# }
		#
		# # otherwise, error out
		# stop(glue::glue('{var_name} not in R session, persistent disk, or google bucket'))
	}
}
