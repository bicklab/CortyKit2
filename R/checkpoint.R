checkpoint = function(x, dir, PD = TRUE, GCS = TRUE) {

  var_name = deparse(substitute(x))
  pd_file_path = glue('~/{dir}/{var_name}.csv')
  gcs_file_path = glue('{GOOGLE_BUCKET}/{dir}/{var_name}.csv')

  # if the variable is in the R session, save it to PD and/or GCS
  if (var_name %in% ls(parent.env(environment()))) {
    if (PD) { write_csv_arrow(x = x, file = pd_file_path) }
    if (GCS) {
      gsutil('-m cp {pd_file_path} {GOOGLE_BUCKET}/{dir}/')
      if (rlang::is_empty(system(glue('gsutil ls {gcs_file_path}'), intern = TRUE))) {
        stop('failed to save to GCS')
      }
      return(TRUE)
    }

    # if the variable is not in the R session we need to find it saved somewhere...
  } else {

    # if it's on the persistent disk, read it in from there
    if (file.exists(pd_file_path)) {
      return(read_csv_arrow(pd_file_path))
    }

    # otherwise check for it on google cloud storage
    if (!rlang::is_empty(system(glue('gsutil ls {gcs_file_path}'), intern = TRUE))) {
      # if we find it, transfer it to PD then read it in and return
      gsutil('-m cp {gcs_file_path} {pd_file_path}')
      return(read_csv_arrow(pd_file_path))
    }

    # otherwise, error out
    stop(glue('{var_name} not in R session, persistent disk, or google bucket'))
  }
}
