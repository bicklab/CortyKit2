#' @title gsutil
#'
#' @param text the gsutil commands
#' @param echo whether to print the command to screen before submitting to terminal
#'
#' @return terminal output from gsutil command
#' @export
gsutil = function(text, echo = FALSE) {
  text = glue::glue('gsutil -u {GOOGLE_PROJECT} ', text)
  if (echo) base::print(text)
  system(command = text, intern = TRUE)
}

#' @title google ls
#'
#' @param uri the URI to list
#' @param echo whether to print the command to screen before submitting to terminal
#'
#' @return
#' @export
#'
#' @examples
gsls = function(uri, echo = TRUE) {
	gsutil(paste('ls', text))
}

#' @gscp google copy
#'
#' @param src source
#' @param dest destination
#' @param echo whether to print the command to screen before submitting to terminal#'
#'
#' @return terminal output
#' @export
gscp = function(src, dest, echo = TRUE) {
	gsutil(paste('cp', src, dest))
}
