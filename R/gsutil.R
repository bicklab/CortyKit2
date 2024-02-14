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
  base::system(command = text, intern = TRUE)
}

