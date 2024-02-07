gsutil = function(text, echo = FALSE) {
  text = glue::glue('gsutil -u {GOOGLE_PROJECT} ', text)
  if (echo) base::print(text)
  base::system(command = text, intern = TRUE)
}

