gsutil = function(text) {
  text = glue('gsutil -u {GOOGLE_PROJECT} ', text)
  print(text)
  system(command = text, intern = TRUE)
}

