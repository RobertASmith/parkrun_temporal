output_stargazer <- function(output.file, ...) {
  
  output <- capture.output(stargazer(...))
  
  cat(paste(output, collapse = "\n"), "\n", 
      file=output.file, 
      append=FALSE)
}