get_data <- function(){
  out <- worcs::load_data(to_envir = FALSE)$df
  class(out) <- "data.frame"
  return(out)
}

