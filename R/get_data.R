get_data <- function(){
  out <- worcs::load_data(to_envir = FALSE)$df
  out$PISA <- rowMeans(out[, grep("pisa", names(out), ignore.case = T, value = T)], na.rm = TRUE)
  out[grep("pisa_", names(out), ignore.case = T, value = T)] <- NULL
  class(out) <- "data.frame"
  return(out)
}

