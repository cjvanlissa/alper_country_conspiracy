rsq_numeric <- function(obs, preds, mn){
  tss <- sum((obs-mn)^2)
  rss <- sum((preds - obs) ^ 2)
  return(1 - rss/tss)
}

eval_results <- function(dat, models){

  mses <- sapply(models, function(x){
    if("cvm" %in% names(x$mse_cv)){
      return(x$mse_cv["cvm"])
    } else {
      return(mean(x$mse_cv))
    }
  })
  mse_sds <- sapply(models, function(x){
    if("cvsd" %in% names(x$mse_cv)){
      return(x$mse_cv["cvsd"])
    } else {
      return(sd(x$mse_cv))
    }
  })
  rsqs_train <- sapply(models, `[[`, "rsq_train")
  # On test data
  rsqs_test <- sapply(models, `[[`, "rsq")

  df_rsq <- data.frame(
    mse = mses,
    mse_se = mse_sds,
    rsq_test = rsqs_test,
    rsq_train = rsqs_train,
    model = names(rsqs_test))

  # Choose best model
  #rsqs <- unlist(lapply(do.call(c, models), `[[`, "rsq"))
  best_model <- df_rsq$model[which.min(df_rsq$mse)]
  if(!best_model %in% c("BRMA", "MetaCART")){
    within_se <- df_rsq[df_rsq$model %in% c("BRMA", "MetaCART"), , drop = FALSE]
    within_se <- within_se[within_se$mse <= min(df_rsq$mse) + df_rsq$mse_se[which.min(df_rsq$mse)], , drop = FALSE]
    if(nrow(within_se) > 0){
      best_model <- within_se$model[which.min(within_se$mse)]
    }
  }
  return(
    list(
      rsqs = df_rsq,
      best = best_model,
      model = models[[best_model]]$res
    )
  )
}
