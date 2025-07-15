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

interpret_model_metaforest <- function(res_metaforest){

  shaps <- c("corruption" = "n", "power_distance" = "p", "GDP" = "n", "PISA.Reading" = "n", "pandemic_response" = "n",
             "individualism" = "n", "inequality" = "p", "PISA.Science" = "n", "PISA.Math" = "n", "indulgence" = "o",
             "political_stability" = 'n', "human_development_index" = "n", "university_graduates" = "n",
             "WEIRDness" = "o", "uncertainty_avoidance" = "o", "longterm_orientation" = "o",
             "masculinity" = "o", "hospitalbeds_per_1000_people" = "n", "dataset" = 'o')
  var_importance <- sort(res_metaforest$res$forest$variable.importance, decreasing = FALSE)
  var_importance <- data.frame(Variable = names(var_importance),
                               importance = unname(var_importance))
  var_importance$Variable <- ordered(var_importance$Variable, levels = var_importance$Variable)
  var_importance$Shape <- ordered(shaps[as.character(var_importance$Variable)], levels = c("p", 'n', 'o'), labels = c("Positive", "Negative", "Other"))
  library(ggplot2)
  vim_plot <- ggplot(var_importance, aes(y = Variable, x = importance, shape = Shape)) +
    geom_segment(aes(x = 0, xend = importance,
                     y = Variable, yend = Variable), colour = "grey50", linetype = 2) +
    geom_vline(xintercept = 0, colour = "grey50", linetype = 1) +
    geom_point(size = 2) + xlab("Variable Importance (Permutation importance)") +
    theme_bw() + theme(panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank(), axis.title.y = element_blank())

  ggsave("vim_plot.svg", vim_plot, device = "svg")
  set.seed(79974)
  pd_plot <- metaforest::PartialDependence(res_metaforest$res, vars = as.character(var_importance$Variable), pi = .95, rawdata = TRUE, output = "list")
  pd_plot[[1]] <- pd_plot[[1]] + scale_x_discrete(labels = substr(levels(pd_plot[[1]]$data$dataset), 1,1))
  pd_plot <- metaforest:::merge_plots(rev(pd_plot))
  ggsave("pd_plot.svg", pd_plot, device = "svg", width = 11)
  return(list(vimp = "vim_plot.svg", pd = "pd_plot.svg"))
}
