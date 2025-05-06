do_ranger <- function(dat){
  library(mlr)
  library(tuneRanger)
  library(ranger)
  reg_task = mlr::makeRegrTask(data = dat$train, target = "moral_concern", blocking = factor(dat$train_id))
  # Tuning
  res_tune_ranger = tuneRanger::tuneRanger(reg_task, measure = list(mse), tune.parameters = c("mtry", "min.node.size"))
  pred <- predict(res_tune_ranger$model$learner.model, data = dat$test)$predictions
  pred_train <- predict(res_tune_ranger$model$learner.model, data = dat$train)$predictions
  # Get cv error
  cv_mses <- sapply(dat$folds, function(f){
    Args <- as.list(res_tune_ranger$recommended.pars)
    Args[c("mse", "exec.time")] <- NULL
    Args <- c(Args,
              list(
                formula = quote(moral_concern ~ .),
                data = dat$train[-f, ]
              )
              )
    forest_model <- do.call(ranger::ranger, args = Args)
    preds <- ranger:::predict.ranger(forest_model, data = dat$train[f, ])$predictions
    mean((dat$train$moral_concern[f]-preds)^2)
    })

  out <- list(
    res_cv = res_tune_ranger,
    res = res_tune_ranger$model$learner.model,
    tune_pars = unlist(res_tune_ranger$recommended.pars)[c("mtry", "min.node.size")],
    mse_cv = cv_mses,
    rsq = rsq_numeric(dat$test$moral_concern, pred, mean(dat$train$moral_concern))
    , rsq_train = rsq_numeric(dat$train$moral_concern, pred_train, mean(dat$train$moral_concern))
  )
  class(out) <- "res_ranger"
  return(out)
}
