do_brma <- function(dat, ...){
  yvar = "conspiracy"
  # X <- model.matrix(as.formula(paste0(yvar, "~", paste0(setdiff(names(dat$train), c(yvar, "vi")), collapse = " + "))), dat$train)[, -1]
  # Y <- as.numeric(dat$train[[yvar]])
  # vi <- dat$train$vi
  # all.folds <- dat$folds
  tuning_pars <- expand.grid(
    relevant_pars = 1:3
  )

  cv_rmses <- sapply(1:nrow(tuning_pars), function(i){
    sapply(dat$folds, function(thisfold){
    fit_cv <- pema::brma(
      formula = as.formula(paste0(yvar, "~", paste0(setdiff(names(dat$train), c(yvar, "vi")), collapse = " + "))),
      data = dat$train[-thisfold, ],
      method = "hs",
      prior = c(relevant_pars = tuning_pars$relevant_pars[i]),
      ...
      )
    preds <- pema:::predict.brma(fit_cv, newdata = dat$train[thisfold, ], type = c("mean"))
    mean((dat$train$conspiracy[thisfold] - preds)^2)
  })
  })

  cvm <- colMeans(cv_rmses)
  best_model <- pema::brma(
    formula = as.formula(paste0(yvar, "~", paste0(setdiff(names(dat$train), c(yvar, "vi")), collapse = " + "))),
    data = dat$train,
    method = "hs",
    prior = c(relevant_pars = tuning_pars$relevant_pars[which.min(cvm)]),
    ...
  )
  pred <- pema:::predict.brma(best_model, newdata = dat$test, type = c("mean"))
  pred_train <- pema:::predict.brma(best_model, newdata = dat$train, type = c("mean"))
    out <- list(
      res_cv = cv_rmses,
      res = best_model,
      tune_pars = as.vector(tuning_pars[which.min(cvm), , drop = FALSE]),
      mse_cv = c(cvm = min(cvm),
                 cvsd = sd(cv_rmses[, which.min(cvm)])),
      rsq = rsq_numeric(dat$test$conspiracy, as.numeric(pred), mean(dat$train$conspiracy)),
      rsq_train = rsq_numeric(dat$train$conspiracy, as.numeric(pred_train), mean(dat$train$conspiracy))
    )
    class(out) <- "res_brma"
    return(out)
}
