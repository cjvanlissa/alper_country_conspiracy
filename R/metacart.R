do_metacart <- function(dat, ...) {
  library(metacart)
  tuning_pars <- expand.grid(
    whichweights = c("REmrt", "FEmrt"),
    c.pruning = seq(
      from = 0,
      to = 1,
      by = .2
    ),
    stringsAsFactors = FALSE
  )

  cv_rmses <- sapply(1:nrow(tuning_pars), function(i) {
    sapply(dat$folds, function(thisfold) {
      Args <- list(
        formula = as.formula(paste0("conspiracy ~", paste0(setdiff(names(dat$train), c("conspiracy", "vi")), collapse = " + ")
        )),
        data = dat$train[-thisfold, ],
        vi = dat$train$vi[-thisfold],
        c.pruning = tuning_pars$c.pruning[i],
        xval = 10)

      fit_cv <- suppressWarnings(do.call(tuning_pars$whichweights[i], Args))
      preds <- suppressWarnings(unlist(predict(fit_cv, newdata = dat$train[thisfold, ])))
      mean((dat$train$conspiracy[thisfold] - preds)^2)
    })
  })

  cvm <- colMeans(cv_rmses)
  Args <- list(
    formula = as.formula(paste0("conspiracy ~", paste0(setdiff(names(dat$train), c("conspiracy", "vi")), collapse = " + ")
    )),
    data = dat$train,
    vi = dat$train$vi,
    c.pruning = tuning_pars$c.pruning[which.min(cvm)],
    xval = 10)
  best_model <- suppressWarnings(do.call(tuning_pars$whichweights[which.min(cvm)], Args))
  pred <- suppressWarnings(unlist(predict(best_model, newdata = dat$test)))
  pred_train <- suppressWarnings(unlist(predict(best_model, newdata = dat$train)))

  out <- list(
    res_cv = cv_rmses,
    res = best_model,
    tune_pars = as.vector(tuning_pars[which.min(cvm), , drop = FALSE]),
    mse_cv = c(cvm = min(cvm), cvsd = sd(cv_rmses[, which.min(cvm)])),
    rsq = rsq_numeric(
      dat$test$conspiracy,
      as.numeric(pred),
      mean(dat$train$conspiracy)
    ),
    rsq_train = rsq_numeric(
      dat$train$conspiracy,
      as.numeric(pred_train),
      mean(dat$train$conspiracy)
    )
  )
  class(out) <- "res_metacart"
  return(out)
}
