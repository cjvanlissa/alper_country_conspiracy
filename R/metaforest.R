do_metaforest <- function(dat, ...) {
  yvar = "conspiracy"
  tuning_pars <- expand.grid(
    whichweights = c("random", "fixed", "unif"),
    mtry = seq.int(
      from = 1,
      to = ceiling(.8 * ncol(dat$train)),
      length.out = 3
    ),
    min.node.size = round(seq.int(
      from = 10,
      to = ceiling(.2 * nrow(dat$train)),
      length.out = 3
    )),
    stringsAsFactors = FALSE
  )
  cv_rmses <- sapply(1:nrow(tuning_pars), function(i) {
    sapply(dat$folds, function(thisfold) {
      Args <- list(
        formula = as.formula(paste0(
          yvar, "~", paste0(setdiff(names(dat$train), c(yvar, "vi")), collapse = " + ")
        )),
        data = dat$train[-thisfold, ],
        whichweights = tuning_pars$whichweights[i],
        mtry = tuning_pars$mtry[i],
        min.node.size = tuning_pars$min.node.size[i]
      )
      fit_cv <- do.call(metaforest::MetaForest, Args)
      preds <- metaforest:::predict.MetaForest(fit_cv, data = dat$train[thisfold, ])$predictions
      mean((dat$train$conspiracy[thisfold] - preds)^2)
    })
  })

  cvm <- colMeans(cv_rmses)
  Args <- list(
    formula = as.formula(paste0(
      yvar, "~", paste0(setdiff(names(dat$train), c(yvar, "vi")), collapse = " + ")
    )),
    data = dat$train,
    whichweights = tuning_pars$whichweights[which.min(cvm)],
    mtry = tuning_pars$mtry[which.min(cvm)],
    min.node.size = tuning_pars$min.node.size[which.min(cvm)]
  )

  best_model <- do.call(metaforest::MetaForest, Args)
  pred <- metaforest:::predict.MetaForest(best_model, data = dat$test)$predictions

  pred_train <- metaforest:::predict.MetaForest(best_model, data = dat$train)$predictions

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
  class(out) <- "res_metaforest"
  return(out)
}
