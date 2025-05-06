do_psychometrics <- function(datasets){
  res <- lapply(names(datasets), function(n){
    dat <- datasets[[n]]
    if(ncol(dat) < 4){
      return(NULL)
    }
    out <- try({
      # Model 1: configural invariance. The same factor structure is imposed on all groups.
      model <- paste0("consp =~ ", paste0(names(dat)[-ncol(dat)], collapse = " + "))
      fit1 <- lavaan::cfa(model, data = dat, group = "Country")
      # Model 2: weak invariance. The factor loadings are constrained to be equal across groups.
      fit2 <- lavaan::cfa(model, data = dat, group = "Country", group.equal = "loadings")
      # Model 3: strong invariance. The factor loadings and intercepts are constrained to be equal across groups.
      fit3 <- lavaan::cfa(model, data = dat, group = "Country",
                  group.equal = c("intercepts", "loadings"))
      # model comparison tests
      fits <- data.frame(dataset = n, invariance = c("configural", "weak", "strong"), t(sapply(list(fit1, fit2, fit3), function(x) tidySEM::table_fit(x)[, c("rmsea", "srmr", "cfi", "tli")])))
      tst <- lavaan::lavTestLRT(fit1, fit2, fit3)
      cbind(fits, tst)
    })
    if(inherits(out, "try-error")) return(NULL)
    out
  })

  res_psychometrics <- tidySEM:::bind_list(res)
  res_psychometrics[] <- lapply(res_psychometrics, unlist)
  return(res_psychometrics)
}
