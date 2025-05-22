split_train_test <- function(df, k = 10){
  # Scale DV per dataset
  mean_per_dataset <- tapply(df$conspiracy, factor(df$dataset), mean)
  sd_per_dataset <- tapply(df$conspiracy, factor(df$dataset), sd)
  df$conspiracy <- as.numeric((df$conspiracy - mean_per_dataset[df$dataset])/sd_per_dataset[df$dataset])
  df$vi <- abs(rnorm(nrow(df), sd = .01))

  df$dataset <- factor(df$dataset)
  df[["Country"]] <- NULL

  # Split the data into train and test data sets ----------------------------
  n <- nrow(df)
  train <- sample.int(n, size = floor(.7*n))
  test_id <- setdiff(1:nrow(df), train)
  df_train <- df[train, ]
  df_test <- df[-train, ]

  # Create k-folds ----------------------------------------------------------
  all.folds <- split(sample.int(nrow(df_train)), cut(1:nrow(df_train), k, labels=FALSE))

  # Clean Data

  facs <- names(df_train)[sapply(df_train, inherits, what = "factor")]
  nums <- names(df_train)[sapply(df_train, inherits, what = c("numeric", "integer"))]
  nums <- setdiff(nums, c("conspiracy", "vi")) # Exclude DV and vi
  scld <- scale(df_train[nums], center = TRUE, scale = TRUE)
  means <- attr(scld, "scaled:center")
  sds <- attr(scld, "scaled:scale")
  df_train[nums] <- scld

  scld_test <- df_test[nums]
  scld_test <- sweep(scld_test, 2, means)
  scld_test <- sweep(scld_test, 2, sds, FUN = "/")
  df_test[nums] <- scld_test

  return(
    list(
      train = df_train,
      test = df_test,
      train_id = train,
      test_id = test_id,
      folds = all.folds,
      train_means = means,
      train_sds = sds
    )
  )
}
