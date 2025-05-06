set.seed(1)
shts <- readxl::excel_sheets("descriptives.xlsx")
descs <- lapply(shts, readxl::read_xlsx, path = "descriptives.xlsx")
names(descs) <- shts
shts <- readxl::excel_sheets("Country level factors 070325_shuffled.xlsx")[-1]
df <- lapply(shts, readxl::read_xlsx, path = "Country level factors 070325_shuffled.xlsx", na = c("", "NA", "na", "0  ?"))
names(df) <- shts

countries <- table(unlist(lapply(df, `[[`, "Country")))
countries[] <- runif(length(countries), -.1, .1)

datasets <- lapply(shts, function(dataset){
  dsc <- descs[[dataset]]
  cntries <- df[[dataset]]$Country
  n_per_c <- sample(100:200, size = length(cntries))
  truescore <- rnorm(n = sum(n_per_c)) +
    unlist(mapply(function(mn, tims){ rep(mn, tims) }, mn = countries[cntries], tims = n_per_c))
  itemscores <- matrix(truescore, nrow = length(truescore), ncol = nrow(dsc))
  itemscores <- itemscores + rnorm(n = length(itemscores), sd = .1)
  itemscores <- data.frame(apply(itemscores, 2, cut, breaks = dsc$Range[1]+1L, labels = FALSE))
  names(itemscores) <- dsc$Item
  itemscores$Country <- unlist(sapply(seq_along(cntries), function(i) rep(cntries[i], n_per_c[i])))
  itemscores
})
names(datasets) <- shts
saveRDS(datasets, "sim_datasets.RData")
