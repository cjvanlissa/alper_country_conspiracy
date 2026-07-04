# In this file, write the R-code necessary to load your original data file
# (e.g., an SPSS, Excel, or SAS-file), and convert it to a data.frame. Then,
# use the function open_data(your_data_frame) or closed_data(your_data_frame)
# to store the data.

library(worcs)
library(tidySEM)
library(lavaan)
# Item data -----------------------------------------------------------

shts <- readxl::excel_sheets("country-level data updated 180626_last.xlsx")
shts <- shts[-c(7:9)]
df <- lapply(shts, readxl::read_xlsx, path = "country-level data updated 180626_last.xlsx", na = c("", "NA", "na", "0  ?"))

# Examine descriptives
desc <- lapply(df, tidySEM::descriptives)
desc <- do.call(rbind, lapply(seq_along(shts), function(i){
  data.frame(desc[[i]], dataset = shts[i])
}))

df <- do.call(rbind, lapply(seq_along(shts), function(i){
  data.frame(df[[i]], dataset = shts[i])
}))

desc <- tidySEM::descriptives(df)
write.csv(desc, "descriptives.csv", row.names = FALSE)
if("conspiracy_shuffled" %in% names(df)){
  names(df)[match("conspiracy_shuffled", names(df))] <- "conspiracy"
}
df$vi <- df$SE_conspiracy^2
df[["SE_conspiracy"]] <- NULL
open_data(df)
