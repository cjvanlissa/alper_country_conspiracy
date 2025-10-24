read_psychometrics_items <- function(){
codebooks <- readxl::read_xlsx("codebook.xlsx")
fnamz <- c("EVV" = "./EVV data.sav", "Hornsey" = "./hornsey individual data 130825.sav", "ManyLabs COVID" = "./ManyLabs COVID analysis data.sav",
           "PsyCorona" = "./psycorona analysis data.sav", "Rutjens" = "./rutjens data.sav", "YouGov"= "./yougov data counts.sav"
)
codebooks$fnam <- fnamz[codebooks$Sample]

output <- lapply(1:nrow(codebooks), function(i){
  f <- foreign::read.spss(codebooks$fnam[i], to.data.frame = TRUE, use.value.labels = FALSE)
  dat <- f[, c(codebooks$`Country Variable`[i], strsplit(codebooks$`Conspiracy Items`[i], ",")[[1]])]
  names(dat)[1] <- "Country"
  dat
})
names(output) <- codebooks$Sample
return(output)
}
