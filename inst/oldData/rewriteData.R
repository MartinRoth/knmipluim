# Transfer .dat examples to .rda data
tnFiles <- c("tn260_1981_2010.dat", "tn260_GL_2050.dat", "tn260_WH_2050.dat")
txFiles <- c("tx260_1981_2010.dat", "tx260_GL_2050.dat", "tx260_WH_2050.dat")

library(stringr)
library(foreach)
library(iterators)

rewriteData <- function(fileName, variable) {
  baseName <- str_split(fileName, "\\.")[[1]][1]

  tmp <- fread(paste0("inst/oldData/",fileName))
  setnames(tmp, c("date", variable))
  tmp[, name := baseName]
  tmp[, date := as.Date(paste(date), format="%Y%m%d")]
  setcolorder(tmp, c(3,1,2))

  # assign(baseName, fread(paste0("inst/oldData/",fileName)))
  # setnames(get(baseName), c("date", variable))
  # get(baseName)[, date := as.Date(paste(date), format="%Y%m%d")]
  #
  # save(list=baseName, file = paste0("data/", baseName, ".rda"))
  tmp
}

tnDat <- foreach (file = iter(tnFiles), .combine = "rbind") %do% {
  rewriteData(file, "tn")
}

txDat <- foreach (file = iter(txFiles), .combine = "rbind") %do% {
  rewriteData(file, "tx")
}

for (i in 1 : 3) {
  fileName <- txFiles[i]
  rewriteData(fileName, "tx")
}
